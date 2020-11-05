#Setting up the data is essentially the same process as the WHO Afro report 
#Download the data from the WHO COVID-190 dashboard, change the date format to yyyy-mm-dd, change Côte d'Ivoire to Cote d'Ivoire
#Set the working directory to the Master folder


# 1) SET UP ----

today<- Sys.Date() -1# Set date as to that of the data to fetch.
iter = 1000 # Number of iterations for the poisson error simulation (bootstrap), Set to 1000. Or 10 for a quick test.
set.seed(as.numeric(today)) # setting seed allows repeatability of poisson error simulations. Use the date as a reference point for the seed.

source('script/sourced_functions_Weekly_Ratio_reports.R') # Source several functions used in the below script. See README file for details. 

time_window<- 7 # Time window over which doubling time is calculated
t2.define<- today
t1.define<- t2.define - time_window

# library to read excel files
library(readxl)
# large library of generic data science tools for manipulating data
library(tidyverse)
library(magrittr)
# import a relatively sensible colour palette
library(RColorBrewer)
# class interval library to group countries together with similar cases/deaths counts 
# so that they are given the same colour
library(classInt)
# libraries to create all of the maps (require a lot of other packages to work)
library(sf)
library(geojsonio)
library(cartography)
# library with useful function for reading/manipulating dates
library(lubridate)
# library to manipulate images
library(magick)
# rolling windows for weekly ratios
library(RcppRoll)
#Packages for producing animations
library(gganimate)
library(transformr)
library(viridis)
library(RColorBrewer)
library(animation)
library(magick)
library(viridis)


# LOADING DATA ----

# WHO list of countries & their popsize.
# Various encoding of the countries name used for different purpose (match various data sources, for maps, shortened for figures etc.)
who_country_aliases_and_populations<-
  read.csv('input_files/Africa_country_list.csv', stringsAsFactors = FALSE) %>%
  rename( popsize = pop)


## Download data from https://covid19.who.int/table and read it in
data <- 
  read.csv(paste0('./data/', today, '/WHO-COVID-19-global-data.csv'), stringsAsFactors = FALSE) %>%
  rename(Date_reported=Date_reported) %>%
  mutate(Date_reported=as.Date(Date_reported, format = "%Y-%m-%d"))

## Extract data for WHO AFRO
africa_data <- data %>%
  select(.,c(1,3,6,8))%>%
  rename("date"="Date_reported",
         'country'="Country",
         'cum_cases'="Cumulative_cases" ,
         "cum_deaths"="Cumulative_deaths")

# All columns in WHO_cases_and_deaths are WHO countries.
# After modifying "Côte d'Ivoire" into "Cote d'Ivoire" (without the ^), and `São Tomé and Príncipe into Sao Tome and Principe, then the variable "country" of the who_country_aliases_and_populations is the one that matches the countries name of the data
# + formatting variable names to fit in rest of the script

WHO_cases_and_deaths<- africa_data %>% 
  mutate_at('country', ~replace(., .=="Côte d’Ivoire" , "Cote d'Ivoire"))%>% 
  # revert back to per day deaths / cases
  group_by(country) %>%
  mutate(cum_cases=as.integer(cum_cases),cum_deaths=as.integer(cum_deaths),cases = as.integer(c(0,diff(cum_cases))), deaths = as.integer(c(0,diff(cum_deaths)))) %>%
  ungroup() %>%
  # convert to per 10k pop
  inner_join(who_country_aliases_and_populations %>% 
               select(country,popsize)) %>% 
  mutate(popsize = popsize / 10000,cum_cases_per_10k=cum_cases / popsize,cum_deaths_per_10k=cum_deaths / popsize,cases_per_10k=cases / popsize,deaths_per_10k=deaths / popsize)


# check data is complete
if(!(WHO_cases_and_deaths %>% 
     group_by(country) %>% 
     summarise(n()) %>%
     distinct(n()) %>%
     length()) == 1) stop('Some countries are missing reporting dates')

if(sum(is.na(WHO_cases_and_deaths$cases)) > 0 | sum(is.na(WHO_cases_and_deaths$deaths)) > 0) stop('Some countries have NA entries')

who_countrywide_data <-  
  WHO_cases_and_deaths[which(WHO_cases_and_deaths$date == as.Date(format(today,"%Y/%m/%d"))),]%>% 
  select(.,c(1,2,3,4,8,9)) %>%
  inner_join(who_country_aliases_and_populations %>% 
               select(country,ISO3))  


WHO_cases_and_deaths_weekly_ratio <- WHO_cases_and_deaths %>%
  left_join(weekly_ratios_2(WHO_cases_and_deaths,"cases")) %>%
  left_join(weekly_ratios_2(WHO_cases_and_deaths,"deaths"), suffix= c("_c","_d"), by = c("country", "date"))


#If I am testing anything new on the maps I remove the # from "filter(date == today)", so that you don"t have to wait for the entire animation to run to see any changes 

who_WR_data<- # Assemble the weekly ratios formatted for maps plotting
  who_country_aliases_and_populations[,c('ISO3', 'country')] %>%
  left_join(WHO_cases_and_deaths_weekly_ratio %>%
              select(date, country,ratio_c,ratio_d), by = 'country') %>%
  #filter(date == today) %>%
  rename(WR_cases = ratio_c,
         WR_deaths = ratio_d,
         countryterritoryCode = ISO3) %>%
  arrange(country)
  
  who_WR_data$WR_cases[who_WR_data$WR_cases == 0]<- 0.00000001 
  who_WR_data$WR_deaths[who_WR_data$WR_deaths == 0]<- 0.0000001
  who_WR_data$WR_cases[!is.finite(who_WR_data$WR_cases)]<- 0 
  who_WR_data$WR_deaths[!is.finite(who_WR_data$WR_deaths)]<- 0
  who_WR_data$WR_cases[who_WR_data$WR_cases < 0]<- 0 
  who_WR_data$WR_deaths[who_WR_data$WR_deaths < 0]<- 0



#Creating Weekly ratio case map MP4 
  
  #Loading geographical data
  
library(geojsonio)
africa <- geojson_read("./input_files/Africa1.geojson", what="sp")
data<- who_WR_data

 #Tidying data so it is compatible with the Geojson file

library(broom)
africa_fortified <- tidy(africa, region = "NAME") %>%
  rename(NAME = id)


africa@data %<>% left_join(data %>% select(-country), by=c("ISO_A3"="countryterritoryCode"))


africa_fortified<- left_join(africa_fortified, africa@data, by= "NAME") %>%
  select(long, lat, group, NAME, date, WR_cases, WR_deaths)



#Check the number of NA values in data 
africa_fortified_NAs<- africa_fortified %>% filter(is.na(date))


#Assign NA to values for all Non WHO countries WR cases so that they come up as grey on the maps
africa_fortified$WR_cases[africa_fortified$NAME == "Djibouti"] <- NA
africa_fortified$WR_cases[africa_fortified$NAME == "Egypt"] <- NA
africa_fortified$WR_cases[africa_fortified$NAME == "Libya"] <- NA
africa_fortified$WR_cases[africa_fortified$NAME == "Morocco"] <- NA
africa_fortified$WR_cases[africa_fortified$NAME == "Somalia"] <- NA
africa_fortified$WR_cases[africa_fortified$NAME == "Somaliland"] <- NA
africa_fortified$WR_cases[africa_fortified$NAME == "Sudan"] <- NA
africa_fortified$WR_cases[africa_fortified$NAME == "Tunisia"] <- NA
africa_fortified$WR_cases[africa_fortified$NAME == "Western Sahara"] <- NA


#Remove any countries which still have NA date values, these are not included in the WHO dataset 
africa_fortified<- africa_fortified%>% drop_na(date) 


# Test Africa map outline
library(ggplot2)
ggplot() +
  geom_polygon(data = africa, aes( x = long, y = lat, group = group), fill="white", color="black") +
  theme_void() +
  coord_map()

#sorting breaks, the last value should be the maximum that is present in the data (africa_fortified$WR_cases)

africa_fortified$brks <- cut(africa_fortified$WR_cases, 
                   breaks=c(-1, 0, 0.5, 1, 2, 132), 
                   labels=c("0", "< 0.5", "0.5- 1", "1 - 2", 
                            "> 2"))

#Creating the Map 


 WRcaseMap <-ggplot() +
  geom_polygon(data = africa_fortified, aes(fill = brks, x = long, y = lat, group = group), 
                size=0.15) +
  geom_path(data = africa_fortified, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.2) +
  theme_void() +
  scale_fill_manual(name= "Weekly Ratio Cases", na.value = "grey",
                    guide = guide_legend( keyheight = unit(5, units = "mm"), keywidth=unit(15, units = "mm")), 
                    labels = c("0", "< 0.5", "0.5 - 1", "1 - 2", "> 2", "Non-WHO Country"),
                    values = c("#FFFFFF", "#339900", "#99CC33", "#FFCC66", "#CC0000")) +
  theme(legend.position="left") +
  coord_map() +
  labs(title = "Date: {frame_time}", 
       caption = "Data: WHO COVID-19 Dashboard") +
   theme(plot.title = element_text(size= 26, face = "bold", hjust=0.01, color = "#4e4d47", 
                                   margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
         plot.caption = element_text( size=20, color = "#4e4d47", margin = margin(b = 1.0, r=-99, unit = "cm") ),
         legend.background = element_rect(fill = "#f5f5f2", color = NA),
         legend.text = element_text(size = 22, face= "bold"),
         legend.title = element_text(size = 24, face ="bold")) +
   transition_time(date) 
  
#Animating the map  
   
WRCasemapMP4 <- animate(WRcaseMap,
                  nframes = length(unique(africa_fortified$date)), 
                  fps = 5, 
                  duration = 90, 
                  width = 950, 
                  height = 750,
                  renderer = ffmpeg_renderer(
                    format = "mp4",
                    ffmpeg = NULL,
                    options = list(pix_fmt = "yuv420p")))

anim_save("WeeklyRatioCasesMap.mp4", animation=WRCasemapMP4)


#Repeating the process, but for WR deaths this time 
library(geojsonio)
africa <- geojson_read("./input_files/Africa1.geojson", what="sp")
data<- who_WR_data

library(broom)
africa_fortified <- tidy(africa, region = "NAME") %>%
  rename(NAME = id)


africa@data %<>% left_join(data %>% select(-country), by=c("ISO_A3"="countryterritoryCode"))


africa_fortified<- left_join(africa_fortified, africa@data, by= "NAME") %>%
  select(long, lat, group, NAME, date, WR_cases, WR_deaths)



#Check the number of NA values in data 
africa_fortified_NAs<- africa_fortified %>% filter(is.na(date))


#Assign NA to values for all Non WHO countries WR Deaths so that they come up as grey on the maps
africa_fortified$WR_deaths[africa_fortified$NAME == "Djibouti"] <- NA
africa_fortified$WR_deaths[africa_fortified$NAME == "Egypt"] <- NA
africa_fortified$WR_deaths[africa_fortified$NAME == "Libya"] <- NA
africa_fortified$WR_deaths[africa_fortified$NAME == "Morocco"] <- NA
africa_fortified$WR_deaths[africa_fortified$NAME == "Somalia"] <- NA
africa_fortified$WR_deaths[africa_fortified$NAME == "Somaliland"] <- NA
africa_fortified$WR_deaths[africa_fortified$NAME == "Sudan"] <- NA
africa_fortified$WR_deaths[africa_fortified$NAME == "Tunisia"] <- NA
africa_fortified$WR_deaths[africa_fortified$NAME == "Western Sahara"] <- NA


#Remove an countries which still have NA date values, these are not included in the WHO dataset 
africa_fortified<- africa_fortified%>% drop_na(date)


africa_fortified$brks <- cut(africa_fortified$WR_deaths, 
                             breaks=c(-1, 0, 0.5, 1, 2, 132), 
                             labels=c("0", "< 0.5", "0.5- 1", "1 - 2", 
                                      "> 2"))


WRdeathsMap<- ggplot() +
  geom_polygon(data = africa_fortified, aes(fill = brks, x = long, y = lat, group = group), 
               size=0.15) +
  geom_path(data = africa_fortified, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.2) +
  theme_void() +
  scale_fill_manual(name= "Weekly Ratio Deaths", na.value = "grey",
                    guide = guide_legend( keyheight = unit(5, units = "mm"), keywidth=unit(15, units = "mm")), 
                    labels = c("0", "< 0.5", "0.5 - 1", "1 - 2", "> 2", "Non-WHO Country"),
                    values = c("#FFFFFF", "#339900", "#99CC33", "#FFCC66", "#CC0000")) +
  theme(legend.position="left") +
  coord_map() +
  labs(title = "Date: {frame_time}", 
       caption = "Data: WHO COVID-19 Dashboard") +
  theme(plot.title = element_text(size= 26, face = "bold", hjust=0.01, color = "#4e4d47", 
                                  margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.caption = element_text( size=20, color = "#4e4d47", margin = margin(b = 1.0, r=-99, unit = "cm") ),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.text = element_text(size = 22, face= "bold"),
        legend.title = element_text(size = 24, face ="bold")) +
  transition_time(date) 


WRdeathsMapMP4 <- animate(WRdeathsMap,
                  nframes = length(unique(africa_fortified$date)), 
                  fps = 5, 
                  duration = 90, 
                  width = 950, 
                  height = 750,
                  renderer = ffmpeg_renderer(
                    format = "mp4",
                    ffmpeg = NULL,
                    options = list(pix_fmt = "yuv420p")))

anim_save("WeeklyRatioDeathsMap.mp4", animation=WRdeathsMapMP4)



#Cum case per 10k map gif 


library(geojsonio)
africa <- geojson_read("./input_files/Africa1.geojson", what="sp")

data<- who_country_aliases_and_populations[,c('ISO3', 'country')] %>%
  left_join(WHO_cases_and_deaths %>%
              select(date, country,cum_cases_per_10k, cum_deaths_per_10k), by = 'country') %>%
  #filter(date == today) %>%
  rename(countryterritoryCode = ISO3) %>%
  arrange(country)

library(broom)
africa_fortified <- tidy(africa, region = "NAME") %>%
  rename(NAME = id)


africa@data %<>% left_join(data %>% select(-country), by=c("ISO_A3"="countryterritoryCode"))




africa_fortified<- left_join(africa_fortified, africa@data, by= "NAME") %>%
  select(long, lat, group, NAME, date, cum_cases_per_10k, cum_deaths_per_10k)



#Assign NA to values for all Non WHO countriesy WR cases and deaths so that they come up as grey on the maps
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Djibouti"] <- NA
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Egypt"] <- NA
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Libya"] <- NA
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Morocco"] <- NA
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Somalia"] <- NA
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Somaliland"] <- NA
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Sudan"] <- NA
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Tunisia"] <- NA
africa_fortified$cum_cases_per_10k[africa_fortified$NAME == "Wesetern Sahara"] <- NA


africa_fortified_NAs<- africa_fortified %>% filter(is.na(date))

africa_fortified<- africa_fortified%>% drop_na(date)


#sorting breaks

africa_fortified$brks <- cut(africa_fortified$cum_cases_per_10k, 
                            breaks=c(-1, 0, 1, 5, 10, 20, 40,60, 120), 
                             labels=c("0", "0 - 1", "1- 5", "5 - 10", 
                                      "10 - 20", "20 - 40","40 - 60", ">60"))


CumCases10kMap<- ggplot() +
  geom_polygon(data = africa_fortified, aes(fill = brks, x = long, y = lat, group = group), 
               size=0.15) +
  geom_path(data = africa_fortified, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.2) +
  theme_void() +
  scale_fill_brewer(name= "Cumulative Cases Per \n 10k Population", palette = "Blues", na.value = "grey",
                    guide = guide_legend( keyheight = unit(5, units = "mm"), keywidth=unit(15, units = "mm")), 
                    labels = c("0", "0 - 1", "1- 5", "5 - 10", 
                               "10 - 20", "20 - 40","40 - 60", ">60", "Non-WHO Country")) +
  theme(legend.position="left") +
  coord_map() +
  labs(title = "Date: {frame_time}", 
       caption = "Data: WHO COVID-19 Dashboard") +
  theme(plot.title = element_text(size= 26, face = "bold", hjust=0.01, color = "#4e4d47", 
                                  margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.caption = element_text( size=20, color = "#4e4d47", margin = margin(b = 1.0, r=-99, unit = "cm") ),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.text = element_text(size = 22, face= "bold"),
        legend.title = element_text(size = 24, face ="bold")) +
  transition_time(date) 


CumCases10kMapMP4 <- animate(CumCases10kMap,
                          nframes = length(unique(africa_fortified$date)), 
                          fps = 5, 
                          duration = 60, 
                          width = 950, 
                          height = 750,
                          renderer = ffmpeg_renderer(
                            format = "mp4",
                            ffmpeg = NULL,
                            options = list(pix_fmt = "yuv420p")))

anim_save("CumCasesPer10k.mp4", animation=CumCases10kMapMP4)



#Cum death per 10k map gif 


library(geojsonio)
africa <- geojson_read("./input_files/Africa1.geojson", what="sp")

data<- who_country_aliases_and_populations[,c('ISO3', 'country')] %>%
  left_join(WHO_cases_and_deaths %>%
              select(date, country,cum_cases_per_10k, cum_deaths_per_10k), by = 'country') %>%
  #filter(date == today) %>%
  rename(countryterritoryCode = ISO3) %>%
  arrange(country)

library(broom)
africa_fortified <- tidy(africa, region = "NAME") %>%
  rename(NAME = id)


africa@data %<>% left_join(data %>% select(-country), by=c("ISO_A3"="countryterritoryCode"))


africa_fortified<- left_join(africa_fortified, africa@data, by= "NAME") %>%
  select(long, lat, group, NAME, date, cum_cases_per_10k, cum_deaths_per_10k)


africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Djibouti"] <- NA
africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Egypt"] <- NA
africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Libya"] <- NA
africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Morocco"] <- NA
africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Somalia"] <- NA
africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Somaliland"] <- NA
africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Sudan"] <- NA
africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Tunisia"] <- NA
africa_fortified$cum_deaths_per_10k[africa_fortified$NAME == "Western Sahara"] <- NA

africa_fortified<- africa_fortified%>% drop_na(date)

#sorting breaks

africa_fortified$brks <- cut(africa_fortified$cum_deaths_per_10k, 
                             breaks=c(-1, 0, 0.01, 0.05, 0.1, 0.5, 1, 3), 
                             labels=c("0", "0 - 0.01", "0.01- 0.05", "0.05 - 0.1", 
                                      "0.1 - 0.5", "0.5 - 1"," >1"))



CumDeaths10kMap<- ggplot() +
  geom_polygon(data = africa_fortified, aes(fill = brks, x = long, y = lat, group = group), 
               size=0.15) +
  geom_path(data = africa_fortified, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.2) +
  theme_void() +
  scale_fill_brewer(name= "Cumulative Deaths Per \n 10k Population", palette = "Reds", na.value = "grey",
                    guide = guide_legend( keyheight = unit(5, units = "mm"), keywidth=unit(15, units = "mm")), 
                    labels = c("0", "0 - 0.01", "0.01- 0.05", "0.05 - 0.1", 
                               "0.1 - 0.5", "0.5 - 1"," >1", "Non-WHO Country")) +
  theme(legend.position="left") +
  coord_map() +
  labs(title = "Date: {frame_time}", 
       caption = "Data: WHO COVID-19 Dashboard") +
  theme(plot.title = element_text(size= 26, face = "bold", hjust=0.01, color = "#4e4d47", 
                                  margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.caption = element_text( size=20, color = "#4e4d47", margin = margin(b = 1.0, r=-99, unit = "cm") ),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.text = element_text(size = 22, face= "bold"),
        legend.title = element_text(size = 24, face ="bold")) +
  transition_time(date) 


CumDeaths10kMapMP4 <- animate(CumDeaths10kMap,
                             nframes = length(unique(africa_fortified$date)), 
                             fps = 5, 
                             duration = 60, 
                             width = 950, 
                             height = 750,
                             renderer = ffmpeg_renderer(
                               format = "mp4",
                               ffmpeg = NULL,
                               options = list(pix_fmt = "yuv420p")))

anim_save("CumDeathsPer10k.mp4", animation=CumDeaths10kMapMP4)


#New Cases Animation 
#I have made one that lasts 90 seconds to match up with the weekly ratio animations, and one that lasts 60s seconds to match the cum per 10k animations 

NewCaseDateValues<- aggregate(WHO_cases_and_deaths$cases, by=list(WHO_cases_and_deaths$date), sum)

NewCaseDateValues<- NewCaseDateValues %>%
  rename(Date = Group.1,
         Cases = x)

NewCaseDateValues$Date<- as.Date(NewCaseDateValues$Date)


NewCasePlot<- ggplot(NewCaseDateValues, aes(x = Date, y = Cases)) +
  geom_line(size = 1) +
  geom_point(colour = "red", size = 3) +
  transition_reveal(Date) +
  xlab('Date') + 
  ylab('Daily New Cases') + 
  theme_classic(base_size = 15)

NewCaseAnimation90<- animate(NewCasePlot, 
        nframes = length(unique(NewCaseDateValues$Date)), 
        fps = 5, 
        duration = 90, 
        width = 400, 
        height = 250,
        renderer = ffmpeg_renderer(
          format = "mp4",
          ffmpeg = NULL,
          options = list(pix_fmt = "yuv420p")))

anim_save("NewCasePlot90.mp4", animation= NewCaseAnimation90)

NewCaseAnimation60<- animate(NewCasePlot, 
                             nframes = length(unique(NewCaseDateValues$Date)), 
                             fps = 5, 
                             duration = 60, 
                             width = 400, 
                             height = 250,
                             renderer = ffmpeg_renderer(
                               format = "mp4",
                               ffmpeg = NULL,
                               options = list(pix_fmt = "yuv420p")))

anim_save("NewCasePlot60.mp4", animation= NewCaseAnimation60)




#New Deaths Animation 

NewDeathDateValues<- aggregate(WHO_cases_and_deaths$deaths, by=list(WHO_cases_and_deaths$date), sum)

NewDeathDateValues<- NewDeathDateValues %>%
  rename(Date = Group.1,
         Deaths = x)

NewDeathDateValues$Date<- as.Date(NewDeathDateValues$Date)


NewDeathPlot<- ggplot(NewDeathDateValues, aes(x = Date, y = Deaths)) +
  geom_line(size = 1) +
  geom_point(colour = "red", size = 3) +
  transition_reveal(Date) +
  xlab('Date') + 
  ylab('Daily New Deaths') + 
  theme_classic(base_size = 15)

NewDeathAnimation90<- animate(NewDeathPlot, 
                             nframes = length(unique(NewDeathDateValues$Date)), 
                             fps = 5, 
                             duration = 90, 
                             width = 400, 
                             height = 250,
                             renderer = ffmpeg_renderer(
                               format = "mp4",
                               ffmpeg = NULL,
                               options = list(pix_fmt = "yuv420p")))

anim_save("NewDeathPlot90.mp4", animation= NewDeathAnimation90)

NewDeathAnimation60<- animate(NewDeathPlot, 
                             nframes = length(unique(NewDeathDateValues$Date)), 
                             fps = 5, 
                             duration = 60, 
                             width = 400, 
                             height = 250,
                             renderer = ffmpeg_renderer(
                               format = "mp4",
                               ffmpeg = NULL,
                               options = list(pix_fmt = "yuv420p")))

anim_save("NewDeathPlot60.mp4", animation= NewDeathAnimation60)


#Find Mean values

WRCaseMeans<-  aggregate(who_WR_data$WR_cases, by=list(who_WR_data$country), mean)

WRDeathMeans<-  aggregate(who_WR_data$WR_deaths, by=list(who_WR_data$country), mean)

combined<- image_append(NewCaseAnimation90, WRCasemapGIF)


