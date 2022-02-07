#### Preamble ####
# Purpose: To clean the data and getting it ready for plotting and table formatting
# Author: Yuzhi Pi
# Date: Feb 6, 2022
# Contact: sherry.pi@mail.utoronto.ca
# License: MIT
# Pre-requisites: install package opendatatoronto; dplyr; tidyvers and kableExtra

#### Work Place Setup ####
library(opendatatoronto)
library(dplyr)
library(tidyverse)
library(kableExtra)

# Below is copied from https://open.toronto.ca/dataset/police-annual-statistical-report-homicide/ under "for developers"
# Steps to load the data from Open Data Toronto

# get package
package <- show_package("7d72bbbe-8adc-4b36-8ad1-5359f1c7a9cc")


# get all resources for this package
resources <- list_package_resources("7d72bbbe-8adc-4b36-8ad1-5359f1c7a9cc")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()

data <- tibble(
  Event_Unique_Id = data$Event_Unique_Id,
  Occurrence_year = data$Occurrence_year,
  Division = data$Division,
  Homicide_Type = data$Homicide_Type,
  Occurrence_Date = data$Occurrence_Date,
  Hood_ID = data$Hood_ID,
  Neighbourhood = data$Neighbourhood,
  ObjectId = data$ObjectId
)

class(data)

glimpse(data)
```

# Data cleaning 
cleaned_data <- 
  data %>%
  select(Occurrence_year, Division, Homicide_Type, Occurrence_Date, Hood_ID, Neighbourhood,)
# Remove coloums that are not needed

cleaned_data <-
  cleaned_data %>%
  separate(col = Occurrence_Date,
           into = c('year', 'month', 'other'),
           sep = '-')
# Obtain month data by separating Occurance_date, remove column "other"

cleaned_data <- cleaned_data[-c(6)]

glimpse(cleaned_data)

#Trend of Count of Homicide by Year
cleaned_data %>% ggplot(aes(x= Occurrence_year)) + geom_bar() + theme_classic() +
  labs(x = "Occurence Year", y = "Frequency", title = "Figure 1: Summmary of Homicide Occurrence Year")+
  geom_text(stat = 'count', aes(label = ..count..,vjust = -0.5))

#Trend of Count of Homicide by month
cleaned_data %>% ggplot(aes(x= month)) + geom_bar() + theme_classic() +
  labs(x = "Occurrence Month", y = "Case Count", title = "Figure 2: Summmary of Homicide Occurrence Month")+
  geom_text(stat = 'count', aes(label = ..count..,vjust = -0.5))

# Geometric Location
cleaned_data %>% ggplot(aes(x= Division)) + geom_bar(fill = "grey") + theme_classic() +
  labs(x = "Police Division", y = "Count", title = "Figure 3: Summmary of Homicide Occurrence Location") +
  coord_flip()+
  geom_text(stat = 'count', aes(label = ..count..,vjust = 0.5))

# Aggregate different homicide type by year
tableprep <- cleaned_data %>% 
  select(Occurrence_year, Homicide_Type) %>%
  group_by(Occurrence_year) %>% 
  count(Homicide_Type) %>% 
  ungroup() %>% 
  group_by(Occurrence_year) %>% 
  mutate(percentage = paste(round(100 * (n /sum(n)), 1), "%"))

knitr::kable(tableprep, caption = "Summary Table of Homicide Type and Percentage","pipe", align = c("l", "l", "c", "c"), col.names = c("Occurance Year", "Homicide Type", "Number of Cases Per Year", "% of Total Case For the Year"))


#Ready the data for plotting stacked line graph
graphprep <- cleaned_data %>% 
  select(Occurrence_year, Homicide_Type) %>%
  group_by(Occurrence_year) %>% 
  count(Homicide_Type) %>% 
  ungroup() %>% 
  group_by(Occurrence_year) %>% 
  mutate(percentage = round(100 * (n /sum(n)), 1))

#Plot Stacked Line for count
ggplot(graphprep,aes(x = Occurrence_year, y = n, color = Homicide_Type, group = Homicide_Type))+
  geom_line() + 
  geom_point() +
  labs(x = "Occurrence Year", y = "Number of Cases", title = "Figure 4: Count of Different Homicide Type" )


#Plot Stacked Line for percentage
ggplot(graphprep,aes(x = Occurrence_year, y = percentage, color = Homicide_Type, group = Homicide_Type))+
  geom_line() + 
  geom_point() +
  labs(x = "Occurrence Year", y = "Percentage", title = "Figure 5: Percentage of Different Homicide Type")