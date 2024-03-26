#HEADER --------------------------------------------------------------------
#
#Scrip name:   "data_manipil.R"
#Purpose:      illustrate some functions or packages for date processing
#Author:       Yixu Liao
#Email:        lyx233@mail.ustc.edu.cn
#Date:         2024/3/24        
#
#SETUP ---------------------------------------------------------------------

#import and save data

download.file("tinyurl.com/dcmac2017dec/data/surveys_wide.csv",
              dest="surveys_wide.csv")

survey_wide <- read.csv("surveys_wide.csv")   #import data

write.csv(survey_wide,"data_save")   #save data

#inspect data structure

str(survey_wide)
head(survey_wide)

#load packages

library(tidyverse)

library(ggplot2)

library("tidyr")

#check whether a column or row has missing data

sum(is.na(survey_wide$month))   #check for missing data in a specific column

sum(is.na(survey_wide[1,]))   #check for missing data in a specific row

#extract values from a column or select/add a column

values <- survey_wide$month   #extract values from a column

survey_wide1 <- survey_wide |>
  select(day) |>
  mutate(day1=day)   #select a specific column and add a new column

#transform a wider table to a long format

survey_long <- survey_wide |> gather(key = month,
            value = count,-c(month,day,year,plot_id))   #transform

head(survey_long)

str(survey_long)

#visualize the data

ggplot(survey_long,aes(x = day, y = year)) +
  geom_point(alpha = 0.5,color="tomato") +
  labs(title = "Data Visualization",x = "day", y = "year")
  