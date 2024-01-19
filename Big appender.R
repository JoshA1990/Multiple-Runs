

#                                  MULTIPLE RUNS


###################################Install and load the packages###################################
#(note only need to install packages once, but need to reload library each time)
#Working with strings package
#install.packages ("stringr")
library(stringr)

#data wrangling/ analysis package
library(dplyr)

#Viewing data package
library(kableExtra) 

#data wrangling/ analysis package
library(tidyverse)

library(lubridate)

#Dates package
library(eeptools)

#visualisation
library(ggplot2)

#read in multiples
library(readxl)

library(purrr)

#First set the working directory
getwd()
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NQN")

#######################################################


# Get a list of all CSV files in the folder
csv_files <- list.files(path = 'C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NQN',
                        full.names = TRUE,
                        pattern = "\\.csv$")

# Read all CSV files into a list of data frames
result <- csv_files %>% map(read.csv, check.names = TRUE) %>% 
  map(~mutate(.x , nationality_group_name = paste(Nationality_grouping, name, sep = "_"),
                        Nationality_grouping = NULL,
                        name = NULL,
                        X= NULL))%>%
  reduce(left_join, by = "nationality_group_name")





# Left join based on the common column
result <- reduce(left_join, by = "nationality_group_name")

write_csv(result, "C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/Outputs/NQN/Complete.csv")

          
