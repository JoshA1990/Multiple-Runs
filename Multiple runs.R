

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

#First set the working directory
getwd()
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/Joiners_leavers_FTE/Data from ESR/ESR")

#Looks through entire working directory and adds each csv to a list

#########    MUST BE IN ORDER ALPHABETICALLY AND CHRONOLOGICALLY 
file_list <- list.files(pattern='*.csv')
df_list <- lapply(file_list, read_csv)

# Set working directory to get other files
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")

nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("Org Codes NHS Digital.csv")

#### Define/ call the function from a different R script

source("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Ad-Hoc/.R")


####   Start a loop

#Define the starting point of the loop
i <- 1

#Define the ending point of the loop- may need to change depending on monthly or yearly
j <- length(df_list)


while (i < j) {
  #Have to rename to make it easier to remove from list
  names(df_list)[1] <- "Raw_Data"
  #Assigns earliest two entries to Raw_Data to allow to parse
  Raw_Data_y1 <- df_list[[1]]
  Raw_Data_y2 <- df_list[[2]]
  #Call function
  #################   Change function depending on what you need
  band_5_nurses()
  #Reassign globally after deleting entry
  df_list <<- within(df_list, rm("Raw_Data"))
  i <- i + 1
}





