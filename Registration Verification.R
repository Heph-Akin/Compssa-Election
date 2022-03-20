install.packages("readxl")


library("readxl")
library("tidyr")
library(dplyr)
library(tidyverse)
library(ggplot2)


# Import registration Spreadsheet downloaded from Google sheets
Compssa_list <-  read_excel("C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA_Election\\COMPSSA Student Database without duplicates.xlsx")
Vote_reg <- read.csv("C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA_Election\\Voters List.csv")

names(Compssa_list)[names(Compssa_list) == "Matriculation Number"] <- "Matriculation.number"


Compssa_list$Matriculation.number <- as.integer(Compssa_list$Matriculation.number)
Vote_reg$Valid <- as.logical(Vote_reg$Valid)


# Collect registrations with valid bio-data
Vote_reg <- Vote_reg %>%
  replace(is.na(.), FALSE)

Valid_bio <- Vote_reg %>%
  filter(Valid == "TRUE")

# Invalid Registrations (Invalid Bio-data)
invalid_bio <- Vote_reg %>%
  filter(Valid == "FALSE")


# Number of FALSE Registrations (Invalid Biodata)
nrow(Vote_reg) - nrow(Valid_bio)
  
# Validate valid bio-data registrations from attendance 
Valid_attend <- Valid_bio %>%
  left_join(Compssa_list, "Matriculation.number" ) %>%
  drop_na(`Full Name`) %>%
  arrange(Department.x, Level.x, Matriculation.number)


#Invalid Registrations from attendance
Invalid_attend <- Valid_bio %>%
  left_join(Compssa_list, "Matriculation.number" ) %>%
  filter(is.na(`Full Name`)) %>%
  arrange(Department.x, Level.x, Matriculation.number)


  
# Number of FALSE Registrations (Not on attendance list)
nrow(Valid_bio) - nrow(Valid_attend)


# Export csv for Valid and Invalid Registrations
write.csv(Valid_attend, "C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA_Election\\Valid Registrations.csv")
write.csv(invalid_bio, "C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA_Election\\Invalid Registrations(Biodata).csv")
write.csv(Invalid_attend, "C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA_Election\\Invalid Registrations(Attendance).csv")



  
