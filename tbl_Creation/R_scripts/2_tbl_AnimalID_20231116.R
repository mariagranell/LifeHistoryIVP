# 2023 Nov 16

# The goal of this file is to create three collumns, and 1 row per individual.
# AnimalCode = 3 or 4 letter code that would be the main thing to use
# AnimalName = Animal full name
# OtherID = wrong full names or codes that have been used to identify this individual

# General -----------------------------------------------------------------

#I am starting from tbl_LifeHistory to generate a list of AnimalID
#In previous tbl_AnimalID I had used a different approach where I had built on Mathias code
#Every change in LH_AnimalID will require tbl_AnimalID to be re-generated alongside all other tbl

# Paths ------------------------------------------------------------------
setwd("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL")

# Paths ------------------------------------------------------------------
 library(tidyverse)
 library(dplyr)
 library(lubridate)
 source('/Users/mariagranell/Repositories/data/functions.R')


# tbl_LifeHistory ---------------------------------------------------------
tbl_LifeHistory <- read.csv("tbl_LifeHistory_180424.csv")
str(tbl_LifeHistory)

# tbl_AnimalID ------------------------------------------------------------
AnimalID <- tbl_LifeHistory %>%
  #arrange(LH_AnimalCode) %>%
  select(LH_RowNumber, LH_AnimalCode, LH_AnimalName, OtherID) %>%
  rename(AnimalCode = LH_AnimalCode, AnimalName = LH_AnimalName)

# General -----------------------------------------------------------------
#Once the table is generated, it will have to be corrected here on its own

#To generate the first table, it does not matter whether AnimalID_Std is taken from tbl_AnimalID or from tbl_LifeHistory as they were at that time the same
#However, the question will arise later on

# Don´t worry about the warning

AnimalID_clean <- AnimalID %>%
  #REPLACE / BY ; because Other ID can have two entries separated by / remove the space at the beggining if creates any
  mutate(OtherID = str_replace(string = OtherID,pattern = "/", replacement = ";")) %>%
  #SEPARATE OTHER ID
  separate(OtherID, sep = ";", into = c("OtherID_1","OtherID_2")) %>%
  # remove the space at the beggining if creates any
  mutate(OtherID_2 = str_replace(string = OtherID_2,pattern = " ", replacement = "")) %>%
  pivot_longer(., cols = c("OtherID_1", "OtherID_2"), values_to = "Other_ID", names_to = NULL) %>%
  distinct()

# There is another file that recorded a lot of names that were used wrong.
# Add entries from Matias
Matias_name_corrections <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/Source_files/Mathia_NameCorrection_20211118.csv") %>%
      mutate(bad_name = iconv(bad_name, to = "UTF-8"),
           good_name = iconv(good_name, to = "UTF-8")) %>%
      mutate(bad_name = str_trim(bad_name, side = "both"),
             good_name = str_trim(good_name, side = "both")) %>%
      distinct() %>%
      filter(bad_name != good_name, good_name != "New male") %>%
      # kom it has two entries. I am going to keep only the variation of Kom and Kommunis
      separate_rows(good_name, sep = "/") %>% separate_rows(full_name, sep = "/") %>%
      mutate(full_name = ifelse(good_name == "Kom", "Kommunis", full_name),
             full_name = ifelse(good_name == "Bin", "Bingo", full_name))


# from this file I am only intrested in Code, Name and Other.
extra_OtherID <- Matias_name_corrections %>% select(good_name, full_name, bad_name) %>%
  filter(str_trim(good_name) != "") %>%
  rename(AnimalCode = good_name,
         AnimalName = full_name,
         Other_ID = bad_name)

# This is a huge file, thus I will only introduce the ones that are named consistently
# This stays here as forther infomration in case I need it
#code_not_found_in_LH <- extra_OtherID %>% left_join(., AnimalID_clean, by = "AnimalCode", relationship = "many-to-many") %>%
#  filter(is.na(LH_RowNumber))

#code_and_name_not_found_in_LH <- code_not_found_in_LH %>% filter(AnimalName.x != AnimalName.y) %>%
#  distinct(AnimalName.x, AnimalName.y, AnimalCode)

# merge the names from matias file into LH. inluding only names that are names consistenly. there is more information in the ¨
# file that will be lost with the merge, specially baby names
tbl_AnimalID <- AnimalID_clean %>%
  left_join(extra_OtherID, by = c("AnimalCode", "AnimalName", "Other_ID"), relationship =  "many-to-many") %>%
  # extra names
  add_row(LH_RowNumber = 796, AnimalCode = "Zeu", AnimalName = "Zeus", Other_ID = "MaleCollar") %>%

  group_by(LH_RowNumber, AnimalCode, AnimalName) %>%
  summarise(OtherID = if(all(is.na(Other_ID))) NA else paste(na.omit(Other_ID), collapse = ";"),
              .groups = 'drop')%>%
  filter(!is.na(AnimalCode) & !is.na(AnimalName))

# Manual correction ----
tbl_AnimalID <- tbl_AnimalID %>% mutate(AnimalName = ifelse(AnimalCode == "Nil", "Nile", AnimalName))

#write csv
 #write.csv(tbl_AnimalID,"tbl_AnimalID.csv",row.names = FALSE)
