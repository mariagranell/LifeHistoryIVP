# ---------------
# Title: tbl_age
# Author: Maria Granell Ruiz (based on the code by Phillipe Vulloid)
# Date: 14 Feb 2023
# Goal: Create a table that contains the approximate age of LH entries, if possible add specific age in years.
# ---------------

# Criteria:
# baby: 0 yr
# juvenile: 1-3 yr
# sub-adult: males 4 yr
# adult: females- that gave birth to a live baby (miscarriage doesn´t counts) OR 4yr and older
#        males- migrated OR 5yr old


# Package -----------------------------------------------------------------
library(tidyverse)
library(lubridate)

# create a table with: AnimalID, AnimalCode, OtherID, Sex, DOB
# Path ------------------------------------------------------------------
setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria")

# tbl_AnimalID
tbl_AnimalID <- read.csv("tbl_AnimalID.csv")

# tbl_AnimalCode
tbl_AnimalCode <- read.csv("tbl_AnimalCode.csv")

# tbl_Sex
tbl_Sex <- read.csv("tbl_Sex.csv")

# tbl_DOB
tbl_DOB <- read.csv("tbl_DOB.csv")

# tbl_LifeHistory
tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")


# Calculate age in years when it has DOB---------------------------------------------------
AnimalID_DOB <- tbl_AnimalID %>%
  left_join(., tbl_DOB, by=c("AnimalID" = "AnimalID")) %>%

  # Date format
  mutate(DOB = ymd(DOB)) %>%
  mutate(Age_yr = add_age(., unit = "Years"))

head(AnimalID_DOB)

# number of individuals with no Aye_yr
NoAge <- AnimalID_DOB %>%
  filter(is.na(Age_yr))
nrow(NoAge)

# Include the column first seen-------------------------------------------------------------
# I will use the add age function to the first seen category
# NOT RELIABLE some are new males that were adults the first time seen
AnimalID_DOB_FD <- AnimalID_DOB %>%
  left_join(.,tbl_LifeHistory%>%
              select(
                AnimalID_Std,
                FirstDate
              ), by = c("AnimalID" = "AnimalID_Std")) %>%
  mutate(FirstDate = ymd(FirstDate))

AnimalID_DOB_FD$Age_yr <- ifelse(is.na(AnimalID_DOB_FD$Age_yr), add_age(AnimalID_DOB_FD,
                                                        birthdate = AnimalID_DOB_FD$FirstDate,
                                                        unit = "Years")
                                              , AnimalID_DOB_FD$Age_yr)

# filter LH with reliable data
# check if they are dead
# in LH check info about: DateAdult, EmigrationNatalDate

# Add the age categories ---------------------
# baby: 0 yr
# juvenile: 1-3 yr
# sub-adult: males 4 yr
# adult: females- that gave birth to a live baby (miscarriage doesn´t counts) OR 4yr and older
#        males- migrated OR 5yr old

AnimalID_Age_Sex <- AnimalID_DOB %>%
  left_join(., tbl_Sex, by=c("AnimalID" = "AnimalID")) %>%
  mutate(Age_class = case_when(
    Age_yr < 1 ~ "baby",
    Age_yr < 4 ~ "juvenile",
    Age_yr <= 5 & Sex == "M" ~ "sub-adult",
    Age_yr >= 4 & Sex == "F" ~ "adult",
    Age_yr >= 5 ~ "adult"
  ))


# write csv tbl -----------------------------------------------------------

AnimalID_Age_Sex %>%
  select(AnimalID,LH_AnimalCode,Age_yr,Age_class)%>%
  write.csv(.,"tbl_Age_notclean.csv",row.names = FALSE)
