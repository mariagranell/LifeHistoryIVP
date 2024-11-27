# ---------------
# Title: tbl_GroupMembership
# Author: Maria Granell Ruiz (based on the code by Josefien Tankink)
# Date: 20 Feb 2023
# Goal: Create a table that contains the current group of LH entries.
# ---------------

# Package -----------------------------------------------------------------
library(tidyverse)
library(lubridate)

# create a table with: AnimalName, AnimalCode, OtherID, Sex, DOB
# Path ------------------------------------------------------------------
setwd("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL")

# tbl_AnimalName
tbl_AnimalID <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_AnimalID.csv")
# tbl_Sex
tbl_Sex <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_Sex.csv")
# tbl_LifeHistory
tbl_LifeHistory <- read.csv("tbl_LifeHistory_180424.csv")%>%
  filter(!(LH_AnimalName == "Dinosaur"& ReliableData == "NO"))

# Create a joined data to work with
d <- tbl_Sex %>%
  left_join(.,tbl_LifeHistory %>%
            dplyr::select(LH_AnimalCode, LH_AnimalName, CurrentGroup,
                   DOB, FirstDate, BirthGroup,
                   EmigrationNatalDate,
                   ImmigrationDate1 ,ImmigrationGroup1, LastDate1,
                   ImmigrationDate2, ImmigrationGroup2, LastDate2,
                   ImmigrationDate3, ImmigrationGroup3, LastDate3,
                   ImmigrationDate4, ImmigrationGroup4, LastDate4
                  ),
             by=c("AnimalCode"="LH_AnimalCode", "AnimalName" = "LH_AnimalName")) %>%
  mutate(Sex = as.factor(Sex)) #%>%
  #mutate(ImmigrationDate1 = as.factor(ImmigrationDate1))

# I want to create a table that melts down all the info of individuals. So only 4 columns:
# Animal_ID, StartDate_mb, EndDate_mb, Group_mb

# I will tackle this in cases

# 1. Data cleaning ------------------
## Check to confirm First Date is a consensus between DOB and FirstDate
# I remove Babyrenn2020 because is very unreliable
d <- d[d$AnimalName != "Babyrenn2020",]

# From comments. LastDate2 for Kommunis is wrong. 2021 instead 2022. Corrected now!
d[d$AnimalName == "Kommunis", "LastDate2"] <- "2021-12-14"

## some individuals don't have An ImmigrationDate1 but have an ImmigrationGroup1. Here I will just add invidiuals
# that have an EmigrationNatalDate and approximate the entry date a day after they left
# their natal groups. I will also filter for only the main groups
# a<-d %>%filter(is.na(ImmigrationDate1) & !is.na(LastDate1) & !is.na(EmigrationNatalDate)) %>%filter(grepl('\\bCR\\b|\\bLT\\b|\\bAK\\b|\\bBD\\b|\\bKB\\b|\\bNH\\b', ImmigrationGroup1)) %>% mutate(ImmigrationDate1 = as.character(as.Date(EmigrationNatalDate) + 1))
# I'll make the changes manually since is only two cases
d[d$AnimalName == "Asseblief", "ImmigrationDate1"] <- as.character(ymd(d[d$AnimalName == "Asseblief", "EmigrationNatalDate"]) + 1)
d[d$AnimalName == "Gaaf", "ImmigrationDate1"] <- as.character(ymd(d[d$AnimalName == "Gaaf", "EmigrationNatalDate"]) + 1)

# 2. From birth to last seen, if last seen is empty i´ll asume today date ----------------------
todaydate <- as.character(Sys.Date())
ceroGP <- d%>%
  filter (is.na(EmigrationNatalDate) & is.na(ImmigrationDate1)) %>%
  mutate(
    StartDate_mb = FirstDate,
    EndDate_mb = LastDate1,
    Group_mb = ifelse(!is.na(BirthGroup), # select the first group they were ever recorded
                   BirthGroup,
                   ImmigrationGroup1),
    Tenure_type = ifelse(!is.na(BirthGroup),
                         "BirthGroup",
                         ifelse(!is.na(ImmigrationGroup1),
                         "MigrationGroup1", NA))
  ) %>%

  # if it dosen´t have a last seen date must be they are still in the group
  mutate( EndDate_mb = ifelse(is.na(EndDate_mb), todaydate, EndDate_mb)) %>%
  select(AnimalName, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

# 2.1 group slpits ----------------------------------------------------------------------
# some are still not resolved becuase there is no immigration date. You can check this ->
# View(d %>% filter(!is.na(ImmigrationGroup1) & is.na(ImmigrationDate1))) or this ->
# View(d %>% filter(!is.na(ImmigrationGroup1) & is.na(ImmigrationDate1) & !is.na(BirthGroup)))

gpsplits <- d %>%
  filter(!is.na(ImmigrationGroup1) & is.na(ImmigrationDate1) & !is.na(BirthGroup) & !is.na(EmigrationNatalDate)) %>%
  # if they are KB,CR,BD,NH,AK or LT they are for sure not splits, So we have to remove those entries
  # the \\b \\b makes sure it only matches that particular patter
  filter(!grepl('\\bCR\\b|\\bLT\\b|\\bAK\\b|\\bBD\\b|\\bKB\\b|\\bNH\\b', ImmigrationGroup1)) %>%
  mutate(
    StartDate_mb = EmigrationNatalDate,
    EndDate_mb = LastDate1,
    Group_mb = ImmigrationGroup1,
    Tenure_type = "GroupSplit"
  ) %>%
   # if it dosen´t have a last immigration date must be they are still in the group
  mutate( EndDate_mb = ifelse(is.na(EndDate_mb), todaydate, EndDate_mb)) %>%
  select(AnimalName, Group_mb,StartDate_mb, EndDate_mb, Tenure_type) %>%
  mutate(Group_mb = str_replace_all(Group_mb, pattern = c("Ifamily"), replacement = "IFamily"))


# 3. when they emigrated. how long they stayed in their natal group ----------------------
emigrated <- d%>%
  filter (!is.na(EmigrationNatalDate)) %>%
  mutate(
    StartDate_mb = FirstDate,
    EndDate_mb = EmigrationNatalDate,
    Group_mb = BirthGroup,
    Tenure_type = "BirthGroup"

  ) %>%
  select(AnimalName, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

# 4. first inmigration ----------------------

firstimmi <- d%>%
  filter ( !is.na(ImmigrationDate1)) %>%
  mutate(
    StartDate_mb = ImmigrationDate1,
    EndDate_mb = LastDate1,
    Group_mb = ImmigrationGroup1,
    Tenure_type = "MigrationGroup1"
  ) %>%
   # if it dosen´t have a last immigration date must be they are still in the group
  mutate( EndDate_mb = ifelse(is.na(EndDate_mb), todaydate, EndDate_mb)) %>%
  select(AnimalName, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)



# 5. second immigration ------------------
secondimmi <- d%>%
  filter ( !is.na(ImmigrationDate2)) %>%
  mutate(
    StartDate_mb = ImmigrationDate2,
    EndDate_mb = LastDate2,
    Group_mb = ImmigrationGroup2,
    Tenure_type = "MigrationGroup2"
  ) %>%
   # if it dosen´t have a last immigration date must be they are still in the group
  mutate( EndDate_mb = ifelse(is.na(EndDate_mb), todaydate, EndDate_mb)) %>%
  select(AnimalName, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

# 6. third immigration --------------------------
thirdimmi <- d%>%
  filter ( !is.na(ImmigrationDate3)) %>%
  mutate(
    StartDate_mb = ImmigrationDate3,
    EndDate_mb = LastDate3,
    Group_mb = ImmigrationGroup3,
    Tenure_type = "MigrationGroup3"
  ) %>%
   # if it dosen´t have a last immigration date must be they are still in the group
  mutate( EndDate_mb = ifelse(is.na(EndDate_mb), todaydate, EndDate_mb)) %>%
  select(AnimalName, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

# 7. fourth immigration ---------------------
fourthimmi <- d%>%
  filter ( !is.na(ImmigrationDate4)) %>%
  mutate(
    StartDate_mb = ImmigrationDate4,
    EndDate_mb = LastDate4,
    Group_mb = ImmigrationGroup4,
    Tenure_type = "MigrationGroup4"
  ) %>%
   # if it dosen´t have a last immigration date must be they are still in the group
  mutate( EndDate_mb = ifelse(is.na(EndDate_mb), todaydate, EndDate_mb)) %>%
  select(AnimalName, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

# 8. put all tables together ------------------

tbl_GroupMembership <- fourthimmi %>%
  bind_rows(.,thirdimmi) %>%
  bind_rows(.,secondimmi) %>%
  bind_rows(.,firstimmi) %>%
  bind_rows(.,thirdimmi) %>%
  bind_rows(.,emigrated) %>%
  bind_rows(.,ceroGP) %>%
  bind_rows(.,gpsplits) %>%
  unique(.)

# Manual data cleaning --------------------------
View(tbl_GroupMembership%>%
  left_join(.,tbl_LifeHistory %>%
  select(LH_AnimalName, Fate_probable), by = c("AnimalName" = "LH_AnimalName")) %>%
  filter(!is.na(Fate_probable)), "Unreliable data")

# change Apa LT end date to 2022-07-26 becuase data in BGE. Corrected!
# lif date of brain extraction: 20.08.2022, 5 days before as approx of End_Date in NH. Corrected!

# write the csv -----------------------------
write.csv(tbl_GroupMembership, "../TBL/Archive_tbl/lh_181124/tbl_GroupMembership.csv", row.names = FALSE)
