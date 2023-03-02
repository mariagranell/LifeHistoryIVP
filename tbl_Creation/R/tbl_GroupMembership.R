# ---------------
# Title: tbl_GroupMembership
# Author: Maria Granell Ruiz (based on the code by Josefien Tankink)
# Date: 20 Feb 2023
# Goal: Create a table that contains the current group of LH entries.
# ---------------

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
# tbl_LifeHistory
tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")

# Create a joined data to work with
d <- tbl_Sex %>%
  left_join(.,tbl_LifeHistory %>%
            select(AnimalID_Std, CurrentGroup,
                   DOB, FirstDate, BirthGroup,
                   EmigrationNatalDate,
                   ImmigrationDate1 ,ImmigrationGroup1, LastDate1,
                   ImmigrationDate2, ImmigrationGroup2, LastDate2,
                   ImmigrationDate3, ImmigrationGroup3, LastDate3,
                   ImmigrationDate4, ImmigrationGroup4, LastDate4
                  ),
            by = c("AnimalID" = "AnimalID_Std")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(ImmigrationDate1 = as.factor(ImmigrationDate1))

# I want to create a table that melts down all the info of individuals. So only 4 columns:
# Animal_ID, Start_date, Last_date, Group

# I will tackle this in cases

# 1. Check to confirm First Date is a consensus between DOB and FirstDate ------------------
# I remove Babyrenn2020 becuase is very unreliable
View(d %>%
  filter (DOB != FirstDate & !is.na(DOB))
)
d <- d[d$AnimalID != "Babyrenn2020",]

# I will remove the LastSeen2, ImmigrationGp3 and DateImmigration3 for Kom. The supossed gap it had in BD
# does not make sense, so I will ignore it because I know that until today 1 Mar he's still in BD
d[d$AnimalID == "Kommunis", "LastDate2"] <- "2021-12-14"

# remove LastSeen1 for some individuals of IFam because we are following them now
d[d$AnimalID == "Inhlanhla", "LastDate1"] <- NA

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
  select(AnimalID, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

# 2.1 group slpits ----------------------------------------------------------------------
# some are still not resolved becuase there is no immigration date. You can check this ->
# View(d %>% filter(!is.na(ImmigrationGroup1) & is.na(ImmigrationDate1))) or this ->
# View(d %>% filter(!is.na(ImmigrationGroup1) & is.na(ImmigrationDate1) & !is.na(BirthGroup)))

gpsplits <- d %>%
  filter(!is.na(ImmigrationGroup1) & is.na(ImmigrationDate1) & !is.na(BirthGroup) & !is.na(EmigrationNatalDate)) %>%
  mutate(
    StartDate_mb = EmigrationNatalDate,
    EndDate_mb = LastDate1,
    Group_mb = ImmigrationGroup1,
    Tenure_type = "GroupSplit"
  ) %>%
   # if it dosen´t have a last immigration date must be they are still in the group
  mutate( EndDate_mb = ifelse(is.na(EndDate_mb), todaydate, EndDate_mb)) %>%
  select(AnimalID, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)



# 3. when they emigrated. how long they stayed in their natal group ----------------------
emigrated <- d%>%
  filter (!is.na(EmigrationNatalDate)) %>%
  mutate(
    StartDate_mb = FirstDate,
    EndDate_mb = EmigrationNatalDate,
    Group_mb = BirthGroup,
    Tenure_type = "BirthGroup"

  ) %>%
  select(AnimalID, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

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
  select(AnimalID, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

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
  select(AnimalID, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

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
  select(AnimalID, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

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
  select(AnimalID, Group_mb,StartDate_mb, EndDate_mb, Tenure_type)

# 8. put all tables together ------------------

tbl_GroupMembership <- fourthimmi %>%
  bind_rows(.,thirdimmi) %>%
  bind_rows(.,secondimmi) %>%
  bind_rows(.,firstimmi) %>%
  bind_rows(.,thirdimmi) %>%
  bind_rows(.,emigrated) %>%
  bind_rows(.,ceroGP) %>%
  bind_rows(.,gpsplits)

# Manual data cleaning --------------------------
View(tbl_GroupMembership%>%
  left_join(.,tbl_LifeHistory %>%
  select(AnimalID_Std, Fate_probable), by = c("AnimalID" = "AnimalID_Std")), "Unreliable data")

# remove duplicates
tbl_GroupMembership <- tbl_GroupMembership %>% distinct(., .keep_all = TRUE)

# lif date of brain extraction: 20.08.2022, 5 days before as approx of End_Date in NH
tbl_GroupMembership[34,4] <- "2022-08-15"

# write the csv -----------------------------
write.csv(tbl_GroupMembership,"tbl_GroupMembership.csv",row.names = FALSE)
