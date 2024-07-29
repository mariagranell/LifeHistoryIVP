# ---------------
# Title: tbl_age
# Author: Maria Granell Ruiz (based on the code by Phillipe Vulloid)
# Date: 14 Feb 2023
# Goal: Create a table that contains the approximate age of LH entries, if possible add specific age in years.
# Goal2: check with descriptive statistics some data about age.
# ---------------

# Criteria:
# baby: 0 yr
# juvenile: 1-3 yr
# sub-adult: males 4 yr
# adult: females- that gave birth to a live baby (miscarriage doesn´t counts) OR 4yr and older
#        males- migrated OR 5yr old

# TODO many babies are classied as adults because they were never named as last seen

# Package -----------------------------------------------------------------
library(tidyverse)
library(lubridate)
source("/Users/mariagranell/Repositories/data/life_history/Vervet_Functions.R")

# create a table with: AnimalName, AnimalCode, OtherID, Sex, DOB
# Path ------------------------------------------------------------------
setwd("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL")

# tbl_AnimalID
tbl_AnimalID <- read.csv("tbl_AnimalID.csv")

# tbl_Sex
tbl_Sex <- read.csv("tbl_Sex.csv")

# tbl_DOB
tbl_DOB <- read.csv("tbl_DOB.csv")

# tbl_LifeHistory
tbl_LifeHistory <- read.csv("tbl_LifeHistory_180424.csv") %>%
  filter(!(LH_AnimalName == "Dinosaur"& ReliableData == "NO"))

# MERGE DATAFRAMES --------------------
# its fine to have several names
d <- tbl_AnimalID %>%
  left_join(.,tbl_Sex, by = "AnimalName") %>%
  rename(tbl_sex = Sex) %>%
  left_join(.,tbl_DOB, by = "AnimalName") %>%
  rename(tbl_dob = DOB) %>%
  left_join(.,tbl_LifeHistory, by = c("AnimalName" = "LH_AnimalName")) %>%
  rename(lh_dob = DOB, lh_sex = Sex)

# CREATE A COLUMN DOB_estimate
# It can only be used for age categories! not adviced to use for any other calculation.

# Double check DOB ---------------------
# if they have a DOB they have a Birthdaygroup and a mother.
# wes and pun cannot have a DOB if they don´t have a BirthGroup.
# Kaya from LT has a DOB but no mum.. but we will allow it since is a female
d <- d%>%
  mutate(tbl_dob = case_when(LH_AnimalCode %in% c("Wes", "Pun") ~ NA, TRUE ~ tbl_dob),
         lh_dob = case_when(LH_AnimalCode %in% c("Wes", "Pun") ~ NA, TRUE ~ lh_dob))

d$DOB_estimate <- ifelse(!is.na(d$tbl_dob),d$tbl_dob, d$lh_dob)

# Some individuals have the DOB wrong, that is claimiing they were born in the 1900.
# I will only allow in DOB_estimate, DOB bigger than 2009 (IVP started in 2010)
d$DOB_estimate <-ifelse(d$DOB_estimate > "2009-12-01", d$DOB_estimate, NA )


# number of individuals with no DOB and no Sex --------------
# because of the data I can asume Jalitah and Corfu First dates can be DOB_estimated
# the rest are too empty
# TODO there is no Jalitah
d[d$AnimalName == "Jalitah", "DOB_estimate"] <- d[d$AnimalName == "Jalitah", "FirstDate"]
d[d$AnimalName == "Corfu", "DOB_estimate"] <- d[d$AnimalName == "Corfu", "FirstDate"]

# from the comments this is the estimated DOB. when a year is given I will always choose xxxx-11-01 -------------------------
d[d$AnimalName %in% c("Nkosikasi", "Queen"), "DOB_estimate"] <- "2005-11-01"
d[d$AnimalName %in% c("Rissiepit", "Ulaka", "Elton"), "DOB_estimate"] <- "2006-11-01"
d[d$AnimalName %in% c("Tipies", "Beminde", "Byron", "Gelosi", "Gino", "Hamlet", "Mfolozi", "Okucane"), "DOB_estimate"] <- "2007-11-01"
d[d$AnimalName %in% c("Nala", "Heleza","Feekie","Drew","Charles", "Govu", "Izulu", "Mpukuvane", "Spotted", "Bougainville"), "DOB_estimate"] <- "2008-11-01"
d[d$AnimalName %in% c("Engel", "Gesels", "Ghangaan", "Hleka", "Inhlanhla", "Charles", "Govu", "Izulu", "Mpukuvane", "Spotted",
                    "Babelas", "Bingo", "Donsig", "Jackie", "Merlin", "Mnandi", "Mousse", "Pikito", "Styx","Pompon"), "DOB_estimate"] <- "2009-11-01"
d[d$AnimalName %in% c("Laurel", "Curaco", "Darwin", "Dwergie", "Tweed", "Wolfie"), "DOB_estimate"] <- "2010-11-01" # anotherone
d[d$AnimalName %in% c("Amur", "Camilla", "Lionel", "Nessie", "Raspberry", "Watnou",
                    "Cone", "Flatty", "Tugela", "Vincent", "Zambezi", "Curious"), "DOB_estimate"] <- "2011-11-01"
d[d$AnimalName %in% c("Cuba", "Huge", "Java", "Mackanzie", "Moonie", "Nihau", "Tortuga", "Whiskers", "Handi", "Helmet", "Inner", "Pale"), "DOB_estimate"] <- "2012-11-01"
d[d$AnimalName %in% "Pizza", "DOB_estimate"] <- "2013-11-01"
d[d$AnimalName %in% c("Manhattan","Reindeer", "Alcatraz", "Bermuda", "Dokos",
                    "Hamlet", "Ireland", "Seychelles", "Symetric", "Tasmania", "Umbrella"), "DOB_estimate"] <- "2014-11-01"
d[d$AnimalName %in% "Cameron", "DOB_estimate"] <- "2016-11-01"
# IF adults individuals. when first seen already adults, thus I will remove 4 yr from their DOB estimate (2022-06-28). I will put now 2018-06-28
#Zeus, Hera, Meduda, Freyja, Athena, Circe, Pandora
d[d$AnimalName %in% c("Zeus", "Hera", "Medusa", "Freyja", "Athena", "Circe", "Pandora"), "DOB_estimate"] <- "2018-06-28"

# Females with no DOB --------------

# the row DOB_estimated will only be used for age categories.
# thus, as long as the first seen column considers them as an adut we can add it, that is for example 2023 -4 years, i.e. 2019.
adult_min_age <- year(Sys.Date()) -4
d[,"DOB_estimate"] <- ifelse(is.na(d$DOB_estimate) & d$tbl_sex == "F" & d$FirstDate < paste0(adult_min_age, "-01-01"), d$FirstDate, d$DOB_estimate)

# if the data was collected after 2020 in anygroup, DOB can be approximated as FirstDate
d[,"DOB_estimate"] <-
  ifelse(is.na(d$DOB_estimate) & d$tbl_sex == "F" & d$FirstDate > "2020-01-01", d$FirstDate, d$DOB_estimate)


# Babies with no DOB -------------------------
# Since we are doing the separation by Sex we miss estimating the age for those individuals (mostly BB) that
# don´t have their sex determined. But is the same logic as before. we will only have individuals with BB in their names
d <-d %>% mutate(DOB_estimate = ifelse(is.na(DOB_estimate) & is.na(tbl_sex) & str_detect(AnimalName, "BB|Baby"), FirstDate, DOB_estimate))


# Males with no DOB -------------------------

# Males with a BirthGroup -----------

# when their first migration was before they where 3 years old we cannot assume that they FirstDate is their DOB.
# we will calculate a new DOB by subtracting 4 years from the EmigrationNatalDate
# ignore the warning
 d<- d%>%
   mutate(minus = ifelse(EmigrationNatalDate != "Stillpresent" | is.na(EmigrationNatalDate),
                         difftime(ymd(EmigrationNatalDate), ymd(FirstDate) , units = "days") / 365.25, NA)) %>%
   mutate(DOB_estimate =
    ifelse(is.na(DOB_estimate) & tbl_sex == "M" & !is.na(BirthGroup) & !is.na(EmigrationNatalDate) & EmigrationNatalDate != "Stillpresent" & minus < 3,
           as.character(ymd(EmigrationNatalDate) %m-% years(4)), DOB_estimate))

# when there is a proper difference we can assume FirstDate as DOB
 d <- d%>%
   mutate(DOB_estimate =
   ifelse(is.na(DOB_estimate) & tbl_sex == "M" & !is.na(BirthGroup) & !is.na(EmigrationNatalDate),
          FirstDate, DOB_estimate))

# Few manual cases
d <- d%>%
  mutate(DOB_estimate =
  case_when(LH_AnimalCode %in% c("Eve", "Kil") ~ as.character(ymd(ImmigrationDate1) %m-% years(4)),
            LH_AnimalCode == "Imp" ~ as.character(ymd(EmigrationNatalDate) %m-% years(4)),
            LH_AnimalCode == "Tik" ~ FirstDate,
            TRUE ~ DOB_estimate))

sum(is.na(d$DOB_estimate))
#View(d%>% filter(is.na(DOB_estimate) & tbl_sex == "M" & !is.na(BirthGroup) & !is.na(EmigrationNatalDate)), "diff")

# Baby males ----------
d <-d %>% mutate(DOB_estimate = ifelse(is.na(DOB_estimate) & tbl_sex == "M" & str_detect(AnimalName, "BB|Baby"), FirstDate, DOB_estimate))

# Males with no BirthGroup ----------------

# That is, they are already adults when they migrated, approx 4 yr old minimun. For the individuals that have a
# FirstSeen I will remove 4 years of that initial date. Including for the cases
# of Japan and Ryan despite their ImmigrationDate1 happened before FristSeen

d <- d%>%
  mutate(DOB_estimate = ifelse(is.na(DOB_estimate) & tbl_sex == "M" & is.na(BirthGroup),
                        as.character(ymd(FirstDate) %m-% years(4)),
                        DOB_estimate))

# Calculate age in years when it has DOB---------------------------------------------------
AnimalID_DOB <- tbl_AnimalID %>%
    left_join(tbl_DOB, by = c("AnimalName", "AnimalCode")) %>%
    mutate(DOB = ymd(DOB)) %>%
    mutate(Age_yr = ifelse(is.na(DOB), NA, add_age(birthdate = DOB, unit = "Years")))

head(AnimalID_DOB)

NoAge <- AnimalID_DOB %>%
  filter(is.na(Age_yr))
nrow(NoAge)

# Include the column first seen and DOB_estimate-------------------------------------------------------------
# I will use the add age function to the first seen category
# NOT RELIABLE some are new males that were adults the first time seen
AnimalID_DOB_FD <- AnimalID_DOB %>%
  left_join(.,d%>%
              select(
                AnimalName,
                FirstDate,
                DOB_estimate
              ), by = "AnimalName", relationship = "many-to-many") %>%
  filter(!is.na(AnimalName)) %>%
  mutate(FirstDate = ymd(FirstDate))

AnimalID_DOB_FD <- AnimalID_DOB_FD %>%
    mutate(Age_yr_estimate = ifelse(is.na(DOB_estimate),
                                    NA,
                                    add_age(birthdate = ymd(DOB_estimate), unit = "Years")))
head(AnimalID_DOB_FD)


# filter LH with reliable data
# check if they are dead
# in LH check info about: DateAdult, EmigrationNatalDate

# Add the age categories ---------------------
# baby: 0 yr
# juvenile: 1-3 yr
# sub-adult: males 4 yr
# adult: females- that gave birth to a live baby (miscarriage doesn´t counts) OR 4yr and older
#        males- migrated OR 5yr old

AnimalID_Age_Sex <- AnimalID_DOB_FD %>%
  left_join(., tbl_Sex, by= c("AnimalName","AnimalCode")) %>%
  mutate(Age_class = case_when(
    Age_yr_estimate < 1 ~ "baby",
    Age_yr_estimate < 4 ~ "juvenile",
    Age_yr_estimate <= 5 & Sex == "M" ~ "sub-adult",
    Age_yr_estimate >= 4 & Sex == "F" ~ "adult",
    Age_yr_estimate >= 5 ~ "adult"
  ))

# I will also clean DOB collumn from entries that clain they were born before 2009.
d$DOB <-ifelse(d$DOB > "2009-12-01", d$DOB, NA )

# write csv tbl -----------------------------------------------------------
tbl_Age <- AnimalID_Age_Sex %>% select(AnimalName,AnimalCode, DOB, FirstDate, DOB_estimate, Age_yr, Age_class)

# write.csv(tbl_Age,"tbl_Age.csv",row.names = FALSE)

