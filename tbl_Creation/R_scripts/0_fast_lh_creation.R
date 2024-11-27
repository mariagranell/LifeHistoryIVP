# ---------------
# Title: Resume lh creation
# Date: 27 nov 2024
# Author: mgranellruiz
# Goal: Compile all the R scripts to create my version of life history.
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source("/Users/mariagranell/Repositories/data/life_history/Vervet_Functions.R")

library(readODS)

# path ------------------------
setwd("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL")

# data ------------------------
IVP_LH <- read_ods("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/Source_files/IVPLifeHistoryJakobMCleaning_27112024.ods", sheet=2) %>%
   mutate(across(where(is.character), ~na_if(.x, "NA")))

# I will create new data coluns in where there will not be data loss when appliying ymd(). However some data will
# be approximated. I will still work with a ymd() format or the closest to that. I will include a column statisng if the data is approximated

# change manual entries of dates like "Still present" to the current date in the format DD/MM/YYYY.
todaydate <- as.character(format(Sys.Date(), "%d/%m/%Y"))
IVP_LH[] <- lapply(IVP_LH, gsub, pattern = "Still present", replacement = todaydate)

# Reformat the dates and manually correct the errors------------------

# remodel the format of the columns
ivp_lh <- IVP_LH %>%
   mutate_at(vars(DOB, FirstRecorded, DateAdult, DepartureNatalGp, DateImmigration1, LastSeen1,
                  DateImmigration2, LastSeen2,
                  DateImmigration3, LastSeen3,
                  DateImmigration4, LastSeen4), dmy) %>%
   mutate_at(vars(DOB,FirstRecorded, DateAdult, DepartureNatalGp, DateImmigration1, LastSeen1,
                  DateImmigration2, LastSeen2,
                  DateImmigration3, LastSeen3,
                  DateImmigration4, LastSeen4), as.character)

#Make necessary changes -------------------------
{ LH <- ivp_lh %>%

   #RENAME
   rename(LH_RowNumber = Nb,
          AnimalName = Individual,
          AnimalCode = Code,
          OtherID = Nicknames,
          DOBAccuracy = "DOB Accuracy",
          FirstDate = FirstRecorded,
          MotherID = Mother,
          FatherID = Father,
          BirthGroup = BirthGp,
          EmigrationNatalDate = DepartureNatalGp,
          FirstDate = FirstRecorded,
          LastDate1 = LastSeen1,
          LastDate2 = LastSeen2,
          LastDate3 = LastSeen3,
          LastDate4 = LastSeen4,
          ImmigrationDate1 = DateImmigration1,
          ImmigrationDate2 = DateImmigration2,
          ImmigrationDate3 = DateImmigration3,
          ImmigrationDate4 = DateImmigration4,
          ImmigrationGroup1 = ImmigrationGp1,
          ImmigrationGroup2 = ImmigrationGp2,
          ImmigrationGroup3 = ImmigrationGp3,
          ImmigrationGroup4 = ImmigrationGp4,
          CurrentGroup = PresentGp) %>%

   #DEAL WITH DUPLICATED ANIMALID
   filter(!(AnimalName == "Goose" & FirstDate == "2021-05-25"),
          !(AnimalName == "Zanzibar" & is.na(DOB)),
          !(AnimalName == "Dinosaur" & ReliableData == "NO")) %>%
  mutate(AnimalName = ifelse(AnimalName == "Mercury" & DOB == "2012-01-01", "Mercury1", AnimalName)) %>%

   #ANIMALCODE MANUAL CORRECTION
   mutate(AnimalCode = case_when(AnimalName == "BigEars" ~ "BigEars",
                                    TRUE ~ AnimalCode)) %>%

   # CLEAN OTHERID
   mutate(OtherID = str_replace(string = OtherID,pattern = ",", replacement = ";"),
          OtherID = str_replace(string = OtherID, pattern = "; ", replacement = ";")) %>%
   #OLD NAMES CORRECTION
   mutate(OtherID = case_when(AnimalName == "Goose" ~ "CrazyEyes", # Goose had two entries
                              # I included CrazyEyes to this entry to make it complete
                              TRUE ~ OtherID)) %>%

   # CLEAN DOB
   # wes and Pun have a DOB but no Mum and no group. Suspicious.
   # Kaya from LT has a DOB but no mum.. but we will allow it since is a female
   mutate(DOB = case_when(AnimalCode %in% c("Wes", "Pun") ~ NA, TRUE ~ DOB))

   # Changes not needed anymore: #ANIMALID MANUAL CORRECTION, #ANIMALCODE MANUAL CORRECTION, #OLD NAMES CORRECTION, #BIRTH GROUP CORRECTION, #DOB CORRECTION, #EMMIGRATION NATAL DATE CORRECTION, #IMMIGRATION GROUP1 CORRECTION, # REMOVE ALL SPACES IN ANIMAL CODE, #CURRENT GROUP CORRECTION, #ADD STD ANIMALID

 # Generalte a Fate column -----------------------------------------
 #Checking from the comments seeing which animals are supposed to be where
 LH <- LH %>%
   mutate( Fate_probable = ifelse(grepl("Kill*|Dead|Died|kill*|dead|died|body|Body|decease*", Comments), "dead",
                  ifelse(grepl("Dissap|diss", Comments),"dissapeared",
                                  ifelse(grepl("migra*|Migra*", Comments), "migrated",
                     "NA")))) %>%
   mutate( Fate_probable = na_if(Fate_probable, "NA"))

 # Mantain Still present ---------------------------------------------------
 LH[] <- lapply(LH, gsub, pattern = todaydate, replacement = "Stillpresent")
}
#### Create KeyOTherID ------
{
lh_AnimalIDclean <- LH %>% separate_rows(OtherID, sep = ";") %>% # Separate OtherID
  rename(Other_ID = OtherID)

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

# merge the names from matias file into LH. inluding only names that are names consistenly. there is more information in the ¨
# file that will be lost with the merge, specially baby names
tbl_AnimalID <- lh_AnimalIDclean %>%
  left_join(extra_OtherID, by = c("AnimalCode", "AnimalName", "Other_ID"), relationship =  "many-to-many") %>%
  group_by(LH_RowNumber, AnimalCode, AnimalName) %>%
  summarise(OtherID = if(all(is.na(Other_ID))) NA else paste(na.omit(Other_ID), collapse = ";"),
              .groups = 'drop')%>%
  mutate(OtherID = str_replace_all(OtherID, ",", ";")) %>%
  filter(!is.na(AnimalCode) & !is.na(AnimalName))

KeyOtherID <- tbl_AnimalID %>%
  dplyr::select(AnimalCode, OtherID) %>%
  separate_rows(OtherID, sep = ";") %>%
  filter(!is.na(OtherID)) %>% distinct() %>%
  mutate(OtherID = str_trim(OtherID, side = "left"))

write.csv(KeyOtherID, "../TBL/Archive_tbl/lh_181124/KeyOtherID.csv", row.names = FALSE)
}

# Select collumns that should be cleaned:
# LH_RowNumber, AnimalName, AnimalCode, OtherID, Sex, DOB, DOBAccuracy, MotherID, FatherID, Fate_probable, ReliableData, Comments)

## DOB estimate -------
# CREATE A COLUMN DOB_estimate
# It can only be used for age categories! not adviced to use for any other calculation.
# ignore the warnings
{
d <- LH
# Double check DOB ---------------------
d$DOB_estimate <- d$DOB

# Some individuals have the DOB wrong, that is claimiing they were born in the 1900.
# I will only allow in DOB_estimate, DOB bigger than 2009 (IVP started in 2010)
d$DOB_estimate <-ifelse(d$DOB_estimate > "2009-12-01", d$DOB_estimate, NA )


# number of individuals with no DOB and no Sex --------------
# because of the data I can asume Jalitah and Corfu First dates can be DOB_estimated
# the rest are too empty
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
d[,"DOB_estimate"] <- ifelse(is.na(d$DOB_estimate) & d$Sex == "F" & d$FirstDate < paste0(adult_min_age, "-01-01"), d$FirstDate, d$DOB_estimate)

# if the data was collected after 2020 in anygroup, DOB can be approximated as FirstDate
d[,"DOB_estimate"] <-
  ifelse(is.na(d$DOB_estimate) & d$Sex == "F" & d$FirstDate > "2020-01-01", d$FirstDate, d$DOB_estimate)


# Babies with no DOB -------------------------
# Since we are doing the separation by Sex we miss estimating the age for those individuals (mostly BB) that
# don´t have their sex determined. But is the same logic as before. we will only have individuals with BB in their names
d <-d %>% mutate(DOB_estimate = ifelse(is.na(DOB_estimate) & is.na(Sex) & str_detect(AnimalName, "BB|Baby"), FirstDate, DOB_estimate))


# Males with no DOB -------------------------

# Males with a BirthGroup -----------

# when their first migration was before they where 3 years old we cannot assume that they FirstDate is their DOB.
# we will calculate a new DOB by subtracting 4 years from the EmigrationNatalDate
# ignore the warning
 d<- d%>%
   mutate(minus = ifelse(EmigrationNatalDate != "Stillpresent" | is.na(EmigrationNatalDate),
                         difftime(ymd(EmigrationNatalDate), ymd(FirstDate) , units = "days") / 365.25, NA)) %>%
   mutate(DOB_estimate =
    ifelse(is.na(DOB_estimate) & Sex == "M" & !is.na(BirthGroup) & !is.na(EmigrationNatalDate) & EmigrationNatalDate != "Stillpresent" & minus < 3,
           as.character(ymd(EmigrationNatalDate) %m-% years(4)), DOB_estimate))

# when there is a proper difference we can assume FirstDate as DOB
 d <- d%>%
   mutate(DOB_estimate =
   ifelse(is.na(DOB_estimate) & Sex == "M" & !is.na(BirthGroup) & !is.na(EmigrationNatalDate),
          FirstDate, DOB_estimate))

# Few manual cases
d <- d%>%
  mutate(DOB_estimate =
  case_when(AnimalCode %in% c("Eve", "Kil") ~ as.character(ymd(ImmigrationDate1) %m-% years(4)),
            AnimalCode == "Imp" ~ as.character(ymd(EmigrationNatalDate) %m-% years(4)),
            AnimalCode == "Tik" ~ FirstDate,
            TRUE ~ DOB_estimate))

sum(is.na(d$DOB_estimate))
#View(d%>% filter(is.na(DOB_estimate) & Sex == "M" & !is.na(BirthGroup) & !is.na(EmigrationNatalDate)), "diff")

# Baby males ----------
d <-d %>% mutate(DOB_estimate = ifelse(is.na(DOB_estimate) & Sex == "M" & str_detect(AnimalName, "BB|Baby"), FirstDate, DOB_estimate))

# Males with no BirthGroup ----------------

# That is, they are already adults when they migrated, approx 4 yr old minimun. For the individuals that have a
# FirstSeen I will remove 4 years of that initial date. Including for the cases
# of Japan and Ryan despite their ImmigrationDate1 happened before FristSeen

d <- d%>%
  mutate(DOB_estimate = ifelse(is.na(DOB_estimate) & Sex == "M" & is.na(BirthGroup),
                        as.character(ymd(FirstDate) %m-% years(4)),
                        DOB_estimate))

# Include the column first seen and DOB_estimate-------------------------------------------------------------
# I will use the add age function to the first seen category
# NOT RELIABLE some are new males that were adults the first time seen
AnimalID_DOB_FD <- LH %>%
  left_join(.,d%>%
              dplyr::select(
                AnimalName,
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
  mutate(Age_class = case_when(
    Age_yr_estimate < 1 ~ "baby",
    Age_yr_estimate < 4 ~ "juvenile",
    Age_yr_estimate <= 5 & Sex == "M" ~ "sub-adult",
    Age_yr_estimate >= 4 & Sex == "F" ~ "adult",
    Age_yr_estimate >= 5 ~ "adult"
  ))

# I will also clean DOB collumn from entries that claim they were born before 2009.
d$DOB <-ifelse(d$DOB > "2009-12-01", d$DOB, NA )


tbl_DOB_estimate <- d %>%
  dplyr::select(LH_RowNumber, AnimalName, AnimalCode, OtherID,
                Sex, DOB, DOBAccuracy, DOB_estimate ,MotherID, FatherID, Fate_probable, ReliableData, Comments)
}
tbl_DOB_estimate <- AnimalID_Age_Sex %>%
  dplyr::select(LH_RowNumber, AnimalName, AnimalCode, OtherID,
                Sex, DOB, DOBAccuracy, DOB_estimate , Age_class, FirstDate,
                MotherID, FatherID, Fate_probable, ReliableData, Comments)

## Group memebership -------
{
d <- LH
# 1. Data cleaning ------------------
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
}

### Export ---
# MERGE DATAFRAMES --------------------
factchecked_LH <- tbl_DOB_estimate %>%
  #left_join(.,tbl_Sex, by = c("AnimalName","AnimalCode","LH_RowNumber")) %>% we have the sex
  left_join(.,tbl_GroupMembership, by = "AnimalName", multiple = "all") %>% # the problem is mercury but don+t worry.
  distinct()

write.csv(factchecked_LH, "/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/fast_factchecked_LH_27112024.csv", row.names = FALSE)
