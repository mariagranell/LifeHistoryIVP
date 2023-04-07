 # Package -----------------------------------------------------------------

 library(tidyverse)
 library(lubridate)
 library(testthat)
 source("/Users/mariagranell/Repositories/phllipe_vulloid/Vervet_Functions_20221107.R")


 # Life History -------------------------------------------------------------


 #LH file
 #After steph changes
 #We won't work with anymore with another LHF, unless the structure is retained
 IVP_LH <- read.csv2('/Users/mariagranell/Repositories/data_summaries/life_history/IVP_Life history_290323.csv',
                     na.strings=c(""," ","NA")) # make blancs into NA
 names(IVP_LH)

 # remove extra rows with no individuals
 IVP_LH <- IVP_LH %>%
   filter(!is.na(Individual))

 # I will create new data coluns in where there will not be data loss when appliying ymd(). However some data will
 # be approximated. I will still work with a ymd() format or the closest to that. I will include a column statisng if the data is approximated

 # change manual entries od dates like "Still present" to the current date.
 todaydate <- as.character(format(Sys.Date(), "%Y-%m-%d"))
 IVP_LH[] <- lapply(IVP_LH, gsub, pattern = "Still present", replacement = todaydate)


 # Reformat the dates and manually correct the errors------------------
 # Ignore the warning message
 { # remodel the format of the columns
 ivp_lh <- IVP_LH %>%
   mutate_at(vars(DOB,FirstRecorded, DateAdult, DepartureNatalGp, DateImmigration1, LastSeen1,
                  DateImmigration2, LastSeen2,
                  DateImmigration3, LastSeen3,
                  DateImmigration4, LastSeen4), ymd) %>%
   mutate_at(vars(DOB,FirstRecorded, DateAdult, DepartureNatalGp, DateImmigration1, LastSeen1,
                  DateImmigration2, LastSeen2,
                  DateImmigration3, LastSeen3,
                  DateImmigration4, LastSeen4), as.character)

 # and remove the problems when reformating the date. Run the view lines to see the problematic rows
 #view(anti_join(IVP_LH,ivp_lh, by =c("Individual", "DOB"))) # assuming the losses
 #view(anti_join(IVP_LH,ivp_lh, by =c("Individual", "FirstRecorded"))) # changes below
 ivp_lh[ivp_lh$Individual == "Whidbey", "FirstRecorded" ] <- "2016-10-01" # invented date. It says 2016
 ivp_lh[ivp_lh$Individual == "Alabama", "FirstRecorded" ] <- "2022-10-31"
 ivp_lh[ivp_lh$Individual == "BBHera22", "FirstRecorded" ] <- "2023-01-18"
 #view(anti_join(IVP_LH,ivp_lh, by =c("Individual", "DepartureNatalGp")))
 ivp_lh[ivp_lh$Individual == "Heelal", "DepartureNatalGp" ] <- "2023-01-20"
 #view(anti_join(IVP_LH,ivp_lh, by =c("Individual", "DateImmigration1")))
 ivp_lh[ivp_lh$Individual == "Camila", "DateImmigration1" ] <- "2015-02-01" # assume the day 01. Rest is the same
 ivp_lh[ivp_lh$Individual == "Alcatraz", "ImmigrationGp1" ] <- "Jacaranda" # wrong place
 #view(anti_join(IVP_LH,ivp_lh, by =c("Individual", "LastSeen1")))
 ivp_lh[ivp_lh$Individual == "BBDian22", "LastSeen1" ] <- "2022-11-10" # 222 instead 2022
 ivp_lh[ivp_lh$Individual == "Spek", "LastSeen1" ] <- "2022-11-31"
 ivp_lh[ivp_lh$Individual == "Halfy", "LastSeen1" ] <- "2015-07-09" # assume the day 01. Rest is the same
 ivp_lh[ivp_lh$Individual == "Baby Siele 2016", "LastSeen1" ] <- "2017-01-01" # assume the day 01. Rest is the same
 #view(anti_join(IVP_LH,ivp_lh, by =c("Individual", "LastSeen2")))
 ivp_lh[ivp_lh$Individual == "Vladivostok", "LastSeen2" ] <- "2023-02-04"
 #view(anti_join(IVP_LH,ivp_lh, by =c("Individual", "LastSeen4")))
 ivp_lh[ivp_lh$Individual == "Primavera", "ReliableData" ] <- "yes"}




 # tblLifeHistory ----------------------------------------------------------

 #Make necessary changes
 LH <- ivp_lh %>%

   #REMOVE INDIVIDUAL THAT ARE EMPTY
   filter(Individual != "") %>%

   #RENAME
   rename(LH_RowNumber = Nb,
          LH_AnimalID = Individual,
          LH_AnimalCode = Code,
          FirstDate = FirstRecorded, #Ask Miguel what it means
          LH_MotherID = Mother,
          LH_FatherID = Father, #How was it determined?
          BirthGroup = BirthGp,
          EmigrationNatalDate = DepartureNatalGp, #Emmigration1? The other emmigration are given by last date right?
          FirstDate = FirstRecorded,
          LastDate1 = LastSeen1,
          LastDate2 = LastSeen2,
          LastDate3 = LastSeen3,
          LastDate4 = LastSeen4,
          ImmigrationDate1 = DateImmigration1, #Date of entry in a new group?
          ImmigrationDate2 = DateImmigration2,
          ImmigrationDate3 = DateImmigration3,
          ImmigrationDate4 = DateImmigration4,
          ImmigrationGroup1 = ImmigrationGp1,
          ImmigrationGroup2 = ImmigrationGp2,
          ImmigrationGroup3 = ImmigrationGp3,
          ImmigrationGroup4 = ImmigrationGp4,
          CurrentGroup = PresentGp) %>%


   #DEAL WITH DUPLICATED ANIMALID
   filter(!(LH_AnimalID == "Goose" & FirstDate == "2021-05-25"),
          !(LH_AnimalID == "Zanzibar" & is.na(DOB)))%>%


   #ANIMALID MANUAL CORRECTION
   mutate(LH_AnimalID = case_when(LH_AnimalID == "Yelowstone" ~ "Yellowstone",
                                  LH_AnimalID == "Baby Pann 2017" ~ "Baby Pannekoekie 2017",
                                  #LH_AnimalID == "Okucane" ~ "Okuncane",
                                  #LH_AnimalID == "Snortjie"~ "Snorretjie",
                                  TRUE ~ LH_AnimalID)) %>%

   #ANIMALCODE MANUAL CORRECTION
   mutate(LH_AnimalCode = case_when(LH_AnimalID == "BigEars" ~ "BigEars",
                                    TRUE ~ LH_AnimalCode)) %>%

   #OLD NAMES CORRECTION
   rename(OtherID = Nicknames) %>%
   mutate(OtherID = case_when(LH_AnimalID == "Propriano" ~ "Prague",
                              LH_AnimalID == "Aathabasca" ~ "Ath", # Athabasca had two rows with different spelling
                              # Athabasca was deleted but include Ath in the nicknames of Aat
                              LH_AnimalID == "Goose" ~ "CrazyEyes", # Goose had two entries
                              # I included CrazyEyes to this entry to make it complete
                              TRUE ~ OtherID)) %>%

   #BIRTH GROUP CORRECTION
   mutate(BirthGroup = case_when(LH_AnimalID == "Baby Kodiak 2020" ~ "CR",
                                 TRUE ~ BirthGroup)) %>%

   #DOB CORRECTION
   #mutate(DOB = case_when(LH_AnimalID %in% c("Puntag","Wesley") ~ ymd(NA),TRUE ~ DOB)) %>%

   #EMMIGRATION NATAL DATE CORRECTION
   mutate(EmigrationNatalDate = case_when(LH_AnimalID == "Mvula" ~ "2015-06-13",
                                          TRUE ~ EmigrationNatalDate)) %>%

   #IMMIGRATION GROUP1 CORRECTION
   mutate(ImmigrationGroup1 = case_when(LH_AnimalID == "Delux" ~ NA,
                                        LH_AnimalID == "Nora" ~ NA,
                                        LH_AnimalID == "Gangtok" ~ NA,
                                TRUE ~ ImmigrationGroup1)) %>%

   #CURRENT GROUP CORRECTION
   #mutate(CurrentGroup = case_when(LH_AnimalID == "Nul" ~ as.character(NA), TRUE ~ CurrentGroup)) %>%

   #ADD STD ANIMALID
   std_AnimalID(animalID = LH_AnimalID)

LH%>%
   summarise(DOB = sum(is.na((DOB))),
          FirstDate = sum(is.na((FirstDate))),
          DateAdult = sum(is.na((DateAdult))),
          EmigrationNatalDate = sum(is.na((EmigrationNatalDate))),
          ImmigrationDate1 = sum(is.na((ImmigrationDate1))),
          ImmigrationDate2 = sum(is.na((ImmigrationDate2))),
          ImmigrationDate3 = sum(is.na((ImmigrationDate3))),
          ImmigrationDate4 =sum(is.na((ImmigrationDate4))),
          LastDate1 = sum(is.na((LastDate1))),
          LastDate2 = sum(is.na((LastDate2))),
          LastDate3 = sum(is.na((LastDate3))),
          LastDate4 = sum(is.na((LastDate4))))


 #Kenneth and Joe and Liam may be the same animals, this will be sent by Miguel to the project manager and we will see how to solve this


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

 # Generate tbl_LifeHistory ------------------------------------------------
 tbl_LifeHistory <- LH

 write.csv(tbl_LifeHistory,"/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/tbl_LifeHistory.csv",row.names = FALSE)