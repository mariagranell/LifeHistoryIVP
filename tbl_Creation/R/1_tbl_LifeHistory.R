 # Package -----------------------------------------------------------------

 library(tidyverse)
 library(lubridate)


 # Life History -------------------------------------------------------------


 #LH file
 #After steph changes
 #We won't work with anymore with another LHF, unless the structure is retained
 IVP_LH <- read.csv2('/Users/mariagranell/Repositories/data_summaries/life_history/IVP_Lifehistory_020822.csv',
                     na.strings=c(""," ","NA")) # make blancs into NA
 names(IVP_LH)

 # I will create new data coluns in where there will not be data loss when appliying ymd(). However some data will
 # be approximated. I will still work with a ymd() format or the closest to that. I will include a column statisng if the data is approximated

 # change manual entries od dates like "Still present" to the current date.
 todaydate <- as.character(format(Sys.Date(), "%d/%m/%Y"))
 IVP_LH[] <- lapply(IVP_LH, gsub, pattern = "Still present", replacement = todaydate)

 a <-IVP_LH %>%
   summarize(DOB = sum(is.na(DOB)),
          FirstRecorded = sum(is.na(FirstRecorded)),
          DateAdult = sum(is.na(DateAdult)),
          EmigrationNatalDate = sum(is.na(DepartureNatalGp)),
          ImmigrationDate1 = sum(is.na(DateImmigration1)),
          ImmigrationDate2 = sum(is.na(DateImmigration2)),
          ImmigrationDate3 = sum(is.na(DateImmigration3)),
          ImmigrationDate4 = sum(is.na(DateImmigration4)),
          LastDate1 = sum(is.na(LastSeen1)),
          LastDate2 = sum(is.na(LastSeen2)),
          LastDate3 = sum(is.na(LastSeen3)),
          LastDate4 = sum(is.na(LastSeen4)))

 #IVP_LH$DOB <- as.Date(format(as.POSIXct(IVP_LH$DOB, format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$FirstRecorded <- as.Date(format(as.POSIXct(IVP_LH$FirstRecorded, format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$DepartureNatalGp <- as.Date(format(as.POSIXct(IVP_LH$DepartureNatalGp,format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$DateImmigration1 <- as.Date(format(as.POSIXct(IVP_LH$DateImmigration1,format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$LastSeen1 <- as.Date(format(as.POSIXct(IVP_LH$LastSeen1, format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$DateImmigration2 <- as.Date(format(as.POSIXct(IVP_LH$DateImmigration2,format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$LastSeen2 <- as.Date(format(as.POSIXct(IVP_LH$LastSeen2, format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$DateImmigration3 <- as.Date(format(as.POSIXct(IVP_LH$DateImmigration3,format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$LastSeen3 <- as.Date(format(as.POSIXct(IVP_LH$LastSeen3, format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$DateImmigration4 <- as.Date(format(as.POSIXct(IVP_LH$DateImmigration4,format = "%d/%m/%Y"), "%Y-%m-%d"))
 IVP_LH$LastSeen4 <- as.Date(format(as.POSIXct(IVP_LH$LastSeen4, format = "%d/%m/%Y"), "%Y-%m-%d"))


 # tblLifeHistory ----------------------------------------------------------

 #Make necessary changes
 LH <- IVP_LH %>%

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

   #CHANGE DATE FORMAT
   # there is some loss of information. If the date only had 1 year and not a proper date the entry is loss to NA.
   mutate(DOB = dmy(DOB), # different format
          FirstDate = ymd(FirstDate),
          DateAdult = dmy(DateAdult), # is almost empty
          EmigrationNatalDate = ymd(EmigrationNatalDate),
          ImmigrationDate1 = ymd(ImmigrationDate1),
          ImmigrationDate2 = ymd(ImmigrationDate2),
          ImmigrationDate3 = ymd(ImmigrationDate3),
          ImmigrationDate4 = ymd(ImmigrationDate4),
          LastDate1 = ymd(LastDate1),
          LastDate2 = ymd(LastDate2),
          LastDate3 = ymd(LastDate3),
          LastDate4 = ymd(LastDate4)) %>%

   #DEAL WITH DUPLICATED ANIMALID
   filter(!(LH_AnimalID == "Goose" & FirstDate == "2021-05-25"),
          !(LH_AnimalID == "Zanzibar" & is.na(DOB)),
          !(LH_AnimalID == "Aathabasca")) %>% #Athabasca had two rows with different spelling


   #ANIMALID MANUAL CORRECTION
   mutate(LH_AnimalID = case_when(LH_AnimalID == "Yelowstone" ~ "Yellowstone",
                                  LH_AnimalID == "Baby Pann 2017" ~ "Baby Pannekoekie 2017",
                                  LH_AnimalID == "Okucane" ~ "Okuncane",
                                  LH_AnimalID == "Snortjie"~ "Snorretjie",
                                  TRUE ~ LH_AnimalID)) %>%


   #OLD NAMES CORRECTION
   rename(OtherID = Nicknames) %>%
   mutate(OtherID = case_when(LH_AnimalID == "Propriano" ~ "Prague",
                              TRUE ~ OtherID)) %>%

   #BIRTH GROUP CORRECTION
   mutate(BirthGroup = case_when(LH_AnimalID == "Baby Kodiak 2020" ~ "CR",
                                 TRUE ~ BirthGroup)) %>%

   #DOB CORRECTION
   mutate(DOB = case_when(LH_AnimalID %in% c("Puntag","Wesley") ~ ymd(NA),
                          TRUE ~ DOB)) %>%

   #EMMIGRATION NATAL DATE CORRECTION
   mutate(EmigrationNatalDate = case_when(LH_AnimalID == "Mvula" ~ ymd("2015-06-13"),
                                          TRUE ~ EmigrationNatalDate)) %>%

   #LAST DATE 1 CORRECTION
   mutate(LastDate1 = case_when(LH_AnimalID == "Halfy" ~ ymd("2015-07-09"),
                                TRUE ~ LastDate1)) %>%

   #IMMIGRATION GROUP1 CORRECTION
   mutate(ImmigrationGroup1 = case_when(LH_AnimalID == "Delux" ~ as.character(NA),
                                        LH_AnimalID == "Nora" ~ as.character(NA),
                                        LH_AnimalID == "Ermelo" ~ "NH",
                                        LH_AnimalID == "Gangtok" ~ as.character(NA),
                                TRUE ~ ImmigrationGroup1)) %>%

   #CURRENT GROUP CORRECTION
   mutate(CurrentGroup = case_when(LH_AnimalID == "Nul" ~ as.character(NA),
                                          TRUE ~ CurrentGroup)) %>%

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


 #view(LH %>% filter(LH_AnimalID == "Ermelo") %>% pull(ImmigrationGroup1))

 #view(LH %>% filter(AnimalID_Std %in% c("Aathabasca", "Athabasca")))
 #Kenneth and Joe and Liam may be the same animals, this will be sent by Miguel to the project manager and we will see how to solve this

 # Duplicated ID -----------------------------------------------------------
 #Should be one entry per animalID

 #LH_DuplicatedID <- LH %>% group_by(LH_AnimalID) %>% mutate(n=n()) %>% ungroup() %>% filter(n1)
 #view(LH_DuplicatedID)
 #Seems that different animals may have been named the same
 #Goose and Zanzibar seems to have been assigned 2x
 #Removed entries based on Miguel advice


 # Generalte a Fate column -----------------------------------------
 #Checking from the comments seeing whihc animals are supposed to be where
 LH <- LH %>%
   mutate( Fate_probable = ifelse(grepl("Kill*|Dead|Died|kill*|dead|died|body|Body|decease*", Comments), "dead",
                  ifelse(grepl("Dissap|diss", Comments),"dissapeared",
                                  ifelse(grepl("migra*|Migra*", Comments), "migrated",
                     "NA")))) %>%
   mutate( Fate_probable = na_if(Fate_probable, "NA"))
 LH[LH$AnimalID_Std == "Liffey", 30] <- "dead"

 # Generate tbl_LifeHistory ------------------------------------------------

 directory <- "/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/"
 name_table <- "tbl_LifeHistory.csv"
 full_table_name <- gsub(" ", "", paste(directory, name_table))

 tbl_LifeHistory <- LH

 write.csv(tbl_LifeHistory,full_table_name,row.names = FALSE)