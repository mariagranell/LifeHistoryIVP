 # Package -----------------------------------------------------------------

 library(tidyverse)
 library(dplyr)
 library(lubridate)
 library(testthat)
 source("/Users/mariagranell/Repositories/data/life_history/Vervet_Functions_20221107.R")


 # Life History -------------------------------------------------------------


 #Load the LH file
 IVP_LH <- read.csv2('/Users/mariagranell/Repositories/data/life_history/tbl_Creation/CSV/IVP_Life_history_260523.csv',
                     na.strings=c(""," ","NA")) # make blancs into NA
 colnames(IVP_LH)

 # no extra rows with no individuals
 IVP_LH <- IVP_LH %>%
   dplyr::filter(!is.na(Individual))

 # I will create new data coluns in where there will not be data loss when appliying ymd(). However some data will
 # be approximated. I will still work with a ymd() format or the closest to that. I will include a column statisng if the data is approximated

 # change manual entries od dates like "Still present" to the current date in the foormat DD/MM/YYYY.
 todaydate <- as.character(format(Sys.Date(), "%d/%m/%Y"))
 IVP_LH[] <- lapply(IVP_LH, gsub, pattern = "Still present", replacement = todaydate)


 # Reformat the dates and manually correct the errors------------------
 # Ignore the warning message
  # remodel the format of the columns
 ivp_lh <- IVP_LH %>%
   mutate_at(vars(DOB,FirstRecorded, DateAdult, DepartureNatalGp, DateImmigration1, LastSeen1,
                  DateImmigration2, LastSeen2,
                  DateImmigration3, LastSeen3,
                  DateImmigration4, LastSeen4), dmy) %>%
   mutate_at(vars(DOB,FirstRecorded, DateAdult, DepartureNatalGp, DateImmigration1, LastSeen1,
                  DateImmigration2, LastSeen2,
                  DateImmigration3, LastSeen3,
                  DateImmigration4, LastSeen4), as.character)

 # Check if there was any important values deleted because of the change of format. If the number is the
 # same nothing was deleted. The following code only prints the collumns in where there is adiscrepancy for you to check
 {# List of columns to check
columns_to_check <- c("DOB", "FirstRecorded", "DateAdult", "DepartureNatalGp",
                      "DateImmigration1", "LastSeen1", "DateImmigration2", "LastSeen2",
                      "DateImmigration3", "LastSeen3", "DateImmigration4", "LastSeen4")

# Print the number of empty cells for each column in IVP_LH and ivp_lh if they are different
cat("DIFFERENT EMPTY CELLS COUNT\n")
for (col in columns_to_check) {
  count_IVP_LH <- sum(is.na(IVP_LH[[col]]))
  count_ivp_lh <- sum(is.na(ivp_lh[[col]]))

  if (count_IVP_LH != count_ivp_lh) {
    cat(col,
        "\nfor IVP_LH:", count_IVP_LH,
        "\nfor ivp_lh:", count_ivp_lh,
        "\n\n")
  }
}
 rm(col, columns_to_check, count_ivp_lh, count_IVP_LH)}

 # changes for LH file IVP_Life_history_260523.csv
 ### DOB # assuming the losses
 # empty_rows_IVP <- IVP_LH %>% filter(is.na(DOB)) ; view(ivp_lh%>% filter(is.na(DOB)) %>% anti_join(empty_rows_IVP, by ="Individual"))
 ### FirstRecorded
 # empty_rows_IVP <- IVP_LH %>% filter(is.na(FirstRecorded)) ; view(ivp_lh%>% filter(is.na(FirstRecorded)) %>% anti_join(empty_rows_IVP, by ="Individual"))
 ivp_lh[ivp_lh$Individual == "Whidbey", "FirstRecorded" ] <- "2016-10-01" # invented date. It says 2016
 ### DateImmigration1
 # empty_rows_IVP <- IVP_LH %>% filter(is.na(DateImmigration1)) ; view(ivp_lh%>% filter(is.na(DateImmigration1)) %>% anti_join(empty_rows_IVP, by ="Individual"))
 ivp_lh[ivp_lh$Individual == "Camilla", "DateImmigration1" ] <- "2015-02-01" # assume the day 01. Rest is the same
 #ivp_lh[ivp_lh$Individual == "Alcatraz", "ImmigrationGp1" ] <- "Jacaranda" # wrong place
 ### LastSeen1
 # empty_rows_IVP <- IVP_LH %>% filter(is.na(LastSeen1)) ; view(ivp_lh%>% filter(is.na(LastSeen1)) %>% anti_join(empty_rows_IVP, by ="Individual"))
 ivp_lh[ivp_lh$Individual == "Halfy", "LastSeen1" ] <- "2015-07-09" # assume the day 01. Rest is the same
 ivp_lh[ivp_lh$Individual == "Baby Siele 2016", "LastSeen1" ] <- "2017-01-01" # assume the day 01. Rest is the same
 ivp_lh[ivp_lh$Individual == "Spek", "LastSeen1" ] <- "2022-11-31"
 ### LastSeen4
 # empty_rows_IVP <- IVP_LH %>% filter(is.na(LastSeen4)) ; view(ivp_lh%>% filter(is.na(LastSeen4)) %>% anti_join(empty_rows_IVP, by ="Individual"))
 #ivp_lh[ivp_lh$Individual == "Primavera", "ReliableData" ] <- "yes" # wrong place

 # tblLifeHistory ----------------------------------------------------------

 #Make necessary changes
 LH <- ivp_lh %>%

   #REMOVE INDIVIDUAL THAT ARE EMPTY
   filter(Individual != "") %>%

   #RENAME
   rename(LH_RowNumber = Nb,
          LH_AnimalName = Individual,
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
   filter(!(LH_AnimalName == "Goose" & FirstDate == "2021-05-25"),
          !(LH_AnimalName == "Zanzibar" & is.na(DOB)))%>%


   #ANIMALID MANUAL CORRECTION
   #mutate(LH_AnimalName = case_when(LH_AnimalName == "Yelowstone" ~ "Yellowstone",
   #                               LH_AnimalName == "Baby Pann 2017" ~ "BBPann2017",
   #                               LH_AnimalName == "Okucane" ~ "Okuncane",
   #                               LH_AnimalName == "Snortjie"~ "Snorretjie",
   #                               TRUE ~ LH_AnimalName)) %>%

   #ANIMALCODE MANUAL CORRECTION
   mutate(LH_AnimalCode = case_when(LH_AnimalName == "BigEars" ~ "BigEars",
                                    TRUE ~ LH_AnimalCode)) %>%

   #OLD NAMES CORRECTION
   rename(OtherID = Nicknames) %>%
   mutate(OtherID = case_when(LH_AnimalName == "Propriano" ~ "Prague",
                              LH_AnimalName == "Aathabasca" ~ "Ath", # Athabasca had two rows with different spelling
                              # Athabasca was deleted but include Ath in the nicknames of Aat
                              LH_AnimalName == "Goose" ~ "CrazyEyes", # Goose had two entries
                              # I included CrazyEyes to this entry to make it complete
                              TRUE ~ OtherID)) %>%

   #BIRTH GROUP CORRECTION
   mutate(BirthGroup = case_when(LH_AnimalName == "Baby Kodiak 2020" ~ "CR",
                                 TRUE ~ BirthGroup)) %>%

   #DOB CORRECTION

   #EMMIGRATION NATAL DATE CORRECTION
   mutate(EmigrationNatalDate = case_when(LH_AnimalName == "Mvula" ~ "2015-06-13",
                                          TRUE ~ EmigrationNatalDate)) %>%

   #IMMIGRATION GROUP1 CORRECTION
   mutate(ImmigrationGroup1 = case_when(LH_AnimalName == "Delux" ~ NA,
                                        LH_AnimalName == "Nora" ~ NA,
                                        LH_AnimalName == "Gangtok" ~ NA,
                                TRUE ~ ImmigrationGroup1)) %>%
   # REMOVE ALL SPACES IN ANIMAL CODE
   mutate(LH_AnimalCode = str_trim(LH_AnimalCode, side = "both"),
          LH_AnimalName = str_trim(LH_AnimalName, side = "both"))
   #CURRENT GROUP CORRECTION

   #ADD STD ANIMALID
   #std_AnimalID(animalID = LH_AnimalName)

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
 tbl_LifeHistory_15112022 <- LH

 #write.csv(tbl_LifeHistory_15112022,"/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/tbl_LifeHistory_15112022.csv",row.names = FALSE)