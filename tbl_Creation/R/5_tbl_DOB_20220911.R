

# General -----------------------------------------------------------------
#Once the table is generated, it will have to be corrected here on its own 
setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/")

#To generate the first table, it does not matter whether AnimalID_Std is taken from tbl_AnimalID or from tbl_LifeHistory as they were at that time the same 
#However, the question will arise later on 


#If animal where first seen after 01-Jan 2014 in group AK, BD, NH, KB, LT we will assume that first date == Birth date 

# Paths ------------------------------------------------------------------

# input file is with animalID, animalCode

# tbl_AnimalID ------------------------------------------------------------
tbl_AnimalID <- read.csv("tbl_AnimalID.csv")
head(tbl_AnimalID)

# tbl_LifeHistory ---------------------------------------------------------
tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")
str(tbl_LifeHistory)

# Join AnimalID and LH Info -----------------------------------------------
names(tbl_LifeHistory)

AnimalIDDOB <- tbl_AnimalID %>% 
  left_join(., tbl_LifeHistory %>% 
              select(AnimalID_Std,
                     DOB,
                     FirstDate,
                     LH_MotherID,
                     BirthGroup,
                     EmigrationNatalDate), by=c("AnimalID" = "AnimalID_Std")) %>%
  #DATE FORMAT 
  mutate(DOB = ymd(DOB),
         FirstDate = ymd(FirstDate)) %>%
  
  #MONTH OF BIRTH - FIRST DATE
  mutate(DOB_Month = month(DOB),
         FirstDate_Month = month(FirstDate)) %>%
  
  #YEAR OF BIRTH 
  mutate(DOB_Year = year(DOB),
         FirstDate_Year = year(FirstDate))

# Missing DOB -------------------------------------------------------------

#Animals that have a mother should have a date of birth
#Mother no DOB
Mother_NoDOB <- AnimalIDDOB %>% 
  filter(!is.na(LH_MotherID),
         is.na(DOB))
nrow(Mother_NoDOB)
#155 entries

#view(AnimalIDDOB %>% filter(is.na(FirstDate)))


#Animals that have a mother should have a firstdate
#Mother no FirstDate 
Mother_NoFirstDate <- AnimalIDDOB %>% 
  filter(!is.na(LH_MotherID),
         is.na(FirstDate))
nrow(Mother_NoFirstDate)
#4 animals that supposedly have a mother have no first date and no DOB
#These should be checked


#Animals that have a group of birth should have a DOB 
#Mother no DOB
BirthGroup_NoDOB <- AnimalIDDOB %>% 
  filter(!is.na(BirthGroup),
         is.na(DOB),
         FirstDate > "2014-01-01")
nrow(BirthGroup_NoDOB)
#166 entries
  


# Animals that should have DOB  -------------------------------------------
#According to Miguel, one should assign FirstDate as DOB to all animals first seen in group AK, BD, NH, KB, LT after 01/01/2014

#Code below shows there is nothing to correct 

AnimalIDDOB %>% 
  distinct(BirthGroup)

RelevantGroup_Births <- AnimalIDDOB %>%
  
  filter(BirthGroup %in% c("AK",
                           "BD",
                           "KB",
                           "NH",
                           "LT") & DOB_Year > 2013)
nrow(RelevantGroup_Births)
#263 entries


#Birth in relevant groups and periods that lack DOB
RelevantGroup_Births_MissingDOB <- RelevantGroup_Births %>% 
  filter(is.na(DOB))
nrow(RelevantGroup_Births_MissingDOB)
#All animals have a date of birth 
#Nothing to correct here 


# Unrelaiable birth date --------------------------------------------------
#Babies not borm from Occtober to January

ImpossibleDOB <- AnimalIDDOB %>% 
  filter(DOB_Month > 1,
         DOB_Month < 10) %>% 
  arrange(DOB_Month,
          DOB)

#Write csv for Miguel 
#write.csv(ImpossibleDOB,"ImpossibleDOB_ForMiguelTocheck_20221109.csv",row.names = FALSE)

#Bring back Miguel correction 
ImpossibleDOB_MiguelCorrected_20221109 <- read.csv("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/CSV/ImpossibleDOB_MiguelCorrected_20221109.csv")
View(ImpossibleDOB_MiguelCorrected_20221109)


#Unreliable DOB to remove 
UnreliableDOB_ToRemove <- ImpossibleDOB_MiguelCorrected_20221109 %>% 
  filter(RemoveDOB == "yes") %>% 
  select(AnimalID,
         DOB) %>% 
  mutate(DOB = dmy(DOB))
View(UnreliableDOB_ToRemove )



# Mother but noDOB --------------------------------------------------------
#Animals that have a mother should have a date of birth

#We are going to assign first date as DOB in the following cases
#Groups were followed regularly AK, BD, KB, NH, LT
#After 1st January 2014
#i) if first date falls between september and January (Have a look how many first dates in February), then DOB == FirstDate 
#If first date falls outside 


#Mother no DOB
Mother_NoDOB <- AnimalIDDOB %>% 
  filter(!is.na(LH_MotherID),
         is.na(DOB), 
         FirstDate > "2014-01-01",
         BirthGroup %in% c("AK",
                           "BD",
                           "KB",
                           "NH",
                           "LT")) %>% 
  arrange(BirthGroup,
          FirstDate)
nrow(Mother_NoDOB)
#146 entries if no restriction on first date
#100 entries if restiction on first date 
#64 entries if considering only group of interets 


#Mother NODOB to exclude 
Mother_NoDOB_ToExclude <- Mother_NoDOB %>% 
  filter(FirstDate_Month > 1, 
         FirstDate_Month < 9) %>% 
  select(AnimalID)
#only 6 entries 
#Miguel should have a look at them 


#Mother NODOB assign first date as dob
Mother_NoDOB_AssignDOB <- Mother_NoDOB %>% 
  anti_join(.,Mother_NoDOB_ToExclude)
nrow(Mother_NoDOB_AssignDOB)
#58 entries



# Mother no first date  ---------------------------------------------------
#Animals that have a mother should have a firstdate


#Mother no FirstDate 
Mother_NoFirstDate_ToRemove <- AnimalIDDOB %>%
  filter(!is.na(LH_MotherID),
         is.na(FirstDate)) %>% 
  select(AnimalID)
nrow(Mother_NoFirstDate)
#4 animals that supposedly have a mother have no FirstDate
#They can be removed because they also have no DOB



# Mother no first and no dob ----------------------------------------------
#Mother no FirstDate 
Mother_NoFirstDateNoDOB_ToRemove <- AnimalIDDOB %>% 
  filter(!is.na(LH_MotherID),
         is.na(FirstDate),
         is.na(DOB)) %>% 
  select(AnimalID)
View(Mother_NoFirstDateNoDOB_ToRemove)
#4 animals that supposedly have a mother have no FirstDate and no DOB, same as above 




# Birth group No DOB & FirstDate ------------------------------------------

#Animals that have a group of birth should have a DOB 
#Mother no DOB
BirthGroup_NoDOBNoFirstDate <- AnimalIDDOB %>% 
  filter(!is.na(BirthGroup),
         is.na(DOB),
         is.na(FirstDate))

#To Remove
BirthGroup_NoDOBNoFirstDate_ToRemove <- BirthGroup_NoDOBNoFirstDate %>% 
  select(AnimalID)
View(BirthGroup_NoDOBNoFirstDate_ToRemove)
         

# Birth group no DOB ------------------------------------------------------
BirthGroup_NoDOB <- AnimalIDDOB %>% 
  
  #REMOVE BIRTH GROUP AND DOB
  filter(!is.na(BirthGroup),
         is.na(DOB)) %>% 
  
  #REMOVE ANIMAL WITH NO DOB AND NO FIRST DATE 
  #Since they will be removed
  anti_join(.,BirthGroup_NoDOBNoFirstDate_ToRemove) %>% 
  
  #REMOVE DATES AT WHICH SURVEY IS NOT THAT RELIABLE 
  #AN alternative would be to exlude 2 years after the beginning of serious survey, ask Miguel
  filter(FirstDate > "2014-01-01") %>% 
  
  #EXCLUDE GROUPS THAT HAD POOR COVERAGE
  #not often visited, thus no possibility to assess DOB
  filter(!(BirthGroup %in% c ("CR", "IF", "RL"))) %>% 
  
  
  arrange(BirthGroup,
          FirstDate)
  
  #FILTER GROUP THAT WERE FOLLOWED REGULARLY
View(BirthGroup_NoDOB)
#307 entries without date restriction
#170 entries if date restricted to after 1st Jan 2014
#74 entries after group restrictions, 11 of which are born between February and End September 


#Write csv for Miguel 
write.csv(BirthGroup_NoDOB %>% 
            select(AnimalID,
                   FirstDate,
                   BirthGroup,
                   LH_MotherID) %>% 
            mutate(DOB_Assign = ""),'BirthGroup_NoDOB_ForMiguelToCheck_20221110.csv',row.names = FALSE)


#Bring back Miguel check
#He has indicated as no when the first date should not be assigned as DOB 
BirthGroup_NoDOB_MiguelChecked <- read.csv("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/CSV/BirthGroup_NoDOB_MiguelCorrected_20221110.csv")


#Prepare file to be added into tbl_DOB using Miguel comments and correction
BirthGroup_NoDOB_MiguelChecked_DOBToAdd <- BirthGroup_NoDOB_MiguelChecked %>% 
  
  #EXCLUDE DOB ASSIGNEMENT FOR ANIMALS SELECTED BY MIGUEL
  filter(DOB_Assign != "no") %>% 
  
  #ASSIGN DOB TO REMAINING ANIMALS
  mutate(DOB_ToAdd = case_when(comments == "" ~ FirstDate,
         TRUE ~ comments)) %>% 
  
  select(AnimalID,
         DOB_ToAdd) %>% 
  
  mutate(DOB_ToAdd = dmy(DOB_ToAdd))


View(BirthGroup_NoDOB_MiguelChecked_DOBToAdd)




# tbl_DOB_InConstruct -----------------------------------------------------

tbl_DOB_InConstruct <- AnimalIDDOB %>% 
  
  #REMOVE UNRELIABLE DOB (CHECKED BY MIGUEL)
  #6 rows removed
  anti_join(., UnreliableDOB_ToRemove) %>% 
  
  #REMOVE MOTHER NO DOB WITH CORRECTED BY MIGUEL
  anti_join(.,Mother_NoDOB_ToExclude) %>% 
  
  #ASSIGN FIRST DATE AS DOB 
  mutate(DOB2 = case_when(AnimalID %in% Mother_NoDOB_AssignDOB$AnimalID ~ FirstDate,
                          TRUE ~ DOB)) %>% 
  
  #REMOVE MOTHER NO FIRST DATE
  anti_join(.,Mother_NoFirstDate_ToRemove) %>% 
  
  #REMOVE BIRTH GROUP NO DOB NO FIRST DATE 
  anti_join(.,BirthGroup_NoDOBNoFirstDate_ToRemove) %>% 
  
  #ASSIGN DOB TO BIRTH GROUP NO DOB AS INSTRUCTED BY MIGUEL
  left_join(., BirthGroup_NoDOB_MiguelChecked_DOBToAdd) %>% 
  mutate(DOB3 = case_when(!is.na(DOB_ToAdd) ~ DOB_ToAdd,
                          TRUE ~ DOB2))




# tbl_DOB -----------------------------------------------------------------

tbl_DOB <- tbl_DOB_InConstruct %>% 
  select(AnimalID,
         DOB3) %>% 
  filter(!is.na(DOB3)) %>% 
  rename(DOB = DOB3)
View(tbl_DOB )
nrow(tbl_DOB)


# write csv tbl -----------------------------------------------------------

write.csv(tbl_DOB,"tbl_DOB.csv",row.names = FALSE)
