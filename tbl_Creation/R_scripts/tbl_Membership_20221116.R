


# Package -----------------------------------------------------------------


library(tidyverse)
library(lubridate)




# General -----------------------------------------------------------------

#I would need to have a tbl_Birth group. Or actually this could be queried from the membership table

#One issue that we found we Miguel is that CurrentGroup must be updated. The consequence of
# animals having CurrentGroup != NA whereas they have disappeared from the population but
# do not have a closing date (NatalEmigrationDate,LastDate1,2,3,4) lead to the assigment of
# these animals to the alive category. Miguel will send an email to the project and ask about it

setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria")

# tbl_AnimalID ------------------------------------------------------------

tbl_AnimalID <- read.csv("tbl_AnimalID.csv")


# tbl_LifeHistory ---------------------------------------------------------

tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")
View(tbl_LifeHistory)


# tbl_Sex -----------------------------------------------------------------

tbl_Sex <- read.csv("tbl_Sex.csv")


# tbl_DOB -----------------------------------------------------------------

tbl_DOB <- read.csv("tbl_DOB.csv")


# tbl_Natal ---------------------------------------------------------------

tbl_Natal <- read.csv("tbl_Natal.csv")
names(tbl_Natal)
View(tbl_Natal)


# Bind tbl info with relevant LHF -----------------------------------------

Immigration_RawInfo <- tbl_AnimalID %>% 
  left_join(.,tbl_Natal) %>% 
  left_join(., tbl_Sex) %>% 
  left_join(., tbl_DOB) %>% 
  left_join(., tbl_LifeHistory %>% 
              select(AnimalID_Std,
                     FirstDate,
                     BirthGroup,
                     EmigrationNatalDate:LastDate4,
                     CurrentGroup), by=c("AnimalID" = "AnimalID_Std")) %>% 
  
  #DATE FORMAT
  mutate(DOB = ymd(DOB),
         FirstDate = ymd(FirstDate),
         EmigrationNatalDate = ymd(EmigrationNatalDate),
         ImmigrationDate1 = ymd(ImmigrationDate1),
         ImmigrationDate2 = ymd(ImmigrationDate2),
         ImmigrationDate3 = ymd(ImmigrationDate3),
         ImmigrationDate4 = ymd(ImmigrationDate4),
         LastDate1 = ymd(LastDate1),
         LastDate2 = ymd(LastDate2),
         LastDate3 = ymd(LastDate3),
         LastDate4 = ymd(LastDate4))
View(Immigration_RawInfo)


# Split group -------------------------------------------------------------

Groups <- Immigration_RawInfo %>% 
  select(BirthGroup,ImmigrationGroup1, ImmigrationGroup2,ImmigrationGroup3,ImmigrationGroup4,CurrentGroup) %>% 
  pivot_longer(cols = BirthGroup:CurrentGroup, names_to = "GroupType", values_to = "Group") %>% 
  distinct(Group) %>% 
  arrange(Group) %>% 
  mutate(Split = "") %>% 
  mutate(Followed = "")

#Source_files for Miguel
#write.csv(Groups,"Groups_ForMiguelToCheck_20221111.csv",row.names = FALSE)

#Bring back Miguel files


# Natal group general info and DC -----------------------------------------

#Animal with a DOB but that have no birth group 
View(Immigration_RawInfo %>% 
       filter(!is.na(DOB),
              is.na(BirthGroup)))
#2 entries that must be solved 
#Wesley and Puntag, First date prrobably recorded as DOB. Correction made in tbl_LifeHistory. Correctly, they did not appear in tbl_DOB
#All good


#Animal with emmigration natal date but no birth group 
View(Immigration_RawInfo%>% 
       filter(!is.na(EmigrationNatalDate),
              is.na(BirthGroup)))
#0 entries all good 


#Are there females that emmigrated?
View(Immigration_RawInfo %>% 
       filter(Sex == "F",
              !is.na(EmigrationNatalDate)))
#They all seem to be splinter group so no emigration
#No more emmigration info after that so all good 


#Are there females that emmigrated?
  View(Immigration_RawInfo %>% 
         filter(Sex == "F",
                !is.na(EmigrationNatalDate)))
#They all seem to be splinter group so no emigration
    
    
#Emmigrated but no date of leaving natal group 
View(Immigration_RawInfo %>% 
         filter(!is.na(DOB),
                is.na(EmigrationNatalDate),
                (!is.na(ImmigrationGroup1) |
                  !is.na(ImmigrationDate1))))
#All splinters thus normal they have no immigration group or dates

  


# Natal DOB No emmigration ------------------------------------------------
#Could be dead/disappeared from natal
#Could be alive in natal group


#No natal emmigration
Natal_DOB_NoEmi <- Immigration_RawInfo %>% 
    
    #RETAIN ANIMAL BORN IN POP
    filter(!is.na(DOB)) %>%
    
    #RETAIN ANIMAL DID NOT EMMIGRATE
    #I think it creates an issue as some animals were assigned an emmigration natal date 
    filter(is.na(ImmigrationGroup1)) 
nrow(Natal_DOB_NoEmi)
#353 entries

Natal_DOB_NoEmi_WithEmiDate <- Natal_DOB_NoEmi %>% 
       filter(!is.na(EmigrationNatalDate))
nrow(Natal_DOB_NoEmi_WithEmiDate)
#91 entries with an emigration natal date 

Natal_DOB_NoEmi_WithEmiDate_SameDate <- Natal_DOB_NoEmi_WithEmiDate %>% 
  filter(LastDate1 == EmigrationNatalDate)
nrow(Natal_DOB_NoEmi_WithEmiDate_SameDate)
#86 entries where emigration natal date == Last date 1 

Natal_DOB_NoEmi_WithEmiDate_DiffDate <- anti_join(Natal_DOB_NoEmi_WithEmiDate, Natal_DOB_NoEmi_WithEmiDate_SameDate)
nrow(Natal_DOB_NoEmi_WithEmiDate_DiffDate)
#5 entries where emigration natal date != Last date 1

 


# Natal DOB no emmigration DC ---------------------------------------------


#With last date (would assume dead) and with current group (would assume alive)
View(Natal_DOB_NoEmi %>% 
       filter(!is.na(LastDate1),
              !is.na(CurrentGroup)))
#0 entries, all good 


#With last date1 (would assume still alive) and no current group (would assume dead)
View(Natal_DOB_NoEmi %>% 
       filter(is.na(LastDate1),
              is.na(CurrentGroup)))
#Dunston (LT). Dead according to Miguel but there is now way to find a death date
#Babysiele2016 (BD) must be solved
#These two will be an issue as will be assumed alive whereas they are dead
#They should be assigned a lastDate1


# ToAdd Natal DOB Disappeared ---------------------------------------------
#In the latest version of DB, Steph wrongly assigned lastdate1 as the last date of animals that did not emmigrate. Was previosuly recorded as Last seen 


#Natal disappeared from natal
Natal_DOB_Disappeared <-  Natal_DOB_NoEmi %>% 
  filter(!is.na(LastDate1))
View(Natal_DOB_Disappeared)
#215 entries


#Difference Last day and emigration natal day 
View(Natal_DOB_Disappeared %>% 
       filter(!is.na(EmigrationNatalDate)) %>% 
       filter(LastDate1 != EmigrationNatalDate) %>% 
       mutate(Diff = LastDate1 - EmigrationNatalDate))
#Difference is quite small
#What shall I take? Ask Miguel

View(Natal_DOB_Disappeared)
 View(tbl_FromTo)
#Natal disapeared to bind
Natal_DOB_Disappeared_ToAdd <- Natal_DOB_Disappeared %>% 
  
  #RENAME
  rename(Group = BirthGroup,
         From = DOB,
         To = LastDate1,
         Group = BirthGroup) %>%
  
  #CHANGE TO
  mutate(To = case_when(!is.na(EmigrationNatalDate) ~ EmigrationNatalDate,
                        TRUE ~ To)) %>% 
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "Yes", 
         Source = "Natal_DOB_Disappeared") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = 1) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)

View(Natal_DOB_Disappeared_ToAdd)
#215 entries




# ToAdd Natal DOB Emmigrated ----------------------------------------------


#Natal that have Emigrated
Natal_DOB_Emi <- Immigration_RawInfo %>% 
  
  #RETAIN ANIMAL BORN IN POP
  filter(!is.na(DOB)) %>%
  
  #REMOVE ANIMALS THAT DISAPPEARED (DEATH - IMMIGRATION UNKNOWN OR FOLLOWED GROUPS)
  filter(!(AnimalID %in% (Natal_DOB_Disappeared_ToAdd$AnimalID))) %>% 

  #RETAIN ANIMAL THAT DID EMMIGRATE
  #Why didn't I use Immigration group?
  filter(!is.na(EmigrationNatalDate)) 

View(Natal_DOB_Emi)
#66 rows
#74 rows if filter on immigration group (I think it adds 8 splinter, dealt with in next section )
#All with a 
#So seems to be all good 


#To Bind 
Natal_DOB_Emi_ToAdd <- Natal_DOB_Emi %>% 
  
  #RENAME
  rename(Group = BirthGroup,
         From = DOB,
         To = EmigrationNatalDate,
         Group = BirthGroup) %>%
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "Yes",
         Source = "Natal_DOB_Emi") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = 1) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Natal_DOB_Emi_ToAdd)


# ToAdd Natal DOB Disappeared Splinter ------------------------------------
# Queried this by getting data better through tweaking of codes 


Natal_DOB_Splinter <- Immigration_RawInfo %>% 
  
  #RETAIN ANIMAL BORN IN POP
  filter(!is.na(DOB)) %>%
  
  #RETAIN DISAPPEARED IN SPLINTER 
  filter(is.na(EmigrationNatalDate),
         #is.na(ImmigrationDate1),
         !is.na(ImmigrationGroup1))
View(Natal_DOB_Splinter)
#8 entries 
#None of them have Immigration 2 info 
#Ask Miguel whether I should take last date 



#To Bind 
Natal_DOB_Splinter_ToAdd <- Natal_DOB_Splinter %>% 
  
  #RENAME
  rename(Group = BirthGroup,
         From = DOB,
         To = EmigrationNatalDate, #What should strictly be taken, but here will be NA. Ask Miguel if could take lastDate1 as a proxy. For Natal_NoDOB that seemed possible
         Group = BirthGroup) %>% 
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "Yes",
         Source = "Natal_DOB_Splinter") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = 1) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Natal_DOB_Splinter_ToAdd)



# ToAdd Natal DOB Current Group Alive -------------------------------------

#Natal Not Disappeared (alive + emmigrated)
Natal_DOB_NotDisappeared <- Immigration_RawInfo %>% 
  
  #RETAIN ANIMAL BORN IN POP
  filter(!is.na(DOB)) %>%
  
  #EXCLUDE ANIMALS THAT DISAPPEARED
  filter(!(AnimalID %in% Natal_DOB_Disappeared_ToAdd$AnimalID)) %>% 
  
  #EXCLUDE DISAPPEARANCE IN SPLINTER 
  filter(!(AnimalID %in% Natal_DOB_Splinter_ToAdd$AnimalID))
View(Natal_DOB_NotDisappeared)
#204 entries
#Contains animals that may still be alive 


#Did not disapear AND that did not emmigrate
#Alternative 1
#Difference between non-disappeared and emmigrated 
Natal_DOB_NonMigrant_Alive <- anti_join(Natal_DOB_NotDisappeared, Natal_DOB_Emi)
View(Natal_DOB_NonMigrant_Alive)
#138 for which birth group and current group are the same 
#2 NA Current group that must be solved


# #Animals that did not disappear from natal group 
# #Alternative 2
# Natal_DOB_NotDisappeared_Alt <- Natal_DOB_NotDisappeared %>% 
#   filter(is.na(ImmigrationGroup1))
# View(Natal_DOB_NotDisappeared_Alt )
# #138 queried with alternative 1 above
# #Same as for alternative above 
# #All good

# 
# #With Emmigration
# Natal_DOB_NotDisappeared_Alt_Emi <- Natal_DOB_NotDisappeared_Alt %>%
#   filter(!is.na(EmigrationNatalDate))
# View(Natal_DOB_NotDisappeared_Alt_Emi)
# #91 entries that have an emigration natal date
# #Do not understant why they were not picked up in the disappeared
# #Seem to have been solved now
# 
# 
# #Same date
# Natal_DOB_NotDisappeared_Alt_Emi_SameDate <- Natal_DOB_NotDisappeared_Alt_Emi  %>% filter(LastDate1 == EmigrationNatalDate)
# View(Natal_DOB_NotDisappeared_Alt_Emi_SameDate)
# #86 entries where emmi and last date are the same
# 
# #Diff date 
# Natal_DOB_NotDisappeared_Alt_Emi_DiffDate <-  Natal_DOB_NotDisappeared_Alt_Emi %>% filter(LastDate1 != EmigrationNatalDate)
# View(Natal_DOB_NotDisappeared_Alt_Emi_DiffDate)
# #5 entries


#To Bind 
Natal_DOB_NonMigrant_Alive_ToAdd <- Natal_DOB_NonMigrant_Alive %>% 
  
  #REMOVE ANIMALS WITH NA CURRENT GROUP
  #perhaps miguel can do stg about them, they have disappeared but no clue when 
  filter(AnimalID != "Dunston",
         AnimalID != "Babysiele2016") %>% 
  
  #RENAME
  rename(Group = BirthGroup,
         From = DOB,
         To = LastDate1,
         Group = BirthGroup) %>%
  
  mutate(To = today()) %>% 
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "Yes",
         Source = "Natal_DOB_NonMigrant_Alive") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = 1) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Natal_DOB_NonMigrant_Alive_ToAdd)



# Natal NoDOB -------------------------------------------------------------

Natal_NoDOB <- Immigration_RawInfo %>% 
  
  filter(is.na(DOB),
         !is.na(BirthGroup))
View(Natal_NoDOB)
#259 entries



# Natal NoDOB NoEmi -------------------------------------------------------

#No natal emmigration
Natal_NoDOB_NoEmi <- Natal_NoDOB %>% 
  
  #RETAIN ANIMAL DID NOT EMMIGRATE
  #I think it creates an issue as some animals were assigned an emmigration natal date 
  filter(is.na(ImmigrationGroup1)) 
View(Natal_NoDOB_NoEmi)
#192 entries



# ToAdd Natal NoDOB Disappeared -------------------------------------------


#No DOB that disappeared from natal group through death, emmigrated in unknown group
Natal_NoDOB_Disappeared <- Natal_NoDOB_NoEmi %>% 
  
  #REMOVE IMMIGRATION AND NO LAST DATE1
  filter(!is.na(LastDate1))
View(Natal_NoDOB_Disappeared)
#127 entries
#Everest and Kilimanjaro have a immigration date1. Is that an error or was the immigration group forgotten



#Last date different than natal emigration 
Poo <- Natal_NoDOB_Disappeared %>% 
       filter(!is.na(EmigrationNatalDate)) %>% 
       filter(LastDate1 != EmigrationNatalDate) %>% 
       mutate(Diff = ymd(LastDate1) - ymd(EmigrationNatalDate))
#No difference in dates, all good
View(Poo)


#To add 
Natal_NoDOB_Disappeared_ToAdd <- Natal_NoDOB_Disappeared %>% 
  
  #REMOVE NO FIRST DATE
  filter(AnimalID != "Babydaisy2019") %>% 
  
  #RENAME
  rename(Group = BirthGroup,
         From = FirstDate,
         To = LastDate1,
         Group = BirthGroup) %>%
  
  #ADJUST TO DATE
  mutate(To = case_when(!is.na(EmigrationNatalDate) ~ EmigrationNatalDate,
                        TRUE ~ To)) %>% 
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "Yes",
         Source = "Natal_NoDOB_Disappeared") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = 1) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Natal_NoDOB_Disappeared_ToAdd)


# ToAdd Natal NoDOB Emmigrated --------------------------------------------
# This can be dealt with first immigration


#Natal NoDOB that have Emigrated
Natal_NoDOB_Emi <- Natal_NoDOB %>% 
  
  #REMOVE ANIMALS THAT DISAPPEARED (DEATH - IMMIGRATION UNKNOWN OR FOLLOWED GROUPS)
  filter(!(AnimalID %in% (Natal_NoDOB_Disappeared_ToAdd$AnimalID))) %>% 
  
  #RETAIN ANIMAL THAT DID EMMIGRATE ELSEWHERE
  filter(!is.na(EmigrationNatalDate)) 

View(Natal_NoDOB_Emi)
#56 rows
#Imposter doesn't have a first date, should be assigned ask Miguel 


#To Bind 
Natal_NoDOB_Emi_ToAdd <- Natal_NoDOB_Emi %>% 
  
  #REMOVE IMPOSTER
  #Ask Miguel to correct 
  filter(AnimalID != "Imposter") %>% 
  
  #RENAME
  rename(Group = BirthGroup,
         From = FirstDate,
         To = EmigrationNatalDate,
         Group = BirthGroup) %>%
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "Yes",
         Source = "Natal_NoDOB_Emi") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = 1) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Natal_NoDOB_Emi_ToAdd)



# ToAdd Natal NoDOB Splinter ----------------------------------------------

# Query this by getting data better through tweaking of codes 

Natal_NoDOB_Splinter <- Natal_NoDOB %>% 
  
  #RETAIN DISAPPEARED IN SPLINTER 
  filter(is.na(EmigrationNatalDate),
         #is.na(ImmigrationDate1), #3 rows with, 11 without 
         !is.na(ImmigrationGroup1))
View(Natal_NoDOB_Splinter)
#11 entries
#At the difference than Natal_DOB that had no emigrationnatal or immigration1 recorded, these ones have an immigrationgroup1 date so the date from which they were in the splinter can be assumed

#No immigration date 1
View(Natal_NoDOB_Splinter %>% 
       filter(is.na(ImmigrationDate1)))
#3 entries 
#For Miguel to check


#Immigration in a group that was followed
View(Natal_NoDOB_Splinter %>% 
       filter(ImmigrationGroup1 == "BD"))
#3 entries 
#For Miguel to check
#Entries will be difficult for both natal and immigration1 as dates are missing

#To Bind 
Natal_NoDOB_Splinter_ToAdd <- Natal_NoDOB_Splinter %>% 
  
  #REMOVE FOR MIGUEL TO CHECK 
  #REMOVE ANIMALS THAT EMMIGRATED IN BD
  filter(ImmigrationGroup1 != "BD") %>% 
  
  #REMOVE UNKNOWN IMMIGRATIONDATE 1
  #As not able to assign a To date 
  filter(!is.na(ImmigrationDate1)) %>% 
  
  #RENAME
  rename(Group = BirthGroup,
         From = FirstDate,
         To = ImmigrationDate1,
         Group = BirthGroup) %>%
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "Yes",
         Source = "Natal_NoDOB_Splinter") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = 1) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Natal_NoDOB_Splinter_ToAdd)




# ToAdd Natal NoDOB Non-Migrant Alive -------------------------------------
# In the DOB alive, I had proceeded differently. I had queried non-emmigrated by difference between the non-disappeared and the immigrant

#Nora from immigrationgroup 1 == NA should be added 

#Natal Not Disappeared (alive + emmigrated)
Natal_NoDOB_NotDisappeared <- Natal_NoDOB %>% 
  
  #EXCLUDE ANIMALS THAT DISAPPEARED
  filter(!(AnimalID %in% Natal_NoDOB_Disappeared$AnimalID)) %>% 
  
  #EXCLUDE DISAPPEARANCE IN SPLINTER 
  filter(!(AnimalID %in% Natal_NoDOB_Splinter$AnimalID)) 

  # #EXCLUDE MIGRANTS
  # filter(!(AnimalID %in% Natal_NoDOB_Emi$AnimalID))

View(Natal_NoDOB_NotDisappeared)
#121 entries 
#65 entries if remove emigrated animals 
#Contains animals that may still be alive 


#Did not disapear AND that did not emmigrate
#Alternative 1
#Difference between non-disappeared and emmigrated 
Natal_NoDOB_NonMigrant_Alive <- anti_join(Natal_NoDOB_NotDisappeared, Natal_NoDOB_Emi)
View(Natal_NoDOB_NonMigrant_Alive)
#65 antries 
#8 animals with an immigration date. They shoudl have been dealt with before 


#No first date 
Natal_NoDOB_NonMigrant_Alive_NoFirstDate <- Natal_NoDOB_NonMigrant_Alive  %>% 
  filter(is.na(FirstDate))
View(Natal_NoDOB_NotDisappeared_NotEmi_NoFirstDate)
#Among the 65 animals, 15 have no first date and we cannot thus extract from date


# #Animals that did not disappear from natal group
# #Alternative 2
# Natal_NoDOB_NotDisappeared_Alt <- Natal_NoDOB_NotDisappeared %>%
#   filter(is.na(ImmigrationGroup1))
# View(Natal_NoDOB_NotDisappeared_Alt )
# #65 queried with alternative 1 above
# #Same as for alternative above
# #All good
# 
# 
# #With Emmigration
# Natal_NoDOB_NotDisappeared_Alt_Emi <- Natal_NoDOB_NotDisappeared_Alt %>%
#   filter(!is.na(EmigrationNatalDate))
# View(Natal_NoDOB_NotDisappeared_Alt_Emi)
# #0 all good
# 
# # 
# # #Same date
# # Natal_DOB_NotDisappeared_Alt_Emi_SameDate <- Natal_DOB_NotDisappeared_Alt_Emi  %>% filter(LastDate1 == EmigrationNatalDate)
# # View(Natal_DOB_NotDisappeared_Alt_Emi_SameDate)
# # #86 entries where emmi and last date are the same
# # 
# # #Diff date 
# # Natal_DOB_NotDisappeared_Alt_Emi_DiffDate <-  Natal_DOB_NotDisappeared_Alt_Emi %>% filter(LastDate1 != EmigrationNatalDate)
# # View(Natal_DOB_NotDisappeared_Alt_Emi_DiffDate)
# # #5 entries


#Same birth anc current birth group
Natal_NoDOB_NonMigrant_Alive_SameGroups<- Natal_NoDOB_NonMigrant_Alive  %>% 
       filter(BirthGroup == CurrentGroup)
View(Natal_NoDOB_NotDisappeared_NotEmi_SameGroups)
#50 entries for which BirthGroup and currentGroup is the same
#All have a first date 
#Can be used for the to add section
#The 15 animals that are missing are the same as the one 


#Different birth and current birth
Natal_NoDOB_NonMigrant_Alive_DifGroups<- Natal_NoDOB_NonMigrant_Alive  %>% 
  anti_join(Natal_NoDOB_NonMigrant_Alive_SameGroups)
View(Natal_NoDOB_NonMigrant_Alive_DifGroups)
#15 entries
#Miguel must check
#None have a first date or a last date, thus I think nothing can be done with them



#To Bind 
Natal_NoDOB_NonMigrant_Alive_ToAdd <- Natal_NoDOB_NonMigrant_Alive %>% 
  
  #REMOVE ANIMALS WITH NA CURRENT GROUP
  filter(AnimalID %in% Natal_NoDOB_NonMigrant_Alive_SameGroups$AnimalID) %>% 
  
  #RENAME
  rename(Group = BirthGroup,
         From = FirstDate,
         To = LastDate1,
         Group = BirthGroup) %>%
  
  mutate(To = today()) %>% 
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "Yes",
         Source = "Natal_NoDOB_NonMigrant_Alive") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = 1) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Natal_NoDOB_NonMigrant_Alive_ToAdd)




# Immigration -------------------------------------------------------------
#Create a dataset with immigrants 
#Remove using object after removal of r  

Immigrants <- Immigration_RawInfo %>% 
  
  #REMOVE NATAL THAT DISAPPEARED FROM NATAL GROUP
  filter(!(AnimalID %in% Natal_DOB_Disappeared$AnimalID)) %>% 
  filter(!(AnimalID %in% Natal_NoDOB_Disappeared$AnimalID)) %>% 
  
  #REMOVE ANIMALS THAT STILL ALIVE IN NATAL GROUP 
  filter(!(AnimalID %in% Natal_DOB_NonMigrant_Alive$AnimalID)) %>% 
  filter(!(AnimalID %in% Natal_NoDOB_NonMigrant_Alive$AnimalID)) %>%  
  
  #REMOVE SPLINTER?
  #Should ideally be kept as we would like FromTo in the splinter group
  #However, the data collection is likely to make it impossible
  #Thus for the time being, there is no need to get that information
  #None of the splinter had immigration 2 info
  filter(!(AnimalID %in% Natal_DOB_Splinter$AnimalID)) %>% 
  filter(!(AnimalID %in% Natal_NoDOB_Splinter$AnimalID)) 

View(Immigrants)
#254 entries if don t remove splinters 
#235 after removal of splinters


# Immigration 1 DC --------------------------------------------------------


#Immigrant no immigration groups
#For Miguel to check 
Immigrants1_NoImmigrationGroup <- Immigrants %>% 
       filter(is.na(ImmigrationGroup1))
View(Immigrants_NoImmigrationGroup)
#3 animals 
#Recent entries
#Immigrants? 


#No Immigration date
Immigrants1_NoImiDate <- Immigrants %>% 
  filter(is.na(ImmigrationDate1))
View(Immigrants1_NoImiDate)
#73 entries
#First date could possibly be taken for non-natal animals 
#Emigration Natal could possibly be taken for natal (split groups) and non-natal (people may have wrongly assumed they had left their old group on the same date)



# ToAdd Immigration1 Imi Date ---------------------------------------------------
#Immigrants can either have a last date or still be in the group where they have emmigrated


Immigrants1_ImmiDate <- Immigrants %>% 
  filter(!is.na(ImmigrationDate1))
View(Immigrants1_ImmiDate)
#162 entries of animals that have possibly immigrated 1x


#Without last date 1
Immigrants1_ImmiDate_NoLastDate <- Immigrants1_ImmiDate %>% 
  filter(is.na(LastDate1))
View(Immigrants1_ImmiDate_NoLastDate)
#23 entries 
#Can either i) be alive in the immigration group 1 or ?
#Only 1 animal (Madagascar) seems to have emigrated in further group 
View(Immigrants1_ImmiDate_NoLastDate %>% 
       filter(AnimalID == "Madagascar"))


#Animals still alive in group1
Immigrants1_ImmiDate_NoLastDate_SameGroup <- Immigrants1_ImmiDate_NoLastDate %>% 
  filter(ImmigrationGroup1 == CurrentGroup)
View(Immigrants1_ImmiDate_NoLastDate_SameGroup)
  

#Animals that have moved to different group
Immigrants1_ImmiDate_NoLastDate_DiffGroup <- anti_join(Immigrants1_ImmiDate_NoLastDate,Immigrants1_ImmiDate_NoLastDate_SameGroup)
View(Immigrants1_ImmiDate_NoLastDate_DiffGroup)
#1 entries 
#Madagascar for Miguel to check: He says one should discard immigration 1 data and only have a look at immigration 2


Immigrants1_ImmiDate_ToAdd <- Immigrants1_ImmiDate %>% 
  
  #REMOVE ANIMALS (MADAGASCAR)
  filter(AnimalID != "Madagascar") %>% 
  
  #RENAME
  rename(Group = ImmigrationGroup1,
         From = ImmigrationDate1) %>% 
  
  #ADD TO DATE
  mutate(To = case_when(!is.na(LastDate1) ~ LastDate1,
                        AnimalID %in% Immigrants1_ImmiDate_NoLastDate_SameGroup$AnimalID ~ today())) %>%
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "No",
         Source = "Immigrants1_ImmiDate") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = case_when(Natal == "Yes" ~ 2,
                             Natal == "No" ~ 1)) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Immigrants1_ImmiDate_ToAdd)
#161 entries 

names(AnimalID_Natal)




# ToAdd Immigration1 NoImmiDate NonNatal ----------------------------------------

#Non Natal
Immigrants1_NonNatal_NoImiDate <- Immigrants1_NoImiDate %>% 
  filter(is.na(BirthGroup))
View(Immigrants1_NonNatal_NoImiDate)
#23 entries
#Take first date
#Only 4 animals without first date 
#For Miguel to check


#Natal No first date 
Immigrants1_NonNatal_NoImiDate_NoFirstDate <- Immigrants1_NonNatal_NoImiDate %>% 
  filter(is.na(FirstDate))
View(Immigrants1_NonNatal_NoImiDate_NoFirstDate)
#For Miguel to check


#Natal No Last date 
Immigrants1_NonNatal_NoImiDate_NoLastDate1 <- Immigrants1_NonNatal_NoImiDate %>% 
  filter(is.na(LastDate1))
View(Immigrants1_NonNatal_NoImiDate_NoLastDate1)
#For Miguel to check
#No animals seem to still be alive in Immigration group 1


#To Add 
Immigrants1_NoImmiDate_NonNatal_ToAdd <- Immigrants1_NonNatal_NoImiDate %>% 
  
  #REMOVE ANIMALS WITH NO FIRST DATE
  filter(!(AnimalID %in% Immigrants1_NonNatal_NoImiDate_NoFirstDate$AnimalID)) %>% 
  
  
  #REMOVE ANIMALS WITH NO LAST DATE
  filter(!(AnimalID %in% Immigrants1_NonNatal_NoImiDate_NoLastDate1$AnimalID)) %>% 
  
  
  #RENAME
  rename(Group = ImmigrationGroup1,
         From = FirstDate,
         To = LastDate1) %>%
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "No",
         Source = "Immigrants1_NoImmiDate_NonNatal") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = case_when(Natal == "Yes" ~ 2,
                             Natal == "No" ~ 1)) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Immigrants1_NoImmiDate_NonNatal_ToAdd )
#17 entries
#Loss of 6 rows
#Skhumbuzo has no groupID 



# ToAdd Immigration 1 NoImmi Date Natal -----------------------------------------
#Natal may not have any immigration date because their group split for example

#Natal
Immigrants1_Natal_NoImiDate <- Immigrants1_NoImiDate %>% 
  filter(!is.na(BirthGroup))
View(Immigrants1_NoNatalDate_NoImiDate)
#50 entries
#All but 1 have an immigration2 info
#Could take 



#Natal No first date 
Immigrants1_Natal_NoImiDate_NoEmiDate <- Immigrants1_Natal_NoImiDate %>% 
  filter(is.na(EmigrationNatalDate))
View(Immigrants1_Natal_NoImiDate_NoEmiDate)
#0 entries


#Natal No last date
Immigrants1_Natal_NoImiDate_NoLastDate1 <- Immigrants1_Natal_NoImiDate %>% 
  filter(is.na(LastDate1))
View(Immigrants1_Natal_NoImiDate_NoLastDate1 )
#3 entries
#2 of them still seems to be in Immigrant1 group 
#For Miguel to check


#No last date same group
#Still alive in ImmigrationGroup1
Immigrants1_Natal_NoImiDate_NoLastDate1_SameGroups <- Immigrants1_Natal_NoImiDate_NoLastDate1 %>% 
  filter(AnimalID == "Inhlanhla" |
           ImmigrationGroup1 == CurrentGroup)
View(Immigrants1_Natal_NoImiDate_NoLastDate1_SameGroups)


#NoLast day diff group
Immigrants1_Natal_NoImiDate_NoLastDate1_DiffGroup <- anti_join(Immigrants1_Natal_NoImiDate_NoLastDate1,Immigrants1_Natal_NoImiDate_NoLastDate1_SameGroups)
View(Immigrants1_Natal_NoImiDate_NoLastDate1_DiffGroup)


#ToAdd 
Immigrants1_NoImmiDate_Natal_ToAdd <- Immigrants1_Natal_NoImiDate %>% 
  
  #REMOVE ANIMALS WITH NO FIRST DATE
  filter(!(AnimalID %in% Immigrants1_Natal_NoImiDate_NoEmiDate$AnimalID)) %>% 
  
  
  #REMOVE ANIMALS WITH NO LAST DAY AND NOT ALIVE IN IMMIGRATION GROUP 1
  filter(!(AnimalID %in% Immigrants1_Natal_NoImiDate_NoLastDate1_DiffGroup$AnimalID)) %>% 
  
  
  #RENAME
  rename(Group = ImmigrationGroup1,
         From = EmigrationNatalDate) %>% 
  
  #ADD TO DATE
  mutate(To = case_when(!is.na(LastDate1) ~ LastDate1,
                        AnimalID %in% Immigrants1_Natal_NoImiDate_NoLastDate1_SameGroups$AnimalID ~ today())) %>%
  
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "No",
         Source = "Immigrants1_NoImmiDate_Natal") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = case_when(Natal == "Yes" ~ 2,
                             Natal == "No" ~ 1)) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Immigrants1_NoImmiDate_Natal_ToAdd)
#49 entries 





# Immigration2 ------------------------------------------------------------
#Not sure whether to query on 
#i) immigration 2 variables directly
#ii) first exclude animals for which I have not immigration 1 entry
#I will follow i) for now as messed up data in immigration do not mean messed up data in immigration2. I can then query which animals have immigration2 data and no immigration 1


#Immigrant no immigration groups
Immigrants2 <-  Immigrants %>% 
  filter(!is.na(ImmigrationGroup2) |
           !is.na(ImmigrationDate2) |
           !is.na(LastDate2))
View(Immigrants2)
#47 entries
  
  

# Immigration2 DC ---------------------------------------------------------


#Immigrant no immigration groups
Immigrants2_NoImmigrationGroup <- Immigrants2 %>% 
  filter(is.na(ImmigrationGroup2))
View(Immigrants2_NoImmigrationGroup)
#None 


#No Immigration date
Immigrants2_NoImiDate <- Immigrants2 %>% 
  filter(is.na(ImmigrationDate2))
View(Immigrants2_NoImiDate)
#3 entries 
#For Miguel to check 
#Shall I take another date 




# ToAdd Immigration2 ------------------------------------------------------
# All immigrant2 have a group
# Only 3 have no immigration date 


#No Immigration date
Immigrants2_NoImmiDate <- Immigrants2 %>% 
  filter(is.na(ImmigrationDate2))
View(Immigrants2_NoImiDate)
#3 entries 
#For Miguel to check 
#Shall I take another date 


#With immigration date
Immigrants2_ImmiDate <- Immigrants2 %>% 
  filter(!is.na(ImmigrationDate2))
View(Immigrants2_ImmiDate)
#44 entries


#Without last date 2
Immigrants2_ImmiDate_NoLastDate2 <- Immigrants2 %>% 
  filter(is.na(LastDate2))
View(Immigrants2_ImmiDate_NoLastDate)
#13 entries 
#Can either i) be alive in the immigration group 2 or ?


#Animals still alive in group2
Immigrants2_ImmiDate_NoLastDate2_SameGroup <- Immigrants2_ImmiDate_NoLastDate2 %>% 
  filter(ImmigrationGroup2 == CurrentGroup)
View(Immigrants2_ImmiDate_NoLastDate2_SameGroup)
#10 of the 13 animals with no last date2 are still in this group


#Animals with different group 
Immigrants2_ImmiDate_NoLastDate2_DiffGroup <- anti_join(Immigrants2_ImmiDate_NoLastDate2,Immigrants2_ImmiDate_NoLastDate2_SameGroup)
View(Immigrants2_ImmiDate_NoLastDate2_DiffGroup)
#3 entries 
#For Miguel to check


Immigrants2_ImmiDate_ToAdd <- Immigrants2 %>% 
  
  #REMOVE ANIMALS WITH NO IMMIGRATION DATE 
  filter(!(AnimalID %in% Immigrants2_NoImmiDate$AnimalID)) %>% 
  
  #REMOVE ANIMALS WITH NO LAST DATE2 AND CURRENT GROUP != GROUP2
  filter(!(AnimalID %in% Immigrants2_ImmiDate_NoLastDate2_DiffGroup$AnimalID)) %>% 

  #RENAME
  rename(Group = ImmigrationGroup2,
         From = ImmigrationDate2) %>% 
  
  #ADD TO DATE
  mutate(To = case_when(!is.na(LastDate2) ~ LastDate2,
                        AnimalID %in% Immigrants2_ImmiDate_NoLastDate2_SameGroup$AnimalID ~ today())) %>%
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "No",
         Source = "Immigrants2_ImmiDate_ToAdd") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = case_when(Natal == "Yes" ~ 3,
                             Natal == "No" ~ 2)) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Immigrants2_ImmiDate_ToAdd)
#43 entries 


# Immigration3 ------------------------------------------------------------
#Not sure whether to query on 
#i) immigration 3 variables directly
#ii) first exclude animals for which I have not immigration 1 and 2 entry
#I will follow i) for now as messed up data in immigration do not mean messed up data in immigration3. I can then query which animals have immigration3 data and no immigration 1


#Immigrant no immigration groups
Immigrants3 <-  Immigrants %>% 
  filter(!is.na(ImmigrationGroup3) |
           !is.na(ImmigrationDate3) |
           !is.na(LastDate3))
View(Immigrants3)
#13 entries



# Immigration3 DC ---------------------------------------------------------


#Immigrant no immigration groups
Immigrants3_NoImmigrationGroup <- Immigrants3 %>% 
  filter(is.na(ImmigrationGroup3))
View(Immigrants3_NoImmigrationGroup)
#None 


#No Immigration date
Immigrants3_NoImiDate <- Immigrants3 %>% 
  filter(is.na(ImmigrationDate3))
View(Immigrants3_NoImiDate)
#1 entry 
#For Miguel to check 
#Shall I take another date 




# ToAdd Immigration3 ------------------------------------------------------
# All immigrant3 have a group
# Only 3 have no immigration date 


#No Immigration date
Immigrants3_NoImmiDate <- Immigrants3 %>% 
  filter(is.na(ImmigrationDate3))
View(Immigrants3_NoImiDate)
#1 entry
#For Migue to check 


#With immigration date
Immigrants3_ImmiDate <- Immigrants3 %>% 
  filter(!is.na(ImmigrationDate3))
View(Immigrants3_ImmiDate)
#12 entries


#Without last date 3
Immigrants3_ImmiDate_NoLastDate3 <- Immigrants3 %>% 
  filter(is.na(LastDate3))
View(Immigrants3_ImmiDate_NoLastDate3)
#3 entries out of 13 entries 
#Can either i) be alive in the immigration group 3 or ?


#Animals still alive in group3
Immigrants3_ImmiDate_NoLastDate3_SameGroup <- Immigrants3_ImmiDate_NoLastDate3 %>% 
  filter(ImmigrationGroup3 == CurrentGroup)
View(Immigrants3_ImmiDate_NoLastDate3_SameGroup)
#2 animals 


#Animals with different group 
Immigrants3_ImmiDate_NoLastDate3_DiffGroup <- anti_join(Immigrants3_ImmiDate_NoLastDate3,Immigrants3_ImmiDate_NoLastDate3_SameGroup)
View(Immigrants3_ImmiDate_NoLastDate3_DiffGroup)
#1 entry
#For Miguel to check, but this animals has probably been lost in a split group


#ToADD
Immigrants3_ImmiDate_ToAdd <- Immigrants3 %>% 
  
  #REMOVE ANIMALS WITH NO IMMIGRATION DATE 
  filter(!(AnimalID %in% Immigrants3_NoImmiDate$AnimalID)) %>% 
  
  #REMOVE ANIMALS WITH NO LAST DATE3 AND CURRENT GROUP != GROUP3
  filter(!(AnimalID %in% Immigrants3_ImmiDate_NoLastDate3_DiffGroup$AnimalID)) %>% 
  
  #RENAME
  rename(Group = ImmigrationGroup3,
         From = ImmigrationDate3) %>% 
  
  #ADD TO DATE
  mutate(To = case_when(!is.na(LastDate3) ~ LastDate3,
                        AnimalID %in% Immigrants3_ImmiDate_NoLastDate3_SameGroup$AnimalID ~ today())) %>%
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "No",
         Source = "Immigrants3_ImmiDate") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = case_when(Natal == "Yes" ~ 4,
                             Natal == "No" ~ 3)) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Immigrants3_ImmiDate_ToAdd)
#12 entries 



# Immigration4 ------------------------------------------------------------
#Not sure whether to query on 
#i) immigration 4 variables directly
#ii) first exclude animals for which I have not immigration 1 and 2 entry
#I will follow i) for now as messed up data in immigration do not mean messed up data in immigration4. I can then query which animals have immigration4 data and no immigration 1


#Immigrant no immigration groups
Immigrants4 <-  Immigrants %>% 
  filter(!is.na(ImmigrationGroup4) |
           !is.na(ImmigrationDate4) |
           !is.na(LastDate4))
View(Immigrants4)
#3 entries



# Immigration4 DC ---------------------------------------------------------


#Immigrant no immigration groups
Immigrants4_NoImmigrationGroup <- Immigrants4 %>% 
  filter(is.na(ImmigrationGroup4))
View(Immigrants4_NoImmigrationGroup)
#None 


#No Immigration date
Immigrants4_NoImiDate <- Immigrants4 %>% 
  filter(is.na(ImmigrationDate4))
View(Immigrants4_NoImiDate)
#None



# ToAdd Immigration4 ------------------------------------------------------


#No Immigration date
Immigrants4_NoImmiDate <- Immigrants4 %>% 
  filter(is.na(ImmigrationDate4))
View(Immigrants4_NoImiDate)
#None


#Without last date 4
Immigrants4_ImmiDate_NoLastDate4 <- Immigrants4 %>% 
  filter(is.na(LastDate4))
View(Immigrants4_ImmiDate_NoLastDate4)
#1 For Miguel to check 


#Animals still alive in group4
Immigrants4_ImmiDate_NoLastDate4_SameGroup <- Immigrants4_ImmiDate_NoLastDate4 %>% 
  filter(ImmigrationGroup4 == CurrentGroup)
View(Immigrants4_ImmiDate_NoLastDate4_SameGroup)
#0 animals


#Animals with different group 
Immigrants4_ImmiDate_NoLastDate4_DiffGroup <- anti_join(Immigrants4_ImmiDate_NoLastDate4,Immigrants4_ImmiDate_NoLastDate4_SameGroup)
View(Immigrants4_ImmiDate_NoLastDate4_DiffGroup)
#1 entry
#For Miguel to check, but this animals has probably been lost in a split group


#ToADD
Immigrants4_ImmiDate_ToAdd <- Immigrants4 %>% 
  
  #REMOVE ANIMALS WITH NO LAST DATE4 AND CURRENT GROUP != GROUP4
  filter(!(AnimalID %in% Immigrants4_ImmiDate_NoLastDate4_DiffGroup$AnimalID)) %>% 
  
  #RENAME
  rename(Group = ImmigrationGroup4,
         From = ImmigrationDate4) %>% 
  
  #ADD TO DATE
  mutate(To = case_when(!is.na(LastDate4) ~ LastDate4,
                        AnimalID %in% Immigrants4_ImmiDate_NoLastDate4_SameGroup$AnimalID ~ today())) %>%
  
  #ADD NATAL GROUp
  mutate(NatalGroup = "No",
         Source = "Immigrants4_ImmiDate") %>% 
  
  #ADD GROUP NB
  mutate(GroupNB = case_when(Natal == "Yes" ~ 5,
                             Natal == "No" ~ 4)) %>% 
  
  select(AnimalID,
         From,
         To,
         Group,
         Source,
         NatalGroup,
         GroupNB)
View(Immigrants4_ImmiDate_ToAdd)
#2 entries


# tbl_FromTo --------------------------------------------------------------

tbl_FromTo <- 
  
  #DOB DISAPPEARED
  Natal_DOB_Disappeared_ToAdd %>% 
  
  #DOB EMIGRATED 
  bind_rows(., Natal_DOB_Emi_ToAdd) %>% 
  
  #DOB SPLINTER 
  bind_rows(., Natal_DOB_Splinter_ToAdd) %>% 
  
  #DOB ALIVE
  bind_rows(., Natal_DOB_NonMigrant_Alive_ToAdd) %>% 
  
  #NO DOB DISAPPEARED 
  bind_rows(., Natal_NoDOB_Disappeared_ToAdd) %>% 
  
  #NO DOB EMIGRATED 
  bind_rows(., Natal_NoDOB_Emi_ToAdd) %>% 

  #NO DOB SPLINTER 
  bind_rows(., Natal_NoDOB_Splinter_ToAdd ) %>% 

  #NO DOB ALIVE 
  bind_rows(., Natal_NoDOB_NonMigrant_Alive_ToAdd) %>% 

  #1ST IMMIGRATION IMMI DATE 
  bind_rows(., Immigrants1_ImmiDate_ToAdd) %>% 
  
  
  #1ST IMMIGRATION NO IMMI DATE NON NATAL
  bind_rows(., Immigrants1_NoImmiDate_NonNatal_ToAdd) %>% 
  
  #1ST IMMIGRATION NO IMMI DATE NATAL
  bind_rows(., Immigrants1_NoImmiDate_Natal_ToAdd) %>%  
  
  #2ND IMMIGRATION 
  bind_rows(., Immigrants2_ImmiDate_ToAdd) %>% 

  #3RD IMMIGRATION 
  bind_rows(., Immigrants3_ImmiDate_ToAdd) %>% 

  #4TH IMMIGRATION 
  bind_rows(., Immigrants4_ImmiDate_ToAdd) %>% 
  
  arrange(AnimalID,
          From)

View(tbl_FromTo)
#946 entries
#NatalGroup and Group Number would strictly not be needed, but I will keep them for now as they will be useful to query immigrant info


# write.csv ---------------------------------------------------------------

write.csv(tbl_FromTo, "tbl_FromTo_20221116.csv",row.names = FALSE)



# tbl_FromTo DC -----------------------------------------------------------


#is.na from 
View(tbl_FromTo %>% 
       filter(is.na(From)))
#None 


#is.na To
View(tbl_FromTo %>% 
       filter(is.na(To)))
#8 entries that must be solved


#GroupNb queried
GroupNB_Q <- tbl_FromTo %>% 
  group_by(AnimalID) %>% 
  mutate(GroupNB_Q = row_number()) %>% 
  ungroup()


#Discrepancies group number 
GroupNB_Q_Discrepancies <- GroupNB_Q %>% 
  filter(GroupNB != GroupNB_Q)
View(GroupNB_Q_Discrepancies)
#These two animals must be checked
#Why are they missing previosu group


#Missing Group1
GroupNB_MissingG1 <- GroupNB_Q %>%
  group_by(AnimalID) %>% 
  slice_min(GroupNB) %>% 
  ungroup() %>% 
  filter(GroupNB > 1)
View(GroupNB_MissingG1)
#Same animals above


#AnimalID not in FromTo
Animal_NotInFromTo <- anti_join(tbl_AnimalID, tbl_FromTo)
View(Animal_NotInFromTo)
#29 animals missing


#Missing animals With LH info
FromTo_MissingID_Info <- Animal_NotInFromTo %>% 
  left_join(.,tbl_LifeHistory, by=c("AnimalID" = "AnimalID_Std"))
View(FromTo_MissingID_Info)

#First Date 
View(FromTo_MissingID_Info %>% 
       filter(!is.na(FirstDate)))
#9 animals 

#DOB
View(FromTo_MissingID_Info %>% 
       filter(!is.na(DOB)))
#2animals 


#To is smaller than from of th enext group


#Main issue seems that we would not be able to assign first date to these 29 animals


#I could also do a data check on the sequence of group to see, whether the group number matches assigned does match the one generated


