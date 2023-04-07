

# General -----------------------------------------------------------------
#Once the table is generated, it will have to be corrected here on its own 

source("/Users/mariagranell/Repositories/phllipe_vulloid/Vervet_Functions_20221107.R")

#To generate the first table, it does not matter whether AnimalID_Std is taken from tbl_AnimalID or from tbl_LifeHistory as they were at that time the same 
#However, the question will arise later on 

setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria")

# tbl_AnimalID ------------------------------------------------------------
tbl_AnimalID <- read.csv("tbl_AnimalID.csv")

# tbl_LifeHistory ---------------------------------------------------------
tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")



# Join AnimalID and LH Info -----------------------------------------------
names(tbl_LifeHistory)

AnimalIDOtherID <- tbl_AnimalID %>% 
  
  left_join(., tbl_LifeHistory %>% 
              select(AnimalID_Std,
                     OtherID), by=c("AnimalID" = "AnimalID_Std")) %>% 
  
  #REMOVE NO OTHER ID
  filter(!(is.na(OtherID)))



# Create a table with the standard name, standard other name1 and standard other name 2.
# Don´t worry about the warning

AnimalIDOtherID_StdID <- AnimalIDOtherID %>% 
  
  #REPLACE / BY ; because Other ID can have two entries separated by /
  mutate(OtherID = str_replace(string = OtherID,pattern = "/", replacement = ";")) %>% 
  
  #SEPARATE OTHER ID 
  separate(OtherID, sep = ";", into = c("OtherID_1","OtherID_2")) %>% 
  
  #STANDARDIZE OTHERID 1
  std_AnimalID(animalID = OtherID_1) %>% 
  mutate(OtherID_1_Std = AnimalID_Std) %>% 
  
  #STANDARDIZE OTHERID 2
  std_AnimalID(animalID = OtherID_2) %>% 
  mutate(OtherID_2_Std = AnimalID_Std) 


#Put in long format
AnimalIDOtherID_StdID_Long <- AnimalIDOtherID_StdID %>% 
  pivot_longer(cols = OtherID_1_Std:OtherID_2_Std, names_to = "OtherID_Count", values_to = "OtherID")

# Generate tbl_OtherID ----------------------------------------------------

tbl_OtherID <- 
  AnimalIDOtherID_StdID_Long %>% 
  select(AnimalID, 
         OtherID) %>% 
  filter(!(is.na(OtherID)))

# New entries
tbl_OtherID <- tbl_OtherID%>%
  add_row(AnimalID = "Zeus", OtherID = "MaleCollar") %>%
  add_row(AnimalID = "Plainjane", OtherID = "PlainJane") %>%
  add_row(AnimalID = "Vryheid", OtherID = "NewMale06.22")

# Generate csv ------------------------------------------------------------

write.csv(tbl_OtherID,"tbl_OtherID.csv",row.names = FALSE)
  


