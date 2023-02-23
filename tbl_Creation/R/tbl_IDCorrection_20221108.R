

# package -----------------------------------------------------------------
library(tidyverse)
library(fuzzyjoin)


# General considerations --------------------------------------------------


#We need a conversion from correct AnimalID to all possible spelling 
#We need a conversion from correct AnimalCodes to all possible spelling

#Will this file be built manually or? I am not sure yet how I am going to do this 
#Perhaps going on a case by case basis will be the best to give ideas

# Paths---------------------------------------------------------------------

# path to the file with the names corrected
input_correction_path <- "/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/CSV/"
input_correction_table <- "Mathia_NameCorrection_20211118.csv"
input_fullpath_correction <- gsub(" ", "", paste(input_corrention_path, input_correction_table))

# path to the life history file
input_path <- "/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/"
input_table <- "tbl_LifeHistory.csv"
input_fullpath <- paste(input_path,input_table)

# Spelling origin ---------------------------------------------------------

#tbl_LifeHistory


#Mathias correction file 





# Mathias correction ------------------------------------------------------
#Mathias had generated a code that would correct names up to a certain date
#I believe it provides the correct AnimalCode and AnimalID. The spelling could either be the pelling of the AnimalId or the Animal code 

#Mathias file does not necessarily includes all possible corrections:
#i) Miguel may have extracted-created csv file involving data from that period after Mathias did the file
#ii) Does not include animals that were born after

#Thus we are going to build on his file

#AnimalCorrection Mathias
AnimalID_Correction_Mathias <- read.csv(input_fullpath_correction) %>%
  
  #SELECT 
  select(-main_group,
         sex) %>% 
  #RENAME
  rename(AnimalCode = good_name,
         AnimalID = full_name,
         Spelling = bad_name,
         AnimalCorrection_MathiasNotes = notes)

View(AnimalID_Correction_Mathias)


View(AnimalID_Correction_Mathias %>% 
       distinct(AnimalID))
#Contains 486 AnimalID 
#Latest versionn of DB contains 800 animals




# Life history file correction --------------------------------------------


#get tbl_LifeHistory
tbl_LifeHistory <- read.csv(input_fullpath)


#Count of AnimalID
View(tbl_LifeHistory %>% 
       distinct(LH_AnimalID))
#Contains 800 animals



# Animals in LHF not in mathias file --------------------------------------

names(AnimalID_Correction_Mathias)


#AnimalID in LH with no correspondance in Mathias file 
LH_AnimalID_NotMathias <- tbl_LifeHistory %>% 
  distinct(LH_AnimalID,
           LH_AnimalCode) %>% 
  anti_join(., AnimalID_Correction_Mathias, by=c("LH_AnimalID" = "AnimalID"))
#430 animals in LH have no match in mathias correction file

#AnimalCode in LH with no correspondance in Mathias file 
LH_AnimalCode_NotMathias <- tbl_LifeHistory %>% 
  distinct(LH_AnimalID,
           LH_AnimalCode) %>% 
  anti_join(., AnimalID_Correction_Mathias, by=c("LH_AnimalCode" = "AnimalCode"))
View(LH_AnimalID_NotMathias)
#425 animalscode in LH have no match in mathias correction file


#Bind animal codes and animalsID not found in Mathias file



#Try the same but joining on 
LH_AnimalID_NotMathias <- tbl_LifeHistory %>% 
  distinct(LH_AnimalID, LH_AnimalCode) %>% 
  fuzzy_left_join(., AnimalID_Correction_Mathias %>% 
              distinct(AnimalID), by=c("LH_AnimalID" = "AnimalID", "LH_AnimalCode" = "AnimalCode"))




View(LH_AnimalID_NotMathias)



