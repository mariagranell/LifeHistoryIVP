

# Package -----------------------------------------------------------------
library(tidyverse)



# General -----------------------------------------------------------------
#Once the table is generated, it will have to be corrected here on its own 
#To generate the first table, it does not matter whether AnimalID_Std is taken from tbl_AnimalID or from tbl_LifeHistory as they were at that time the same 
#However, the question will arise later on 

# Paths ------------------------------------------------------------------

# input file is wither animalID, animalCode
input_path <- "/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/"


output_path <- input_path
output_table <- "tbl_Sex.csv"
output_fullpath <- gsub(" ", "", paste(output_path, output_table))


# tbl_AnimalID ------------------------------------------------------------
input_table <- "tbl_AnimalID.csv"
input_fullpath <- gsub(" ", "",paste(input_path,input_table))
tbl_AnimalID <- read.csv(input_fullpath)
View(tbl_AnimalID)


# tbl_LifeHistory ---------------------------------------------------------
input_table <- "tbl_LifeHistory.csv"
input_fullpath <- gsub(" ", "",paste(input_path,input_table))
tbl_LifeHistory <- read.csv(input_fullpath)



# Join AnimalID and Sex ---------------------------------------------------

AnimalIDSex <- tbl_AnimalID %>% 
  left_join(., tbl_LifeHistory %>% 
              select(AnimalID_Std,
                     LH_AnimalCode,
                     Sex), by=c("AnimalID" = "AnimalID_Std")) %>% 
  
  #MAKE SEX AS CAPTITAL 
  mutate(Sex = toupper(Sex)) %>%

  #MAKE EMPTY ANIMAL CODES NA
  mutate(LH_AnimalCode = na_if(LH_AnimalCode,"")) %>%
  
  #ADD CODE LENGTH 
  mutate(CodeLength = str_length(LH_AnimalCode)) %>% 
  
  #ADD TO CHECK VARIABLE 
  mutate(ToCheck = case_when( grepl("Baby",AnimalID) ~ "Baby" ,
                              Sex == "F" & CodeLength == 4 ~ "No",
                              Sex == "M" & CodeLength == 3 ~ "No",
                              Sex == "F" & CodeLength != 4 ~ "Yes",
                              Sex == "M" & CodeLength != 3 ~ "Yes",
                              is.na(Sex) & CodeLength %in% c(3,4) ~ "Yes",
                              is.na(LH_AnimalCode) & !(is.na(Sex)) ~ "Yes"
                              )) %>%
  
  arrange(desc(ToCheck),
          AnimalID)
  

View(AnimalIDSex)
#Should babies have a sex? Shall we keep what is available? Or decide that they have no name? Or add name whenever available


# Datacheck ---------------------------------------------------------------

#Do babies have sex?
BabiesSex <- AnimalIDSex %>% 
  filter(grepl("Baby",AnimalID))
View(BabiesSex)
#Some do some dont, what do we do

#What are the end numbers?
AnimalIDSex %>%
  group_by(ToCheck, Sex)%>%
  summarise(n =n())

# Generate tbl_Sex --------------------------------------------------------

tbl_Sex <- AnimalIDSex %>% 
  select(AnimalID, LH_AnimalCode, Sex) %>%
  filter(!(is.na(Sex)))

# Add manually individuals ------------------------------------------------

tbl_Sex[tbl_Sex$AnimalID == "Engel", 3] <- "F"
tbl_Sex[tbl_Sex$AnimalID == "Teddibeer", 3] <- "M"




# write csv table ---------------------------------------------------------


write.csv(tbl_Sex,output_fullpath,row.names = FALSE)
