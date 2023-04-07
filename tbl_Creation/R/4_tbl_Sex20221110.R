

# Package -----------------------------------------------------------------
library(tidyverse)



# General -----------------------------------------------------------------
#Once the table is generated, it will have to be corrected here on its own 
#To generate the first table, it does not matter whether AnimalID_Std is taken from tbl_AnimalID or from tbl_LifeHistory as they were at that time the same 
#However, the question will arise later on 

# Paths ------------------------------------------------------------------
setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria")

# tbl_AnimalID ------------------------------------------------------------
tbl_AnimalID <- read.csv("tbl_AnimalID.csv")

# tbl_LifeHistory ---------------------------------------------------------
tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")



# Join AnimalID and Sex ---------------------------------------------------

AnimalIDSex <- tbl_AnimalID %>% 
  left_join(., tbl_LifeHistory %>% 
              select(AnimalID_Std,
                     Sex), by=c("AnimalID" = "AnimalID_Std")) %>% 
  
  #MAKE SEX AS CAPTITAL 
  mutate(Sex = toupper(Sex)) %>%

  #MAKE EMPTY ANIMAL CODES NA this is not a thing anymore but no problem
  mutate(AnimalCode = na_if(AnimalCode,"")) %>%
  
  #ADD CODE LENGTH 
  mutate(CodeLength = str_length(AnimalCode)) %>%
  
  #ADD TO CHECK VARIABLE 
  mutate(ToCheck = case_when( grepl("Baby",AnimalID) ~ "Baby" ,
                              grepl("BB",AnimalCode) ~ "Baby" ,
                              Sex == "F" & CodeLength == 4 ~ "No",
                              Sex == "M" & CodeLength == 3 ~ "No",
                              Sex == "F" & CodeLength != 4 ~ "Yes",
                              Sex == "M" & CodeLength != 3 ~ "Yes",
                              is.na(Sex) & CodeLength %in% c(3,4) ~ "Yes",
                              is.na(AnimalCode) & !(is.na(Sex)) ~ "Yes"
                              )) %>%
  
  arrange(desc(ToCheck),
          AnimalID)
  

View(AnimalIDSex %>% filter(ToCheck == "Yes" | is.na(ToCheck) ))
#Should babies have a sex? Shall we keep what is available? Or decide that they have no name? Or add name whenever available


# Datacheck ---------------------------------------------------------------

#What are the end numbers?
AnimalIDSex %>%
  group_by(ToCheck, Sex)%>%
  summarise(n =n())

# Add manually individuals ------------------------------------------------

AnimalIDSex[AnimalIDSex$AnimalCode %in% c("Apo","Pap","Tas"), 3] <- "M"
AnimalIDSex[AnimalIDSex$AnimalCode %in% c("Ekse","Naga"), 3] <- "F"

# Generate tbl_Sex --------------------------------------------------------

tbl_Sex <- AnimalIDSex %>%
  select(AnimalID, AnimalCode, Sex) %>%
  filter(!(is.na(Sex)))


# write csv table ---------------------------------------------------------


write.csv(tbl_Sex,"tbl_Sex.csv",row.names = FALSE)
