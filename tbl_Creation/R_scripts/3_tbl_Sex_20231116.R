

# Package -----------------------------------------------------------------
library(tidyverse)



# General -----------------------------------------------------------------
#Once the table is generated, it will have to be corrected here on its own 
#To generate the first table, it does not matter whether AnimalID_Std is taken from tbl_AnimalID or from tbl_LifeHistory as they were at that time the same 
#However, the question will arise later on 

# Paths ------------------------------------------------------------------
setwd("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL")

# tbl_AnimalID ------------------------------------------------------------
tbl_AnimalID <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_AnimalID.csv")

# tbl_LifeHistory ---------------------------------------------------------
tbl_LifeHistory <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_LifeHistory_181124.csv") %>%
  filter(!(LH_AnimalName == "Dinosaur"& ReliableData == "NO"))


table(tbl_LifeHistory$Sex)
# Join AnimalID and Sex ---------------------------------------------------

AnimalIDSex <- tbl_AnimalID %>%
  left_join(., tbl_LifeHistory %>%
    select(LH_AnimalCode, LH_AnimalName, Sex), by= c("AnimalCode"="LH_AnimalCode", "AnimalName" = "LH_AnimalName")) %>%
  #AnimalIDSex <- LH%>%

  #MAKE SEX AS CAPTITAL 
  mutate(Sex = toupper(Sex)) %>%

  #MAKE EMPTY ANIMAL CODES NA this is not a thing anymore but no problem
  mutate(AnimalCode = na_if(AnimalCode,"")) %>%
  
  #ADD CODE LENGTH 
  mutate(CodeLength = str_length(AnimalCode)) %>%
  
  #ADD TO CHECK VARIABLE 
  mutate(ToCheck = case_when( grepl("Baby",AnimalName) ~ "Baby" ,
                              grepl("BB",AnimalCode) ~ "Baby" ,
                              Sex == "F" & CodeLength == 4 ~ "No",
                              Sex == "M" & CodeLength == 3 ~ "No",
                              Sex == "F" & CodeLength != 4 ~ "Yes",
                              Sex == "M" & CodeLength != 3 ~ "Yes",
                              is.na(Sex) & CodeLength %in% c(3,4) ~ "Yes",
                              is.na(AnimalCode) & !(is.na(Sex)) ~ "Yes"
                              )) %>%
  
  arrange(desc(ToCheck),
          AnimalName)
  

View(AnimalIDSex %>% filter(ToCheck == "Yes" | is.na(ToCheck) )) # It is correct
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
  select(LH_RowNumber, AnimalCode, AnimalName, Sex) %>%
  filter(!is.na(Sex))


# write csv table ---------------------------------------------------------


#write.csv(tbl_Sex,"tbl_Sex.csv",row.names = FALSE)
