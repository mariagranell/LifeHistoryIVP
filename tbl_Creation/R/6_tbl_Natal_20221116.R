
# Package -----------------------------------------------------------------


library(tidyverse)
library(lubridate)

# Path
setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria")

# tbl_AnimalID ------------------------------------------------------------

tbl_AnimalID <- read.csv("tbl_AnimalID.csv")


# tbl_LifeHistory ---------------------------------------------------------

tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")
View(tbl_LifeHistory)



# AnimalIDNatal -----------------------------------------------------------

# put no when it does not have DOB and BirthGroup
AnimalID_Natal <- tbl_AnimalID %>% 
  left_join(., tbl_LifeHistory, by=c("AnimalID" = "AnimalID_Std")) %>% 
  select(AnimalID,DOB,BirthGroup) %>% 
  mutate(Natal = case_when(!is.na(DOB) | !is.na(BirthGroup) ~ "Yes",
                           TRUE ~ "No"))
View(AnimalID_Natal)

# Generate tbl ------------------------------------------------------------

tbl_Natal <- AnimalID_Natal %>% 
  select(AnimalID,
         Natal)


# write.csv ---------------------------------------------------------------

write.csv(tbl_Natal,"tbl_Natal.csv",row.names = FALSE)

         