# ---------------
# Title: Export tables
# Date: 7 April 2023
# Author: mgranellruiz
# Goal: Put together the more useful lh tables that were created in these scripts
# to give to master students and colleagues
# ---------------

# library ---------------------

# path ------------------------
setwd("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL")

# load the data ---------------
# tbl_AnimalID
tbl_AnimalID <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_AnimalID.csv") %>%
  filter(!is.na(AnimalCode) & !is.na(AnimalName))

# tbl_Sex
tbl_Sex <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_Sex.csv")

# tbl_Age. Is a mess...
tbl_Age <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_Age.csv")

# tbl_GroupMembership
tbl_GroupMembership <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_GroupMembership.csv")

# tbl_LifeHistory
tbl_LifeHistory <- read.csv("../TBL/Archive_tbl/lh_181124/tbl_LifeHistory_181124.csv") %>%
  filter(!(LH_AnimalName == "Dinosaur"& ReliableData == "NO"))
tbl_lh <- tbl_LifeHistory%>%
  select(LH_RowNumber, LH_AnimalName, LH_AnimalCode, LH_MotherID, LH_FatherID, ReliableData, Fate_probable, Comments)

# MERGE DATAFRAMES --------------------
factchecked_LH <- tbl_AnimalID %>%
  left_join(.,tbl_Sex, by = c("AnimalName","AnimalCode","LH_RowNumber")) %>%
  left_join(.,tbl_Age, by = c("AnimalName","AnimalCode"), multiple = "all") %>%
  left_join(.,tbl_GroupMembership, by = "AnimalName", multiple = "all") %>%
  left_join(.,tbl_lh, by = c("AnimalName" = "LH_AnimalName", "AnimalCode" = "LH_AnimalCode", "LH_RowNumber"), multiple = "all") %>%
  rename(Mother = LH_MotherID, Father = LH_FatherID)

write.csv(factchecked_LH, "/tbl_Creation/TBL/Archive_tbl/lh_181124/bad_factchecked_LH_181124.csv", row.names = FALSE)

