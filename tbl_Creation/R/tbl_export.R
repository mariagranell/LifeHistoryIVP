# ---------------
# Title: Export tables
# Date: 7 April 2023
# Author: mgranellruiz
# Goal: Put together the more useful lh tables that were created in these scripts
# to give to master students and colleagues
# ---------------

# library ---------------------

# path ------------------------
setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria")

# load the data ---------------
# tbl_AnimalID
tbl_AnimalID <- read.csv("tbl_AnimalID.csv")

# tbl_Sex
tbl_Sex <- read.csv("tbl_Sex.csv")

# tbl_OtherID
tbl_OtherID <- read.csv("tbl_OtherID.csv")

# tbl_Age
tbl_Age <- read.csv("tbl_Age.csv")

# tbl_GroupMembership
tbl_GroupMembership <- read.csv("tbl_GroupMembership.csv")

# tbl_LifeHistory
tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")
tbl_lh <- tbl_LifeHistory%>%
  select(LH_AnimalID, LH_AnimalCode, LH_MotherID, LH_FatherID, ReliableData, Fate_probable, Comments)

# MERGE DATAFRAMES --------------------
factchecked_LH <- tbl_AnimalID %>%
  left_join(.,tbl_OtherID, by = "AnimalID", multiple = "all") %>%
  left_join(.,tbl_Sex, by = c("AnimalID","AnimalCode")) %>%
  left_join(.,tbl_Age, by = c("AnimalID","AnimalCode"), multiple = "all") %>%
  left_join(.,tbl_GroupMembership, by = "AnimalID", multiple = "all") %>%
  left_join(.,tbl_lh, by = c("AnimalID" = "LH_AnimalID", "AnimalCode" = "LH_AnimalCode"), multiple = "all") %>%
  rename(Mother = LH_MotherID, Father = LH_FatherID)

write.csv(factchecked_LH,"factchecked_LH.csv",row.names = FALSE)

