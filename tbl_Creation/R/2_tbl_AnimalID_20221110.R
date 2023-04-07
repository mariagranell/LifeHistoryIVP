

# General -----------------------------------------------------------------

#I am starting from tbl_LifeHistory to generate a list of AnimalID
#In previous tbl_AnimalID I had used a different approach where I had built on Mathias code
#Every change in LH_AnimalID will require tbl_AnimalID to be re-generated alongside all other tbl

# Paths ------------------------------------------------------------------
setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria")

# tbl_LifeHistory ---------------------------------------------------------
tbl_LifeHistory <- read.csv("tbl_LifeHistory.csv")
str(tbl_LifeHistory)



# tbl_AnimalID ------------------------------------------------------------
tbl_AnimalID <- tbl_LifeHistory %>% 
  arrange(LH_AnimalCode) %>%
  select(AnimalID_Std, LH_AnimalCode) %>%
  rename(AnimalID = AnimalID_Std, AnimalCode = LH_AnimalCode)


#write csv
write.csv(tbl_AnimalID,"tbl_AnimalID.csv",row.names = FALSE)
