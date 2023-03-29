

# General -----------------------------------------------------------------

#I am starting from tbl_LifeHistory to generate a list of AnimalID
#In previous tbl_AnimalID I had used a different approach where I had built on Mathias code
#Every change in LH_AnimalID will require tbl_AnimalID to be re-generated alongside all other tbl

# Paths ------------------------------------------------------------------
input_path <- "/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/"
input_table <- "tbl_LifeHistory.csv"
input_fullpath <- gsub(" ", "", paste(input_path, input_table))

output_path <- input_path
output_table <- "tbl_AnimalID.csv"
output_fullpath <- gsub(" ", "", paste(output_path, output_table))

# tbl_LifeHistory ---------------------------------------------------------
tbl_LifeHistory <- read.csv(input_fullpath)
View(tbl_LifeHistory)



# tbl_AnimalID ------------------------------------------------------------
tbl_AnimalID <- tbl_LifeHistory %>% 
  arrange(AnimalID_Std) %>%
  select(AnimalID_Std) %>%
  rename(AnimalID = AnimalID_Std)
View(tbl_AnimalID)
#Zanzibar and Goose re duplicated because they duplicated in LHF

#write csv
write.csv(tbl_AnimalID,output_fullpath,row.names = FALSE)
