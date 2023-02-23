
# Package -----------------------------------------------------------------


# Description -------------------------------------------------------------
#The aim is to generate a tbl with all the animal short names 


# Paths ------------------------------------------------------------------
input_path <- "/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria/"
input_table <- "tbl_LifeHistory.csv"
input_fullpath <- gsub(" ", "",paste(input_path,input_table))

output_path <- input_path
output_table <- "tbl_AnimalCode.csv"
output_fullpath <- gsub(" ", "", paste(output_path, output_table))

# tbl_LifeHistory --------------------------------------------------------
tbl_LifeHistory <- read.csv(input_fullpath)
View(tbl_LifeHistory)



# tbl_AnimalCode ------------------------------------------------------------
tbl_AnimalCode <- tbl_LifeHistory %>%
  arrange(LH_AnimalCode) %>%
  select(LH_AnimalCode) %>%
  rename(AnimalCode = LH_AnimalCode)
View(tbl_AnimalCode)
#Zanzibar and Goose re duplicated because they duplicated in LHF

# change the " " to NA -------------------------------------------------------
tbl_AnimalCode <- tbl_AnimalCode %>%
  mutate_all(na_if,"")

#write csv
write.csv(tbl_AnimalCode,output_fullpath,row.names = FALSE)



# Check the animal code nature -------------------------------------------------------------------------
# number of males and females
nrow(tbl_AnimalCode)
df <- as.data.frame(tbl_AnimalCode)
df$sexy <- as.factor(ifelse(nchar(df$AnimalCode) == 3, "M", "F"))

df%>%
  group_by(sexy)%>%
  summarise(n =n())
# f= 393 m= 370, na=32


