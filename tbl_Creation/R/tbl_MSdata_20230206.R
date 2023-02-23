

# package -----------------------------------------------------------------

library(tidyverse)


# General remark ----------------------------------------------------------

#Possibly better to write them down in a word document and have them saved somewhere 

#The pipeline is not well adapted to saturated concentrations. This info should be included in S/N file or in another way.
# The issue is that saturated return a "" [] that the pipeline then converts to a 0



# Signal-Noise ------------------------------------------------------------

#wd
setwd("C:/Users/Philippe/Dropbox/PhD_20150515/Research projects/Vervet_Miguel/HormoneAnalyses/HeightNoise")


#Signal Noise
SignalNoise <- read.csv("HeightNoise_20230203.csv") %>% 
  
  #SELECT 
  select(NPACID,
         Hormone,
         Height,
         Noise,
         NoiseSide) %>% 
  
  #ADD SN 
  mutate(SN = Height/Noise) %>% 
  
  #CHANGE NAME OF HORMONE
  mutate(Hormone = case_when(Hormone == "Testosterone" ~ "T",
                             TRUE ~ Hormone))



# Sample List -------------------------------------------------------------

#wd
setwd("C:/Users/Philippe/Dropbox/PhD_20150515/Research projects/Vervet_Miguel/HormoneAnalyses/NPAC_ProcessingFolder/SampleList")


#SampleList
SampleList <- read.csv("SampleList_Vervet_20230201.csv") %>% 
  rename(MSType = SampleAnalyteType) %>%  #To avoid confusion with sample type in extraction list
  #ADD BATCH POSITION 
  group_by(BatchID) %>% 
  mutate(BatchPosition = row_number()) %>% 
  ungroup() %>% 
  #RELOCATE
  relocate(BatchPosition, .after= BatchID)
View(SampleList)

  
# Primary flags -----------------------------------------------------------

# https://support.waters.com/KB_Inf/MassLynx/WKB1004_What_do_the_abbreviations_in_the_Primary_Flag_field_mean
# 
# b: Peak starts or ends on the baseline
# d: Peak starts or ends on a drop line
# s: Peak is a shoulder on another peak
# t: Peak starts or ends at the start or end of the trace.
# M: The peak start or end point was manually altered
# -: The peak was manually deleted
# !: Flagged peak
# I: The response would either give a negative concentration or the calibration equation is not solvable for the given response (indeterminate flag)
# X: Point was manually excluded from the calibration curve.




# Hormone NonProcessed ----------------------------------------------------
#MS summary of non-processed E2 levels with standard curve optimized for <= 100 ng/ml


#wd
setwd("C:/Users/Philippe/Dropbox/PhD_20150515/Research projects/Vervet_Miguel/HormoneAnalyses/NPAC_ProcessingFolder/CSV/NonProcessed")

#Batch 1
E2_NP_V1 <- read.csv("E2_V1_NonProcessed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "E2")
P4_NP_V1 <- read.csv("P4_V1_NonProcessed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "P4")
T_NP_V1 <- read.csv("T_V1_NonProcessed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "T")
A4_NP_V1 <- read.csv("A4_V1_NonProcessed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "A4")
DHEA_NP_V1 <- read.csv("DHEA_V1_NonProcessed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "DHEA")
Cort_NP_V1 <- read.csv("Cortisol_V1_NonProcessed_Low_20220117.csv", sep = ";") %>% 
  mutate(Hormone = "Cort")
Cortisone_NP_V1 <- read.csv("Cortisone_V1_NonProcessed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "Cortisone")


#Batch 2
E2_NP_V2 <- read.csv("E2_V2_NonProcessed_Low_20221202.csv")%>% 
  mutate(Hormone = "E2")
P4_NP_V2 <- read.csv("P4_V2_NonProcessed_Low_20221202.csv")%>% 
  mutate(Hormone = "P4")
T_NP_V2 <- read.csv("T_V2_NonProcessed_Low_20221202.csv") %>% 
  mutate(Hormone = "T")
A4_NP_V2 <- read.csv("A4_V2_NonProcessed_Low_20221202.csv") %>% 
  mutate(Hormone = "A4")
DHEA_NP_V2 <- read.csv("DHEA_V2_NonProcessed_Low_20221202.csv") %>% 
  mutate(Hormone = "DHEA")
Cort_NP_V2 <- read.csv("Cortisol_V2_NonProcessed_Low_20221202.csv") %>% 
  mutate(Hormone = "Cort")
Cortisone_NP_V2 <- read.csv("Cortisone_V2_NonProcessed_Low_20221202.csv") %>% 
  mutate(Hormone = "Cortisone")


#Batch 3
#There was no standard curve on that batch but, I labelled them as low
E2_NP_V3 <- read.csv("E2_V3_NonProcessed_Low_20230118.csv", sep = ";")%>% 
  mutate(Hormone = "E2")
View(E2_NP_V3)
P4_NP_V3 <- read.csv("P4_V3_NonProcessed_Low_20230118.csv", sep = ";")%>% 
  mutate(Hormone = "P4")
T_NP_V3 <- read.csv("T_V3_NonProcessed_Low_20230118.csv", sep = ";") %>% 
  mutate(Hormone = "T")
#still missing 
A4_NP_V3 <- read.csv("A4_V3_NonProcessed_Low_20230118.csv", sep = ";") %>% 
  mutate(Hormone = "A4")
DHEA_NP_V3 <- read.csv("DHEA_V3_NonProcessed_Low_20230118.csv", sep = ";") %>% 
  mutate(Hormone = "DHEA")
Cort_NP_V3 <- read.csv("Cortisol_V3_NonProcessed_Low_20230118.csv", sep = ";") %>% 
  mutate(Hormone = "Cort")
Cortisone_NP_V3 <- read.csv("Cortisone_V3_NonProcessed_Low_20230118.csv", sep = ";") %>% 
  mutate(Hormone = "Cortisone")


#Batch 4
E2_NP_V4 <- read.csv("E2_V4_NonProcessed_Low_20230126.csv")%>% 
  mutate(Hormone = "E2")
P4_NP_V4 <- read.csv("P4_V4_NonProcessed_Low_20230126.csv")%>% 
  mutate(Hormone = "P4")
T_NP_V4 <- read.csv("T_V4_NonProcessed_Low_20230126.csv") %>% 
  mutate(Hormone = "T")
A4_NP_V4 <- read.csv("A4_V4_NonProcessed_Low_20230126.csv") %>% 
  mutate(Hormone = "A4")
DHEA_NP_V4 <- read.csv("DHEA_V4_NonProcessed_Low_20230126.csv") %>% 
  mutate(Hormone = "DHEA")
Cort_NP_V4 <- read.csv("Cortisol_V4_NonProcessed_Low_20230126.csv") %>% 
  mutate(Hormone = "Cort")
Cortisone_NP_V4 <- read.csv("Cortisone_V4_NonProcessed_Low_20230126.csv") %>% 
  mutate(Hormone = "Cortisone")


#Batch 5
E2_NP_V5 <- read.csv("E2_V5_NonProcessed_Low_20230201.csv", sep = ";")%>% 
  mutate(Hormone = "E2")
P4_NP_V5 <- read.csv("P4_V5_NonProcessed_Low_20230201.csv", sep = ";")%>% 
  mutate(Hormone = "P4")
T_NP_V5 <- read.csv("T_V5_NonProcessed_Low_20230201.csv", sep = ";") %>% 
  mutate(Hormone = "T")
A4_NP_V5 <- read.csv("A4_V5_NonProcessed_Low_20230201.csv", sep = ";") %>% 
  mutate(Hormone = "A4")
DHEA_NP_V5 <- read.csv("DHEA_V5_NonProcessed_Low_20230201.csv", sep = ";") %>% 
  mutate(Hormone = "DHEA")
Cort_NP_V5 <- read.csv("Cortisol_V5_NonProcessed_Low_20230201.csv", sep = ";") %>% 
  mutate(Hormone = "Cort")
Cortisone_NP_V5 <- read.csv("Cortisone_V5_NonProcessed_Low_20230201.csv", sep = ";") %>% 
  mutate(Hormone = "Cortisone")




# Hormones Low ------------------------------------------------------------
#MS summary of processed E2 levels with standard curve optimized for <= 100 ng/ml

#wd
setwd("C:/Users/Philippe/Dropbox/PhD_20150515/Research projects/Vervet_Miguel/HormoneAnalyses/NPAC_ProcessingFolder/CSV/Low")

#Batch 1
E2_Low_V1 <- read.csv("E2_V1_Processed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "E2")
P4_Low_V1 <- read.csv("P4_V1_Processed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "P4")
T_Low_V1 <- read.csv("T_V1_Processed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "T")
A4_Low_V1 <- read.csv("A4_V1_Processed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "A4")
DHEA_Low_V1 <- read.csv("DHEA_V1_Processed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "DHEA")
Cort_Low_V1 <- read.csv("Cortisol_V1_Processed_Low_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "Cort")
Cortisone_Low_V1 <- read.csv("Cortisone_V1_Processed_Low_20230117.csv", sep = ";") %>%
  mutate(Hormone = "Cortisone")


#Batch 2
E2_Low_V2 <- read.csv("E2_V2_Processed_Low_20221202.csv")%>% 
  mutate(Hormone = "E2")
P4_Low_V2 <- read.csv("P4_V2_Processed_Low_20221202.csv")%>% 
  mutate(Hormone = "P4")
T_Low_V2 <- read.csv("T_V2_Processed_Low_20221202.csv")%>% 
  mutate(Hormone = "T")
A4_Low_V2 <- read.csv("A4_V2_Processed_Low_20221202.csv") %>% 
  mutate(Hormone = "A4")
DHEA_Low_V2 <- read.csv("DHEA_V2_Processed_Low_20221202.csv") %>% 
  mutate(Hormone = "DHEA")
Cort_Low_V2 <- read.csv("Cortisol_V2_Processed_Low_20221202.csv") %>% 
  mutate(Hormone = "Cort")
Cortisone_Low_V2 <- read.csv("Cortisone_V2_Processed_Low_20221202.csv") %>%
  mutate(Hormone = "Cortisone")


#Batch 3
#There was no standard curve on that batch but I labelled all processed sample as low 
#Keep in mind that these are going to contain 
E2_Low_V3 <- read.csv("E2_V3_Processed_Low_20230118.csv", sep = ";")%>% 
  mutate(Hormone = "E2")
P4_Low_V3 <- read.csv("P4_V3_Processed_Low_20230118.csv", sep = ";")%>% 
  mutate(Hormone = "P4")
T_Low_V3 <- read.csv("T_V3_Processed_Low_20230118.csv", sep = ";")%>% 
  mutate(Hormone = "T")
A4_Low_V3 <- read.csv("A4_V3_Processed_Low_20230118.csv", sep = ";") %>% 
  mutate(Hormone = "A4")
DHEA_Low_V3 <- read.csv("DHEA_V3_Processed_Low_20230118.csv", sep = ";") %>% 
  mutate(Hormone = "DHEA")
Cort_Low_V3 <- read.csv("Cortisol_V3_Processed_Low_20230118.csv", sep = ";") %>% 
  mutate(Hormone = "Cort")
Cortisone_Low_V3 <- read.csv("Cortisone_V3_Processed_Low_20230118.csv", sep = ";") %>%
  mutate(Hormone = "Cortisone")


#Batch 4
E2_Low_V4 <- read.csv("E2_V4_Processed_Low_20230126.csv")%>% 
  mutate(Hormone = "E2")
P4_Low_V4 <- read.csv("P4_V4_Processed_Low_20230126.csv")%>% 
  mutate(Hormone = "P4")
T_Low_V4 <- read.csv("T_V4_Processed_Low_20230126.csv") %>% 
  mutate(Hormone = "T")
A4_Low_V4 <- read.csv("A4_V4_Processed_Low_20230126.csv") %>% 
  mutate(Hormone = "A4")
DHEA_Low_V4 <- read.csv("DHEA_V4_Processed_Low_20230126.csv") %>% 
  mutate(Hormone = "DHEA")
Cort_Low_V4 <- read.csv("Cortisol_V4_Processed_Low_20230126.csv") %>% 
  mutate(Hormone = "Cort")
Cortisone_Low_V4 <- read.csv("Cortisone_V4_Processed_Low_20230126.csv") %>% 
  mutate(Hormone = "Cortisone")


#Batch 5
E2_Low_V5 <- read.csv("E2_V5_Processed_Low_20230201.csv", sep = ";")%>% 
  mutate(Hormone = "E2")
P4_Low_V5 <- read.csv("P4_V5_Processed_Low_20230102.csv", sep = ";")%>% 
  mutate(Hormone = "P4")
T_Low_V5 <- read.csv("T_V5_Processed_Low_20230102.csv", sep = ";") %>% 
  mutate(Hormone = "T")
A4_Low_V5 <- read.csv("A4_V5_Processed_Low_20230102.csv", sep = ";") %>% 
  mutate(Hormone = "A4")
DHEA_Low_V5 <- read.csv("DHEA_V5_Processed_Low_20230102.csv", sep = ";") %>% 
  mutate(Hormone = "DHEA")
Cort_Low_V5 <- read.csv("DHEA_V5_Processed_Low_20230102.csv", sep = ";") %>% 
  mutate(Hormone = "Cort")
Cortisone_Low_V5 <- read.csv("Cortisone_V5_Processed_Low_20230102.csv", sep = ";") %>% 
  mutate(Hormone = "Cortisone")



# Hormones High -----------------------------------------------------------
#wd
setwd("C:/Users/Philippe/Dropbox/PhD_20150515/Research projects/Vervet_Miguel/HormoneAnalyses/NPAC_ProcessingFolder/CSV/High")

#Batch 1
DHEA_High_V1 <- read.csv("DHEA_V1_Processed_High_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "DHEA")
Cort_High_V1 <- read.csv("Cortisol_V1_Processed_High_20230117.csv", sep = ";") %>% 
  mutate(Hormone = "Cort")
Cortisone_High_V1 <- read.csv("Cortisone_V1_Processed_High_20230117.csv", sep = ";")%>%
  mutate(Hormone = "Cortisone")


#Batch 2
DHEA_High_V2 <- read.csv("DHEA_V2_Processed_High_20221202.csv") %>% 
  mutate(Hormone = "DHEA")
Cort_High_V2 <- read.csv("Cortisol_V2_Processed_High_20221202.csv") %>% 
  mutate(Hormone = "Cort")
Cortisone_High_V2 <- read.csv("Cortisone_V2_Processed_High_20221202.csv")%>%
  mutate(Hormone = "Cortisone")


#Batch 3
#There was no standard curve on that batch so I I labelled the processed sample as low 


#Batch 4
DHEA_High_V4 <- read.csv("DHEA_V4_Processed_High_20230126.csv") %>% 
  mutate(Hormone = "DHEA")
Cort_High_V4 <- read.csv("Cortisol_V4_Processed_High_20230126.csv") %>% 
  mutate(Hormone = "Cort")
Cortisone_High_V4 <- read.csv("Cortisone_V4_Processed_High_20230126.csv")%>%
  mutate(Hormone = "Cortisone")



#Batch 5 
DHEA_High_V5 <- read.csv("DHEA_V5_Processed_High_20230203.csv", sep = ";") %>% 
  mutate(Hormone = "DHEA")
Cort_High_V5 <- read.csv("Cortisol_V5_Processed_High_20230203.csv", sep = ";") %>% 
  mutate(Hormone = "Cort")
Cortisone_High_V5 <- read.csv("Cortisone_V5_Processed_High_20230203.csv", sep = ";")%>%
  mutate(Hormone = "Cortisone")




# Bind NP -----------------------------------------------------------------

NP <- bind_rows(E2_NP_V1,
                P4_NP_V1,
                T_NP_V1,
                A4_NP_V1,
                DHEA_NP_V1,
                Cort_NP_V1,
                Cortisone_NP_V1,
                E2_NP_V2,
                P4_NP_V2,
                T_NP_V2,
                A4_NP_V2,
                DHEA_NP_V2,
                Cort_NP_V2,
                Cortisone_NP_V2,
                E2_NP_V3,
                P4_NP_V3,
                T_NP_V3,
                #A4_NP_V3,
                DHEA_NP_V3,
                Cort_NP_V3,
                Cortisone_NP_V3,
                E2_NP_V4,
                P4_NP_V4,
                T_NP_V4,
                A4_NP_V4,
                DHEA_NP_V4,
                Cort_NP_V4,
                Cortisone_NP_V4,
                E2_NP_V5,
                P4_NP_V5,
                T_NP_V5,
                A4_NP_V5,
                DHEA_NP_V5,
                Cort_NP_V5,
                Cortisone_NP_V5) %>% 
  mutate(ProcessType = "NP")
View(NP)
#1807 only A4 B3 missing


# Bind Low ----------------------------------------------------------------

Low <- bind_rows(E2_Low_V1,
                 P4_Low_V1,
                 T_Low_V1,
                 A4_Low_V1,
                 DHEA_Low_V1,
                 Cort_Low_V1,
                 Cortisone_Low_V1,
                 E2_Low_V2,
                 P4_Low_V2,
                 T_Low_V2,
                 A4_Low_V2,
                 DHEA_Low_V2,
                 Cort_Low_V2,
                 Cortisone_Low_V2,
                 E2_Low_V3,
                 P4_Low_V3,
                 T_Low_V3,
                 A4_Low_V3,
                 DHEA_Low_V3,
                 Cort_Low_V3,
                 Cortisone_Low_V3,
                 E2_NP_V4,
                 P4_NP_V4,
                 T_NP_V4,
                 A4_NP_V4,
                 DHEA_NP_V4,
                 Cort_NP_V4,
                 Cortisone_NP_V4,
                 E2_NP_V5,
                 P4_NP_V5,
                 T_NP_V5,
                 A4_NP_V5,
                 DHEA_NP_V5,
                 Cort_NP_V5,
                 Cortisone_NP_V5) %>% 
  mutate(ProcessType = "Low")
View(Low)
#1799

Low_Duplicated <-   Low %>% 
  group_by_all() %>% 
  filter(n()>1) %>% 
  ungroup() %>% 
  arrange(Name,
          Hormone,
          ProcessType)
View(Low_Duplicated)


# Bind High ---------------------------------------------------------------

High <- bind_rows(DHEA_High_V1,
                Cort_High_V1,
                Cortisone_High_V1,
                DHEA_High_V2,
                Cort_High_V2,
                Cortisone_High_V2,
                DHEA_High_V4,
                Cort_High_V4,
                Cortisone_High_V4,
                DHEA_High_V5,
                Cort_High_V5,
                Cortisone_High_V5) %>% 
  mutate(ProcessType = "High")

View(High)
names(High)

# Bind all ----------------------------------------------------------------

MSFiles_AllHormones <- bind_rows(NP, 
                                 Low,
                                 High) %>% 
  #ADD NPACID
  left_join(.,SampleList %>% 
              select(NPACID,
                     BatchID,
                     BatchPosition,
                     MSType,
                     File.Name), by=c("Name" = "File.Name")) %>%
  relocate(NPACID:MSType)
View(MSFiles_AllHormones)

View(MSFiles_AllHormones)
#4119 entries, missing batch 3 NP 



# Add Info ----------------------------------------------------------------
# Add relevant information to MS files of all hormones
# 1) Join SampleID (required to add Signal-Noise)
# 2) Add signal noise
# 3) Add peak modification

# b: Peak starts or ends on the baseline
# d: Peak starts or ends on a drop line
# s: Peak is a shoulder on another peak
# t: Peak starts or ends at the start or end of the trace.
# M: The peak start or end point was manually altered
# -: The peak was manually deleted
# !: Flagged peak
# I: The response would either give a negative concentration or the calibration equation is not solvable for the given response (indeterminate flag)
# X: Point was manually excluded from the calibration curve.

  
#Na []
#Among those are some that [] was too high to be determined
NA_Conc <- MSFiles_AllHormones %>% 
  filter(is.na(ng.ml),
         MSType == "Analyte")
View(NA_Conc)


#High [] 
#took 90ng/ml but completely arbitrary
HighConc <- MSFiles_AllHormones %>% 
  filter(ng.ml > 90,
         MSType == "Analyte") %>% 
  distinct(NPACID,
           Hormone)


#True NA
NA_Conc_True <- anti_join(NA_Conc,
                          HighConc) %>% 
  select(NPACID,
         BatchID,
         ProcessType,
         Hormone) %>% 
  mutate(NA_Replace = 0)
View(NA_Conc_True)


#MS Data 
MSFiles <- MSFiles_AllHormones %>% 
  
  #ADD SIGNAL NOISE
  left_join(.,SignalNoise %>% 
            select(NPACID,
                   Hormone,
                   SN)) %>% 
  
  #NA REPLACE 
  left_join(., NA_Conc_True) %>% 
  mutate(ng.ml = case_when(NA_Replace == 0 ~ 0,
                           is.na(NA_Replace) ~ ng.ml)) %>% 
  
  #ADD PEAK MODIFICATION
  #doesn't contain all possible modification. More can be added if required
  mutate(Peak_Editing = case_when(ProcessType %in% c("Low", "High") & Primary.Flags %in% c("MM-", "MM-I") ~ "Removed",
                                  ProcessType %in% c("Low", "High") & Primary.Flags == "MM" ~ "Modified",
                                  ProcessType == "NP" & Primary.Flags %in% c("MM-", "MM-I") ~ "Error",
                                  ProcessType == "NP" & Primary.Flags == "MM" ~ "Error",
                                  TRUE ~ "None")) %>% 


  
  #ADD QUANTIFICATION CATEGORY
  #Applied to processed data only (will see whether it is an issue or not)
  #errors here will cascade when applying urine volume and sg correction
  mutate(QuantificationCategory = case_when(ProcessType %in% c("Low", "High") & Peak_Editing %in% c("MM-", "MM-I") | 
                                              ProcessType %in% c("Low", "High") & SN < 3 | 
                                              ProcessType %in% c("Low", "High") & ng.ml == 0 ~ "ND",
                                            ProcessType %in% c("Low", "High") & SN >= 3 & SN < 8 ~ "D",
                                            ProcessType == "NP" ~ as.character(NA),
                                            TRUE ~ "Q")) %>% 
  
  #LEVEL QUANTIFICATION CATEGORY AS FACTOR 
  #Could be moved to DataPreparation_HormoneLevels
  mutate(QuantificationCategory = fct_relevel(QuantificationCategory,
                                              "ND",
                                              "D",
                                              "Q"))

View(MSData)


#write csv 
#put the directory where to put it 
setwd("C:/Users/Philippe/Dropbox/PhD_20150515/Research projects/Vervet_Miguel/Database/tbl_Creation")
write.csv(MSFiles,"MSData_Vervet_20230206.csv",row.names = FALSE)

#Get csv 
MSFiles <- read.csv("MSData_Vervet_20230206.csv")



# DC edited data non-processed --------------------------------------------

# #Are there any non-processed data which peak has been modified?
View(MSFiles %>%
       filter(Peak_Editing == "Error"))
#All good

#Non-processed value with edited peak?
View(MSFiles %>%
       filter(ProcessType == "NP") %>%
       distinct(Peak_Editing))
#No 


#write a csv file
#write.csv(MSFiles,paste("MSFiles_",today(),".csv", sep=""),row.names = FALSE)


# Analytes Subsetting -----------------------------------------------------


#Retain analytes
MSFiles_Analytes <- MSFiles %>% 
  #RETAIN ANALYTE
  filter(MSType == "Analyte")
View(MSFiles_Analytes)
#3086 without batch 3 non-processed A4


#Count of samples put on machine
MS_AnalytesCount<- MSFiles_Analytes %>% 
  distinct(NPACID,Hormone,.keep_all = TRUE) %>% 
  #filter(!SampleID %in% c(0,1)) %>% 
  count(Hormone)
View(MS_AnalytesCount)
#186 samples analyzed


#NPACID put 2x on the machine 
MS_NPACRepeat <- MSFiles_Analytes %>% 
  distinct(NPACID, BatchID, BatchPosition) %>% 
  add_count(NPACID, name = "Count") %>% 
  filter(Count > 1)
View(MS_NPACRepeat)
#All good 


# 100 Threshold -----------------------------------------------------------

#Processed high
#using a 100ng/ml threshold
MSFiles_Analytes_100_High <- MSFiles_Analytes %>% 
  
  #RETAIN PROCESSED LOW 
  filter(ProcessType == "High") %>% 
  
  #RETAIN HIGH CONCENTRATIONS
  filter(ng.ml > 100)
View(MSFiles_Analytes_100_High)


#Processed Low
#using a 100ng/ml threshold
MSFiles_Analytes_100_Low <- MSFiles_Analytes %>% 
  
  #RETAIN PROCESSED LOW 
  filter(ProcessType == "Low") %>% 
  
  #RETAIN LOW CONCENTRATIONS
  filter(ng.ml <= 100 | BatchID == 3) #because batch 3 was only named low processed 


#Merge Processed low and high
MSFiles_Analytes_Processed <- bind_rows(MSFiles_Analytes_100_Low,
                                           MSFiles_Analytes_100_High)
View(MSFiles_Analytes_Processed)


# Duplicated samples ------------------------------------------------------
#Samples that have low <100 and high > 100 will return duplicated rows
#Keep samples with low processing

#Duplicated samples
MSdata_Duplicated <- MSFiles_Analytes_Processed %>%

  #ADD COUNT PER NPACID
  add_count(NPACID,
            BatchID, 
            BatchPosition,
            Hormone, 
            name = "Count") %>%

  #RETAIN SAMPLES THAT SATISFY BOTH CONDITIONS
  filter(Count > 1) %>%

  #ADD DEVIATION OF [] STD
  #May be used to select samples to be retained and removed
  left_join(.,MSFiles %>%
              filter(MSType == "Standard",
                     Std..Conc == 100) %>%
              select(BatchID,
                     ProcessType,
                     Hormone,
                     X.Dev) %>%
              rename(StdDev = X.Dev))
View(MSdata_Duplicated)
#1 sample 


#CAUSE : Samples which std curve low and high make [] fall <100 with low and > 100 with high. a) assign the [] for which the deviation from 100ng/ml is the lowest or b) assign the processrange high. 



#CAUSE: unwanted duplicates
#When [] around 100, remove process range == "Low", regardless of deviation with 100ng/ml standard
#Just for the sake 
MSdata_Duplicated_100_ToRemove <- MSdata_Duplicated %>%
  filter(ProcessType == "Low")

#For mole-rats there is another cause of duplication that does not apply here because I have adjusted the code. However I can always go back to the MR code for reference 



# Lost samples ------------------------------------------------------------
#Low process > 100 and High process < 100 are excluded from pipeline using a threashold of 100ng/ml
#They will need to be added

MSdata_Lost <- MSFiles_Analytes %>%
  #RETAIN SAMPLES HIGH < 100 AND LOW > 100
  filter( ProcessType == "High" & ng.ml < 100 |
            ProcessType == "Low" & ng.ml > 100) %>%

  #ADD COUNT PER NPACID
  add_count(NPACID,BatchID, BatchPosition,Hormone, name = "Count") %>%

  #RETAIN SAMPLES THAT SATISFY BOTH CONDITIONS
  filter(Count > 1) %>%

  #ADD DEVIATION OF [] STD
  #Will be necessary to select the sample
  left_join(.,MSFiles %>%
              filter(MSType == "Standard",
                     Std..Conc == 100) %>%
              select(BatchID,
                     ProcessType,
                     Hormone,
                     X.Dev) %>%
              rename(StdDev = X.Dev)) %>%
  distinct()
View(MSdata_Lost)
#4 samples excluded when using 100ng/ml threshold


#Retain samples that have the lowest deviation from the 100ng/ml standard
#For teh sake of consistency I should rather retain all the low process I believe
MSdata_Lost_ToAdd <- MSdata_Lost %>%

  #RETAIN SAMPLES WITH 100 NG/ML LOWEST DEVIATION
  group_by(NPACID,BatchID, BatchPosition,Hormone) %>%
  slice_min(abs(StdDev)) %>%
  ungroup() %>%

  #SELECT
  select(- Count,
         - StdDev)
View(MSdata_Lost_ToAdd)

  

# Processed selection ------------------------------------------------------
#Select samples that must be retained in the final file 

MSFiles_Analytes_Selected <- 
  #100 NG/ML THRESHOLD SELECTED SAMPLES
  MSFiles_Analytes_Processed %>% 
  #REMOVE DUPLICATED 100
  anti_join(., MSdata_Duplicated_100_ToRemove) %>%
  #ADD LOST SAMPLES
  bind_rows(MSdata_Lost_ToAdd) %>%
  #RENAME CONCENTRATION
  rename(Conc_Processed = ng.ml)

View(MSFiles_Analytes_Selected)

#1281
#At this stage, I should have no duplicated samples
#All NPAC ID should have 1 row/Hormone



# #MS Analytesthat that do not show up as processed samples?
Analytes_Processed_Missing <- anti_join(MSFiles_Analytes,
                                        MSFiles_Analytes_Selected,
                                        by=c("NPACID","BatchID","BatchPosition","Hormone"))
View(Analytes_Processed_Missing)
#No => all good

#MS analytes selected duplicates
MSAnalytes_Selected_Duplicates <- MSFiles_Analytes_Selected %>%
  #ADD COUNT PER NPACID
  add_count(NPACID,BatchID, BatchPosition,Hormone, name = "Count") %>%
  #RETAIN SAMPLES THAT SATISFY BOTH CONDITIONS
  filter(Count > 1)
View(MSAnalytes_Selected_Duplicates)
#None all good


# Format
# Add the original concentration to processed analytes

MSFiles_SelectedAnalytes_Formatted <- 
  
  #SELECTED PROCESSED ANALYTES
  MSFiles_Analytes_Selected %>%

  #ADD NP []
  left_join(.,MSFiles_Analytes %>%
              #NP VALUES
              filter(ProcessType == "NP") %>% 
              select(NPACID,
                     BatchID,
                     BatchPosition,
                     Hormone,
                     ng.ml) %>% 
              #RENAME []
              rename(Conc_NonProcessed = ng.ml)) %>%

  #RELOCATE NON-PROCESSED CONCENTRATION
  relocate(Conc_NonProcessed, .before = Conc_Processed)
View(MSFiles_SelectedAnalytes_Formatted)
#1302

View(MSFiles_SelectedAnalytes_Formatted)
#Only A4 NP Batch 3 should be missing



# Analytes csv ------------------------------------------------------------
#write a csv file of MS analytes
#Will enter pipeline for further correction

#wd
setwd("C:/Users/Philippe/Dropbox/PhD_20150515/Research projects/Vervet_Miguel/Database/tbl_Creation")

#write csv
write.csv(MSFiles_SelectedAnalytes_Formatted, paste("tbl_MSData",today(),".csv", sep=""),row.names = FALSE)



# Visualisation of quantification -----------------------------------------
# Assess variation of limit of detection and quantification for each hormone and through time (Analyses period)
# Would probably be better placed in a distinct R file
#idea: x-axis sample organized in ascending order of [], y-axis: [], with different shape or colour to see whether sample where ND, D or Q. a different graph should be made for different amount of injection and also based on analyses period. 


# MS Data DC --------------------------------------------------------------

#NA NPACID 
View(MSFiles_Analytes_Formatted %>% 
  filter(is.na(NPACID)))






