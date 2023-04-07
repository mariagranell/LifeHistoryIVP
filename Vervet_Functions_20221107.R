

# Standardize name  -------------------------------------------------------
# To be applied before joins 

std_AnimalID <- function(df, animalID = AnimalID) { 
  
  df %>% 
  
  #REMOVE () AND THEIR CONTENT 
  mutate(AnimalID_Std = str_replace({{animalID}}, 
                                    pattern = "\\(.*\\)",
                                    replacement = "")) %>% 
  
  #REMOVE - 
  mutate(AnimalID_Std = str_replace(AnimalID_Std, 
                                    pattern = "-",
                                    replacement = "")) %>%
    
  #REMOVE /
    mutate(AnimalID_Std = str_replace(AnimalID_Std, 
                                      pattern = "/",
                                      replacement = "")) %>% 
  
  #REMOVE SPACE 
  mutate(AnimalID_Std = str_replace_all(AnimalID_Std, 
                                        pattern = c(" "),
                                        replacement = "")) %>% 
  
  #AS LOWER CASE
  mutate(AnimalID_Std = str_to_lower(AnimalID_Std)) %>% 
  
  #CAPITAL AS FIRST LETTER 
  mutate(AnimalID_Std = str_to_title(AnimalID_Std))
}



# Age ---------------------------------------------------------------------

#add age if I have already BirthDate available
# modifications by maria, added Date as Sys.Date and added a pull(unit) so we don´t get the
# whole dataframe but just the desired column
add_age <- function(data, birthdate = DOB , date = Date, unit = NULL ){
  Date <- Sys.Date()
  if (unit == "Months"){
    data %>%
      mutate(Age_Months = round(as.integer({{date}} - {{birthdate}})/30.4375,digits = 2)) %>%
      pull(Age_Months)
    #return <- data$Age_Months
  } else if (unit == "Days") {
    data %>%
      mutate(Age_Days = round(as.integer({{date}} - {{birthdate}}),digits=2)) %>%
      pull(Age_Days)
    #return <- data$Age_Days
  } else if (unit == "Years") {
    data %>%
      mutate(Age_Years = round(as.integer({{date}} - {{birthdate}})/364.25,digits = 2)) %>%
      pull(Age_Years)
  }
}
