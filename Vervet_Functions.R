
library("stringr")

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

# add age if I have already BirthDate available
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

add_age <- function(birthdate, date = Sys.Date(), unit) {
  if (unit == "Months") {
    round(as.numeric(difftime(date, birthdate, units = "days")) / 30.4375, digits = 2)
  } else if (unit == "Days") {
    round(as.numeric(difftime(date, birthdate, units = "days")), digits = 2)
  } else if (unit == "Years") {
    round(as.numeric(difftime(date, birthdate, units = "days")) / 365.25, digits = 2)
  } else {
    stop("Invalid unit specified.")
  }
}

# Integrate OtherID ------------------------
# For that you need to use the KeyOtherID file you creatte when creating lh
KeyOtherID <- read.csv("/Users/mariagranell/Repositories/data/life_history/tbl_Creation/TBL/KeyOtherID.csv")

integrate_otherid <- function(df, id_column) {

  # Ensure the id_column is quoted (if passed unquoted)
  id_column <- enquo(id_column)

  # Define static columns
  other_id_column <- "OtherID"
  animal_code_column <- "AnimalCode"

  # Remove spaces from the ID column in df
  df <- df %>%
    mutate(!!id_column := str_replace_all(!!id_column, " ", ""))

  # Remove spaces from the OtherID column in KeyOtherID
  key_df <- KeyOtherID %>%
    mutate(OtherID = str_replace_all(OtherID, " ", ""))

  # Perform the left join and replacement
  df_corrected <- df %>%
    left_join(key_df, by = setNames(other_id_column, quo_name(id_column))) %>%
    mutate(!!id_column := coalesce(!!sym(animal_code_column), !!id_column)) %>%
    dplyr::select(-!!sym(animal_code_column))

  return(df_corrected)
}