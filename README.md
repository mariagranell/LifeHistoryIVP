# Linking Life History file to the IVP

Original code: Phillipe Vulloid <br/>
Modification and writer: Maria Granell Ruiz 14/02/2023 <br/>
Extra contributors: Josefien Tankink and Miguel Gareta <br/>

### Goal
The goal of this workflow is to create a clean and legible Life history file to use as a base for further analysis.
Ideally this workflow will be used every time there is a need to use a new version of the life history life.

## Workflow

The idea is to start with the life history file sent to you by the IVP. 
With that, you start going through the scripts from 1 to 8 (you can skip 7; it still needs to be completed). 
Each script will generate a clean table on the topic. Step 8 will merge all the relevant information in the file ```factchecked_LH```. 
You can use the "topic" tables independently or use ```factchecked_LH``` in your day-to-day analysis. Just be aware that the generated tables use the date you created them; for example, monkeys that were "Still present" in a group will now have the date when you ran the script.

## How to create the tables

Inside the folder tbl_Creation there are three consecutive folders:
 - ```Source_files```: In here you will have your source files, the original life history file that you will use to generate 
the rest of the tables. Here you also have an Archive folder, that contains mostly manual corrections that Miguel made on Life History
 - ```R_scripts```: Here you can find the R codes that will extract the relevant information from the IVP file and create
individual tables, for instance, sex, age, tenure, etc.
 - ```TBL```: Here is where your tables will be generated, that you can later use to link with your studies and stats. Here you also have an Archive folder, that contains tables that are not updated but that you´d like to save

*Data access*: the Source files and TBL generated files are not included in this public shared platform since they are property of the IVP.
Email me at mgranellruiz@gmail.com if you would like to have them.

## Recommendations
Always analyze data using the Code names as identifiers, since those are the ones use later for the behaviour.

## R folders check

|     | File name        | Checked | Comments                                                                                                                                                                                                                                                                                               | Input                                                                              | Output              |
|-----|------------------|---------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------|---------------------|
| 1   | Life History     | Yes     | Initial cleaning of the data base, there is some information loss for the dates. <br>Rename of the column names                                                                                                                                                                                        | Life History (original file)                                                       | tbl_LifeHistory     |
| 2   | Animal ID        | Yes     | It starts from tbl_LifeHistory, and generate a list of the monkeys names. You can find the LH_RowNumber, the AnimalCode (it can be 3 or 4 letters long), the AnimalName, that is the full name <br/> and a more complete list of OtherID provided by Mathias but not all the information is integrated. From here you also create a KeyOtherID table that contains only the AnimalCode and OtherID, together with the function integrate_otherid() you can change the names on the behavioural files | tbl_LifeHistory                                                                    | tbl_AnimalID  <br/> KeyOtherID.csv      |
| 3   | Sex              | Yes     | Outputs a table with LH_RowNumber, AnimalCode, AnimalName and Sex. <br/>It generates a table with "Code check" column to see life history monkeys with consistencies                                                                                                                                   | tbl_AnimalID<br/>tbl_LifeHistory                                                   | tbl_Sex             |
| 4   | DOB              | Yes     | Outputs a table with AnimalCode, AnimalName and DOB. The original file only contains verified DOB Miguel criteria<br/>Requires tables manually corrected by Miguel                                                                                                                                     | tbl_AnimalID<br/>tbl_LifeHistory<br/>BirthGroup_NoDOB_MiguelCorrected_20221110.csv | tbl_DOB             |
| 5   | Group Membership | Yes     | Outputs a table with AnimalName, Group_mb, StartDate_mb, EndDate_mb and Tenure_type. It combines all the different migrations and group splits in those columns br/> thus, you can have more than one entry per individual. The top date of group membership is when I created the file: 16/11/2023    | tbl_AnimalID<br/>tbl_LifeHistory                                                   | tbl_GroupMembership |
| 6   | Age              | Yes     | Outputs a table with AnimalCode, AnimalName, DOB, DOB_estimate, Age_yr and Age_class. The ages are calculated from the estimate, thus not exact dates <br/> This data should only be used to get an idea but not to publish. Age calculated when the file was created: 16/11/2023                      | tbl_AnimalID<br/>tbl_LifeHistory<br/>tbl_Sex<br/>tbl_DOB<br/>tbl_Natal             | tbl_Age             |
| 7   | Membership       | No      | File not checked changes not included                                                                                                                                                                                                                                                                  |                                                                                    |                     |
| 8   | Factchecked      | Yes     | Outputs a factchecked LH table, i.e. a combination of the above, for anyone to use in their everyday data analysis with all the relevant data cleaned in the above tables                                                                                                                              | all of the above                                                                   | factchecked_LH      |

### Important Note on DOB_estimate and Age Calculations:

In this dataset, DOB_estimate, which stands for 'Estimated Date of Birth', is specifically designed for generating approximate dates of birth when actual birth dates are unknown or not recorded, the different assumptions are recorded in the code 
The primary purpose of these estimates is to facilitate the calculation of Age_class.

The Age_class derived from DOB_estimate is reliable for categorical analysis and broad classifications based on age groups. It is particularly useful for studies where precise age is less critical and where age categories suffice.
However, you should be careful when using DOB_estimate for calculating the full age (Age_yr). Thus, for any formal analysis I recommend to rely on Age_class for age-related analyses when precise birthdates are not available.
But, for any application requiring exact age calculations, it is better to use only data present in DOB.