# Linking Life History file to the IVP

Author: Phillipe Vulloid

Commentator: Maria Granell Ruiz 14/02/2023

## Get started

The first step is to read the ```Vervet_Funtions_20221107.R``` file, there you can find the functions add_age and 
std_AnimalID

## How to create the tables

Inside the folder tbl_Creation there are three consequtive folders:
 - ```CSV```: In here you will have your source files, the orginisl life history file that you will use to generate 
the rest of the tables.
 - ```R```: Here you can find the R codes that will extract the relevant information from the IVP file and create
indivual tables, for instance, sex, age, tenure, etc.
 - ```TBL```: Here is where your tables will be generated, that you can later use to link with your studies and stats.

## R folders check

|     | File name    | Checked    | Comments                                                                                                                                                                                  | Input                                                                              | Output          |
|-----|--------------|------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------|-----------------|
| 1   | Life History | Yes        | Initial cleaning of the data base, there is some information loss for the dates. <br>Rename of the column names                                                                           | Life History (original file)                                                       | tbl_LifeHistory |
| 2   | Animal ID    | Yes        | It starts from tbl_LifeHistory, and generate a list of the monkeys full names                                                                                                             | tbl_LifeHistory                                                                    | tbl_AnimalID    |
| 3   | Animal Codes | Yes        | It created a list of all the animal codes. No very checked how good the file is                                                                                                           | tbl_LifeHistory                                                                    | tbl_AnimalCode  |
| 4   | Sex          | Yes        | Outputs a table with AnimalID, AnimalCode and Sex. <br/>It generates a table with "Code check" column to see life history monkeys with consistencies                                      | tbl_AnimalID<br/>tbl_LifeHistory                                                   | tbl_Sex         |
| 5   | DOB          | Yes        | Outputs a table with AnimalID and DOB. It only contains verified DOB Miguel criteria<br/>Requires tables manually corrected by Miguel                                                     | tbl_AnimalID<br/>tbl_LifeHistory<br/>BirthGroup_NoDOB_MiguelCorrected_20221110.csv | tbl_DOB         |
| 6   | Natal        | Yes        | Outputs a table with AnimalID, DOB, BirthGroup and Natal. <br/>Natal as Was it born in out groups? yes or no factor.<br/> It is consider not Natal if it does not have DOB and BirthGroup | tbl_AnimalID<br/>tbl_LifeHistory                                                   | tbl_Natal       |
| 7   | OtherID      | Yes        | Sometimes one indidivual has been called by several names when collecting the data.<br/>This code produces a table with AnimalID and alternative standarized names (see functions)        | tbl_AnimalID<br/>tbl_LifeHistory                                                   | tbl_OtherID     |
| 7   | FromTo       |            |                                                                                                                                                                                           | tbl_AnimalID<br/>tbl_LifeHistory<br/>tbl_Sex<br/>tbl_DOB<br/>tbl_Natal             |                 |
| 8   | Membership   |            |                                                                                                                                                                                           |                                                                                    |                 |
| 10  | IDCorrection | In process | These files will correct typos when collecting data. <br/> For now they are in process, future implementation                                                                             | Mathia_NameCorrection<br/>tbl_LifeHistory                                          | ?               |
| 11  | MSdata       | No         | Hormone analysis file                                                                                                                                                                     | -                                                                                  | -               |

