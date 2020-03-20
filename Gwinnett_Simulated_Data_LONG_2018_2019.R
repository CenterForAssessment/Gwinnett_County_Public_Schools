################################################################################
###                                                                          ###
###                      GCPS Simulated Data Formatting                      ###
###                                                                          ###
################################################################################

### Load required packages

require(SGP)
require(data.table)

###   Read in simulated data provided by GCPS
Gwinnett_Data_LONG <- as.data.table(readRDS("./Data/Simulated_Data/sim_gcps_data.rds"))

###   Preserve the CONTENT_AREA data as "Test_Name" - create a duplicate field for now
Gwinnett_Data_LONG[, Test_Name := CONTENT_AREA]

#####
###  Create GRADE from CONTENT_AREA
#####

###   Create Initial GRADE variable using CONTENT_AREA values that have the 'GRADE *' convention

###   Quick test to show what we're doing in the first step:
###   Create a test string of some test names and then split them by '/GRADE ' (and remove that part of the string)
grade.test <- c("MATHEMATICS/GRADE K", "SCIENCE/GRADE 5", "Science 6 SEM2", "SOCIAL STUDIES 6 SEM1")
grade.tmp.split <- strsplit(grade.test, "/GRADE ")
sapply(grade.tmp.split, function(x) x[1])
sapply(grade.tmp.split, function(x) x[2]) #  We want this second list element

###   Create a list vector of Gwinnett_Data_LONG$CONTENT_AREA split by '/GRADE '
###   and create GRADE using the second list element
grade.tmp.split <- strsplit(Gwinnett_Data_LONG$CONTENT_AREA, "/GRADE ")

###  Use the list vector to create a GRADE variable (either NA or grade where available)
Gwinnett_Data_LONG[, GRADE := sapply(grade.tmp.split, function(x) x[2])]

table(Gwinnett_Data_LONG[!is.na(GRADE), GRADE, CONTENT_AREA]) # Quick Verification of this step

#  Remove '/GRADE *' from CONTENT_AREA values
Gwinnett_Data_LONG[, CONTENT_AREA := sapply(grade.tmp.split, function(x) x[1])]

table(Gwinnett_Data_LONG[!is.na(GRADE), GRADE, CONTENT_AREA]) # Quick Verification of this step


###  Create GRADE values for Middle School subjects that use 'SEM*' convention

###  First lets capture the 'SEM*' convention in a separate variable and remove from CONTENT_AREA
Gwinnett_Data_LONG[, SEMESTER := as.numeric(NA)]
Gwinnett_Data_LONG[grepl("SEM1", CONTENT_AREA), SEMESTER := 1]
Gwinnett_Data_LONG[grepl("SEM2", CONTENT_AREA), SEMESTER := 2]
table(Gwinnett_Data_LONG[!is.na(SEMESTER), SEMESTER, CONTENT_AREA])

###   clean up CONTENT_AREA for next step (need to remove extra numeric values in strings)
Gwinnett_Data_LONG[, CONTENT_AREA := gsub(" SEM1| SEM2", "", CONTENT_AREA)]

table(Gwinnett_Data_LONG[grepl(" [6-8]", CONTENT_AREA), CONTENT_AREA]) # These are the CONTENT_AREA values we want to mine
table(Gwinnett_Data_LONG[grepl(" [6-8]", CONTENT_AREA), gsub("[^\\d]+", "", CONTENT_AREA, perl=TRUE)]) # Test of values we'll extract

Gwinnett_Data_LONG[grepl(" [6-8]", CONTENT_AREA), GRADE := gsub("[^\\d]+", "", CONTENT_AREA, perl=TRUE)]
table(Gwinnett_Data_LONG[!is.na(GRADE), GRADE, CONTENT_AREA]) # Quick Verification of this step

Gwinnett_Data_LONG[, CONTENT_AREA := gsub(" [6-8]", "", CONTENT_AREA)] # clean up CONTENT_AREA
table(Gwinnett_Data_LONG[!is.na(GRADE), GRADE, CONTENT_AREA]) # Quick Verification of this step

###  Finally change all missing GRADE values to 'EOCT' (Required SGP package convention)
Gwinnett_Data_LONG[is.na(GRADE), GRADE := "EOCT"]

table(Gwinnett_Data_LONG[!is.na(GRADE), GRADE, CONTENT_AREA]) # Quick Verification of this step

#####
###   Finish cleaning up CONTENT_AREA
#####

###   Some cleaning done above as needed for creating GRADE values...

###   No spaces allowed in CONTENT_AREA values
Gwinnett_Data_LONG[, CONTENT_AREA := gsub(" ", "_", CONTENT_AREA)]

###   Make Grade Level subjects consistent in naming convention
Gwinnett_Data_LONG[, CONTENT_AREA := toupper(CONTENT_AREA)] # Fix "Science"
Gwinnett_Data_LONG[CONTENT_AREA == "MATH", CONTENT_AREA := "MATHEMATICS"] # Fix "MATH"
Gwinnett_Data_LONG[CONTENT_AREA == "ACC_MATH", CONTENT_AREA := "ACC_MATHEMATICS"]
Gwinnett_Data_LONG[CONTENT_AREA == "LA/READING", CONTENT_AREA := "LANGUAGE_ARTS"] # Fix ELA


###   Remove symbols and numeric beginnings from EOC subjects

#  Remove '&', ':' and extraneous descriptors
grade.tmp.split <- strsplit(Gwinnett_Data_LONG$CONTENT_AREA, "_&_")
Gwinnett_Data_LONG[, CONTENT_AREA := sapply(grade.tmp.split, function(x) x[1])]
Gwinnett_Data_LONG[CONTENT_AREA == "MATHEMATICS_INDUSTRY", CONTENT_AREA := "MATHEMATICS_OF_IND_AND_GOV"]
Gwinnett_Data_LONG[CONTENT_AREA == "AP_PHYSICS_C:_ELECTRICITY&MAGNETISM", CONTENT_AREA := "AP_PHYSICS_ELEC_AND_MAG"]
Gwinnett_Data_LONG[CONTENT_AREA == "AP_PHYSICS_C:_MECHANICS", CONTENT_AREA := "AP_PHYSICS_MECHANICS"]
Gwinnett_Data_LONG[CONTENT_AREA == "BIO_ENGINEERING/SCI_RESEARCH_III", CONTENT_AREA := "BIO_ENGINEERING"]

Gwinnett_Data_LONG[CONTENT_AREA == "9TH_GRADE_LIT", CONTENT_AREA := "GRADE_9_LIT"]
Gwinnett_Data_LONG[CONTENT_AREA == "10TH_GRADE_LIT", CONTENT_AREA := "GRADE_10_LIT"]
Gwinnett_Data_LONG[CONTENT_AREA == "11TH_GRADE_AMERICAN_LIT", CONTENT_AREA := "AMERICAN_LIT"]
Gwinnett_Data_LONG[CONTENT_AREA == "12TH_GRADE_BRITISH_LIT", CONTENT_AREA := "BRITISH_LIT"]
Gwinnett_Data_LONG[CONTENT_AREA == "AP_SPANISH_LITERATURE", CONTENT_AREA := "AP_SPANISH_LIT"] #  Keep LIT consistent

table(Gwinnett_Data_LONG[!is.na(GRADE), GRADE, CONTENT_AREA]) # Quick Verification of this step

#####
###   Make YEAR consistent for all CONTENT_AREA values
#####

###   Use the Fall/Spring convention for all values
###   Mainly EOG subjects, but also some EOC
###   This is something GCPS can probably include in their data prep

###   Associated CONTENT_AREA values we'll need to change
table(Gwinnett_Data_LONG[!grepl("[.]", YEAR), YEAR, CONTENT_AREA])

###   Treat all non-labeled YEAR values as '.2' (Spring) assessments
Gwinnett_Data_LONG[!grepl("[.]", YEAR), YEAR := paste0(YEAR, ".2")]

###   The .F/.S added to years did not work in the studentGrowthPercentiles Function
###   Change the .F/.S convention to .1/.2
Gwinnett_Data_LONG[, YEAR := gsub(".F", ".1", YEAR)]
Gwinnett_Data_LONG[, YEAR := gsub(".S", ".2", YEAR)]

table(Gwinnett_Data_LONG[, YEAR, CONTENT_AREA])


#####
###   Create VALID_CASE variable
#####

Gwinnett_Data_LONG[, VALID_CASE := "VALID_CASE"]

###   Remove duplicates.  Why are there SO many?
###   Use Georgia's default business rule of taking the highest score

setkey(Gwinnett_Data_LONG, VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID, SCALE_SCORE)
setkey(Gwinnett_Data_LONG, VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID)
sum(duplicated(Gwinnett_Data_LONG[VALID_CASE != "INVALID_CASE"], by=key(Gwinnett_Data_LONG))) #
dups <- data.table(Gwinnett_Data_LONG[unique(c(which(duplicated(Gwinnett_Data_LONG, by=key(Gwinnett_Data_LONG)))-1, which(duplicated(Gwinnett_Data_LONG, by=key(Gwinnett_Data_LONG))))), ], key=key(Gwinnett_Data_LONG))
Gwinnett_Data_LONG[which(duplicated(Gwinnett_Data_LONG, by=key(Gwinnett_Data_LONG)))-1, VALID_CASE := "INVALID_CASE"]

table(Gwinnett_Data_LONG[, VALID_CASE, YEAR])
round(prop.table(table(Gwinnett_Data_LONG[, VALID_CASE, YEAR]), 1)*100, 1) # Annual percentage

###   Save prepared long data
save(Gwinnett_Data_LONG, file = "./Data/Simulated_Data/Gwinnett_Data_LONG.Rdata")
