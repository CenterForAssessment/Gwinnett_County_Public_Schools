#################################################################
###                                                           ###
###                GCPS Simulated SGPs - 2019                 ###
###                                                           ###
#################################################################

### Load required packages
require(SGP)
require(data.table)


###  Load cleaned long data (from Gwinnett_Simulated_Data_LONG.R)
load("Data/Simulated_Data/Gwinnett_Data_LONG.Rdata") # load("../4_output/datasets/Gwinnett_Data_LONG.Rdata")
load("Data/Simulated_Data/Gwinnett_Data_INSTRUCTOR_NUMBER_Weighted.Rdata")


###  Read in 2019 SGP Configuration Scripts (from Github Repo)

source("SGP_CONFIG/2018_2019/MATHEMATICS.R")
source("SGP_CONFIG/2018_2019/LANGUAGE_ARTS.R")

###  Combine 2019 SGP Configuration Scripts
GCPS.config <- c(
  LANGUAGE_ARTS_2018_2019.2.config,
  MATHEMATICS_2018_2019.2.config,
  ALGEBRA_I_2018_2019.2.config)


#####
###   Modify SGPstateData for use with higher level functions (analyzeSGP)
#####

###   Still need to add in the "GCPS" knots and boundaries
###   All other basic meta-data has been added to SGPstateData (SGP version 1.9-5.1)

###   Create knots and boundaries from data (actual or simulated)
gcps_knots_boundaries <- createKnotsBoundaries(Gwinnett_Data_LONG)
SGPstateData[["GCPS"]][["Achievement"]][["Knots_Boundaries"]] <- gcps_knots_boundaries


#####
###    Elementary/Middle School Analyses
#####

###   Step 1.  prepareSGP (Create a SGP object with Long Data)

Gwinnett_SGP <- prepareSGP(
  Gwinnett_Data_LONG,
  data_supplementary=list(INSTRUCTOR_NUMBER=Gwinnett_Data_INSTRUCTOR_NUMBER),
  create.additional.variables=FALSE)

#  The LONG data is located in the @Data slot.
names(Gwinnett_Data_LONG)
names(Gwinnett_SGP@Data)

#  The teacher - student link table is located in the @Data_Supplementary slot.
names(Gwinnett_SGP@Data_Supplementary[["INSTRUCTOR_NUMBER"]])


###   Step 2.  analyzeSGP (produce SGPs for progressions specified in config scripts)

 # Added SIMEX calculations - assumes SCALE_SCORE_CSEM Has been calculated from RELIABILITY

Gwinnett_SGP <- analyzeSGP(
  Gwinnett_SGP,
  state = 'GCPS',
  sgp.config = GCPS.config,
  sgp.percentiles = TRUE,
  sgp.projections = FALSE,
  sgp.projections.lagged = FALSE,
  sgp.percentiles.baseline = FALSE,
  sgp.projections.baseline = FALSE,
  sgp.projections.lagged.baseline = FALSE,
  simulate.sgps=FALSE,
  calculate.simex = TRUE) # Added SIMEX argument

###   Step 3.  combineSGP (merge raw results into the long data (@Data slot))

Gwinnett_SGP <- combineSGP(Gwinnett_SGP,
                           state = 'GCPS')


###   Step 4.   outputSGP (Write long data with results to working directory)

outputSGP(Gwinnett_SGP,
          outputSGP.directory="Data/Simulated_Data", # Change to desired output location
          output.type = c("LONG_Data", "LONG_FINAL_YEAR_Data")) # WIDE still not working with GRADE duplicates

###   Quick summary of results

###  Simulated data does not have a correlation induced between prior/current!!!
Gwinnett_SGP@Data[!is.na(SGP) & VALID_CASE=='VALID_CASE'][, list(
  Test_Scores = round(cor(SCALE_SCORE, SCALE_SCORE_PRIOR_STANDARDIZED, use='pairwise.complete'), 2),
  SGP = format(round(cor(SGP, SCALE_SCORE_PRIOR_STANDARDIZED, use='pairwise.complete'), 2), nsmall = 2),
  SGP_SIMEX_RANKED = format(round(cor(SGP_SIMEX_RANKED, SCALE_SCORE_PRIOR_STANDARDIZED, use='pairwise.complete'), 2), nsmall = 2),
  N_Size = sum(!is.na(SGP))), keyby = list(CONTENT_AREA, GRADE)]


###   Step 5.  summarizeSGP (merge raw results into the long data (@Data slot))

Gwinnett_SGP <- summarizeSGP(Gwinnett_SGP)

#  Summary tables are located in the @Summary slot.
names(Gwinnett_SGP@Summary)
names(Gwinnett_SGP@Summary[["SCHOOL_NUMBER"]])

#  Access the Teacher summary table disaggregated by YEAR, CONTENT_AREA and GRADE
Gwinnett_SGP@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__INSTRUCTOR_NUMBER__CONTENT_AREA__YEAR__GRADE"]]

###   Save Gwinnett SGP Object
save(Gwinnett_SGP, file="Data/Simulated_Data/Gwinnett_SGP.Rdata")
