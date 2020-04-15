#################################################################
###                                                           ###
###                GCPS Simulated SGPs - 2019                 ###
###                                                           ###
#################################################################

### Load required packages
require(SGP)
require(data.table)
#library(tidyverse)


###  Load cleaned long data (from Gwinnett_Simulated_Data_LONG.R)
#load("Data/Simulated_Data/Gwinnett_Data_LONG.Rdata")
load("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/exploration/Student Growth Exploration/4_output/datasets/Gwinnett_Data_LONG.Rdata")

###  Read in 2019 SGP Configuration Scripts (from Github Repo)

#source("SGP_CONFIG/2018_2019/MATHEMATICS.R")
#source("SGP_CONFIG/2018_2019/LANGUAGE_ARTS.R")
source("SGP_CONFIG/2018_2019/MASTER_CONFIG.R")

###  Combine 2019 SGP Configuration Scripts
# GCPS.config <- c(
#   LANGUAGE_ARTS_2018_2019.2.config,
#   MATHEMATICS_2018_2019.2.config,
#   ALGEBRA_I_2018_2019.2.config)
GCPS.config <- c(
  config_list
)


#####
###   Modify SGPstateData for use with higher level functions (analyzeSGP)
#####

###   Still need to add in the "GCPS" knots and boundaries
###   All other basic meta-data has been added to SGPstateData (SGP version 1.9-5.1)

###   Create knots and boundaries from simulated data
# gcps_sim_kbs <- createKnotsBoundaries(Gwinnett_Data_LONG)
# #save(gcps_sim_kbs, file = "./Data/Simulated_Data/gcps_sim_kbs.rda") # Save if desired
# save(gcps_sim_kbs, file = "./exploration/Student Growth Exploration/4_output/datasets/gcps_kbs.rda")
# 
# SGPstateData[["GCPS"]][["Achievement"]][["Knots_Boundaries"]] <- gcps_sim_kbs
# 
# SGPstateData[["GCPS"]][["Assessment_Program_Information"]][["CSEM"]] <- "SCALE_SCORE_CSEM"
# SGPstateData[["GCPS"]][["Assessment_Program_Information"]] <- NULL
# 
# SGPstateData[["GCPS"]][["Growth"]][["Levels"]] <- c("Low", "Typical", "High")
# SGPstateData[["GCPS"]][["Growth"]][["System_Type"]] <- "Cohort Referenced"
# SGPstateData[["GCPS"]][["Growth"]][["Cutscores"]] <-
# 	list(
# 		Cuts=c(35, 66),
# 		Labels=list("1st - 34th", "35th - 65th", "66th - 99th"))

###   Create knots and boundaries from data (actual or simulated)
gcps_knots_boundaries <- createKnotsBoundaries(Gwinnett_Data_LONG)
SGPstateData[["GCPS"]][["Achievement"]][["Knots_Boundaries"]] <- gcps_knots_boundaries


###   Step 1.  prepareSGP (Create a SGP object with Long Data)

Gwinnett_SGP <- prepareSGP(Gwinnett_Data_LONG, state = 'GCPS', create.additional.variables=FALSE)

# below doesn't work (this was new code added by Adam; think it's cost I don't have that Data Instructor Number file)
# Gwinnett_SGP <- prepareSGP(
#   Gwinnett_Data_LONG,
#   data_supplementary=list(INSTRUCTOR_NUMBER=Gwinnett_Data_INSTRUCTOR_NUMBER),
#   create.additional.variables=FALSE)

#  The LONG data is located in the @Data slot.
names(Gwinnett_Data_LONG)
names(Gwinnett_SGP@Data)

#  The teacher - student link table is located in the @Data_Supplementary slot.
names(Gwinnett_SGP@Data_Supplementary[["INSTRUCTOR_NUMBER"]]) 
##### ----> doesn't exist right now have to ask Adam

###   Step 2.  analyzeSGP (produce SGPs for progressions specified in config scripts)

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
  calculate.simex = TRUE,
  parallel.config = list(BACKEND="FOREACH", TYPE="doParallel", WORKERS=list(TAUS=2, SIMEX=2))) # Added SIMEX argument


###   Step 3.  combineSGP (merge raw results into the long data (@Data slot))

Gwinnett_SGP <- combineSGP(Gwinnett_SGP, 
                           state = 'GCPS')


###   Step 4.   outputSGP (Write long data with results to working directory)

outputSGP(Gwinnett_SGP,  state = 'GCPS', output.type = c("LONG_Data", "LONG_FINAL_YEAR_Data")) # WIDE still not working with GRADE duplicates

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
#save(Gwinnett_SGP, file="Data/Simulated_Data/Gwinnett_SGP.Rdata")
save(Gwinnett_SGP, file = "Data/Gwinnett_SGP.Rdata")
