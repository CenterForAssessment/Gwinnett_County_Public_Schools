#################################################################
###                                                           ###
###                GCPS Simulated SGPs - 2019                 ###
###                                                           ###
#################################################################

### Load required packages
require(SGP)
require(data.table)


###  Load cleaned long data (from Gwinnett_Simulated_Data_LONG.R)
load("Data/Simulated_Data/Gwinnett_Data_LONG.Rdata")


###  Read in 2019 SGP Configuration Scripts (from Github Repo)

source("SGP_CONFIG/2018_2019/MATHEMATICS.R")
source("SGP_CONFIG/2018_2019/LANGUAGE_ARTS.R")

###  Combine 2019 SGP Configuration Scripts
GCPS.config <- c(
  LANGUAGE_ARTS_2018_2019.2.config,
  MATHEMATICS_2018_2019.2.config,
  ALGEBRA_I_2018_2019.2.config)


#####
###   Add minimal SGPstateData for use with higher level functions (analyzeSGP)
#####

###   Add in the "GCPS" elements needed
###   Eventually this will be added to SGPstateData and included in the SGP package
###   For now we will just manually change/add to the SGPstateData object

###   Create knots and boundaries from simulated data
gcps_sim_kbs <- createKnotsBoundaries(Gwinnett_Data_LONG)
# save(gcps_sim_kbs, file = "./Data/Simulated_Data/gcps_sim_kbs.rda") # Save if desired

SGPstateData[["GCPS"]][["Achievement"]][["Knots_Boundaries"]] <- gcps_sim_kbs

SGPstateData[["GCPS"]][["Assessment_Program_Information"]][["CSEM"]] <- "SCALE_SCORE_CSEM"
SGPstateData[["GCPS"]][["Assessment_Program_Information"]] <- NULL

SGPstateData[["GCPS"]][["Growth"]][["Levels"]] <- c("Low", "Typical", "High")
SGPstateData[["GCPS"]][["Growth"]][["System_Type"]] <- "Cohort Referenced"
SGPstateData[["GCPS"]][["Growth"]][["Cutscores"]] <-
	list(
		Cuts=c(35, 66),
		Labels=list("1st - 34th", "35th - 65th", "66th - 99th"))


#####
###    Elementary/Middle School Analyses
#####

###   Step 1.  prepareSGP (Create a SGP object with Long Data)

Gwinnett_SGP <- prepareSGP(Gwinnett_Data_LONG, state="GCPS", create.additional.variables=FALSE)

###   Step 2.  analyzeSGP (produce SGPs for progressions specified in config scripts)

Gwinnett_SGP <- analyzeSGP(
  Gwinnett_SGP,
  sgp.config = GCPS.config,
  sgp.percentiles = TRUE,
  sgp.projections = FALSE,
  sgp.projections.lagged = FALSE,
  sgp.percentiles.baseline = FALSE,
  sgp.projections.baseline = FALSE,
  sgp.projections.lagged.baseline = FALSE,
  simulate.sgps=FALSE)


###   Step 3.  combineSGP (merge raw results into the long data (@Data slot))

Gwinnett_SGP <- combineSGP(Gwinnett_SGP)


###   Step 4.   outputSGP (Write long data with results to working directory)

outputSGP(Gwinnett_SGP, output.type = c("LONG_Data", "LONG_FINAL_YEAR_Data")) # WIDE still not working with GRADE duplicates

###   Quick summary of results

###  Simulated data does not have a correlation induced between prior/current!!!
Gwinnett_SGP@Data[!is.na(SGP) & VALID_CASE=='VALID_CASE'][, list(
  Test_Scores = round(cor(SCALE_SCORE, SCALE_SCORE_PRIOR_STANDARDIZED, use='pairwise.complete'), 2),
  SGP = format(round(cor(SGP, SCALE_SCORE_PRIOR_STANDARDIZED, use='pairwise.complete'), 2), nsmall = 2),
  N_Size = sum(!is.na(SGP))), keyby = list(CONTENT_AREA, GRADE)]
