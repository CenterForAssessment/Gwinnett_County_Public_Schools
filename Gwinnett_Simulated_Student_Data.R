################################################################################
###                                                                          ###
###             GCPS Re-simulated Student Data with Correlations             ###
###                                                                          ###
################################################################################

### Load required packages
require(SGP)
require(data.table)


###  Load prelim SGP long data (use this to create new data for only students that will get SGPs - could do with more if configs set up)
load("./Data/Simulated_Data/Gwinnett_SGP_LONG_Data.Rdata")

#  Subset only the data that will be used for preliminary analyses
sim.ids <- Gwinnett_SGP_LONG_Data[!is.na(SGP), ID]
sim.data.long1 <- Gwinnett_SGP_LONG_Data[ID %in% sim.ids & CONTENT_AREA %in% c("LANGUAGE_ARTS", "MATHEMATICS", "ALGEBRA_I"), list(VALID_CASE, ID, YEAR, CONTENT_AREA, GRADE, SCALE_SCORE)]
sim.data.long2 <- Gwinnett_SGP_LONG_Data[ID %in% sim.ids & CONTENT_AREA == "ACC_MATHEMATICS" & YEAR == "2017_2018.2", list(VALID_CASE, ID, YEAR, CONTENT_AREA, GRADE, SCALE_SCORE)]
sim.data.long <- rbindlist(list(sim.data.long1, sim.data.long2))
setkeyv(sim.data.long, SGP:::getKey(sim.data.long))

#  Create blank new data.table to populate with re-simulated data
Gwinnett_Data_LONG <- data.table()


###  Read in Data simulation configuration scripts (from Github Repo)

source("SGP_CONFIG/Data_Simulation/MATHEMATICS_SIM.R")
source("SGP_CONFIG/Data_Simulation/LANGUAGE_ARTS_SIM.R")

###   Combine Basic Configuration Scripts
GCPS.config <- c(
  LANGUAGE_ARTS_2018_2019.2.config,
  MATHEMATICS_2018_2019.2.config)


###   Re-simulate Math and LA with single priors

set.seed(2072)

for (cfg in seq(GCPS.config)) {
  sgp.iter <- GCPS.config[[cfg]]
  for (grade.seq in seq(sgp.iter[["sgp.grade.sequences"]])){
    tmp.sgp.iter <- sgp.iter
    tmp.sgp.iter[["sgp.grade.sequences"]] <- sgp.iter[["sgp.grade.sequences"]][[grade.seq]]

    #  Subset wide data from long for each course progression used with util function from SGP package
    sim.data.wide <- SGP:::getPanelData(sgp.data=sim.data.long, sgp.type="sgp.percentiles", sgp.iter = tmp.sgp.iter)
    c.num <- ncol(sim.data.wide)

    #  Turn scores into something resembling a normal distribution
    py <- qnorm(unlist(sim.data.wide[, ..c.num], use.names=FALSE)/100) # Current Year Score
    py[which(py == Inf)] <- 15
    py[which(py == -Inf)] <- -15
    px <- qnorm(unlist(sim.data.wide[, ..c.num-1], use.names=FALSE)/100) # Prior Year Score
    px[which(px == Inf)] <- 15
    px[which(px == -Inf)] <- -15

    #   Add in random deviate (cc) with similar distribution to create correlation
    N <- nrow(sim.data.wide)
    r1 <- 0.75
    cc <- rnorm(N, 0, sd(py, na.rm=TRUE))
    cc1 <- cc*sqrt((r1/(1-r1)))
    pyy <- (py + cc1)/sqrt(1+(r1/(1-r1)))
    pxx <- (px + cc1)/sqrt(1+(r1/(1-r1)))
    # cor(pxx, pyy, use='complete.obs')

    #   Transform back into probabilities/percentages
    sim.data.wide[, (c.num) := round(pnorm(pyy)*100, 3)]
    sim.data.wide[, (c.num-1) := round(pnorm(pxx)*100, 3)]

    #   Reshape into long format, clean up and stack into new long data object
    tmp.data.long <- melt(sim.data.wide, id.vars="ID", measure=patterns("^GRADE", "^SCALE_SCORE"),
                          value.name=c("GRADE", "SCALE_SCORE"), variable.name="CONTENT_AREA",
                          variable.factor = FALSE, na.rm=TRUE)
    tmp.data.long[, YEAR := "2018_2019.2"]
    tmp.data.long[CONTENT_AREA == 1L, YEAR := tmp.sgp.iter[["sgp.panel.years"]][1]]
    tmp.data.long[, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][1]]
    # tmp.data.long[CONTENT_AREA == 1L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][1]]
    # tmp.data.long[CONTENT_AREA == 2L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][2]]

    Gwinnett_Data_LONG <- rbindlist(list(Gwinnett_Data_LONG, tmp.data.long))
  }
}

table(Gwinnett_Data_LONG[, CONTENT_AREA, GRADE])

#####
###   Run 'special' case configs for current year scores that might have multiple possible priors
#####

###  Combine Multiple Prior Configuration Scripts
GCPS.mult.config <- c(
  LANGUAGE_ARTS_MULTIPLE.config,
  MATHEMATICS_MULTIPLE.config,
  ALGEBRA_I_MULTIPLE.config)


for (cfg in seq(GCPS.mult.config)) {
  tmp.sgp.iter <- GCPS.mult.config[[cfg]]
  alg1 <- ifelse(any(grepl("ALGEBRA_I", tmp.sgp.iter[["sgp.content.areas"]])), TRUE, FALSE)
  sim.data.wide <- SGP:::getPanelData(sgp.data=sim.data.long, sgp.type="sgp.percentiles", sgp.iter = tmp.sgp.iter)

  c.num <- ncol(sim.data.wide)

  py <- qnorm(unlist(sim.data.wide[, ..c.num], use.names=FALSE)/100)
  py[which(py == -Inf)] <- -15
  px <- qnorm(unlist(sim.data.wide[, ..c.num-1], use.names=FALSE)/100)
  px[which(px == -Inf)] <- -15
  px2 <- qnorm(unlist(sim.data.wide[, ..c.num-2], use.names=FALSE)/100)
  px2[which(px2 == -Inf)] <- -15
  if (alg1) {
    px3 <- qnorm(unlist(sim.data.wide[, ..c.num-3], use.names=FALSE)/100)
    px3[which(px3 == -Inf)] <- -15
  }

  N <- nrow(sim.data.wide)
  r1 <- 0.75
  cc <- rnorm(N, 0, sd(py, na.rm=TRUE))
  cc1 <- cc*sqrt((r1/(1-r1)))
  pyy <- (py + cc1)/sqrt(1+(r1/(1-r1)))
  pxx <- (px + cc1)/sqrt(1+(r1/(1-r1)))
  px2 <- (px2 + cc1)/sqrt(1+(r1/(1-r1)))
  if (alg1) {
    px3 <- (px3 + cc1)/sqrt(1+(r1/(1-r1)))
  }
  # cor(pxx, pyy, use='complete.obs')

  sim.data.wide[, (c.num) := round(pnorm(pyy)*100, 3)]
  sim.data.wide[, (c.num-1) := round(pnorm(pxx)*100, 3)]
  sim.data.wide[, (c.num-2) := round(pnorm(px2)*100, 3)]
  if (alg1) {
    sim.data.wide[, (c.num-3) := round(pnorm(px3)*100, 3)]
  }

  tmp.data.long <- melt(sim.data.wide, id.vars="ID", measure=patterns("^GRADE", "^SCALE_SCORE"),
                        value.name=c("GRADE", "SCALE_SCORE"), variable.name="CONTENT_AREA",
                        variable.factor = FALSE, na.rm=TRUE)
  tmp.data.long[, YEAR := "2018_2019.2"]
  tmp.data.long[CONTENT_AREA == 1L, YEAR := tmp.sgp.iter[["sgp.panel.years"]][1]]
  tmp.data.long[CONTENT_AREA == 2L, YEAR := tmp.sgp.iter[["sgp.panel.years"]][2]]
  if (alg1) {
    tmp.data.long[CONTENT_AREA == 3L, YEAR := tmp.sgp.iter[["sgp.panel.years"]][3]]
  }
  tmp.data.long[CONTENT_AREA == 1L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][1]]
  tmp.data.long[CONTENT_AREA == 2L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][2]]
  tmp.data.long[CONTENT_AREA == 3L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][3]]
  if (alg1) {
    tmp.data.long[CONTENT_AREA == 4L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][4]]
  }
  Gwinnett_Data_LONG <- rbindlist(list(Gwinnett_Data_LONG, tmp.data.long))
}

table(Gwinnett_Data_LONG[, CONTENT_AREA, GRADE])
table(Gwinnett_Data_LONG[, CONTENT_AREA, YEAR])


#####
###   Create SCALE_SCORE_CSEM variable (based on simulated CTT reliability)
#####

###   Calculate standard deviation of the observed scores (across years/semesters)
Gwinnett_Data_LONG[, SD_O := sd(SCALE_SCORE), by = c("CONTENT_AREA", "GRADE", "YEAR")]
Gwinnett_Data_LONG[, as.list(summary(SD_O)), keyby = c("CONTENT_AREA", "GRADE", "YEAR")] # should be the same for each row if done right

###   Simulate reliability values for each test - lower for fall semesters
set.seed(2072)
Gwinnett_Data_LONG[, RELIABILITY := as.numeric(NA)]
Gwinnett_Data_LONG[YEAR %in% c("2017_2018.1", "2018_2019.1"), RELIABILITY := sample(seq(0.65, 0.85, 0.01), 1, replace=TRUE), by = c("CONTENT_AREA", "GRADE", "YEAR")]
Gwinnett_Data_LONG[YEAR %in% c("2017_2018.2", "2018_2019.2"), RELIABILITY := sample(seq(0.75, 0.90, 0.01), 1, replace=TRUE), by = c("CONTENT_AREA", "GRADE", "YEAR")]
Gwinnett_Data_LONG[, as.list(summary(RELIABILITY)), keyby = c("CONTENT_AREA", "GRADE", "YEAR")] # should be the same for each row if done right

###   Calculate SEM values for each test (constant across all scores)
Gwinnett_Data_LONG[, SCALE_SCORE_CSEM := round(SD_O * (1 - RELIABILITY), 3)]
Gwinnett_Data_LONG[, as.list(summary(SCALE_SCORE_CSEM)), keyby = c("CONTENT_AREA", "GRADE", "YEAR")] # should be the same for each row if done right

###   Remove extraneous variables
Gwinnett_Data_LONG[, SD_O := NULL]
Gwinnett_Data_LONG[, RELIABILITY := NULL]


#####
###   Create ACHIEVEMENT_LEVEL variable
#####

###   Use boilerplate Achievement Level cutpoints.  Not used but needed for summarizeSGP

Gwinnett_Data_LONG[, VALID_CASE := "VALID_CASE"]

Gwinnett_Data_LONG <- SGP:::getAchievementLevel(Gwinnett_Data_LONG, state="GCPS")
Gwinnett_Data_LONG[, ACHIEVEMENT_LEVEL := factor(ACHIEVEMENT_LEVEL, levels = c("Low", "Typical", "High"), ordered=TRUE)]

table(Gwinnett_Data_LONG[!is.na(ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL, CONTENT_AREA], exclude=NULL)
Gwinnett_Data_LONG[!is.na(ACHIEVEMENT_LEVEL), as.list(summary(SCALE_SCORE)), keyby=c("CONTENT_AREA", "GRADE", "ACHIEVEMENT_LEVEL")]


#####
###   Merge in SCHOOL_NUMBER variable (simulated in Gwinnett_Simulated_School_Instructor_Number.R)
#####

load("Data/Simulated_Data/Gwinnett_Data_SCHOOL_NUMBER.Rdata")
Gwinnett_Data_LONG[, ID := as.character(ID)]
setkey(Gwinnett_Data_SCHOOL_NUMBER, ID, GRADE)
setkey(Gwinnett_Data_LONG, ID, GRADE)

Gwinnett_Data_LONG <- merge(Gwinnett_Data_LONG, Gwinnett_Data_SCHOOL_NUMBER, all.x = TRUE)

table(Gwinnett_Data_LONG[, is.na(SCHOOL_NUMBER), YEAR])
table(Gwinnett_Data_LONG[, is.na(SCHOOL_NUMBER), CONTENT_AREA])


#####
###   Save re-simulated data object
#####

save(Gwinnett_Data_LONG, file = "./Data/Simulated_Data/Gwinnett_Data_LONG-Resimulated.Rdata")


######
######   Simple example of how this works in theory
######

#####
###   Simulating correlated "true-scores" - Correction for Attenuation... Zimmerman, 2007 EPM
#####

N <- 7500
true.x <- rnorm(N, 0, 1)
true.y <- rnorm(N, 0, 1)

#   Creat correlation by adding in multiple of same random deviate
#   True score correlation = 0.95 in this example
cc <- rnorm(N, 0, 1)*sqrt((0.95/(1-0.95))) #rnorm(...) - has to be same deviate use for true score (ability) above.
true.x <- (true.x + cc)/sqrt(1+(0.95/(1-0.95)))
true.y <- (true.y + cc)/sqrt(1+(0.95/(1-0.95)))

cor(true.x, true.y)
mean(true.y)
sd(true.y)
mean(true.x)
sd(true.x)

### Error
error.x <- rnorm(N, 0, 0.5)
error.y <- rnorm(N, 0, 0.5)

# Classical - adding error to the true score (error is independent of true):
obs.x <- true.x + error.x
obs.y <- true.y + error.y

cor(obs.x, obs.y)
mean(obs.y)
sd(obs.y)
mean(obs.x)
sd(obs.x)

plot(obs.x, obs.y, col='grey', ylab = 'Current Year Score', xlab='Prior Year Score',
	main = 'Conditional True and Observed Score Distributions')
points(true.x, true.y, pch=3, col='green', cex=.45)

######
######
######
