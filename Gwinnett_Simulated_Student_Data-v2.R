################################################################################
###                                                                          ###
###             GCPS Re-simulated Student Data with Correlations             ###
###                                                                          ###
################################################################################

### Load required packages
require(SGP)
require(data.table)


###  Load prelim long data
load("./Data/Simulated_Data/Gwinnett_Data_LONG.Rdata")

#  Subset only the data that will be used for preliminary analyses
sim.data.long <- Gwinnett_Data_LONG[, list(VALID_CASE, ID, YEAR, CONTENT_AREA, GRADE, SCALE_SCORE)]
setkeyv(sim.data.long, SGP:::getKey(sim.data.long))


######
######   Simulation Configs
######

my.year <- "2018_2019.2"
config_list <- list()
cohort.min.n <- 1200

subject <- sort(unique(Gwinnett_Data_LONG[, SUBJECT]))

for (j in subject) {
  subj.prog <- courseProgressionSGP(Gwinnett_Data_LONG[SUBJECT %in% j], lag.direction="BACKWARD", year=my.year)

  for (k in names(subj.prog$BACKWARD[[my.year]])) {
		tmp.ca <- strsplit(k, "[.]")[[1]][1]
		tmp.grd<- sub("^0+", "", strsplit(k, "[.]")[[1]][2])

		fall.prog <- setorder(subj.prog$BACKWARD[[my.year]][[k]][, list(N=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1) & N > cohort.min.n], -N)
		sprg.prog <- setorder(subj.prog$BACKWARD[[my.year]][[k]][, list(N=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2) & N > cohort.min.n], -N)

		tmp.config <- list(
			sgp.content.areas=tmp.ca,
			sgp.panel.years=my.year,
			sgp.grade.sequences=tmp.grd)

		if (nrow(fall.prog) > 0) {
			for (l in seq(nrow(fall.prog))) {
				tmp.ca.grd <- strsplit(fall.prog[l, CONTENT_AREA_by_GRADE_PRIOR_YEAR.1], "[.]")[[1]]
				tmp.config[["sgp.content.areas"]] <- c(tmp.ca.grd[1], tmp.config[["sgp.content.areas"]])
				tmp.config[["sgp.panel.years"]]  <-  c(SGP:::yearIncrement(my.year, -0.1), tmp.config[["sgp.panel.years"]])
				tmp.config[["sgp.grade.sequences"]]<-c(sub("^0+", "", tmp.ca.grd[2]), tmp.config[["sgp.grade.sequences"]])
			}
		}

		if (nrow(sprg.prog) > 0) {
			for (l in seq(nrow(sprg.prog))) {
				tmp.ca.grd <- strsplit(sprg.prog[l, CONTENT_AREA_by_GRADE_PRIOR_YEAR.2], "[.]")[[1]]
				tmp.config[["sgp.content.areas"]] <- c(tmp.ca.grd[1], tmp.config[["sgp.content.areas"]])
				tmp.config[["sgp.panel.years"]]  <-  c(SGP:::yearIncrement(my.year, -1), tmp.config[["sgp.panel.years"]])
				tmp.config[["sgp.grade.sequences"]]<-c(sub("^0+", "", tmp.ca.grd[2]), tmp.config[["sgp.grade.sequences"]])
			}
		}
		if (length(tmp.config[["sgp.content.areas"]]) > 1) config_list <- c(config_list, list(tmp.config))
	}
}

sapply(1:length(config_list), function(f) length(config_list[[f]][["sgp.panel.years"]]))

GCPS.sim.data.config <- config_list

save(GCPS.sim.data.config, file = "./Data/Simulated_Data/GCPS.sim.data.config.rda")

config_list[[21]]

#  Create blank new data.table to populate with re-simulated data
Gwinnett_Data_LONG <- data.table()

set.seed(2072)

for (cfg in seq(GCPS.sim.data.config)) {
  tmp.sgp.iter <- GCPS.sim.data.config[[cfg]]
  num.vars <- length(tmp.sgp.iter[["sgp.content.areas"]])
  sim.data.wide <- SGP:::getPanelData(sgp.data=sim.data.long, sgp.type="sgp.percentiles", sgp.iter = tmp.sgp.iter)

  c.num <- ncol(sim.data.wide)

  py <- qnorm(unlist(sim.data.wide[, ..c.num], use.names=FALSE)/100)
  py[which(py == -Inf)] <- -15
  px <- qnorm(unlist(sim.data.wide[, ..c.num-1], use.names=FALSE)/100)
  px[which(px == -Inf)] <- -15
  if (num.vars > 2) {
    px2 <- qnorm(unlist(sim.data.wide[, ..c.num-2], use.names=FALSE)/100)
    px2[which(px2 == -Inf)] <- -15
  }
  if (num.vars > 3) {
    px3 <- qnorm(unlist(sim.data.wide[, ..c.num-3], use.names=FALSE)/100)
    px3[which(px3 == -Inf)] <- -15
  }

  N <- nrow(sim.data.wide)
  r1 <- 0.75
  cc <- rnorm(N, 0, sd(py, na.rm=TRUE))
  cc1 <- cc*sqrt((r1/(1-r1)))
  pyy <- (py + cc1)/sqrt(1+(r1/(1-r1)))
  pxx <- (px + cc1)/sqrt(1+(r1/(1-r1)))
  if (num.vars > 2) {
    px2 <- (px2 + cc1)/sqrt(1+(r1/(1-r1)))
  }
  if (num.vars > 3) {
    px3 <- (px3 + cc1)/sqrt(1+(r1/(1-r1)))
  }
  # cor(pxx, pyy, use='complete.obs')

  sim.data.wide[, (c.num) := round(pnorm(pyy)*100, 3)]
  sim.data.wide[, (c.num-1) := round(pnorm(pxx)*100, 3)]
  if (num.vars > 2) {
    sim.data.wide[, (c.num-2) := round(pnorm(px2)*100, 3)]
  }
  if (num.vars > 3) {
    sim.data.wide[, (c.num-3) := round(pnorm(px3)*100, 3)]
  }

  tmp.data.long <- melt(sim.data.wide, id.vars="ID", measure=patterns("^GRADE", "^SCALE_SCORE"),
                        value.name=c("GRADE", "SCALE_SCORE"), variable.name="CONTENT_AREA",
                        variable.factor = FALSE, na.rm=TRUE)

  tmp.data.long[CONTENT_AREA == 1L, YEAR := tmp.sgp.iter[["sgp.panel.years"]][1]]
  tmp.data.long[CONTENT_AREA == 2L, YEAR := tmp.sgp.iter[["sgp.panel.years"]][2]]
  if (num.vars > 2) {
    tmp.data.long[CONTENT_AREA == 3L, YEAR := tmp.sgp.iter[["sgp.panel.years"]][3]]
  }
  if (num.vars > 3) {
    tmp.data.long[CONTENT_AREA == 4L, YEAR := tmp.sgp.iter[["sgp.panel.years"]][4]]
  }
  tmp.data.long[CONTENT_AREA == 1L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][1]]
  tmp.data.long[CONTENT_AREA == 2L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][2]]
  if (num.vars > 2) {
    tmp.data.long[CONTENT_AREA == 3L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][3]]
  }
  if (num.vars > 3) {
    tmp.data.long[CONTENT_AREA == 4L, CONTENT_AREA := tmp.sgp.iter[["sgp.content.areas"]][4]]
  }
  Gwinnett_Data_LONG <- rbindlist(list(Gwinnett_Data_LONG, tmp.data.long))
}

table(Gwinnett_Data_LONG[, GRADE, CONTENT_AREA])
table(Gwinnett_Data_LONG[, YEAR, CONTENT_AREA])

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

###   Use boilerplate Achievement Level cutpoints.  Not used but needed for summarizeSGP

grd.subj <- unique(Gwinnett_Data_LONG[, list(CONTENT_AREA, GRADE)])
setkey(grd.subj)
gcps.cutscores <- list()

for (ca in unique(grd.subj$CONTENT_AREA)) {
	tmp.dt <- grd.subj[CONTENT_AREA == ca]
	for (j in paste0("GRADE_", tmp.dt$GRADE)) gcps.cutscores[[ca]][[j]] <- c(40, 60)
}

SGPstateData[["GCPS"]][["Achievement"]][["Cutscores"]] <- gcps.cutscores

Gwinnett_Data_LONG <- SGP:::getAchievementLevel(Gwinnett_Data_LONG, state="GCPS")
Gwinnett_Data_LONG[, ACHIEVEMENT_LEVEL := factor(ACHIEVEMENT_LEVEL, levels = c("Low", "Typical", "High"), ordered=TRUE)]

table(Gwinnett_Data_LONG[!is.na(ACHIEVEMENT_LEVEL), ACHIEVEMENT_LEVEL, CONTENT_AREA], exclude=NULL)
Gwinnett_Data_LONG[!is.na(ACHIEVEMENT_LEVEL), as.list(summary(SCALE_SCORE)), keyby=c("CONTENT_AREA", "GRADE", "ACHIEVEMENT_LEVEL")]

Gwinnett_Data_LONG[, ACHIEVEMENT_LEVEL := as.character(ACHIEVEMENT_LEVEL)]


###   Remove Duplicated Cases

setkey(Gwinnett_Data_LONG, VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID, SCALE_SCORE)
setkey(Gwinnett_Data_LONG, VALID_CASE, CONTENT_AREA, GRADE, YEAR, ID)
sum(duplicated(Gwinnett_Data_LONG[VALID_CASE != "INVALID_CASE"], by=key(Gwinnett_Data_LONG))) #
Gwinnett_Data_LONG[which(duplicated(Gwinnett_Data_LONG, by=key(Gwinnett_Data_LONG)))-1, VALID_CASE := "INVALID_CASE"]

Gwinnett_Data_LONG <- Gwinnett_Data_LONG[VALID_CASE == "VALID_CASE"]

#####
###   Save re-simulated data object
#####

save(Gwinnett_Data_LONG, file = "./Data/Simulated_Data/Gwinnett_Data_LONG-Resimulated2.Rdata")
