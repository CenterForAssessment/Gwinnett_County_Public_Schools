################################################################################
###                                                                          ###
###  SGP Configurations for GCPS Lang Arts Data Simulation with Correlation  ###
###                                                                          ###
################################################################################

LANGUAGE_ARTS_2018_2019.2.config <- list(
	###   'Traditional' Spring to Spring analyses
	LANGUAGE_ARTS.2018_2019.2 = list(
		sgp.content.areas=c("LANGUAGE_ARTS", "LANGUAGE_ARTS"),
		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
		sgp.grade.sequences=list(c("1", "2"), c("2", "3"), c("3", "4"), c("4", "5"))),

	###   Fall to Spring (same grade) analyses
	LANGUAGE_ARTS.2018_2019.2 = list(
		sgp.content.areas=c("LANGUAGE_ARTS", "LANGUAGE_ARTS"),
		sgp.panel.years=c("2018_2019.1", "2018_2019.2"),
		sgp.grade.sequences=list(c("7", "7"), c("8", "8")))
)


###   Special configs for progressions with multiple pathways with various/multiple priors
###   Note that sgp.grade.sequences element is not nested in a list().

LANGUAGE_ARTS_MULTIPLE.config <- list(
	###   6th grade LA special case
	LANGUAGE_ARTS.2018_2019.2 = list(
  	sgp.content.areas=c("LANGUAGE_ARTS", "LANGUAGE_ARTS", "LANGUAGE_ARTS"),
  	sgp.panel.years=c("2017_2018.2", "2018_2019.1", "2018_2019.2"),
  	sgp.grade.sequences=c("5", "6", "6"))
)
