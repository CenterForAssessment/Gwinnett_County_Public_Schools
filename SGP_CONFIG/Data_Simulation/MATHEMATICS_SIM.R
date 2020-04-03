################################################################################
###                                                                          ###
###     SGP Configurations for GCPS Math Data Simulation with Correlation    ###
###                                                                          ###
################################################################################

MATHEMATICS_2018_2019.2.config <- list(
	###   'Traditional' Spring to Spring analyses
	MATHEMATICS.2019_2020.1 = list(
  sgp.content.areas=c("MATHEMATICS", "MATHEMATICS"),
		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
		sgp.grade.sequences=list(c("1", "2"), c("2", "3"), c("3", "4"), c("4", "5"))),

	###   Fall to Spring (same grade) analyses
	MATHEMATICS.2018_2019.2 = list(
		sgp.content.areas=c("MATHEMATICS", "MATHEMATICS"),
		sgp.panel.years=c("2018_2019.1", "2018_2019.2"),
		sgp.grade.sequences=list(c("7", "7")))
)


###   Special configs for progressions with multiple pathways with various/multiple priors
###   Note that sgp.grade.sequences element is not nested in a list().

MATHEMATICS_MULTIPLE.config <- list(
	###   6th grade special case
	MATHEMATICS.2019_2020.1 = list(
	  sgp.content.areas=c("MATHEMATICS", "MATHEMATICS", "MATHEMATICS"),
			sgp.panel.years=c("2017_2018.2", "2018_2019.1", "2018_2019.2"),
			sgp.grade.sequences=c("5", "6", "6"))
)

ALGEBRA_I_MULTIPLE.config <- list(
	###   All Algebra I special case
	ALGEBRA_I.2018_2019.2 = list(
		sgp.content.areas=c("MATHEMATICS", "ACC_MATHEMATICS", "ALGEBRA_I", "ALGEBRA_I"),
		sgp.panel.years=c("2017_2018.2", "2017_2018.2", "2018_2019.1", "2018_2019.2"),
		sgp.grade.sequences=c("7", "7", "EOCT", "EOCT"))
)
