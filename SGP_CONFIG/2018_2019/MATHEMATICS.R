################################################################################
###                                                                          ###
###      SGP Configurations code for 2019 Elem/Middle School MATH/Alg 1      ###
###                                                                          ###
################################################################################

MATHEMATICS_2018_2019.2.config <- list(
	###   'Traditional' Spring to Spring analyses
	MATHEMATICS.2019_2020.1 = list(
  sgp.content.areas=c("MATHEMATICS", "MATHEMATICS"),
		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
		sgp.grade.sequences=list(c("1", "2"), c("2", "3"), c("3", "4"), c("4", "5"), c("5", "6")),
		sgp.norm.group.preference=1),

	###   Fall to Spring (same grade) analyses
	MATHEMATICS.2018_2019.2 = list(
		sgp.content.areas=c("MATHEMATICS", "MATHEMATICS"),
		sgp.panel.years=c("2018_2019.1", "2018_2019.2"),
		sgp.grade.sequences=list(c("6", "6"), c("7", "7"), c("8", "8")),
		sgp.norm.group.preference=2)
)

ALGEBRA_I_2018_2019.2.config <- list(
	###   Fall to Spring analyses
	ALGEBRA_I.2018_2019.2 = list(
		sgp.content.areas=c("ALGEBRA_I", "ALGEBRA_I"),
		sgp.panel.years=c("2018_2019.1", "2018_2019.2"),
		sgp.grade.sequences=list(c("EOCT", "EOCT")),
		sgp.norm.group.preference=1),

	ALGEBRA_I.2018_2019.2 = list(
		sgp.content.areas=c("ACC_MATHEMATICS", "ALGEBRA_I"),
		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
		sgp.grade.sequences=list(c("7", "EOCT")),
		sgp.norm.group.preference=2),

	ALGEBRA_I.2018_2019.2 = list(
		sgp.content.areas=c("MATHEMATICS", "ALGEBRA_I"),
		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
		sgp.grade.sequences=list(c("7", "EOCT")),
		sgp.norm.group.preference=3)
)
