################################################################################
###                                                                          ###
###      SGP Configurations code for 2019 Grade Level EOG LANGUAGE_ARTS      ###
###                                                                          ###
################################################################################

LANGUAGE_ARTS_2018_2019.2.config <- list(
	###   'Traditional' Spring to Spring analyses
	LANGUAGE_ARTS.2018_2019.2 = list(
  sgp.content.areas=c("LANGUAGE_ARTS", "LANGUAGE_ARTS"),
		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
		sgp.grade.sequences=list(c("1", "2"), c("2", "3"), c("3", "4"), c("4", "5"), c("5", "6")),
		sgp.norm.group.preference=1),

	###   Fall to Spring (same grade) analyses
	LANGUAGE_ARTS.2018_2019.2 = list(
		sgp.content.areas=c("LANGUAGE_ARTS", "LANGUAGE_ARTS"),
		sgp.panel.years=c("2018_2019.1", "2018_2019.2"),
		sgp.grade.sequences=list(c("6", "6"), c("7", "7"), c("8", "8")),
		sgp.norm.group.preference=2)
)

#
# LANGUAGE_ARTS_2018_2019.S.config <- list(
# 	###   'Traditional' Spring to Spring analyses
# 	LANGUAGE_ARTS.2018_2019.S = list(
#   sgp.content.areas=c("LANGUAGE_ARTS", "LANGUAGE_ARTS"),
# 		sgp.panel.years=c("2017_2018.S", "2018_2019.S"),
# 		sgp.grade.sequences=list(c("1", "2"), c("2", "3"), c("3", "4"), c("4", "5"), c("5", "6")),
# 		sgp.norm.group.preference=1),
#
# 	###   Fall to Spring (same grade) analyses
# 	LANGUAGE_ARTS.2018_2019.S = list(
# 		sgp.content.areas=c("LANGUAGE_ARTS", "LANGUAGE_ARTS"),
# 		sgp.panel.years=c("2018_2019.F", "2018_2019.S"),
# 		sgp.grade.sequences=list(c("6", "6"), c("7", "7"), c("8", "8")),
# 		sgp.norm.group.preference=2)
# )
