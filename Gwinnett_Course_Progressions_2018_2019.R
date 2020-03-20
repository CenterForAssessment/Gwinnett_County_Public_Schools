################################################################################
###                                                                          ###
###                 Identify Fall 2019 Progressions for GCPS                 ###
###                                                                          ###
################################################################################

library(SGP)
library(data.table)

###  Combine long data from Spring 2019 analyses with Fall 2019 (NJ only) data
load("Data/Simulated_Data/Gwinnett_Data_LONG.Rdata")

table(Gwinnett_Data_LONG[which(CONTENT_AREA == "LANGUAGE_ARTS"), as.numeric(GRADE), YEAR])
table(Gwinnett_Data_LONG[which(YEAR == "2018_2019.2"), GRADE, CONTENT_AREA])

###  Run courseProgressionSGP by content area subsets of the Gwinnett_Data_LONG

#  EOG - same CONTENT_AREA priors
ela.prog <- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% "LANGUAGE_ARTS"], lag.direction="BACKWARD", year="2018_2019.2")
math.prog<- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% "MATHEMATICS"], lag.direction="BACKWARD", year="2018_2019.2")

#  EOC - need to group all subjects into relevant content domains (Math/ELA/Science/Soc Sciences/Etc.)
math.subjects <- c("ALGEBRA_I", "ACC_MATHEMATICS", "MATHEMATICS")  # Only two priors Looked at for MS Alg I exploration
alg1.prog<- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% math.subjects], lag.direction="BACKWARD", year="2018_2019.2") # Just two possible priors

####
####     LANGUAGE_ARTS
####

###  Find out which grades are present in the ELA data
names(ela.prog$BACKWARD[["2018_2019.2"]])

###  Elementary School
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.05"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
sum(ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="LANGUAGE_ARTS.04"]$COUNT)   #   12148 "Traditional" Spring to Spring
sum(ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="LANGUAGE_ARTS.05"]$COUNT)   #       4 (Repeaters)

ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.04"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.03"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.02"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.01"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")] # 50 first grade repeaters


###  Middle School
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.08"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.08"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #  12975 (Fall to Spring)
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.08"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #   9345 "Traditional" Spring to Spring

ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.07"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #  13496 (Fall to Spring)
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #   9046 "Traditional" Spring to Spring

ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.06"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #  13561 (Fall to Spring)
ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #  12026 "Traditional" Spring to Spring



####
####     MATHEMATICS
####

###  Find out which grades are present in the Math data
names(math.prog$BACKWARD[["2018_2019.2"]])

###  Elementary School
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.05"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
sum(math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="MATHEMATICS.04"]$COUNT)   #   12068 "Traditional" Spring to Spring
sum(math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="MATHEMATICS.05"]$COUNT)   #       5 (Repeaters)

math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.04"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.03"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.02"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.01"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")] # Not enough 1st graders in second simulated dataset
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.0K"]] # No priors for Kindergarten (as expected)


###  Middle School
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.07"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #   8507 (Fall to Spring)
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #   5579 "Traditional" Spring to Spring

math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.06"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #   8296 (Fall to Spring)
math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #   7220 "Traditional" Spring to Spring

####
####     ALGEBRA_I
####

###  Find out which grades are present in the Math data
names(alg1.prog$BACKWARD[["2018_2019.2"]])

alg1.prog$BACKWARD[["2018_2019.2"]][["ALGEBRA_I.EOCT"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
alg1.prog$BACKWARD[["2018_2019.2"]][["ALGEBRA_I.EOCT"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]
alg1.prog$BACKWARD[["2018_2019.2"]][["ALGEBRA_I.EOCT"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
