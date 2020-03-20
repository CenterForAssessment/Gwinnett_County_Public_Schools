################################################################################
###                                                                          ###
###                 Identify Fall 2019 Progressions for GCPS                 ###
###                                                                          ###
################################################################################

library(SGP)
library(data.table)

###  Combine long data from Spring 2019 analyses with Fall 2019 (NJ only) data
load("Data/Simulated_Data/Gwinnett_Data_LONG.Rdata")

table(Gwinnett_Data_LONG[CONTENT_AREA != "LANGUAGE ARTS", CONTENT_AREA, YEAR])
table(Gwinnett_Data_LONG[which(CONTENT_AREA == "LANGUAGE ARTS"), as.numeric(GRADE), YEAR])
table(Gwinnett_Data_LONG[which(YEAR == "2018_2019.S"), GRADE, CONTENT_AREA])

###  Run courseProgressionSGP by content area subsets of the Gwinnett_Data_LONG

ela.prog <- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% "LANGUAGE ARTS"], lag.direction="BACKWARD", year="2018_2019.S")
math.prog<- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% "MATHEMATICS"], lag.direction="BACKWARD", year="2018_2019.S")
alg1.prog<- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% c("ALGEBRA I", "ACC MATH", "MATHEMATICS")], lag.direction="BACKWARD", year="2018_2019.S") # Just two possible priors

####
####     LANGUAGE ARTS
####

###  Find out which grades are present in the ELA data
names(ela.prog$BACKWARD[["2018_2019.S"]])

###  Elementary School
ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.05"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
sum(ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="LANGUAGE ARTS.04"]$COUNT)   #   17862 "Traditional" Spring to Spring
sum(ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="LANGUAGE ARTS.05"]$COUNT)   #       8 (Repeaters)

ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.04"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.03"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.02"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.01"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")] # 78 first grade repeaters


###  Middle School
ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.08"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]
sum(ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.08"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.1=="LANGUAGE ARTS.07"]$COUNT)   #   13995 "Traditional" Spring to Spring
sum(ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.08"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.1=="LANGUAGE ARTS.08"]$COUNT)   #    27660 (Fall to Spring)

ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]
ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]#  This is weird!!!  ~11k kids with a 8th grade prior?
ela.prog$BACKWARD[["2018_2019.S"]][["LANGUAGE ARTS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]



####
####     MATHEMATICS
####

###  Find out which grades are present in the Math data
names(math.prog$BACKWARD[["2018_2019.S"]])

###  Elementary School
math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.05"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
sum(math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="MATHEMATICS.04"]$COUNT)   #   15690 "Traditional" Spring to Spring
sum(math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="MATHEMATICS.05"]$COUNT)   #      10 (Repeaters)

math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.04"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.03"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.02"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.01"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.0K"]] # No priors for Kindergarten (as expected)


###  Middle School
math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]
sum(math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.07"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.1=="MATHEMATICS.06"]$COUNT)   #     9272 "Traditional" Spring to Spring
sum(math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.07"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.1=="MATHEMATICS.07"]$COUNT)   #    19485 (Fall to Spring)

math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")] # 9045 "Traditional" Spring to Spring

#  This is weird!!!  ~7k kids with a 7th grade prior?
math.prog$BACKWARD[["2018_2019.S"]][["MATHEMATICS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]

ids <- unique(Gwinnett_Data_LONG[which(CONTENT_AREA == "MATHEMATICS" & GRADE == "6" & YEAR == "2018_2019.S"), ID])

g6.math <- Gwinnett_Data_LONG[which(CONTENT_AREA == "MATHEMATICS" & ID %in% ids)]
table(g6.math[, GRADE, YEAR])
table(g6.math[, YEAR, Test_Name]) # Maybe kids that took both 6th and 7th grade tests?

####
####     ALGEBRA I
####

###  Find out which grades are present in the Math data
names(alg1.prog$BACKWARD[["2018_2019.S"]])

alg1.prog$BACKWARD[["2018_2019.S"]][["ALGEBRA I.EOCT"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]

ids <- unique(Gwinnett_Data_LONG[which(CONTENT_AREA == "ALGEBRA I" & YEAR == "2018_2019.S"), ID])

alg1 <- Gwinnett_Data_LONG[which(CONTENT_AREA %in% c("ALGEBRA I", "ACC MATH", "MATHEMATICS") & ID %in% ids)]
table(alg1[, GRADE, YEAR])
table(alg1[, YEAR, Test_Name])
table(alg1[, YEAR, CONTENT_AREA]) #  Just create Fall to Spring Progressions for testing
