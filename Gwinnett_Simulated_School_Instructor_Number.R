################################################################################
###                                                                          ###
###                   GCPS Simulated School and Teacher IDs                  ###
###                                                                          ###
################################################################################

### Load required packages
require(data.table)

###  Load prelim SGP long data (use this to create Teacher IDs only for students with SGPs)
load("./Data/Simulated_Data/Gwinnett_Data_LONG-Resimulated2.Rdata")

###   Elementary Schools

#  Create SCHOOL_NUMBER
base.elem <- Gwinnett_Data_LONG[CONTENT_AREA %in% c("LANGUAGE_ARTS", "MATHEMATICS", "SOCIAL_STUDIES", "SCIENCE") & GRADE %in% 1:5 & YEAR == "2018_2019.2", list(ID, YEAR)] # , CONTENT_AREA, GRADE
setkey(base.elem)
base.elem <- unique(base.elem)
dim(base.elem)
table(base.elem[, YEAR])

set.seed(589)
elem.size <- sample(seq(250, 1000, 1), 90)
sum(elem.size) # need close to 62,000
elem.numbs <- rep(1:90, times= elem.size)
elem.schools <- data.table(ID = unique(base.elem$ID), SCHOOL_NUMBER = sample(elem.numbs, length(unique(base.elem$ID)), replace = TRUE))
elem.schools <- rbindlist(list(copy(elem.schools)[, GRADE := "1"], copy(elem.schools)[, GRADE := "2"], copy(elem.schools)[, GRADE := "3"], copy(elem.schools)[, GRADE := "4"], copy(elem.schools)[, GRADE := "5"])) # Add GRADE for merge?
# elem.schools <- rbindlist(list(elem.schools[, CONTENT_AREA := "LANGUAGE_ARTS"], elem.schools[, CONTENT_AREA := "MATHEMATICS"])) # Add GRADE for merge?


###   Middle Schools

base.mid1 <- Gwinnett_Data_LONG[CONTENT_AREA %in% c("LANGUAGE_ARTS", "MATHEMATICS", "ACC_MATHEMATICS", "SOCIAL_STUDIES", "SCIENCE") & GRADE %in% 6:8 & YEAR == "2018_2019.2", list(ID, YEAR)] # , GRADE, CONTENT_AREA
# base.mid2 <- Gwinnett_Data_LONG[!is.na(SGP), list(ID, YEAR)] # CONTENT_AREA == "ALGEBRA_I" &
base.eoct <- Gwinnett_Data_LONG[GRADE == "EOCT" & YEAR == "2018_2019.2", list(ID, YEAR, CONTENT_AREA)] # CONTENT_AREA == "ALGEBRA_I" &
mid.eoct <- base.eoct[ID %in% unique(base.mid1$ID)]
hs.eoct <- base.eoct[!ID %in% unique(base.mid1$ID)]

base.mid2 <- unique(mid.eoct[, list(ID, YEAR)])

base.mid <- rbindlist(list(base.mid1, base.mid2))
setkey(base.mid)
base.mid <- unique(base.mid)
dim(base.mid)
table(base.mid[, YEAR])

set.seed(589)
mid.size <- sample(seq(500, 2500, 1), 30)
sum(mid.size) # need close to 45,000
mid.numbs <- rep(201:230, times= mid.size)
mid.schools <- data.table(ID = unique(base.mid$ID), SCHOOL_NUMBER = sample(mid.numbs, length(unique(base.mid$ID)), replace = TRUE))  # Add GRADE merge?
mid.schools <- rbindlist(list(copy(mid.schools)[, GRADE := "6"], copy(mid.schools)[, GRADE := "7"], copy(mid.schools)[, GRADE := "8"], copy(mid.schools)[, GRADE := "EOCT"]))


base.hs <- unique(hs.eoct[, list(ID, YEAR)])
dim(base.hs)
table(base.hs[, YEAR])

set.seed(589)
hs.size <- sample(seq(750, 2500, 1), 20)
sum(hs.size) # need close to 45,000
hs.numbs <- rep(3001:3020, times= hs.size)
hs.schools <- data.table(ID = unique(base.hs$ID), SCHOOL_NUMBER = sample(hs.numbs, length(unique(base.hs$ID)), replace = TRUE))  # Add GRADE merge?
hs.schools[, GRADE := "EOCT"]

Gwinnett_Data_SCHOOL_NUMBER <- rbindlist(list(elem.schools, mid.schools, hs.schools))

save(Gwinnett_Data_SCHOOL_NUMBER, file="Data/Simulated_Data/Gwinnett_Data_SCHOOL_NUMBER.Rdata")

setkey(Gwinnett_Data_SCHOOL_NUMBER, ID, GRADE)
setkey(Gwinnett_Data_LONG, ID, GRADE)

Gwinnett_Data_LONG <- merge(Gwinnett_Data_LONG, Gwinnett_Data_SCHOOL_NUMBER, all.x = TRUE)

#####
###   Create INSTRUCTOR_NUMBER
#####

###   Elementary School Teachers

set.seed(589)
# elem.schools.tchr <- data.table(ID = unique(base.elem$ID), SCHOOL_NUMBER = sample(elem.numbs, length(unique(base.elem$ID)), replace = TRUE))
# elem.schools.tchr <- rbindlist(list(copy(elem.schools.tchr)[, GRADE := "1"], copy(elem.schools.tchr)[, GRADE := "2"], copy(elem.schools.tchr)[, GRADE := "3"], copy(elem.schools.tchr)[, GRADE := "4"], copy(elem.schools.tchr)[, GRADE := "5"])) # Add GRADE for merge?

elem.schools.tchr <- elem.schools
setkey(elem.schools.tchr)

for (sch.id in unique(elem.schools.tchr[, SCHOOL_NUMBER])) {
  stu.lookup <- elem.schools.tchr[SCHOOL_NUMBER == sch.id]
  num.tchrs <- ceiling(as.numeric(table(stu.lookup[, GRADE]))/25)+1
  ca <- NULL
  for (g in 1:5) {
    stu.lookup[GRADE == g, INSTRUCTOR_NUMBER := sample(paste(sch.id, g, 1:num.tchrs[g], sep="_"), .N, replace = TRUE)]
  }
  setkeyv(stu.lookup, key(elem.schools.tchr))
  elem.schools.tchr <- elem.schools.tchr[stu.lookup, INSTRUCTOR_NUMBER := i.INSTRUCTOR_NUMBER] # , on = .(ID, SCHOOL_NUMBER, GRADE)
}
table(elem.schools.tchr[, GRADE, is.na(INSTRUCTOR_NUMBER)])


###   Middle School Teachers

set.seed(589)
base.mid3 <- Gwinnett_Data_LONG[CONTENT_AREA %in% c("LANGUAGE_ARTS", "MATHEMATICS", "ACC_MATHEMATICS", "SOCIAL_STUDIES", "SCIENCE") & GRADE %in% 6:8, list(ID, YEAR, GRADE, CONTENT_AREA)] #

grade.lev.ca <- c("LANGUAGE_ARTS", "MATHEMATICS", "ACC_MATHEMATICS", "SOCIAL_STUDIES", "SCIENCE")
grd.subj.lookup <- rbind(
  CJ(GRADE=as.character(6:8), CONTENT_AREA=grade.lev.ca),
  CJ(GRADE="EOCT", CONTENT_AREA=unique(mid.eoct$CONTENT_AREA))
)
mid.schools.tchr <- grd.subj.lookup[mid.schools, on="GRADE", allow.cartesian=TRUE]
mid.sch.actual <- rbindlist(list(base.mid3, mid.eoct[, GRADE := "EOCT"]), use.names=TRUE)[YEAR == "2018_2019.2"][, YEAR := NULL]

setkey(mid.schools.tchr)
setkey(mid.sch.actual)
table(mid.schools.tchr[, GRADE, CONTENT_AREA])
mid.schools.tchr <- merge(mid.schools.tchr, mid.sch.actual, all.y = TRUE)
table(mid.schools.tchr[, GRADE, CONTENT_AREA])

for (sch.id in unique(mid.schools.tchr[, SCHOOL_NUMBER])) {
  stu.lookup <- mid.schools.tchr[SCHOOL_NUMBER == sch.id]
  for (ca in grade.lev.ca){
    ca.abv <- strsplit(ca, "_")[[1]]
    if(length(ca.abv)==1) {
      ca.abv <- substr(ca, 1,3)
    } else {
      ca.abv <- paste(sapply(ca.abv, function(f) substr(f, 1,1), USE.NAMES = FALSE), collapse = "")
    }
    num.tchrs <- ceiling(as.numeric(table(stu.lookup[CONTENT_AREA == ca, GRADE]))/75)+1
    for (g in 6:8) {
      stu.lookup[GRADE == g & CONTENT_AREA == ca, INSTRUCTOR_NUMBER := sample(paste(sch.id, paste0(ca.abv, g), 1:num.tchrs[which(g == 6:8)], sep="_"), .N, replace = TRUE)]
    }
  }
  for (ca in setdiff(unique(stu.lookup$CONTENT_AREA), grade.lev.ca)) {
    ca.abv <- strsplit(ca, "_")[[1]]
    if(length(ca.abv)==1) {
      ca.abv <- substr(ca, 1,3)
    } else {
      romans <- suppressWarnings(sapply(ca.abv, function(f) utils:::.roman2numeric(f), USE.NAMES = FALSE))
      ca.abv[which(!is.na(romans))] <- romans[which(!is.na(romans))]
      ca.abv <- paste(sapply(ca.abv, function(f) ifelse(!is.na(suppressWarnings(as.numeric(f))), f, substr(f, 1,1)), USE.NAMES = FALSE), collapse = "")
    }
    num.tchrs <- ceiling(as.numeric(table(stu.lookup[CONTENT_AREA == ca, GRADE]))/75)+1
    stu.lookup[SCHOOL_NUMBER == sch.id & GRADE == "EOCT" & CONTENT_AREA == ca, INSTRUCTOR_NUMBER := sample(paste(sch.id, ca.abv, 1:num.tchrs, sep="_"), .N, replace = TRUE)]
  }
  setkeyv(stu.lookup, key(mid.schools.tchr))
  mid.schools.tchr <- mid.schools.tchr[stu.lookup, INSTRUCTOR_NUMBER := i.INSTRUCTOR_NUMBER] # , on = .(ID, SCHOOL_NUMBER, GRADE)
}
table(mid.schools.tchr[, GRADE, is.na(INSTRUCTOR_NUMBER)])


###   High School Teachers

set.seed(589)

grd.subj.lookup <- CJ(GRADE="EOCT", CONTENT_AREA=unique(hs.eoct$CONTENT_AREA))
hs.tchr <- grd.subj.lookup[hs.schools, on="GRADE", allow.cartesian=TRUE]
hs.actual <- hs.eoct[YEAR == "2018_2019.2"][, YEAR := NULL][, GRADE := "EOCT"]

setkey(hs.tchr)
setkey(hs.actual)
table(hs.tchr[, GRADE, CONTENT_AREA])
hs.tchr <- merge(hs.tchr, hs.actual, all.y = TRUE)
table(hs.tchr[, GRADE, CONTENT_AREA])

for (sch.id in unique(hs.tchr[, SCHOOL_NUMBER])) {
  stu.lookup <- hs.tchr[SCHOOL_NUMBER == sch.id]
  for (ca in unique(stu.lookup$CONTENT_AREA)) {
    ca.abv <- strsplit(ca, "_")[[1]]
    if(length(ca.abv)==1) {
      ca.abv <- substr(ca, 1,3)
    } else {
      romans <- suppressWarnings(sapply(ca.abv, function(f) utils:::.roman2numeric(f), USE.NAMES = FALSE))
      ca.abv[which(!is.na(romans))] <- romans[which(!is.na(romans))]
      ca.abv <- paste(sapply(ca.abv, function(f) ifelse(!is.na(suppressWarnings(as.numeric(f))), f, substr(f, 1,1)), USE.NAMES = FALSE), collapse = "")
    }
    num.tchrs <- ceiling(as.numeric(table(stu.lookup[CONTENT_AREA == ca, GRADE]))/75)+1
    stu.lookup[SCHOOL_NUMBER == sch.id & GRADE == "EOCT" & CONTENT_AREA == ca, INSTRUCTOR_NUMBER := sample(paste(sch.id, ca.abv, 1:num.tchrs, sep="_"), .N, replace = TRUE)]
  }
  setkeyv(stu.lookup, key(hs.tchr))
  hs.tchr <- hs.tchr[stu.lookup, INSTRUCTOR_NUMBER := i.INSTRUCTOR_NUMBER] # , on = .(ID, SCHOOL_NUMBER, GRADE)
}
table(hs.tchr[, CONTENT_AREA, is.na(INSTRUCTOR_NUMBER)])



###   Format and Save INSTRUCTOR_NUMBER dataset (BASIC)

#  see SGPdata::sgpData_INSTRUCTOR_NUMBER for exemplar
Gwinnett_Data_INSTRUCTOR_NUMBER <-
          rbindlist(list(
                         copy(elem.schools.tchr)[, CONTENT_AREA := "LANGUAGE_ARTS"],
                         copy(elem.schools.tchr)[, CONTENT_AREA := "MATHEMATICS"],
                         copy(elem.schools.tchr)[, CONTENT_AREA := "SCIENCE"],
                         copy(elem.schools.tchr)[, CONTENT_AREA := "SOCIAL_STUDIES"],
                         mid.schools.tchr, hs.tchr), use.names=TRUE)

Gwinnett_Data_INSTRUCTOR_NUMBER[, YEAR := "2018_2019.2"]
Gwinnett_Data_INSTRUCTOR_NUMBER[, GRADE := NULL] # not needed for summarizeSGP
Gwinnett_Data_INSTRUCTOR_NUMBER[, SCHOOL_NUMBER := NULL] # not needed for summarizeSGP (for Instructor/teacher summaries)
# Gwinnett_Data_INSTRUCTOR_NUMBER[, INSTRUCTOR_ENROLLMENT_STATUS := "Enrolled Instructor: Yes"] # This can be added if some students should not be attached/counted for a teacher

var.order <- as.numeric(na.omit(match(names(SGPdata::sgpData_INSTRUCTOR_NUMBER), names(Gwinnett_Data_INSTRUCTOR_NUMBER))))
setcolorder(Gwinnett_Data_INSTRUCTOR_NUMBER, var.order)

save(Gwinnett_Data_INSTRUCTOR_NUMBER, file="Data/Simulated_Data/Gwinnett_Data_INSTRUCTOR_NUMBER_Basic.Rdata")


#####
###   Add weights to some teachers
#####

###   Elementary School Teachers
setkey(719)

class.size.elem <- elem.schools.tchr[, list(N=.N), by=c('SCHOOL_NUMBER', 'GRADE', 'INSTRUCTOR_NUMBER')] # SN and CA extraneous, but keep for finding alt teachers
setkey(class.size.elem, N)
class.size.elem.add <- class.size.elem[1:2000,]
class.size.elem.trim <- class.size.elem[N > 29] # redistribute students in teachers with more than 30 kids

summary(class.size.elem.add$N)
sum.n <- sum(class.size.elem.add$N)/1000
class.size.elem.add[, SAMPLE_WEIGHT1 := (1-(N/sum.n))]
class.size.elem.add[, SAMPLE_WEIGHT2 := ((1-(N/sum.n))^2)^2]

elem.schools.tchr[, INSTRUCTOR_WEIGHT := 1.0]
setkey(elem.schools.tchr, ID)
tch.trim.ids <- class.size.elem.trim$INSTRUCTOR_NUMBER
tch.add.ids <- sample(class.size.elem.add$INSTRUCTOR_NUMBER, length(class.size.elem.add$INSTRUCTOR_NUMBER)*10, prob = class.size.elem.add$SAMPLE_WEIGHT2, replace=TRUE)
tch.add.ids <- c(tch.add.ids, class.size.elem.add$INSTRUCTOR_NUMBER[!class.size.elem.add$INSTRUCTOR_NUMBER %in% tch.add.ids])
# sum(tch.add.ids=="87_1_5")
# sum(tch.add.ids=="63_1_13")
# sum(tch.add.ids=="9_1_25")
# sum(tch.add.ids=="89_3_17")

for (tch in tch.trim.ids) {
  stu.ids <- elem.schools.tchr[INSTRUCTOR_NUMBER == tch, ID]
  redis.numb <- length(stu.ids) - 25

  weight.stus <- sort(sample(stu.ids, redis.numb))
  tmp.weights <- rep(0.4, redis.numb)
  elem.schools.tchr[INSTRUCTOR_NUMBER == tch & ID %in% weight.stus, INSTRUCTOR_WEIGHT := tmp.weights]
  dup.students <- elem.schools.tchr[INSTRUCTOR_NUMBER == tch & ID %in% weight.stus,]

  tmp.sch.num <- dup.students[, SCHOOL_NUMBER][1]
  grd.subj <- strsplit(tch, "_")[[1]][2]
  tmp.tch.ids <- class.size.elem.add[SCHOOL_NUMBER==tmp.sch.num][grep(paste0("_", grd.subj, "_"), INSTRUCTOR_NUMBER), INSTRUCTOR_NUMBER] # Make sure there are teachers in the same school to sample from
  if (length(tmp.tch.ids)==0) next # If no teachers to add to in school, leave as is

  sch.num <- rep(tmp.sch.num, redis.numb)
  new.tch.id <- paste(sch.num, grd.subj, sep="_")
  new.tch.id <- unlist(sapply(unique(new.tch.id), function(f) sample(grep(f, tch.add.ids, value=TRUE), sum(new.tch.id == f), replace = TRUE), USE.NAMES=FALSE))
  dup.students[, SCHOOL_NUMBER := sch.num]
  dup.students[, INSTRUCTOR_NUMBER := new.tch.id]
  dup.students[, INSTRUCTOR_WEIGHT := 1-tmp.weights]
  elem.schools.tchr <- rbindlist(list(elem.schools.tchr, dup.students))
  setkey(elem.schools.tchr, ID)
}

class.size.wtd <- elem.schools.tchr[, list(N=.N, N_WTD = sum(INSTRUCTOR_WEIGHT)), by=c('INSTRUCTOR_NUMBER')] # SN, G and CA extraneous, but keep for finding alt teachers # 'SCHOOL_NUMBER', 'GRADE', 'CONTENT_AREA',
setkey(class.size.wtd, N)
wtd.add <- class.size.wtd[INSTRUCTOR_NUMBER %in% unique(tch.add.ids)]
wtd.trim <- class.size.wtd[INSTRUCTOR_NUMBER %in% tch.trim.ids]
summary(wtd.add$N_WTD)
summary(wtd.trim$N_WTD)


###   Middle School Teachers

setkey(719)
class.size.mid <- mid.schools.tchr[, list(N=.N), by=c('SCHOOL_NUMBER', 'GRADE', 'CONTENT_AREA', 'INSTRUCTOR_NUMBER')] # SN, G and CA extraneous, but keep for finding alt teachers
setkey(class.size.mid, N)
class.size.mid.add <- class.size.mid[1:1500,]
class.size.mid.trim <- class.size.mid[N > 64] # redistribute students in teachers with more than 75 kids

summary(class.size.mid.add$N)
sum.n <- sum(class.size.mid.add$N)/750
class.size.mid.add[, SAMPLE_WEIGHT1 := (1-(N/sum.n))]
class.size.mid.add[, SAMPLE_WEIGHT2 := ((1-(N/sum.n))^2)^2]
# class.size.mid.add[, SAMPLE_WEIGHT2 := (sum.n-(N+1))]

mid.schools.tchr[, INSTRUCTOR_WEIGHT := 1.0]
setkey(mid.schools.tchr, ID)
tch.trim.ids <- class.size.mid.trim$INSTRUCTOR_NUMBER
tch.add.ids <- sample(class.size.mid.add$INSTRUCTOR_NUMBER, length(class.size.mid.add$INSTRUCTOR_NUMBER)*10, prob = class.size.mid.add$SAMPLE_WEIGHT2, replace=TRUE)
tch.add.ids <- c(tch.add.ids, class.size.mid.add$INSTRUCTOR_NUMBER[!class.size.mid.add$INSTRUCTOR_NUMBER %in% tch.add.ids])
all(unique(class.size.mid.add$INSTRUCTOR_NUMBER))
# sum(tch.add.ids=="205_M8_6")
# sum(tch.add.ids=="203_L8_12")
# sum(tch.add.ids=="226_L7_9")
# sum(tch.add.ids=="229_ALG_1")

for (tch in tch.trim.ids) {
  stu.ids <- mid.schools.tchr[INSTRUCTOR_NUMBER == tch, ID]
  redis.numb <- length(stu.ids) - 55
  move.sch <- sample(c(TRUE, FALSE), redis.numb, prob = c(0.15, 0.85), replace = TRUE)

  weight.stus <- sort(sample(stu.ids, redis.numb))
  tmp.weights <- rep(0.2, redis.numb)
  tmp.weights[which(move.sch)] <- 0.5
  mid.schools.tchr[INSTRUCTOR_NUMBER == tch & ID %in% weight.stus, INSTRUCTOR_WEIGHT := tmp.weights]
  dup.students <- mid.schools.tchr[INSTRUCTOR_NUMBER == tch & ID %in% weight.stus,]

  tmp.sch.num <- dup.students[, SCHOOL_NUMBER][1]
  grd.subj <- strsplit(tch, "_")[[1]][2]
  tmp.tch.ids <- class.size.mid.add[SCHOOL_NUMBER==tmp.sch.num][grep(paste0("_", grd.subj, "_"), INSTRUCTOR_NUMBER), INSTRUCTOR_NUMBER] # Make sure there are teachers in the same school to sample from
  if (length(tmp.tch.ids)==0) move.sch <- rep(TRUE, redis.numb)

  sch.num <- rep(tmp.sch.num, redis.numb)
  if (any(move.sch)) {
    tmp.sch.ids <- class.size.mid.add[grep(paste0("_", grd.subj, "_"), INSTRUCTOR_NUMBER), SCHOOL_NUMBER] # Make sure there are teachers in a school to sample from
    if (length(tmp.sch.ids) > 0) {
      sch.num[which(move.sch)] <- sample(as.character(tmp.sch.ids), sum(move.sch), replace = TRUE)
      replace.tch <- TRUE
    } else {replace.tch <- FALSE; print(paste("Teacher", tch, "has no replacement."))}
  }
  if (replace.tch){
    new.tch.id <- paste(sch.num, grd.subj, sep="_")
    new.tch.id <- unlist(sapply(unique(new.tch.id), function(f) sample(grep(f, tch.add.ids, value=TRUE), sum(new.tch.id == f), replace = TRUE), USE.NAMES=FALSE))
    dup.students[, SCHOOL_NUMBER := sch.num]
    dup.students[, INSTRUCTOR_NUMBER := new.tch.id]
    dup.students[, INSTRUCTOR_WEIGHT := 1-tmp.weights]
    mid.schools.tchr <- rbindlist(list(mid.schools.tchr, dup.students))
    setkey(mid.schools.tchr, ID)
  }
}

class.size.wtd <- mid.schools.tchr[, list(N=.N, N_WTD = sum(INSTRUCTOR_WEIGHT)), by=c('INSTRUCTOR_NUMBER')] # SN, G and CA extraneous, but keep for finding alt teachers # 'SCHOOL_NUMBER', 'GRADE', 'CONTENT_AREA',
setkey(class.size.wtd, N)
wtd.add <- class.size.wtd[INSTRUCTOR_NUMBER %in% unique(tch.add.ids)]
wtd.trim <- class.size.wtd[INSTRUCTOR_NUMBER %in% tch.trim.ids]
summary(wtd.add$N_WTD)
summary(wtd.trim$N_WTD)

###   Format and Save INSTRUCTOR_NUMBER dataset (WEIGHTED)

#  see SGPdata::sgpData_INSTRUCTOR_NUMBER for exemplar
Gwinnett_Data_INSTRUCTOR_NUMBER <-
          rbindlist(list(
                         copy(elem.schools.tchr)[, CONTENT_AREA := "LANGUAGE_ARTS"],
                         copy(elem.schools.tchr)[, CONTENT_AREA := "MATHEMATICS"],
                         copy(elem.schools.tchr)[, CONTENT_AREA := "SCIENCE"],
                         copy(elem.schools.tchr)[, CONTENT_AREA := "SOCIAL_STUDIES"],
                         mid.schools.tchr, copy(hs.tchr)[, INSTRUCTOR_WEIGHT := 1.0]), use.names=TRUE)

Gwinnett_Data_INSTRUCTOR_NUMBER[, YEAR := "2018_2019.2"]
# Gwinnett_Data_INSTRUCTOR_NUMBER[, INSTRUCTOR_ENROLLMENT_STATUS := "Enrolled Instructor: Yes"] # This can be added if some students should not be attached/counted for a teacher
Gwinnett_Data_INSTRUCTOR_NUMBER[, GRADE := NULL] # not needed for summarizeSGP
Gwinnett_Data_INSTRUCTOR_NUMBER[, SCHOOL_NUMBER := NULL] # not needed for summarizeSGP (for Instructor/teacher summaries)

var.order <- as.numeric(na.omit(match(names(SGPdata::sgpData_INSTRUCTOR_NUMBER), names(Gwinnett_Data_INSTRUCTOR_NUMBER))))
setcolorder(Gwinnett_Data_INSTRUCTOR_NUMBER, var.order)

save(Gwinnett_Data_INSTRUCTOR_NUMBER, file="Data/Simulated_Data/Gwinnett_Data_INSTRUCTOR_NUMBER_Weighted.Rdata")

#####
#####
#####
