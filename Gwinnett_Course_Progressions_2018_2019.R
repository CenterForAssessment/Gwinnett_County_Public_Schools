################################################################################
###                                                                          ###
###                 Identify Fall 2019 Progressions for GCPS                 ###
###                                                                          ###
################################################################################
# last updated 04/08/2020
# by Shanna Ricketts

library(SGP)
library(data.table)
library(tidyverse)
library(readxl)


###  Combine long data from Spring 2019 analyses with Fall 2019 (NJ only) data
#load("Data/Simulated_Data/Gwinnett_Data_LONG.Rdata")
load("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/exploration/Student Growth Exploration/4_output/datasets/Gwinnett_Data_LONG.Rdata")

# Code from Shanna to make one master configuration file (04/07/2020)
# gives the subject mapped to test names
# test_subject_mapping <- read_excel("../exploration/Student Growth Exploration/3_source_data/prep/post-to-post-mapping.xlsx") %>% 
#   select(post1819, subject) %>% 
#   rename(TEST_NAME = post1819,
#          SUBJECT = subject) %>% 
#   distinct()
#saveRDS(test_subject_mapping, "../4_output/datasets/test_subject_mapping.RDS")
test_subject_mapping <- readRDS("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/4_output/datasets/test_subject_mapping.RDS")
assessedsubject_to_sgpname <- readRDS("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/4_output/datasets/assessedsubject_to_sgpname.rds")

# adding in additional columns that will be needed to loop through
# and create configuration script
Gwinnett_Data_LONG <- Gwinnett_Data_LONG %>% 
  left_join(test_subject_mapping,
            by = c("TEST_NAME" = "TEST_NAME")) %>% 
  mutate(SUBJECT = str_replace_all(SUBJECT, " ", "_")) %>% 
  mutate(SUBJECT = toupper(SUBJECT)) %>% 
  mutate(list_name = ifelse(GRADE == "EOCT", 
                            paste0(CONTENT_AREA, ".", GRADE),
                            paste0(CONTENT_AREA, ".0", GRADE)))


#start of looping code
Gwinnett_Data_LONG <- data.table(Gwinnett_Data_LONG)

# set up empty data frame
df_full <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(df_full) <- c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.0", 
                       "YEAR",
                       "CONTENT_AREA_by_GRADE_PRIOR_YEAR.1",
                       "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2",
                       "CONTENT_AREA_by_GRADE_PRIOR_YEAR.3",
                       "COUNT",
                       "PERCENTAGE_IN_GROUP")

# get the subject names
subject <- Gwinnett_Data_LONG %>% 
  distinct(SUBJECT) %>% 
  pull()


for (j in subject){
  subj.prog <- courseProgressionSGP(Gwinnett_Data_LONG[SUBJECT %in% j], lag.direction="BACKWARD", year="2018_2019.2")
  
  test_names <- Gwinnett_Data_LONG %>% 
    filter(SUBJECT == j) %>% 
    select(list_name) %>% 
    distinct() %>% 
    pull()
  
  df.2 <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df.2) <- c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.0", 
                      "CONTENT_AREA_by_GRADE_PRIOR_YEAR.1",
                      "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2",
                      "CONTENT_AREA_by_GRADE_PRIOR_YEAR.3",
                      "COUNT",
                      "PERCENTAGE_IN_GROUP")
  
  for (i in test_names){
    
    df_i <- subj.prog[["BACKWARD"]][["2018_2019.2"]][[i]]
    df.2 <- bind_rows(df.2, df_i)
  }
  
  df.2 <- df.2 %>% 
    mutate(YEAR = "2018_2019.2") %>% 
    select(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, YEAR, everything())
  
  
  df.1 <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df.1) <- c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.0", 
                      "CONTENT_AREA_by_GRADE_PRIOR_YEAR.1",
                      "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2",
                      # "CONTENT_AREA_by_GRADE_PRIOR_YEAR.3",
                      "COUNT",
                      "PERCENTAGE_IN_GROUP")
  
  for (i in test_names){
    
    df_i <- subj.prog[["BACKWARD"]][["2018_2019.1"]][[i]]
    df.1 <- bind_rows(df.1, df_i)
  }
  
  df.1 <- df.1 %>% 
    mutate(YEAR = "2018_2019.1") %>% 
    select(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, YEAR, everything())
  
  df <- bind_rows(df.2, df.1)
  df_full <- bind_rows(df_full, df)
}

# df_full includes all the course progressions
# now to determine for which ones will calculate SGPs

df_full2 <- df_full %>% 
  left_join(Gwinnett_Data_LONG %>% 
              select(list_name, #`same school year`, 
                     SEMESTER,
                     SUBJECT) %>% 
              distinct() ,
              #arrange(list_name, `same school year`) %>% 
              #distinct(list_name, .keep_all = TRUE) %>% 
              #rename(semester = `same school year`) %>% 
              #mutate(semester = ifelse(is.na(semester), "no", semester)),
            by = c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.0" = "list_name")
  )

# get all the course progressions for the semester courses
# looking at the most immediate prior
df2_sem <- df_full2 %>%
  filter(SEMESTER %in% c(1, 2)) %>% 
  group_by(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, YEAR) %>% 
  mutate(total =sum(COUNT)) %>% 
  ungroup() %>% 
  group_by(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0,
           YEAR,
           CONTENT_AREA_by_GRADE_PRIOR_YEAR.1) %>% 
  summarise(
    total = first(total),
    ct = sum(COUNT),
    semester = first(SEMESTER),
    subject = first(SUBJECT)
    #school_level = first(school_level)
    ) %>% 
  mutate(pct = round(ct * 100 / total, 2)) %>% 
  rename(immediate_prior = CONTENT_AREA_by_GRADE_PRIOR_YEAR.1) %>% 
  mutate(immediate_prior_year = ifelse(YEAR == "2018_2019.2",
                                       "2018_2019.1",
                                       "2017_2018.2"))

# get all the course progressions for the year courses
# getting the most immediate prior
df2_year <- df_full2 %>%
  filter(is.na(SEMESTER)) %>% 
  group_by(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, YEAR) %>% 
  mutate(total =sum(COUNT)) %>% 
  ungroup() %>% 
  group_by(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0,
           YEAR,
           CONTENT_AREA_by_GRADE_PRIOR_YEAR.2) %>% 
  summarise(
    total = first(total),
    ct = sum(COUNT),
    semester = first(SEMESTER),
    subject = first(SUBJECT)
    #school_level = first(school_level)
    )%>% 
  mutate(pct = round(ct * 100 / total, 2)) %>% 
  rename(immediate_prior = CONTENT_AREA_by_GRADE_PRIOR_YEAR.2) %>% 
  mutate(immediate_prior_year = "2017_2018.2")


df2 <- bind_rows(df2_sem, df2_year) %>% 
  select(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0,
         YEAR,
         immediate_prior,
         immediate_prior_year,
         everything()) %>% 
  arrange(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, desc(YEAR), desc(pct)) 
# could save out this file to show the entire progression when just using the 
# immediate prior

# break out grades in order to make configuration script
df2 <- df2 %>% 
  ungroup() %>% 
  mutate(GRADE = ifelse(str_detect(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, "EOCT"),
                        str_sub(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, -4, -1),
                        str_sub(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, -1, -1)) ) %>% 
  mutate(grade_prior = ifelse(str_detect(immediate_prior, "EOCT"),
                              str_sub(immediate_prior, -4, -1),
                              str_sub(immediate_prior, -1, -1)) ) %>% 
  mutate(CONTENT = ifelse(str_detect(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, "EOCT"),
                          str_sub(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, 1, -6),
                          str_sub(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, 1, -4)) ) %>% 
  mutate(content_prior = ifelse(str_detect(immediate_prior, "EOCT"),
                                str_sub(immediate_prior, 1, -6),
                                str_sub(immediate_prior, 1, -4)) ) %>% 
  rename(SEMESTER = semester) %>% 
  select(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, YEAR, CONTENT, GRADE, SEMESTER,
         immediate_prior, immediate_prior_year, content_prior, grade_prior, everything())



# Code from Adam
# table(Gwinnett_Data_LONG[which(CONTENT_AREA == "LANGUAGE_ARTS"), as.numeric(GRADE), YEAR])
# table(Gwinnett_Data_LONG[which(YEAR == "2018_2019.2"), GRADE, CONTENT_AREA])
# 
# ###  Run courseProgressionSGP by content area subsets of the Gwinnett_Data_LONG
# 
# #  EOG - same CONTENT_AREA priors
# ela.prog <- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% "LANGUAGE_ARTS"], lag.direction="BACKWARD", year="2018_2019.2")
# math.prog<- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% "MATHEMATICS"], lag.direction="BACKWARD", year="2018_2019.2")
# 
# #  EOC - need to group all subjects into relevant content domains (Math/ELA/Science/Soc Sciences/Etc.)
# math.subjects <- c("ALGEBRA_I", "ACC_MATHEMATICS", "MATHEMATICS")  # Only two priors Looked at for MS Alg I exploration
# alg1.prog<- courseProgressionSGP(Gwinnett_Data_LONG[CONTENT_AREA %in% math.subjects], lag.direction="BACKWARD", year="2018_2019.2") # Just two possible priors
# 
# ####
# ####     LANGUAGE_ARTS
# ####
# 
# ###  Find out which grades are present in the ELA data
# names(ela.prog$BACKWARD[["2018_2019.2"]])
# 
# ###  Elementary School
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.05"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# sum(ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="LANGUAGE_ARTS.04"]$COUNT)   #   12148 "Traditional" Spring to Spring
# sum(ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="LANGUAGE_ARTS.05"]$COUNT)   #       4 (Repeaters)
# 
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.04"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.03"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.02"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.01"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")] # 50 first grade repeaters
# 
# 
# ###  Middle School
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.08"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.08"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #  12975 (Fall to Spring)
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.08"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #   9345 "Traditional" Spring to Spring
# 
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.07"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #  13496 (Fall to Spring)
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #   9046 "Traditional" Spring to Spring
# 
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.06"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #  13561 (Fall to Spring)
# ela.prog$BACKWARD[["2018_2019.2"]][["LANGUAGE_ARTS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #  12026 "Traditional" Spring to Spring
# 
# 
# 
# ####
# ####     MATHEMATICS
# ####
# 
# ###  Find out which grades are present in the Math data
# names(math.prog$BACKWARD[["2018_2019.2"]])
# 
# ###  Elementary School
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.05"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# sum(math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="MATHEMATICS.04"]$COUNT)   #   12068 "Traditional" Spring to Spring
# sum(math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.05"]][CONTENT_AREA_by_GRADE_PRIOR_YEAR.2=="MATHEMATICS.05"]$COUNT)   #       5 (Repeaters)
# 
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.04"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.03"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.02"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.01"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")] # Not enough 1st graders in second simulated dataset
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.0K"]] # No priors for Kindergarten (as expected)
# 
# 
# ###  Middle School
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.07"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #   8507 (Fall to Spring)
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.07"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #   5579 "Traditional" Spring to Spring
# 
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.06"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]  #   8296 (Fall to Spring)
# math.prog$BACKWARD[["2018_2019.2"]][["MATHEMATICS.06"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]  #   7220 "Traditional" Spring to Spring
# 
# ####
# ####     ALGEBRA_I
# ####
# 
# ###  Find out which grades are present in the Math data
# names(alg1.prog$BACKWARD[["2018_2019.2"]])
# 
# alg1.prog$BACKWARD[["2018_2019.2"]][["ALGEBRA_I.EOCT"]][, list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1", "CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
# alg1.prog$BACKWARD[["2018_2019.2"]][["ALGEBRA_I.EOCT"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.1), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.1")]
# alg1.prog$BACKWARD[["2018_2019.2"]][["ALGEBRA_I.EOCT"]][!is.na(CONTENT_AREA_by_GRADE_PRIOR_YEAR.2), list(Total=sum(COUNT)), keyby=c("CONTENT_AREA_by_GRADE_PRIOR_YEAR.2")]
