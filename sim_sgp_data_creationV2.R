#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Project:SGM
# Date Started: 03/17/2020                                               
# Last Updated: 03/20/2020                                          
# Author: Shanna N. Ricketts
# Script purpose: Used to create simulated data for working with Center for Assessment
# Files created: sim_gcps_data.rds; sim_gcps_data.csv
# Additional notes: Based off mapping_1819.R
  # updated from sim_sgp_data_creation.R
# (03/20/2020) updated to create a file that is unique, by ID, CONTENT_AREA, YEAR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(readxl)
library(tidylog)
library(refuns) #Research Team package for function rnd_like_xl; could simply replace with round

date <- Sys.Date()

savefile <- 1

# 02-28-2020 decided to use district eligible students; SGPs will be done on these students first and then aggregated
# up to ones who are associated with a teacher similar to current quartile approach

# file with all district eligible student scores for 2018-19
# unique by teacher, student, test
distr_elig_1819 <- readRDS("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2018-19 Analysis/4_output/datasets/district_eligible_1819data_2019-09-24.rds")

# file with all district eligible student scores for 2018-19
# unique by teacher, student, test
distr_elig_1718 <- readRDS("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2017-18 Analysis/DataFiles/distr_elig_2018-08-07.rds") %>% 
  distinct(employeeid, subjectarea2, studentid, .keep_all = TRUE)
# there were 4 pairs of duplicates with different scores; simply deleting one to keep moving with exploration

# file that has the testnames from 17-18 mapped to the testnames from 18-19
# the testnames in this file are the ones that are expected to be used moving forward
testname_mapping <- read_excel("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/exploration/Student Growth Exploration/3_source_data/prep/testname_mapping.xlsx") 

# this file has the number of items in each test
# will use to help simulate scores
num_items <- read_excel("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/exploration/Student Growth Exploration/3_source_data/orig/number_of_items.xlsx")

a <- distr_elig_1819 %>% 
  select(assessedsubject,
         studentuniqueid,
         posttestpercentcomputed) %>% 
  # make unique by student and test
  distinct() %>% 
  # filter only for assessments that will be used in 19-20 & future years
  filter(assessedsubject %in% c('10TH GRADE LIT & COMP SEM1',
                                '10TH GRADE LIT & COMP SEM2',
                                '11TH GRADE AMERICAN LIT & COMP SEM1',
                                '11TH GRADE AMERICAN LIT & COMP SEM2',
                                '12TH GRADE BRITISH LIT & COMP SEM1',
                                '12TH GRADE BRITISH LIT & COMP SEM2',
                                '8TH GRADE ALGEBRA SEM 1',
                                '8TH GRADE ALGEBRA SEM 2',
                                '9TH GRADE LIT & COMP SEM1',
                                '9TH GRADE LIT & COMP SEM2',
                                'ACC ALGEBRA I SEM1',
                                'ACC ALGEBRA I SEM2',
                                'ACC GEOMETRY SEM1',
                                'ACC GEOMETRY SEM2',
                                'ACC MATH 6 SEM1',
                                'ACC MATH 6 SEM2',
                                'ACC MATH 7 SEM1',
                                'ACC MATH 7 SEM2',
                                'ACC PRECALCULUS SEM1',
                                'ACC PRECALCULUS SEM2',
                                'ADV CALC II',
                                'ADV MATHEMATICAL DECISION MAKING',
                                'ALGEBRA I SEM1',
                                'ALGEBRA I SEM2',
                                'ALGEBRA I STRATEGIES',
                                'ALGEBRA II SEM1',
                                'ALGEBRA II SEM2',
                                'ALGEBRA II STRATEGIES',
                                'AP AMERICAN GOVERNMENT',
                                'AP BIOLOGY',
                                'AP CALCULUS AB',
                                'AP CALCULUS BC',
                                'AP CHEMISTRY',
                                'AP COMPARATIVE GOVERNMENT',
                                'AP ENVIRONMENTAL SCIENCE',
                                'AP EUROPEAN HISTORY',
                                'AP FRENCH',
                                'AP HUMAN GEOGRAPHY',
                                'AP LANG & COMP',
                                'AP LIT & COMP',
                                'AP MACROECONOMICS',
                                'AP PHYSICS C: ELECTRICITY&MAGNETISM',
                                'AP PHYSICS C: MECHANICS',
                                'AP PHYSICS I',
                                'AP PSYCHOLOGY',
                                'AP SPANISH',
                                'AP SPANISH LITERATURE & CULTURE',
                                'AP STATISTICS',
                                'AP US HISTORY',
                                'AP WORLD HISTORY',
                                'ASTRONOMY',
                                'BIO ENGINEERING/SCI RESEARCH III',
                                'BIOLOGY SEM1',
                                'BIOLOGY SEM2',
                                'CALCULUS',
                                'CHEMISTRY SEM1',
                                'CHEMISTRY SEM2',
                                'CHINESE I',
                                'CHINESE II',
                                'DIFFERENTIAL EQUATIONS',
                                'EARTH SYSTEMS',
                                'ECONOMICS',
                                'ENGLISH ESOL II SEM1',
                                'ENGLISH ESOL II SEM2',
                                'ENGLISH ESOL III SEM1',
                                'ENGLISH ESOL III SEM2',
                                'ENVIRONMENTAL SCIENCE',
                                'FORENSIC SCIENCE',
                                'FRENCH I',
                                'FRENCH II',
                                'FRENCH III',
                                'FRENCH IV',
                                'GEOMETRY SEM1',
                                'GEOMETRY SEM2',
                                'GEOMETRY STRATEGIES',
                                'GERMAN I',
                                'GERMAN II',
                                'GERMAN III',
                                'GERMAN IV',
                                'HUMAN ANATOMY & PHYSIOLOGY',
                                'JOURNALISM I',
                                'LA/READING/GRADE 1',
                                'LA/READING/GRADE 2',
                                'LA/READING/GRADE 3',
                                'LA/READING/GRADE 4',
                                'LA/READING/GRADE 5',
                                'LA/READING/GRADE K',
                                'LANGUAGE ARTS 6 SEM1',
                                'LANGUAGE ARTS 6 SEM2',
                                'LANGUAGE ARTS 7 SEM1',
                                'LANGUAGE ARTS 7 SEM2',
                                'LANGUAGE ARTS 8 SEM1',
                                'LANGUAGE ARTS 8 SEM2',
                                'LATIN I',
                                'LATIN II',
                                'LATIN III',
                                'LATIN IV',
                                'LAW',
                                'MATH 6 SEM1',
                                'MATH 6 SEM2',
                                'MATH 7 SEM1',
                                'MATH 7 SEM2',
                                'MATHEMATICS INDUSTRY & GOVERNMENT',
                                'MATHEMATICS OF FINANCE',
                                'MATHEMATICS/GRADE 1',
                                'MATHEMATICS/GRADE 2',
                                'MATHEMATICS/GRADE 3',
                                'MATHEMATICS/GRADE 4',
                                'MATHEMATICS/GRADE 5',
                                'MATHEMATICS/GRADE K',
                                'MICROBIOLOGY',
                                'MULTIVARIABLE CALCULUS',
                                'NUMBER THEORY',
                                'OCEANOGRAPHY',
                                'PHYSICAL SCIENCE SEM1',
                                'PHYSICAL SCIENCE SEM2',
                                'PHYSICS SEM1',
                                'PHYSICS SEM2',
                                'POLITICAL SYSTEMS',
                                'PRECALCULUS SEM1',
                                'PRECALCULUS SEM2',
                                'PSYCHOLOGY',
                                'Science 6 SEM1',
                                'Science 6 SEM2',
                                'Science 7 SEM1',
                                'Science 7 SEM2',
                                'SCIENCE 8 SEM1',
                                'SCIENCE 8 SEM2',
                                'SCIENCE/GRADE 3',
                                'SCIENCE/GRADE 4',
                                'SCIENCE/GRADE 5',
                                'SOCIAL STUDIES 6 SEM1',
                                'SOCIAL STUDIES 6 SEM2',
                                'SOCIAL STUDIES 7 SEM1',
                                'SOCIAL STUDIES 7 SEM2',
                                'SOCIAL STUDIES 8 SEM1',
                                'SOCIAL STUDIES 8 SEM2',
                                'SOCIAL STUDIES/GRADE 3',
                                'SOCIAL STUDIES/GRADE 4',
                                'SOCIAL STUDIES/GRADE 5',
                                'SOCIOLOGY',
                                'SPANISH I',
                                'SPANISH II',
                                'SPANISH III',
                                'SPANISH IV',
                                'SPANISH NATIVE SPEAK I',
                                'SPANISH NATIVE SPEAK II',
                                'STATISTICAL REASONING',
                                'US HISTORY SEM1',
                                'US HISTORY SEM2',
                                'WORLD GEOGRAPHY SEM1',
                                'WORLD GEOGRAPHY SEM2',
                                'WORLD HISTORY SEM1',
                                'WORLD HISTORY SEM2'
  )) %>% 
  mutate(YEAR = '2018_2019.2') %>% 
  # identify the semester 1 tests
  mutate(YEAR = ifelse(str_detect(assessedsubject, c("SEM1")), "2018_2019.1",
                       YEAR)) %>% 
  mutate(YEAR = ifelse(str_detect(assessedsubject, c("SEM 1")), "2018_2019.1",
                       YEAR)) %>% 
  rename(ID = studentuniqueid,
         CONTENT_AREA = assessedsubject,
         SCALE_SCORE = posttestpercentcomputed)

b <- distr_elig_1718 %>% 
  select(studentid, subjectarea2, post) %>% 
  left_join(testname_mapping,
            by = c("subjectarea2" = "test_name_1718")) %>% 
  # only rows with assessments to be used in 19-20 on would have value
  # so this filter only keeps assessments that will be used in 19-20 on
  filter(!is.na(assessedsubject_1920)) %>% 
  select(-subjectarea2) %>% 
  # make unique by student, test
  # one pair of dupes; simply deleting one to move forward with exploration
  distinct(assessedsubject_1920, studentid, .keep_all = TRUE) %>% 
  rename(ID = studentid,
         CONTENT_AREA = assessedsubject_1920,
         SCALE_SCORE = post) %>% 
  mutate(YEAR = '2017_2018.2') %>% 
  # identify the semester 1 tests
  mutate(YEAR = ifelse(str_detect(CONTENT_AREA, c("SEM1")), "2017_2018.1",
                       YEAR)) %>% 
  mutate(YEAR = ifelse(str_detect(CONTENT_AREA, c("SEM 1")), "2017_2018.2",
                       YEAR)) %>% 
  mutate(ID = as.character(ID))

sgp_file <- bind_rows(a, b)

# creating simulated data ----
library(EnvStats)
set.seed(206)
sgp_file2 <- sgp_file %>% 
  left_join(num_items %>% 
              select(assessedsubject, assessmentmaximumscore),
            by = c("CONTENT_AREA" = "assessedsubject")) %>% 
  # scores I have in data file are percent correct...going to get actual score
  mutate(act_score = rnd_like_xl(SCALE_SCORE / 100 * assessmentmaximumscore, 0)) %>% 
  group_by(CONTENT_AREA) %>% 
  # getting mean and sd of actual score to use for simulation purposes
  mutate(mean_score = rnd_like_xl(mean(act_score),0),
         sd_score = rnd_like_xl(sd(act_score),0),
         ct_scores = sum(!is.na(ID))) %>% 
  # want to keep scores within limits of the actual data
  mutate(sim_score = rnormTrunc(mean = mean_score,
                           sd = sd_score,
                           n = ct_scores,
                           min = min(act_score),
                           max = max(act_score)))

sgp_file3 <- sgp_file2 %>% 
  ungroup() %>% 
  mutate(sim_score2 = trunc(sim_score)) %>% 
  # want to get simulated percent correct using simulated actual score
  mutate(sim_pct_correct = sim_score2 * 100 / assessmentmaximumscore)

# get a random ID for each student
study_id <- data.frame(study_id = sample(1:length(unique(sgp_file3$ID)),
                                         replace = FALSE), ID = unique(sgp_file3$ID))

sgp_file4 <- sgp_file3 %>% 
  left_join(study_id, 
            by = "ID") 

sgp_file5 <- sgp_file4 %>% 
  select(CONTENT_AREA, study_id, sim_pct_correct, YEAR) %>% 
  rename(ID = study_id,
         SCALE_SCORE = sim_pct_correct)

if (savefile == 1){
saveRDS(sgp_file4, paste0("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/exploration/Student Growth Exploration/4_output/datasets/sim_gcps_matched_data_", date, ".rds"))
saveRDS(sgp_file5, paste0("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/exploration/Student Growth Exploration/4_output/datasets/sim_gcps_data_", date, ".rds"))
# write.csv(sgp_file5, paste0("S:/Superintendent/Private/Strategy & Performance/ResearchEvaluation/Shanna Ricketts/Data Analysis/2019-20 Analysis/exploration/Student Growth Exploration/4_output/datasets/sim_gcps_data.csv_", date, ".rds"),
#           row.names = FALSE)
}



