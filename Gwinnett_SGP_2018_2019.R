###################################################
###
### Script for SGP analyses from WIDE data 2018-2019
###
###################################################

### Load SGP package

require(SGP)


### Load data

load("Data/Gwinnett_Data_WIDE_2018_2019.Rdata")


### Calculate SGPs

# create vector of assessments for which SGP will be run
as1 <- sgp_file %>%
  distinct(assessedsubject) %>%
  pull(assessedsubject)


#set up an empty matrix to collect student SGPs
stu_sgp <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(stu_sgp) <- c("ID", "SGP", "SCALE_SCORE_PRIOR",
                       "SCALE_SCORE_PRIOR_STANDARDIZED",
                       "SGP_NORM_GROUP", "GRADE", "assessedsubject_current")


#loop to run through all the course progressions and calculate student SGPs
for(i in as1){
  df_i <- sgp_file %>%
    filter(assessedsubject == i) %>%
    dplyr::select(-assessedsubject)

  assessedsubject_current <- df_i %>%
    select(assessedsubject_current) %>%
    distinct()

  df_i <- df_i %>%
    select(-assessedsubject_current)

  sgp_i <- studentGrowthPercentiles(
    panel.data = df_i,
    sgp.labels = list(my.year = 2019, my.subject = i),
    # percentile cuts not necessary for this purpose
    #percentile.cuts = c(1, 35, 65, 99),
    # grade progression of 4, 5 used for all course progressions to allow code to run
    grade.progression = c(4,5),
    convert.using.loss.hoss = TRUE,
    # goodness of fit outputs saved off for each course progression
    goodness.of.fit.output.format = "PDF"
  )

  results_i <- sgp_i[["SGPercentiles"]][[paste0(i, ".2019")]]

  stu_sgp_i <- cbind(results_i, assessedsubject_current)

  stu_sgp <- bind_rows(stu_sgp, stu_sgp_i)
}


# now that have all student SGPs associate to teachers using tchrs file
sgp_tchrs <- tchrs %>%
  left_join(stu_sgp %>%
              select(ID, SGP, assessedsubject_current),
            by = c("studentuniqueid" = "ID",
                   "assessedsubject" = "assessedsubject_current")) %>%
  group_by(employeeuniqueid) %>%
  # get mean SGP for each teacher; also get count so can implement some n-count limit
  summarise(mean_sgp = mean(SGP, na.rm = TRUE),
            ct_scores = sum(!is.na(SGP)))
