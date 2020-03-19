#########################################################
###
### Script to create WIDE file
###
#########################################################

### Load tidyverse

require(tidyverse)


#creating SGP file in wide format
#including any pre-SGP filters

SGP_Data_WIDE_2018_2019 <- sgp1819_datafile %>% 
  # file a1 lists the course progressions that should be included in SGPs based on pre-defined rules
  left_join(a1 %>%
              select(assessedsubject_current, assessedsubject_prior, include),
            by = c("assessedsubject_current", "assessedsubject_prior")) %>%
  filter(include == "yes") %>%
  # grade progression of 4, 5 used for all course progression to allow code to run
    mutate(grade_prior = 4,
         grade_current = 5)  %>%
  mutate(assessedsubject = paste0(assessedsubject_current, "_", assessedsubject_prior
  ))%>%
  mutate(assessedsubject = str_replace_all(assessedsubject, " ", "_")) %>%
  mutate(assessedsubject = toupper(assessedsubject)) %>%
  select(assessedsubject, assessedsubject_current, studentuniqueid, grade_prior, grade_current,
         post_prior, post_current, ) %>%
  distinct() %>%
  group_by(assessedsubject, studentuniqueid) %>%
  # if student has multiple scores, use higher; rarely happens (~6 cases)
  mutate(post_prior = max(post_prior)) %>%
  ungroup() %>%
  distinct() %>%
  rename(ID = studentuniqueid)


  ### Save results

  save(SGP_Data_WIDE_2018_2019, file="Data/SGP_Data_WIDE_2018_2019.Rdata")
