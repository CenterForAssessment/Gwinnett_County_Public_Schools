################################################################################
###                                                                          ###
###      SGP Configurations code for 2019 ALL TESTS                          ###
###                                                                          ###
################################################################################

source("Gwinnett_Course_Progressions_2018_2019.R")

### could maybe just select the top one in each progression to do
# would include rules here about which ones should be run
# sample size restrictions; number of progressions etc.

df_to_run <- df2 %>% 
  arrange(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, desc(pct)) %>% 
  filter(!is.na(immediate_prior)) %>% 
  group_by(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0, YEAR) %>% 
  mutate(s = as.integer(row_number() == 1L)) %>% 
  ungroup() %>% 
  filter(s == 1) %>% 
  filter(ct>=1200) 
# %>% 
#   # 04/10/2020 going to try out a small subset to see if I can get things to work
#   filter(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0 %in% c("MATHEMATICS.03",
#                                                    "MATHEMATICS.04",
#                                                    "MATHEMATICS.05"))

tests_spring <- df_to_run %>% 
  filter(YEAR == "2018_2019.2") %>% 
  ungroup() %>% 
  distinct(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0) %>% 
  filter(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0 != "AP_HUMAN_GEOGRAPHY.EOCT")%>% 
  #AP_HUMAN_GEOGRAPHY.EOCT was breaking it by throwing a Singular design matrix error
  pull(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0) 

tests_fall <- df_to_run %>% 
  filter(YEAR == "2018_2019.1") %>% 
  ungroup() %>% 
  distinct(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0) %>% 
  pull(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0)

# create empty list
config_list <- list()
config_list_spring <- list()
config_list_fall <- list()

# Spring 2018-2019 Tests
for (i in tests_spring){
  a.2 <-  df_to_run %>% 
    filter(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0 == i) %>% 
    filter(YEAR == "2018_2019.2") 
  
    content.2 <- a.2$CONTENT
    content_prior.2 <- a.2$content_prior
  year.2 <- a.2$YEAR
  year_prior.2 <- a.2$immediate_prior_year
  grade.2 <- a.2$GRADE
  grade_prior.2 <- a.2$grade_prior
  #s.2 <- a_2$s
  
  i.j.config <- list(
      i.j.config.2 <- list(
      sgp.content.areas = c(content_prior.2, content.2),
      sgp.panel.years = c(year_prior.2, year.2),
      sgp.grade.sequences = list(
        c(grade_prior.2, grade.2)
      )
      ,
      sgp.norm.group.preference = 1)
    
  )
  
  config_list <- append(config_list, i.j.config)
}
config_list_fall <- config_list

config_list <- list()
# Fall 2018-2019 Tests
for (i in tests_fall){
a.1 <-  df_to_run %>%
  filter(CONTENT_AREA_by_GRADE_PRIOR_YEAR.0 == i) %>% 
  filter(YEAR == "2018_2019.1") 
  content.1 <- a.1$CONTENT
content_prior.1 <- a.1$content_prior
year.1 <- a.1$YEAR
year_prior.1 <- a.1$immediate_prior_year
grade.1 <- a.1$GRADE
grade_prior.1 <- a.1$grade_prior
#s.1 <- a_1$s

i.j.config <- list(
  i.j.config.1 <- list(
    sgp.content.areas = c(content_prior.1, content.1),
    sgp.panel.years = c(year_prior.1, year.1),
    sgp.grade.sequences = list(c(grade_prior.1, grade.1)
    )
    ,
    sgp.norm.group.preference = 2)
)
config_list <- append(config_list, i.j.config)
}

config_list_spring <- config_list
config_list <- list()
config_list <- append(config_list_fall,
                      config_list_spring)
  
# older versions
# MATHEMATICS_2018_2019.2.config <- list(
# MATHEMATICS.2018_2019.1 <- list(
#   sgp.content.areas = c("MATHEMATICS", "MATHEMATICS"),
#   sgp.panel.years = c("2017_2018.2", "2018_2019.2"),
#   sgp.grade.sequences = list(#c("K", "1"),
#                              c("1", "2"),
#                              c("2", "3"),
#                              c("3", "4"),
#                              c("4", "5"),
#                              c("5", "6"),
#                              c("6", "7")
#                              #,
#                              #c("7", "EOCT")
#                              ),
#   sgp.norm.group.preference = 1),
# 
# #Fall to Spring (same year)
# MATHEMATICS.2018_2019.2 <- list(
#   sgp.content.areas = c("MATHEMATICS", "MATHEMATICS"),
#   sgp.panel.years = c("2018_2019.1", "2018_2019.2"),
#   sgp.grade.sequences = list(c("6", "6"),
#                              c("7", "7"),
#                              c("8", "8")
#                              # ,
#                              # c("EOCT", "EOCT")
#                              ),
#   sgp.norm.group.preference = 2),
# 
# #Spring to Fall (cross years)
# MATHEMATICS.2018_2019.3 <- list(
#   sgp.content.areas = c("MATHEMATICS", "MATHEMATICS"),
#   sgp.panel.years = c("2017_2018.2", "2018_2019.1"),
#   sgp.grade.sequences = list(c("5", "6"),
#                              c("6", "7"),
#                              c("7", "8")
#                              #,
#                              #c("8", "EOCT"),
#                              #c("EOCT", "EOCT")
#                              ),
#   sgp.norm.group.preference = 3)
# )

# MATHEMATICS_2018_2019.2.config <- list(
# 	###   'Traditional' Spring to Spring analyses
# 	MATHEMATICS.2019_2020.1 = list(
#   sgp.content.areas=c("MATHEMATICS", "MATHEMATICS"),
# 		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
# 		sgp.grade.sequences=list(c("1", "2"), c("2", "3"), c("3", "4"), c("4", "5"), c("5", "6")),
# 		sgp.norm.group.preference=1),
# 
# 	###   Fall to Spring (same grade) analyses
# 	MATHEMATICS.2018_2019.2 = list(
# 		sgp.content.areas=c("MATHEMATICS", "MATHEMATICS"),
# 		sgp.panel.years=c("2018_2019.1", "2018_2019.2"),
# 		sgp.grade.sequences=list(c("6", "6"), c("7", "7"), c("8", "8")),
# 		sgp.norm.group.preference=2)
# )
# 
# ALGEBRA_I_2018_2019.2.config <- list(
# 	###   Fall to Spring analyses
# 	ALGEBRA_I.2018_2019.2 = list(
# 		sgp.content.areas=c("ALGEBRA_I", "ALGEBRA_I"),
# 		sgp.panel.years=c("2018_2019.1", "2018_2019.2"),
# 		sgp.grade.sequences=list(c("EOCT", "EOCT")),
# 		sgp.norm.group.preference=1),
# 
# 	ALGEBRA_I.2018_2019.2 = list(
# 		sgp.content.areas=c("ACC_MATHEMATICS", "ALGEBRA_I"),
# 		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
# 		sgp.grade.sequences=list(c("7", "EOCT")),
# 		sgp.norm.group.preference=2),
# 
# 	ALGEBRA_I.2018_2019.2 = list(
# 		sgp.content.areas=c("MATHEMATICS", "ALGEBRA_I"),
# 		sgp.panel.years=c("2017_2018.2", "2018_2019.2"),
# 		sgp.grade.sequences=list(c("7", "EOCT")),
# 		sgp.norm.group.preference=3)
# )
