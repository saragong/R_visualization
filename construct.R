#### construct.R ####
#### construct full, 3-year, and 5-year comparison datasets

#### all_waves: build full panel dataset ####

# remove respondents who don't fit our study
# use screening criteria from PSED: see Appendix A, "ent0403.pdf"
all_waves <- subset(data, 
                    (! STARTUP_FOR_EMPLOYER == "Y") # select independent entrepreneurs
                    & (CONSENT1 == "Y" | CONSENT2 == "Y")
                    & (STARTUP == "Y") # exclude takeovers and franchises
                    & (IS_STARTUP_MANAGER == "Y") # select entrepreneurs with a managerial role
                    ) 

# remove unnecessary columns already used in the data cleaning process
variables_all_waves <- names(data)[! names(data) %in% c("CONSENT1",
                                                        "CONSENT2",
                                                        "STARTUP_FOR_EMPLOYER",
                                                        "STARTUP",
                                                        "IS_STARTUP_MANAGER",
                                                        "PARTNER_EMPLOYED",
                                                        "DATE_REG_WAVE_B",
                                                        "DATE_REG_WAVE_C", 
                                                        "DATE_REG_WAVE_D", 
                                                        "DATE_REG_WAVE_E", 
                                                        "DATE_REG_WAVE_F", 
                                                        "SAMEVAL_WAVE_B", 
                                                        "SAMEVAL_WAVE_C", 
                                                        "SAMEVAL_WAVE_D", 
                                                        "SAMEVAL_WAVE_E", 
                                                        "SAMEVAL_WAVE_F")]

# select variables and convert to numeric for regressions
all_waves <- select(all_waves, all_of(variables_all_waves)) %>% 
  mutate_all(funs(str_replace(., "Y", "1"))) %>% 
  mutate_all(funs(str_replace(., "N", "0"))) %>%
  transmute_at(vars("SAMPID",  # replace with mutate_at to keep other variables
                    "WHITECOLLAR",
                    "NASCENT_ENT",
                    "CURRENT_ENT",
                    "PROFITABLE",
                    "AGE",
                    "FEMALE",
                    "HISPANIC",
                    "WHITE",
                    "BLACK",
                    "NATIVEA",
                    "ASIAN",
                    "HAS_PARTNER",
                    "DUAL_INCOME",
                    "HAS_COLLEGE_DEG",
                    "YRS_WORK_EXPER",
                    "MANAG_EXPER",
                    
                    "SECOND_JOB_WAVE_A",
                    "SECOND_JOB_WAVE_B",
                    "SECOND_JOB_WAVE_C",
                    "SECOND_JOB_WAVE_D",
                    "SECOND_JOB_WAVE_E",
                    "SECOND_JOB_WAVE_F",
                    
                    "FULLTIME_SECOND_JOB_WAVE_A",
                    "FULLTIME_SECOND_JOB_WAVE_B",
                    "FULLTIME_SECOND_JOB_WAVE_C",
                    "FULLTIME_SECOND_JOB_WAVE_D",
                    "FULLTIME_SECOND_JOB_WAVE_E",
                    "FULLTIME_SECOND_JOB_WAVE_F",
                    
                    "FULLTIME_ENT",
                    "DISABILITY",
                    "WAS_RETIRED",
                    "NUM_CHILD",
                    "HOUSEHOLD_INCOME",
                    "HOMEOWNER",
                    contains("HRS_HOUSEWORK"),
                    contains("MOTIVE"),
                    "BUS_PLAN",
                    "FIN_PROJ",
                    "HITECH",
                    "NUM_OWNERS",
                    "NUM_EMPLOYEES",
                    "SOLE_OWNER",
                    "PERCENT_OWNER",
                    contains("REV"),
                    contains("VAL"),
                    contains("MAX_GROWTH_DRIVEN"),
                    contains("QUIT_GROWTH"),
                    contains("CHOOSE_GROWTH")),
               
               as.numeric)


# end  