#### recode.R ####

#### block (1) ####

# CDIVISION
data <- data %>% mutate(CDIVISON = recode(CDIVISON,
                                          `1` = "New England",
                                          `2` = "Middle Atlantic", 
                                          `3` = "East North Central",
                                          `4` = "West North Central",
                                          `5` = "South Atlantic",
                                          `6` = "East South Central",
                                          `7` = "West South Central",
                                          `8` = "Mountain",
                                          `9` = "Pacific",
                                          .default = NA_character_))

# METRO
data <- data %>% mutate(METRO = recode(METRO,
                                       `1` = "Metro Area",
                                       `2` = "Metro Area", 
                                       `3` = "Metro Area",
                                       `4` = "Metro Area",
                                       `5` = "Non-Metro Area",
                                       .default = NA_character_))

# WHITECOLLAR
data <- data %>% mutate(WHITECOLLAR = recode(WHITECOLLAR,
                                             `1` = "Y",
                                             `2` = "Y", 
                                             `3` = "N",
                                             `4` = "N",
                                             `5` = "N",
                                             .default = NA_character_))

# NASCENT_ENT
data <- data %>% mutate(NASCENT_ENT = recode(NASCENT_ENT,
                                             `1` = "Y",
                                             `2` = "N", 
                                             .default = NA_character_))

# STARTUP_FOR_EMPLOYER
data <- data %>% mutate(STARTUP_FOR_EMPLOYER = recode(STARTUP_FOR_EMPLOYER,
                                                      `1` = "Y",
                                                      `2` = "N", 
                                                      .default = NA_character_))

# CURRENT_ENT
data <- data %>% mutate(CURRENT_ENT = recode(CURRENT_ENT,
                                             `1` = "Y",
                                             `2` = "N", 
                                             .default = NA_character_))

# STATUS_WAVE_A
data <- data %>% mutate(STATUS_WAVE_A = recode(STATUS_WAVE_A,
                                            `1` = "Active",
                                            .default = "Quit"))

# PROFITABLE
data <- data %>% mutate(PROFITABLE = recode(PROFITABLE,
                                            `1` = "Y",
                                            .default = "N",
                                            .missing = "N"))

# CONSENT1
data <- data %>% mutate(CONSENT1 = recode(CONSENT1,
                                          `1` = "Y",
                                          .default = "N"))

# CONSENT2
data <- data %>% mutate(CONSENT2 = recode(CONSENT2,
                                          `1` = "Y",
                                          .default = "N"))


#### block (2) ####
# GENDER
data <- data %>% mutate(FEMALE = recode(FEMALE,
                                        `1` = "N",
                                        `2` = "Y", 
                                        .default = NA_character_))

# AGE (recode NAs)
data$AGE <- as.numeric(case_when(data$AGE == 98 ~ NA_character_,
                                 data$AGE == 99 ~ NA_character_,
                                 TRUE ~ as.character(data$AGE)))

# HISPANIC
data <- data %>% mutate(HISPANIC = recode(HISPANIC,
                                          `1` = "Y",
                                          `5` = "N", 
                                          .default = NA_character_))

# WHITE
data <- data %>% mutate(WHITE = recode(WHITE,
                                       `1` = "Y",
                                       `5` = "N", 
                                       .default = NA_character_))

# BLACK
data <- data %>% mutate(BLACK = recode(BLACK,
                                       `1` = "Y",
                                       `5` = "N", 
                                       .default = NA_character_))

# NATIVEA
data <- data %>% mutate(NATIVEA = recode(NATIVEA,
                                         `1` = "Y",
                                         `5` = "N", 
                                         .default = NA_character_))

# ASIAN
data <- data %>% mutate(ASIAN = recode(ASIAN,
                                       `1` = "Y",
                                       `5` = "N", 
                                       .default = NA_character_))

# MARITAL
data <- data %>% mutate(MARITAL = recode(MARITAL,
                                         `1` = "Married",
                                         `2` = "Living with a partner", 
                                         `3` = "Separated",
                                         `4` = "Divorced",
                                         `5` = "Widowed",
                                         `6` = "Never married",
                                         .default = NA_character_))

# new variable - HAS_PARTNER
data <- data %>% add_column(HAS_PARTNER = data$MARITAL, .after = "MARITAL")
data <- data %>% mutate(HAS_PARTNER = recode(HAS_PARTNER,
                                             `Married` = "Y",
                                             `Living with a partner` = "Y", 
                                             `Separated` = "N",
                                             `Divorced` = "N",
                                             `Widowed` = "N",
                                             `Never married` = "N",
                                             .default = NA_character_))

# PARTNER_EMPLOYED
data <- data %>% mutate(PARTNER_EMPLOYED = recode(PARTNER_EMPLOYED,
                                                  `1` = "Y",
                                                  `5` = "N", 
                                                  `3` = "N",
                                                  `4` = "N",
                                                  `5` = "N",
                                                  `6` = "N",
                                                  .default = "N"))

# new variable - DUAL_INCOME
data <- data %>% add_column(DUAL_INCOME = NA, .after = "PARTNER_EMPLOYED")
data$DUAL_INCOME <- case_when(data$HAS_PARTNER == "Y" & data$PARTNER_EMPLOYED == "Y" ~ "Y",
                              data$HAS_PARTNER == "Y" & data$PARTNER_EMPLOYED == "N" ~ "N",
                              data$HAS_PARTNER == "N" ~ "N",
                              is.na(data$HAS_PARTNER) ~ NA_character_)

# EDUCATION
data <- data %>% mutate(EDUCATION = recode(EDUCATION,
                                           `1` = "Up to eigth grade",
                                           `2` = "Some high school", 
                                           `3` = "High school degree",
                                           `4` = "Vocational degree",
                                           `5` = "Some college",
                                           `6` = "Community college degree",
                                           `7` = "Bachelors degree",
                                           `8` = "Some graduate training",
                                           `9` = "Masters degree",
                                           `10` = "Law, MD, PHD, EDD",
                                           .default = NA_character_))

# new variable - HAS_COLLEGE_DEG
data <- data %>% add_column(HAS_COLLEGE_DEG = data$EDUCATION, .after = "EDUCATION")
data <- data %>% mutate(HAS_COLLEGE_DEG = recode(HAS_COLLEGE_DEG,
                                                 `Up to eigth grade` = "N",
                                                 `Some high school` = "N", 
                                                 `High school degree` = "N",
                                                 `Vocational degree` = "N",
                                                 `Some college` = "N",
                                                 `Community college degree` = "Y",
                                                 `Bachelors degree` = "Y",
                                                 `Some graduate training` = "Y",
                                                 `Masters degree` = "Y",
                                                 `Law, MD, PHD, EDD` = "Y",
                                                 .default = NA_character_))

# YRS_WORK_EXPER (recode NAs)
data$YRS_WORK_EXPER <- as.numeric(case_when(data$YRS_WORK_EXPER == 98 ~ NA_character_,
                                            data$YRS_WORK_EXPER == 99 ~ NA_character_,
                                            TRUE ~ as.character(data$YRS_WORK_EXPER)))

# MANAG_EXPER
data <- data %>% mutate(MANAG_EXPER = recode(MANAG_EXPER,
                                             `1` = "N",
                                             `2` = "Y", 
                                             `3` = "N",
                                             `4` = "Y",
                                             .default = NA_character_))

# SECOND_JOB
data <- data %>% mutate(SECOND_JOB_WAVE_A = recode(SECOND_JOB_WAVE_A,
                                            `1` = "Y",
                                            `5` = "N", 
                                            .default = NA_character_))

data <- data %>% mutate(SECOND_JOB_WAVE_B = recode(SECOND_JOB_WAVE_B,
                                                   `1` = "Y",
                                                   `5` = "N", 
                                                   .default = NA_character_))

data <- data %>% mutate(SECOND_JOB_WAVE_C = recode(SECOND_JOB_WAVE_C,
                                                   `1` = "Y",
                                                   `5` = "N", 
                                                   .default = NA_character_))

data <- data %>% mutate(SECOND_JOB_WAVE_D = recode(SECOND_JOB_WAVE_D,
                                                   `1` = "Y",
                                                   `5` = "N", 
                                                   .default = NA_character_))

data <- data %>% mutate(SECOND_JOB_WAVE_E = recode(SECOND_JOB_WAVE_E,
                                                   `1` = "Y",
                                                   `5` = "N", 
                                                   .default = NA_character_))

data <- data %>% mutate(SECOND_JOB_WAVE_F = recode(SECOND_JOB_WAVE_F,
                                                   `1` = "Y",
                                                   `5` = "N", 
                                                   .default = NA_character_))

# FULLTIME_SECOND_JOB
data <- data %>% mutate(FULLTIME_SECOND_JOB_WAVE_A = recode(FULLTIME_SECOND_JOB_WAVE_A,
                                                     `1` = "Y",
                                                     `5` = "N", 
                                                     .default = NA_character_))

data <- data %>% mutate(FULLTIME_SECOND_JOB_WAVE_B = recode(FULLTIME_SECOND_JOB_WAVE_B,
                                                            `1` = "Y",
                                                            `5` = "N", 
                                                            .default = NA_character_))

data <- data %>% mutate(FULLTIME_SECOND_JOB_WAVE_C = recode(FULLTIME_SECOND_JOB_WAVE_C,
                                                            `1` = "Y",
                                                            `5` = "N", 
                                                            .default = NA_character_))

data <- data %>% mutate(FULLTIME_SECOND_JOB_WAVE_D = recode(FULLTIME_SECOND_JOB_WAVE_D,
                                                            `1` = "Y",
                                                            `5` = "N", 
                                                            .default = NA_character_))

data <- data %>% mutate(FULLTIME_SECOND_JOB_WAVE_E = recode(FULLTIME_SECOND_JOB_WAVE_E,
                                                            `1` = "Y",
                                                            `5` = "N", 
                                                            .default = NA_character_))

data <- data %>% mutate(FULLTIME_SECOND_JOB_WAVE_F = recode(FULLTIME_SECOND_JOB_WAVE_F,
                                                            `1` = "Y",
                                                            `5` = "N", 
                                                            .default = NA_character_))

# if SECOND_JOB = "N", replace NA with "N"
data$FULLTIME_SECOND_JOB_WAVE_A <- case_when(data$SECOND_JOB_WAVE_A == "N" ~ "N",
                                      TRUE ~ as.character(data$FULLTIME_SECOND_JOB_WAVE_A))

data$FULLTIME_SECOND_JOB_WAVE_B <- case_when(data$SECOND_JOB_WAVE_B == "N" ~ "N",
                                             TRUE ~ as.character(data$FULLTIME_SECOND_JOB_WAVE_B))

data$FULLTIME_SECOND_JOB_WAVE_C <- case_when(data$SECOND_JOB_WAVE_C == "N" ~ "N",
                                             TRUE ~ as.character(data$FULLTIME_SECOND_JOB_WAVE_C))

data$FULLTIME_SECOND_JOB_WAVE_D <- case_when(data$SECOND_JOB_WAVE_D == "N" ~ "N",
                                             TRUE ~ as.character(data$FULLTIME_SECOND_JOB_WAVE_D))

data$FULLTIME_SECOND_JOB_WAVE_E <- case_when(data$SECOND_JOB_WAVE_E == "N" ~ "N",
                                             TRUE ~ as.character(data$FULLTIME_SECOND_JOB_WAVE_E))

data$FULLTIME_SECOND_JOB_WAVE_F <- case_when(data$SECOND_JOB_WAVE_F == "N" ~ "N",
                                             TRUE ~ as.character(data$FULLTIME_SECOND_JOB_WAVE_F))

# FULLTIME_ENT
data <- data %>% mutate(FULLTIME_ENT = recode(FULLTIME_ENT,
                                              `1` = "Y",
                                              `5` = "N", 
                                              .default = NA_character_))

# HRS_HOUSEWORK (recode NAs)
data$HRS_HOUSEWORK_WAVE_A <- as.numeric(case_when(data$HRS_HOUSEWORK_WAVE_A == 998 ~ NA_character_,
                                                  data$HRS_HOUSEWORK_WAVE_A == 999 ~ NA_character_,
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_A)))

data$HRS_HOUSEWORK_WAVE_B <- as.numeric(case_when(data$HRS_HOUSEWORK_WAVE_B == 998 ~ NA_character_,
                                                  data$HRS_HOUSEWORK_WAVE_B == 999 ~ NA_character_,
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_B)))

data$HRS_HOUSEWORK_WAVE_C <- as.numeric(case_when(data$HRS_HOUSEWORK_WAVE_C == 998 ~ NA_character_,
                                                  data$HRS_HOUSEWORK_WAVE_C == 999 ~ NA_character_,
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_C)))

data$HRS_HOUSEWORK_WAVE_D <- as.numeric(case_when(data$HRS_HOUSEWORK_WAVE_D == 998 ~ NA_character_,
                                                  data$HRS_HOUSEWORK_WAVE_D == 999 ~ NA_character_,
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_D)))

data$HRS_HOUSEWORK_WAVE_E <- as.numeric(case_when(data$HRS_HOUSEWORK_WAVE_E == 998 ~ NA_character_,
                                                  data$HRS_HOUSEWORK_WAVE_E == 999 ~ NA_character_,
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_E)))

data$HRS_HOUSEWORK_WAVE_F <- as.numeric(case_when(data$HRS_HOUSEWORK_WAVE_F == 998 ~ NA_character_,
                                                  data$HRS_HOUSEWORK_WAVE_F == 999 ~ NA_character_,
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_F)))


# fix units
data$HRS_HOUSEWORK_WAVE_C <- as.numeric(case_when(data$HH_UNIT_WAVE_C == 1 ~ as.character(data$HRS_HOUSEWORK_WAVE_C*7),
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_C)))

data$HRS_HOUSEWORK_WAVE_D <- as.numeric(case_when(data$HH_UNIT_WAVE_D == 1 ~ as.character(data$HRS_HOUSEWORK_WAVE_D*7),
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_D)))

data$HRS_HOUSEWORK_WAVE_E <- as.numeric(case_when(data$HH_UNIT_WAVE_E == 1 ~ as.character(data$HRS_HOUSEWORK_WAVE_E*7),
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_E)))

data$HRS_HOUSEWORK_WAVE_F <- as.numeric(case_when(data$HH_UNIT_WAVE_F == 1 ~ as.character(data$HRS_HOUSEWORK_WAVE_F*7),
                                                  TRUE ~ as.character(data$HRS_HOUSEWORK_WAVE_F)))


# DISABILITY
data <- data %>% mutate(DISABILITY = recode(DISABILITY,
                                            `1` = "Y",
                                            `5` = "N", 
                                            .default = NA_character_))

# WAS_RETIRED
data <- data %>% mutate(WAS_RETIRED = recode(WAS_RETIRED,
                                             `1` = "Y",
                                             `5` = "N", 
                                             .default = NA_character_))

# NUM_CHILD - ok
# HOUSEHOLD_INCOME - ok

# HOMEOWNER
data <- data %>% mutate(HOMEOWNER = recode(HOMEOWNER,
                                           `1` = "Y",
                                           `5` = "N", 
                                           .default = NA_character_))


#### block (3) ####
# To convert 5-point Likert scale responses to binary responses, I have assigned
# "Y" if the response is stronger than neutral and "N" otherwise. "N" is also
# assigned to "don't know" responses.

# MOTIVE_NO_BETTER
data <- data %>% mutate(MOTIVE_NO_BETTER = recode(MOTIVE_NO_BETTER,
                                                  `2` = "Y",
                                                  .default = "N"))

# MOTIVE_OPPORTUN
data <- data %>% mutate(MOTIVE_OPPORTUN = recode(MOTIVE_OPPORTUN,
                                                 `1` = "Y",
                                                 `2` = "Y",
                                                 `3` = "N",
                                                 `4` = "N",
                                                 `5` = "N",
                                                 `8` = "N",
                                                 .default = NA_character_))
# MOTIVE_STATUS
data <- data %>% mutate(MOTIVE_STATUS = recode(MOTIVE_STATUS,
                                               `1` = "N",
                                               `2` = "N",
                                               `3` = "N",
                                               `4` = "Y",
                                               `5` = "Y",
                                               `8` = "N",
                                               .default = NA_character_))

# MOTIVE_FLEX
data <- data %>% mutate(MOTIVE_FLEX = recode(MOTIVE_FLEX,
                                             `1` = "N",
                                             `2` = "N",
                                             `3` = "N",
                                             `4` = "Y",
                                             `5` = "Y",
                                             `8` = "N",
                                             .default = NA_character_))

# MOTIVE_TRAD
data <- data %>% mutate(MOTIVE_TRAD = recode(MOTIVE_TRAD,
                                             `1` = "N",
                                             `2` = "N",
                                             `3` = "N",
                                             `4` = "Y",
                                             `5` = "Y",
                                             `8` = "N",
                                             .default = NA_character_))

# MOTIVE_RESPECT
data <- data %>% mutate(MOTIVE_RESPECT = recode(MOTIVE_RESPECT,
                                                `1` = "N",
                                                `2` = "N",
                                                `3` = "N",
                                                `4` = "Y",
                                                `5` = "Y",
                                                `8` = "N",
                                                .default = NA_character_))

# MOTIVE_FREEDOM
data <- data %>% mutate(MOTIVE_FREEDOM = recode(MOTIVE_FREEDOM,
                                                `1` = "N",
                                                `2` = "N",
                                                `3` = "N",
                                                `4` = "Y",
                                                `5` = "Y",
                                                `8` = "N",
                                                .default = NA_character_))

# MOTIVE_SECURITY
data <- data %>% mutate(MOTIVE_SECURITY = recode(MOTIVE_SECURITY,
                                                 `1` = "N",
                                                 `2` = "N",
                                                 `3` = "N",
                                                 `4` = "Y",
                                                 `5` = "Y",
                                                 `8` = "N",
                                                 .default = NA_character_))

# MOTIVE_IDOL
data <- data %>% mutate(MOTIVE_IDOL = recode(MOTIVE_IDOL,
                                             `1` = "N",
                                             `2` = "N",
                                             `3` = "N",
                                             `4` = "Y",
                                             `5` = "Y",
                                             `8` = "N",
                                             .default = NA_character_))

# MOTIVE_CHILD
data <- data %>% mutate(MOTIVE_CHILD = recode(MOTIVE_CHILD,
                                              `1` = "N",
                                              `2` = "N",
                                              `3` = "N",
                                              `4` = "Y",
                                              `5` = "Y",
                                              `8` = "N",
                                              .default = NA_character_))

# MOTIVE_INCOME
data <- data %>% mutate(MOTIVE_INCOME = recode(MOTIVE_INCOME,
                                               `1` = "N",
                                               `2` = "N",
                                               `3` = "N",
                                               `4` = "Y",
                                               `5` = "Y",
                                               `8` = "N",
                                               .default = NA_character_))

# MOTIVE_ACHIEV
data <- data %>% mutate(MOTIVE_ACHIEV = recode(MOTIVE_ACHIEV,
                                               `1` = "N",
                                               `2` = "N",
                                               `3` = "N",
                                               `4` = "Y",
                                               `5` = "Y",
                                               `8` = "N",
                                               .default = NA_character_))

# MOTIVE_CREATE
data <- data %>% mutate(MOTIVE_CREATE = recode(MOTIVE_CREATE,
                                               `1` = "N",
                                               `2` = "N",
                                               `3` = "N",
                                               `4` = "Y",
                                               `5` = "Y",
                                               `8` = "N",
                                               .default = NA_character_))

# MOTIVE_RICH
data <- data %>% mutate(MOTIVE_RICH = recode(MOTIVE_RICH,
                                             `1` = "N",
                                             `2` = "N",
                                             `3` = "N",
                                             `4` = "Y",
                                             `5` = "Y",
                                             `8` = "N",
                                             .default = NA_character_))

# MOTIVE_FULFILL
data <- data %>% mutate(MOTIVE_FULFILL = recode(MOTIVE_FULFILL,
                                                `1` = "N",
                                                `2` = "N",
                                                `3` = "N",
                                                `4` = "Y",
                                                `5` = "Y",
                                                `8` = "N",
                                                .default = NA_character_))

# MOTIVE_POWER
data <- data %>% mutate(MOTIVE_POWER = recode(MOTIVE_POWER,
                                              `1` = "N",
                                              `2` = "N",
                                              `3` = "N",
                                              `4` = "Y",
                                              `5` = "Y",
                                              `8` = "N",
                                              .default = NA_character_))


#### block (4) ####

# STARTUP
data <- data %>% mutate(STARTUP = recode(STARTUP,
                                         `1` = "Y",
                                         `2` = "N",
                                         `3` = "N",
                                         `4` = "N",
                                         `5` = "N",
                                         .default = NA_character_))

# INDUSTRY - ok

# TYPE
data <- data %>% mutate(TYPE = recode(TYPE,
                                      `1` = "Retail",
                                      `2` = "Restaurant",
                                      `3` = "Consumer service",
                                      `4` = "Health / Social services",
                                      `5` = "Manufacturing",
                                      `6` = "Construction",
                                      `7` = "Agriculture",
                                      `8` = "Mining",
                                      `9` = "Wholesale",
                                      `10` = "Transportation",
                                      `11` = "Utilities",
                                      `12` = "Communications",
                                      `13` = "Finance",
                                      `14` = "Insurance",
                                      `15` = "Real estate",
                                      `16` = "Consulting",
                                      .default = NA_character_))

# extract DATE_REG from DATE_REG_WAVE_B through F
data <- data %>% add_column(DATE_REG = data$DATE_REG_WAVE_B, .after = "TYPE")
data$DATE_REG <- as.numeric(case_when(is.na(data$DATE_REG_WAVE_F) == FALSE ~ as.character(data$DATE_REG_WAVE_F),
                                      is.na(data$DATE_REG_WAVE_E) == FALSE ~ as.character(data$DATE_REG_WAVE_E),
                                      is.na(data$DATE_REG_WAVE_D) == FALSE ~ as.character(data$DATE_REG_WAVE_D),
                                      is.na(data$DATE_REG_WAVE_C) == FALSE ~ as.character(data$DATE_REG_WAVE_C),
                                      TRUE ~ as.character(data$DATE_REG_WAVE_B)))

# LEGAL_FORM
data <- data %>% mutate(LEGAL_FORM = recode(LEGAL_FORM,
                                            `1` = "Sole proprietorship",
                                            `2` = "General partnership",
                                            `3` = "Limited partnership",
                                            `4` = "LLC",
                                            `5` = "S corporation",
                                            `6` = "General corporation",
                                            .default = NA_character_))

# BUS_PLAN
data <- data %>% mutate(BUS_PLAN = recode(BUS_PLAN,
                                          `1` = "Y",
                                          .default = "N"))

# FIN_PROJ
data <- data %>% mutate(FIN_PROJ = recode(FIN_PROJ,
                                          `1` = "Y",
                                          .default = "N"))

# HITECH
data <- data %>% mutate(HITECH = recode(HITECH,
                                        `1` = "Y",
                                        .default = "N"))

# NUM_OWNERS - ok (95 means 95+)
# NUM_EMPLOYEES - ok

# SOLE_OWNER
data <- data %>% mutate(SOLE_OWNER = recode(SOLE_OWNER,
                                            `1` = "Y",
                                            .default = "N"))

# PERCENT_OWNER (replace NAs because these are sole owners)
data$PERCENT_OWNER <- as.numeric(case_when(data$SOLE_OWNER == "Y" ~ "100",
                                           TRUE ~ as.character(data$PERCENT_OWNER)))

# IS_STARTUP_MANAGER (replace NAs because these are sole owners)
data <- data %>% mutate(IS_STARTUP_MANAGER = recode(IS_STARTUP_MANAGER,
                                                    `1` = "Y",
                                                    .default = "N"))
data$IS_STARTUP_MANAGER <- case_when(data$SOLE_OWNER == "Y" ~ "Y",
                                     TRUE ~ as.character(data$IS_STARTUP_MANAGER))

# YR1_REV_GUESS - ok
# YR6_REV_GUESS - ok

# YR1_HAS_REV
data <- data %>% mutate(YR1_HAS_REV = recode(YR1_HAS_REV,
                                            `1` = "Y",
                                            `5` = "N",
                                            .default = NA_character_))


#### block (5) ####
# if YRX_SAMEVAL = 1, then extract startup value in YRX from previous year

# Wave B (if same value as last year, replace)
data$VAL_WAVE_B <- case_when(data$SAMEVAL_WAVE_B == 1 ~ data$VAL_WAVE_A,
                          TRUE ~ data$VAL_WAVE_B)

# Wave C
data$VAL_WAVE_C <- case_when(data$SAMEVAL_WAVE_C == 1 ~ data$VAL_WAVE_B,
                          TRUE ~ data$VAL_WAVE_C )

# Wave D
data$VAL_WAVE_D <- case_when(data$SAMEVAL_WAVE_D == 1 ~ data$VAL_WAVE_C,
                          TRUE ~ data$VAL_WAVE_D)

# Wave E
data$VAL_WAVE_E <- case_when(data$SAMEVAL_WAVE_E == 1 ~ data$VAL_WAVE_D,
                          TRUE ~ data$VAL_WAVE_E)

# Wave F
data$VAL_WAVE_F <- case_when(data$SAMEVAL_WAVE_F == 1 ~ data$VAL_WAVE_E,
                          TRUE ~ data$VAL_WAVE_F)

# new variable - YR6_REV_GROWTH_GUESS
data <- data %>% add_column(YR6_REV_GROWTH_GUESS = NA, .after = "YR6_REV_GUESS")
data$YR6_REV_GROWTH_GUESS <- as.numeric(case_when(data$YR1_REV_GUESS == 0 ~ NA_character_,
                                                  TRUE ~ as.character((data$YR6_REV_GUESS - data$YR1_REV_GUESS)/data$YR1_REV_GUESS)))

# GROWTH_DRIVENA
#
data <- data %>% mutate(MAX_GROWTH_DRIVEN_WAVE_A = recode(MAX_GROWTH_DRIVEN_WAVE_A,
                                             `1` = "Y",
                                             `5` = "N",
                                             .default = NA_character_))

# GROWTH_DRIVENB
data <- data %>% mutate(MAX_GROWTH_DRIVEN_WAVE_B = recode(MAX_GROWTH_DRIVEN_WAVE_B,
                                                `1` = "Y",
                                                `5` = "N",
                                                .default = NA_character_))

# GROWTH_DRIVENC
data <- data %>% mutate(MAX_GROWTH_DRIVEN_WAVE_C = recode(MAX_GROWTH_DRIVEN_WAVE_C,
                                                `1` = "Y",
                                                `5` = "N",
                                                .default = NA_character_))

# GROWTH_DRIVEND
data <- data %>% mutate(MAX_GROWTH_DRIVEN_WAVE_D = recode(MAX_GROWTH_DRIVEN_WAVE_D,
                                                `1` = "Y",
                                                `5` = "N",
                                                .default = NA_character_))

# GROWTH_DRIVENE
data <- data %>% mutate(MAX_GROWTH_DRIVEN_WAVE_E = recode(MAX_GROWTH_DRIVEN_WAVE_E,
                                                `1` = "Y",
                                                `5` = "N",
                                                .default = NA_character_))

# GROWTH_DRIVENF
data <- data %>% mutate(MAX_GROWTH_DRIVEN_WAVE_F = recode(MAX_GROWTH_DRIVEN_WAVE_F,
                                                `1` = "Y",
                                                `5` = "N",
                                                .default = NA_character_))

#### block (6) ####

# recode firm status as entrepreneur status (so new firm -> active)

# YR2_STATUS
data <- data %>% mutate(STATUS_WAVE_B = recode(STATUS_WAVE_B,
                                            `1` = "Active",
                                            `2` = "Active",
                                            `3` = "Quit",
                                            .default = NA_character_))

# YR3_STATUS
data <- data %>% mutate(STATUS_WAVE_C = recode(STATUS_WAVE_C,
                                            `1` = "Active",
                                            `2` = "Active",
                                            `3` = "Quit",
                                            .default = NA_character_))

# YR4_STATUS
data <- data %>% mutate(STATUS_WAVE_D = recode(STATUS_WAVE_D,
                                            `1` = "Active",
                                            `2` = "Active",
                                            `3` = "Quit",
                                            .default = NA_character_))

# YR5_STATUS
data <- data %>% mutate(STATUS_WAVE_E = recode(STATUS_WAVE_E,
                                            `1` = "Active",
                                            `2` = "Active",
                                            `3` = "Quit",
                                            .default = NA_character_))

# YR6_STATUS
data <- data %>% mutate(STATUS_WAVE_F = recode(STATUS_WAVE_F,
                                            `1` = "Active",
                                            `2` = "Active",
                                            `3` = "Quit",
                                            .default = NA_character_))

# if respondent answered YRX_STATUS = "Quit", carry over "Quit" to YRX'_STATUS in all following years
data$STATUS_WAVE_C <- case_when(data$STATUS_WAVE_B == "Quit" ~ "Quit",
                             TRUE ~ as.character(data$STATUS_WAVE_C ))

data$STATUS_WAVE_D <- case_when(data$STATUS_WAVE_C == "Quit" ~ "Quit",
                             TRUE ~ as.character(data$STATUS_WAVE_D))

data$STATUS_WAVE_E <- case_when(data$STATUS_WAVE_D == "Quit" ~ "Quit",
                             TRUE ~ as.character(data$STATUS_WAVE_E))

data$STATUS_WAVE_F <- case_when(data$STATUS_WAVE_E == "Quit" ~ "Quit",
                             TRUE ~ as.character(data$STATUS_WAVE_F))


# YR2_WHY_QUIT1
data <- data %>% mutate(WHY_QUIT1_WAVE_B = recode(WHY_QUIT1_WAVE_B,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# YR3_WHY_QUIT1
data <- data %>% mutate(WHY_QUIT1_WAVE_C = recode(WHY_QUIT1_WAVE_C,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# YR4_WHY_QUIT1
data <- data %>% mutate(WHY_QUIT1_WAVE_D = recode(WHY_QUIT1_WAVE_D,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# YR5_WHY_QUIT1
data <- data %>% mutate(WHY_QUIT1_WAVE_E = recode(WHY_QUIT1_WAVE_E,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# YR6_WHY_QUIT1
data <- data %>% mutate(WHY_QUIT1_WAVE_F = recode(WHY_QUIT1_WAVE_F,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# Y2_WHY_QUIT2
data <- data %>% mutate(WHY_QUIT2_WAVE_B = recode(WHY_QUIT2_WAVE_B,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# Y3_WHY_QUIT2
data <- data %>% mutate(WHY_QUIT2_WAVE_C = recode(WHY_QUIT2_WAVE_C,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# Y4_WHY_QUIT2
data <- data %>% mutate(WHY_QUIT2_WAVE_D = recode(WHY_QUIT2_WAVE_D,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# Y5_WHY_QUIT2
data <- data %>% mutate(WHY_QUIT2_WAVE_E = recode(WHY_QUIT2_WAVE_E,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# YR6_WHY_QUIT2
data <- data %>% mutate(WHY_QUIT2_WAVE_F = recode(WHY_QUIT2_WAVE_F,
                                               `10` = "Financing issue",
                                               `11` = "Financing issue",
                                               `12` = "Financing issue",
                                               `19` = "Financing issue",
                                               
                                               `20` = "Revenue issue",
                                               `21` = "Revenue issue",
                                               `22` = "Revenue issue",
                                               `23` = "Revenue issue",
                                               `29` = "Revenue issue",
                                               
                                               `30` = "Business issue",
                                               `31` = "Business issue",
                                               `32` = "Business issue",
                                               `33` = "Business issue",
                                               `34` = "Business issue",
                                               `39` = "Business issue",
                                               
                                               `40` = "Other professional opportunity",
                                               `41` = "Other professional opportunity",
                                               `42` = "Other professional opportunity",
                                               `49` = "Other professional opportunity",
                                               
                                               `90` = "Personal issue",
                                               `91` = "Personal issue",
                                               `92` = "Personal issue",
                                               `93` = "Personal issue",
                                               `95` = "Personal issue",
                                               
                                               .default = NA_character_))

# the variables QUIT_FOR_PERSONAL and QUIT_FOR_OTHER_JOB will be
# defined in the construction of the 3-year and 5-year comparison datasets

data <- data %>%
  add_column(QUIT_GROWTH_WAVE_A = NA, .after = "MAX_GROWTH_DRIVEN_WAVE_A") %>%
  add_column(QUIT_GROWTH_WAVE_B = NA, .after = "MAX_GROWTH_DRIVEN_WAVE_B") %>%
  add_column(QUIT_GROWTH_WAVE_C = NA, .after = "MAX_GROWTH_DRIVEN_WAVE_C") %>%
  add_column(QUIT_GROWTH_WAVE_D = NA, .after = "MAX_GROWTH_DRIVEN_WAVE_D") %>%
  add_column(QUIT_GROWTH_WAVE_E = NA, .after = "MAX_GROWTH_DRIVEN_WAVE_E") %>%
  add_column(QUIT_GROWTH_WAVE_F = NA, .after = "MAX_GROWTH_DRIVEN_WAVE_F")

data$QUIT_GROWTH_WAVE_B <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_A == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_B == "N") ~ "Y",
                                  (data$MAX_GROWTH_DRIVEN_WAVE_A == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_B == "Y") ~ "N",
                                  TRUE ~ NA_character_)

data$QUIT_GROWTH_WAVE_C <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_B == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_C == "N") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_B == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_C == "Y") ~ "N",
                                     TRUE ~ NA_character_)

data$QUIT_GROWTH_WAVE_D <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_C == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_D == "N") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_C == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_D == "Y") ~ "N",
                                     TRUE ~ NA_character_)

data$QUIT_GROWTH_WAVE_E <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_D == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_E == "N") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_D == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_E == "Y") ~ "N",
                                     TRUE ~ NA_character_)

data$QUIT_GROWTH_WAVE_F <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_E == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_F == "N") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_E == "Y" & data$MAX_GROWTH_DRIVEN_WAVE_F == "Y") ~ "N",
                                     TRUE ~ NA_character_)

# --
data <- data %>%
  add_column(CHOOSE_GROWTH_WAVE_A = NA, .after = "QUIT_GROWTH_WAVE_F") %>%
  add_column(CHOOSE_GROWTH_WAVE_B = NA, .after = "QUIT_GROWTH_WAVE_F") %>%
  add_column(CHOOSE_GROWTH_WAVE_C = NA, .after = "QUIT_GROWTH_WAVE_F") %>%
  add_column(CHOOSE_GROWTH_WAVE_D = NA, .after = "QUIT_GROWTH_WAVE_F") %>%
  add_column(CHOOSE_GROWTH_WAVE_E = NA, .after = "QUIT_GROWTH_WAVE_F") %>%
  add_column(CHOOSE_GROWTH_WAVE_F = NA, .after = "QUIT_GROWTH_WAVE_F")

data$CHOOSE_GROWTH_WAVE_B <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_A == "N" & data$MAX_GROWTH_DRIVEN_WAVE_B == "Y") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_A == "N" & data$MAX_GROWTH_DRIVEN_WAVE_B == "N") ~ "N",
                                     TRUE ~ NA_character_)

data$CHOOSE_GROWTH_WAVE_C <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_B == "N" & data$MAX_GROWTH_DRIVEN_WAVE_C == "Y") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_B == "N" & data$MAX_GROWTH_DRIVEN_WAVE_C == "N") ~ "N",
                                     TRUE ~ NA_character_)

data$CHOOSE_GROWTH_WAVE_D <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_C == "N" & data$MAX_GROWTH_DRIVEN_WAVE_D == "Y") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_C == "N" & data$MAX_GROWTH_DRIVEN_WAVE_D == "N") ~ "N",
                                     TRUE ~ NA_character_)

data$CHOOSE_GROWTH_WAVE_E <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_D == "N" & data$MAX_GROWTH_DRIVEN_WAVE_E == "Y") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_D == "N" & data$MAX_GROWTH_DRIVEN_WAVE_E == "N") ~ "N",
                                     TRUE ~ NA_character_)

data$CHOOSE_GROWTH_WAVE_F <- case_when((data$MAX_GROWTH_DRIVEN_WAVE_E == "N" & data$MAX_GROWTH_DRIVEN_WAVE_F == "Y") ~ "Y",
                                     (data$MAX_GROWTH_DRIVEN_WAVE_E == "N" & data$MAX_GROWTH_DRIVEN_WAVE_F == "N") ~ "N",
                                     TRUE ~ NA_character_)


#end


