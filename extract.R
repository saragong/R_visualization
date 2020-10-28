##### extract.R ####
# extract appropriate columns. note that "cdbk.pdf" contains further information
# important selection variables are marked *
# important control variables are marked with ***
# the outcome variables of interest are (1) startup value and (2) entrepreneur attrition

data <- select(raw_data,
               
               # block (1) - screener questions used for the selection of the panel (collected in 2004)
               SAMPID, # respondent ID
               CDIVISON, # geographical census division
               METRO, # metropolitan status
               WHITECOLLAR = QS2_1, # occupation - white collar or blue collar? *
               NASCENT_ENT = QFF1A, # trying to start business? * BSTART
               STARTUP_FOR_EMPLOYER = QFF1B, # trying to start business on behalf of employer? * BJOBST
               CURRENT_ENT = QFF1C, # current businessowner? * OWNMGE
               STATUS_WAVE_A = QFF2, # startup activity in last 12 months?
               PROFITABLE = QFF5, # has monthly revenue exceeded monthly costs for 6 months? indicates not startup
               CONSENT1 = QFF10, # consent - first ask
               CONSENT2 = QFF11, # consent - second ask
               
               
               # block (2) - entrepreneur demographics (collected in 2005 wave)
               FEMALE = AH1_1, # gender
               AGE = AH2_1, # age
               HISPANIC = AH3_1, # hispanic?
               WHITE = AH4a_1, # race: white?
               BLACK = AH4b_1, # race: black?
               NATIVEA = AH4d_1, # race: native american?
               ASIAN = AH4e_1, # race: asian?
               MARITAL = AH5_1, # marital status
               PARTNER_EMPLOYED = AH30_1, # if you have a partner, are they employed (outside the startup)?
               EDUCATION = AH6_1, # education level?
               YRS_WORK_EXPER = AH20_1, # total years of work experience?
               MANAG_EXPER = AX12, # job seniority in previous job
               # HRS_DEVOTED = AH13_1, # total hours devoted to business?
               
               SECOND_JOB_WAVE_A = AH15_1, # second job? (employed by other)
               SECOND_JOB_WAVE_B = BX19,
               SECOND_JOB_WAVE_C = CX19,
               SECOND_JOB_WAVE_D = DX19,
               SECOND_JOB_WAVE_E = EX19,
               SECOND_JOB_WAVE_F = FX19,
               
               FULLTIME_SECOND_JOB_WAVE_A = AH16_1, # second job is full-time or part-time?
               FULLTIME_SECOND_JOB_WAVE_B = BX20,
               FULLTIME_SECOND_JOB_WAVE_C = CX20,
               FULLTIME_SECOND_JOB_WAVE_D = DX20,
               FULLTIME_SECOND_JOB_WAVE_E = EX20,
               FULLTIME_SECOND_JOB_WAVE_F = FX20,
               
               FULLTIME_ENT = AH17_1, # working full-time on startup?
               
               HRS_HOUSEWORK_WAVE_A = AX3, # hours per week spend on childcare / housework?
               HRS_HOUSEWORK_WAVE_B = BX3,
               HRS_HOUSEWORK_WAVE_C = CX3,
               HRS_HOUSEWORK_WAVE_D = DX3,
               HRS_HOUSEWORK_WAVE_E = EX3,
               HRS_HOUSEWORK_WAVE_F = FX3,
               
               HH_UNIT_WAVE_C = CX3a, # units of time for above
               HH_UNIT_WAVE_D = DX3a,
               HH_UNIT_WAVE_E = EX3a,
               HH_UNIT_WAVE_F = FX3a,
               
               
               DISABILITY = AX6, # disability?
               WAS_RETIRED = AX7, # ever retired?
               NUM_CHILD = AZ11, # number of children under 17?
               HOUSEHOLD_INCOME = AZ14, # total household income in last year?
               HOMEOWNER = AZ28, # homeowner?
               
               
               # block (3) - respondents' stated motivations for starting business (collected in 2005 wave)
               MOTIVE_NO_BETTER = AT6, # involved in business for opportunity or for lack of better choice?
               MOTIVE_OPPORTUN = AY4, # starting this new business is more desirable than other opportunities?
               MOTIVE_STATUS = AW1, # motivated by status
               MOTIVE_FLEX = AW2, # motivated by life flexibility
               MOTIVE_TRAD = AW3, # motivated by carrying on a family tradition
               MOTIVE_RESPECT = AW4, # motivated by respect
               MOTIVE_FREEDOM = AW5, # motivated by freedom
               MOTIVE_SECURITY = AW6, # motivated by financial security
               MOTIVE_IDOL = AW7, # motivated by an admired person
               MOTIVE_CHILD = AW8, # motivated by building a business for children
               MOTIVE_INCOME = AW9, # motivated by income
               MOTIVE_ACHIEV = AW10, # motivated by achievement
               MOTIVE_CREATE = AW11, # motivated by creativity
               MOTIVE_RICH = AW12, # motivated by riches
               MOTIVE_FULFILL = AW13, # motivated by fulfillment
               MOTIVE_POWER = AW14, # motivated by power
               # AA2a, AA2b, AA5a, AA5b, # motivation for starting business? (these questions seems a bit redundant)
               
               
               # block (4) - startup background (mostly collected in 2005 wave)
               STARTUP = AA10, # is independent new business? *
               INDUSTRY = AA1, # initial industry of business, as 2002 SIC 4-digit codes *
               TYPE = AB1,  # type of business: retail, finance, consulting, etc.
               DATE_REG_WAVE_B = BA11b, # date of registration of business name - good proxy for startup age
               DATE_REG_WAVE_C = CA11b, # this question was asked in each year, so a business might not have
               DATE_REG_WAVE_D = DA11b, # been registered in 2005, but was later registered
               DATE_REG_WAVE_E = EA11b, 
               DATE_REG_WAVE_F = FA11b, 
               
               # IDEA_ORIGIN = AA9, # origins of business idea?
               LEGAL_FORM = AC1, # legal form of business
               BUS_PLAN = AD1, # has business plan?
               FIN_PROJ = AD26, # has financial projections?
               HITECH = AS6, # is hi-tech?
               NUM_OWNERS = AG2, # number of owners?
               NUM_EMPLOYEES = AT4, # number of employees (not counting owners)?
               SOLE_OWNER = AG1, # sole ownership?
               PERCENT_OWNER = AG6_1, # percent of your ownership?
               IS_STARTUP_MANAGER = AH19_1, # if >1 owner, what is your primary role? *
               YR1_REV_GUESS = AT2, # expected annual revenue during the first year of operation?
               YR6_REV_GUESS = AT3, # expected annual revenue during the fifth year of operation?
               YR1_HAS_REV = AE13,  # business already received revenue?
               # AB9, BB9, CB9, EB9, FB9, # business has physical offices? *
               # AD13, BD13, CD13, DD13, ED13, FD13, #has filed patent, copyright, or trademark? ***
               # AE1, BE1, CE1, DE1, EE1, FE1, # seeking outside funding?
               # AE3, BE3, CE3, DE3, EE3, FE3, # received outside funding?
               
               
               # block (5) - startup growth (collected in each wave)
               # this info is provided iff the startup was registered as a legal entity
               VAL_WAVE_A = AR22, VAL_WAVE_B = BR22, VAL_WAVE_C = CR22, VAL_WAVE_D = DR22, VAL_WAVE_E = ER22, VAL_WAVE_F = FR22, # startup valuation (net market value)
               SAMEVAL_WAVE_B = BR22c, SAMEVAL_WAVE_C = CR22c, SAMEVAL_WAVE_D = DR22c, SAMEVAL_WAVE_E = ER22c, SAMEVAL_WAVE_F = FR22c, # same startup valuation as last year? (leads to NA above)
               MAX_GROWTH_DRIVEN_WAVE_A = AT1, # size preference, wave A
               MAX_GROWTH_DRIVEN_WAVE_B = BT1, 
               MAX_GROWTH_DRIVEN_WAVE_C = CT1,
               MAX_GROWTH_DRIVEN_WAVE_D = DT1,
               MAX_GROWTH_DRIVEN_WAVE_E = ET1,
               MAX_GROWTH_DRIVEN_WAVE_F = FT1,
               # YR2_REV = BV2, YR3_REV = CV2, YR4_REV = DV2, YR5_REV = EV2, YR6_REV = FV2, # annual revenue annual revenue during the first year of operation?
               # YR2_EXP = BV11, YR3_EXP = CV11, YR4_EXP = DV11, YR5_EXP = EV11, YR6_EXP = FV11, # annual expenses during first year of operation?
               
               
               # block (6) - entrepreneur attrition (collected in each wave)
               STATUS_WAVE_B = BA50, STATUS_WAVE_C = CA50, STATUS_WAVE_D = DA50, STATUS_WAVE_E = EA50, STATUS_WAVE_F = FA50, # startup status? (new, active, quit)
               WHY_QUIT1_WAVE_B= BE52, WHY_QUIT1_WAVE_C = CE52, WHY_QUIT1_WAVE_D = DE52, WHY_QUIT1_WAVE_E = EE52, WHY_QUIT1_WAVE_F = FE52, # reason for quitting? mention 1
               WHY_QUIT2_WAVE_B = BE52a, WHY_QUIT2_WAVE_C = CE52a, WHY_QUIT2_WAVE_D = DE52a, WHY_QUIT2_WAVE_E = EE52a, WHY_QUIT2_WAVE_F = FE52a, # reason for quitting? mention 2
               # BE53a, CE53a, DE53a, EE53a, FE53a, BE53b, CE53b, DE53b, EE53b, FE53b, # quit for other job opportunity?
               
               
               # block (7) - UMich's weights, intended to ensure representativeness to the 2005 CPS based on sex, age, income, and race.
               # see p. 25 of "cdbk.pdf" for more info
               WT_WAVE_A=WT_WAVEA,  
               WT_WAVE_B=WT_WAVEB,  
               WT_WAVE_C=WT_WAVEC,  
               WT_WAVE_D=WT_WAVED, 
               WT_WAVE_E=WT_WAVEE, 
               WT_WAVE_F=WT_WAVEF,
) 

# end