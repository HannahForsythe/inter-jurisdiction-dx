source("libraries.R")
source("helper_functions.R")
source("0a_parameters.R")

#-----------------------------------------------------------------------------#
# 0. read in the source data
#-----------------------------------------------------------------------------#
immz <- read_delim("input/immz.zip") %>%
  janitor::clean_names() %>% 
  mutate(report_yr = year(report_date)) 

demog <- read_delim("input/demog.zip") %>%
  janitor::clean_names() 

#-----------------------------------------------------------------------------#
# 1. create denominator
#-----------------------------------------------------------------------------#

# Use year 2020 single-year pop estimates for ages 18+. (Age 85+ coded as 85.)
home_fips <- map_st_cd(home_st)
denom_covid_adu <- read_denom("input/census/pcen_v2020_y20_txt.zip", home_fips) %>%
  filter(age >= 18) %>%
  mutate(
    in_state = TRUE,
    state_cd = home_st
  )

# Use iis total pop for ages <18. (Aggregate to same level as census dataset.)
denom_covid_chi <- demog %>%
  mutate(
    # identify current in-state residents
    in_state = state_cd == home_st,
    # calculate age in years, as of jan 1, 2023
    age = floor(time_length(difftime(as.Date('2023-01-01'), dob), "years"))
  ) %>%
  filter(
    # remove anyone 18+
    age < 18
  ) %>%
  group_by(
    # aggregate
    in_state, state_cd, county_fips, sex, race_1, age
  ) %>%
  summarise(
    pop = n()
  )

denom_covid <- bind_rows(denom_covid_chi, denom_covid_adu)

#-----------------------------------------------------------------------------#
# 2. create numerator
#-----------------------------------------------------------------------------#
# load crosswalk between COVID CVXs and mono-bivalent
covid_crosswalk <- 
  read_delim("input/crosswalk_COVID.csv", col_types = 'ccc') %>%
  janitor::clean_names()
covid_cvx <- covid_crosswalk$cvx
bivalent_cvx <- covid_crosswalk$cvx[covid_crosswalk$valence == "bivalent"]

# Determine COVID coverage status--with and without data from the partner iis--
# of just those persons w/ a history of at least one out-of-state vaccination
covid_shared <- immz %>%
  filter(
    # filter out non-covid shots
    cvx %in% covid_cvx
  ) %>%
  mutate(
    # classify shots by valence & iis
    covid = 1,
    covid_N = (reporting_iis == 'N'),
    covid_biv = (cvx %in% bivalent_cvx),
    covid_biv_N = (cvx %in% bivalent_cvx & reporting_iis == 'N')
  ) %>%
  group_by(person_id) %>%
  summarise(
    # determine each person's status: 
    # init, init w/o partner iis, utd, utd w/o partner iis
    init_w = (sum(covid) >= 1),
    init_wo = (sum(covid_N) >= 1),
    utd_w = (sum(covid_biv) >= 1),
    utd_wo = (sum(covid_biv_N) >=1)
  ) %>%
  mutate(
    # identify persons whose status is changed by data sharing
    init_diff = (init_w - init_wo),
    utd_diff = (utd_w - utd_wo)
  ) 

covid_shared %>% summary_as_factor()

# create the numerator: number of persons initiated or brought utd by 
# partner iis data, aggregated by the same variables as the denominator
num_covid <- covid_shared %>%
left_join(
  # pull in sex, dob, race, current county, etc.
  demog, by = "person_id"
) %>%
  mutate(
    # identify current in-state residents
    in_state = state_cd == home_st,
    # calculate age in years, as of jan 1, 2023
    age = floor(time_length(difftime(as.Date('2023-01-01'), dob), "years")),
    # re-classify anyone 85+ as 85
    age = if_else(age > 85, 85, age)
  ) %>%
  group_by(
    # aggregate
    in_state, state_cd, county_fips, sex, race_1, age
  ) %>%
  summarise(
    # total persons affected by data sharing, 
    # in each demographic category
    init_diff = sum(init_diff, na.rm = TRUE),
    utd_diff = sum(utd_diff, na.rm = TRUE)
  )


#-----------------------------------------------------------------------------#
# 3. How much does data sharing contribute to COVID initiation and UTD
# coverage, among current residents? 
#-----------------------------------------------------------------------------#
covid_state <- left_join(
  # aggregate num
  num_covid %>% group_by(in_state) %>% 
    summarise(init_diff_n = sum(init_diff),
              utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_covid %>% group_by(in_state) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)


#-----------------------------------------------------------------------------#
# 4. Stratify by current county of residence.
#-----------------------------------------------------------------------------#
covid_cnty <- left_join(
  # aggregate num
  num_covid %>% filter(in_state == TRUE) %>% group_by(county_fips) %>% 
    summarise(init_diff_n = sum(init_diff),
              utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_covid %>% filter(in_state == TRUE) %>% group_by(county_fips) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  county_nm = map_cnty_fips(county_fips),
  init_diff_cvg = round(init_diff_n/pop*100, 2),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
) 

#-----------------------------------------------------------------------------#
# 5. Stratify by sex.
#-----------------------------------------------------------------------------#
covid_sex <- left_join(
  # aggregate num
  num_covid %>% 
    filter(in_state == TRUE) %>% 
    group_by(sex) %>% 
    summarise(init_diff_n = sum(init_diff),
              utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_covid %>% 
    filter(in_state == TRUE) %>% 
    group_by(sex) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

# Stratify border counties only
covid_sex_bc <- left_join(
  # aggregate num
  num_covid %>% 
    filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    group_by(sex) %>% 
    summarise(init_diff_n = sum(init_diff),
              utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_covid %>% 
    filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    group_by(sex) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

#-----------------------------------------------------------------------------#
# 6. Stratify by race_1
#-----------------------------------------------------------------------------#
covid_race1 <- left_join(
  # aggregate num
  num_covid %>% 
    filter(in_state == TRUE) %>% 
    group_by(race_1) %>% 
    summarise(init_diff_n = sum(init_diff),
              utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_covid %>% 
    filter(in_state == TRUE) %>% 
    group_by(race_1) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

# Stratify border counties only
covid_race1_bc <- left_join(
  # aggregate num
  num_covid %>% 
    filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    group_by(race_1) %>% 
    summarise(init_diff_n = sum(init_diff),
              utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_covid %>% 
    filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    group_by(race_1) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

#-----------------------------------------------------------------------------#
# 7. Stratify by age group
#-----------------------------------------------------------------------------#
covid_age <- left_join(
  # aggregate num
  num_covid %>% 
    filter(in_state == TRUE) %>% 
    mutate(age_grp = map_age_grp(age))  %>% 
    group_by(age_grp) %>% 
    summarise(init_diff_n = sum(init_diff),
              utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_covid %>% 
    filter(in_state == TRUE) %>% 
    mutate(age_grp = map_age_grp(age))  %>% 
    group_by(age_grp) %>%
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

# Stratify border counties only
covid_age_bc <- left_join(
  # aggregate num
  num_covid %>% 
    filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    mutate(age_grp = map_age_grp(age))  %>% 
    group_by(age_grp) %>% 
    summarise(init_diff_n = sum(init_diff),
              utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_covid %>% 
    filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    mutate(age_grp = map_age_grp(age))  %>% 
    group_by(age_grp) %>%
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

#-----------------------------------------------------------------------------#
# 8. export results
#-----------------------------------------------------------------------------#
# export raw dataframes as csv files
write.csv(covid_state, 
          file = paste0("output/3_coverage/covid_state-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(covid_cnty, 
          file = paste0("output/3_coverage/covid_cnty-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(covid_sex, 
          file = paste0("output/3_coverage/covid_sex-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(covid_sex_bc, 
          file = paste0("output/3_coverage/covid_sex_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(covid_race1, 
          file = paste0("output/3_coverage/covid_race1-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(covid_race1_bc, 
          file = paste0("output/3_coverage/covid_race1_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(covid_age, 
          file = paste0("output/3_coverage/covid_age-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(covid_age_bc, 
          file = paste0("output/3_coverage/covid_age_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
