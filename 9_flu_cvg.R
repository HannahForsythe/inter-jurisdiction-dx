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
# create 18+ denom for each flu season since the start of data exchange 
# use the population estimate for the year when the season STARTED

# Figure out start of the flu season when data exchange first began
flu_start <- if_else(
  month(exchg_start) >= 7, year(exchg_start),
  year(exchg_start)-1    
)

# figure out the census files to use for each season
files_to_read <- tibble(
  "file" = list.files("input/census/"),
  "path" = paste0("input/census/", file),
  "year" = 2000 + as.numeric(substr(file, 13, 14))
) %>%
  filter(
    year >= flu_start
  )

# Load census files for all seasons up to 2020
home_fips <- map_st_cd(home_st)
denom_flu_1 <- map(
  files_to_read$path, 
  \(x) read_denom(x, home_fips),
  .progress = TRUE
) |> list_rbind()

# If any seasons fall outside the 2010-2020 range for which census
# data is available, use the closest year available.
denom_flu <- denom_flu_1
for (yr in flu_start:2022) {
 if (yr < 2010) {
   # Reuse year 2010 data for 2007-08 through 2009-10 seasons
   denom_flu <- bind_rows(
     denom_flu,
     denom_flu_1 %>% filter(year == 2010) %>% mutate(year = yr)
   )   
 } else if (yr > 2020) {
   # Reuse year 2020 data for 2021-22 and 2022-23 seasons
   denom_flu <- bind_rows(
     denom_flu,
     denom_flu_1 %>% filter(year == 2020) %>% mutate(year = yr)
   )
 }
}


# filter to just 18+
denom_flu <- denom_flu %>%
  filter(age >= 18) %>%
  mutate(
    in_state = TRUE,
    state_cd = home_st,
    season = year,
    season_start = as.Date(paste0(season, "-07-01"), "%Y-%m-%d"),
    season_end = as.Date(paste0(season + 1, "-06-30"), "%Y-%m-%d")
  )

#-----------------------------------------------------------------------------#
# 2. create numerator
#-----------------------------------------------------------------------------#
flu_crosswalk <- read_delim("input/crosswalk_flu.csv", col_types = 'c')
flu_cvx <- flu_crosswalk$CVX

# Determine yearly flu coverage status--with and without data from the partner 
# iis--of just those persons w/ at least one out-of-state vaccination
flu_shared <- immz %>%
  filter(
    # filter out non-flu shots and shots before data exchange began
    cvx %in% flu_cvx, 
    admin_date >= as.Date(paste0(flu_start, "-07-01"), "%Y-%m-%d")
  ) %>%
  mutate(
    # classify shots by contributing iis and season 
    flu = 1,
    flu_N = (reporting_iis == 'N'),
    season = if_else(
      month(admin_date) >= 7, year(admin_date),
      year(admin_date)-1    
      ),
    season_start = as.Date(paste0(season, "-07-01"), "%Y-%m-%d"),
    season_end = as.Date(paste0(season + 1, "-06-30"), "%Y-%m-%d")
  ) %>%
  group_by(person_id, season, season_start, season_end) %>%
  summarise(
    # determine each person's status, each season: 
    # utd, utd even w/o partner iis
    utd_w = (sum(flu) >= 1),
    utd_wo = (sum(flu_N) >=1)
  ) %>%
  mutate(
    # identify persons whose status is changed by data sharing
    utd_diff = (utd_w - utd_wo)
  ) 

# examine the coverage increases for these folks - does it look reasonable?
flu_shared %>% summary_as_factor()

# create the numerator: number of 18+ year-olds brought utd each season by 
# partner iis data, aggregated by the same variables as the denominator
num_flu <- flu_shared %>%
  left_join(
    # pull in sex, dob, race, current county, etc.
    demog, by = "person_id"
  ) %>%
  mutate(
    # identify current in-state residents
    in_state = state_cd == home_st,
    # calculate age at start of season
    age = floor(time_length(difftime(season_start, dob), "years")),
    # re-classify anyone 85+ as 85
    age = if_else(age > 85, 85, age)
  ) %>%
  filter(
    # remove anyone under 18 at start of season
    age >= 18
  ) %>%
  group_by(
    # aggregate
    in_state, state_cd, county_fips, sex, race_1, age, season, season_start, season_end
  ) %>%
  summarise(
    # total persons affected by data sharing, 
    # in each demographic category
    utd_diff = sum(utd_diff, na.rm = TRUE)
  )

# does the numerator look reasonable?
num_flu %>% summary_as_factor()

#-----------------------------------------------------------------------------#
# 3. How much does data sharing contribute to each season's
# coverage, among current residents? 
#-----------------------------------------------------------------------------#

flu_state <- left_join(
  # aggregate num
  num_flu %>% 
    group_by(in_state, season, season_start, season_end) %>% 
    summarise(utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_flu %>% 
    group_by(in_state, season, season_start, season_end) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

#-----------------------------------------------------------------------------#
# 4. Stratify by current county of residence.
#-----------------------------------------------------------------------------#
flu_cnty <- left_join(
  # aggregate num
  num_flu %>% filter(in_state == TRUE) %>% 
    group_by(county_fips, season, season_start, season_end) %>% 
    summarise(utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_flu %>% filter(in_state == TRUE) %>% 
    group_by(county_fips, season, season_start, season_end) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  county_nm = map_cnty_fips(county_fips),
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
) 

#-----------------------------------------------------------------------------#
# 5. Stratify by sex.
#-----------------------------------------------------------------------------#
flu_sex <- left_join(
  # aggregate num
  num_flu %>% filter(in_state == TRUE) %>% 
    group_by(sex, season, season_start, season_end) %>% 
    summarise(utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_flu %>% filter(in_state == TRUE) %>% 
    group_by(sex, season, season_start, season_end) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

# Stratify border counties only
flu_sex_bc <- left_join(
  # aggregate num
  num_flu %>% filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    group_by(sex, season, season_start, season_end) %>% 
    summarise(utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_flu %>% filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    group_by(sex, season, season_start, season_end) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

#-----------------------------------------------------------------------------#
# 6. Stratify by race_1
#-----------------------------------------------------------------------------#
flu_race1 <- left_join(
  # aggregate num
  num_flu %>% filter(in_state == TRUE) %>% 
    group_by(race_1, season, season_start, season_end) %>% 
    summarise(utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_flu %>% filter(in_state == TRUE) %>% 
    group_by(race_1, season, season_start, season_end) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

# Stratify border counties only
flu_race1_bc <- left_join(
  # aggregate num
  num_flu %>% filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    group_by(race_1, season, season_start, season_end) %>% 
    summarise(utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_flu %>% filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    group_by(race_1, season, season_start, season_end) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)
#-----------------------------------------------------------------------------#
# 7. Stratify by age group
#-----------------------------------------------------------------------------#
flu_age <- left_join(
  # aggregate num
  num_flu %>% filter(in_state == TRUE) %>% 
    mutate(age_grp = map_age_grp(age)) %>% 
    group_by(age_grp, season, season_start, season_end) %>% 
    summarise(utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_flu %>% filter(in_state == TRUE) %>% 
    mutate(age_grp = map_age_grp(age)) %>% 
    group_by(age_grp, season, season_start, season_end) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)

# Stratify border counties only
flu_age_bc <- left_join(
  # aggregate num
  num_flu %>% filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    mutate(age_grp = map_age_grp(age)) %>% 
    group_by(age_grp, season, season_start, season_end) %>% 
    summarise(utd_diff_n = sum(utd_diff)),
  # aggregate denom
  denom_flu %>% filter(in_state == TRUE, county_fips %in% c(border_fips)) %>% 
    mutate(age_grp = map_age_grp(age)) %>% 
    group_by(age_grp, season, season_start, season_end) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  utd_diff_cvg = round(utd_diff_n/pop*100, 2)
)
#-----------------------------------------------------------------------------#
# 8. export results
#-----------------------------------------------------------------------------#
# Create the output (sub)directory
if (!dir.exists("output/3_coverage")) {
  dir.create("output/3_coverage", recursive = TRUE)
}

# export raw dataframes as csv files
write.csv(flu_state, 
          file = paste0("output/3_coverage/flu_state-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(flu_cnty, 
          file = paste0("output/3_coverage/flu_cnty-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(flu_sex, 
          file = paste0("output/3_coverage/flu_sex-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(flu_sex_bc, 
          file = paste0("output/3_coverage/flu_sex_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(flu_race1, 
          file = paste0("output/3_coverage/flu_race1-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(flu_race1_bc, 
          file = paste0("output/3_coverage/flu_race1_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(flu_age, 
          file = paste0("output/3_coverage/flu_age-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(flu_age_bc, 
          file = paste0("output/3_coverage/flu_age_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
