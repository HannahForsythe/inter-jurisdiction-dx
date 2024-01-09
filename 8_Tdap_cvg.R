source("libraries.R")
source("helper_functions.R")
source("0a_parameters.R")

#-----------------------------------------------------------------------------#
# read in the source data
#-----------------------------------------------------------------------------#
immz <- read_delim("input/immz.zip") %>%
  janitor::clean_names() %>% 
  mutate(report_yr = year(report_date)) 

demog <- read_delim("input/demog.zip") %>%
  janitor::clean_names() 

#-----------------------------------------------------------------------------#
# create denominator
#-----------------------------------------------------------------------------#
# create 18+ denom for each year since the start of data exchange 

# Calendar year when data exchange first began
tdap_start <- year(exchg_start)

# figure out the file names to use for those years
files_to_read <- tibble(
  "file" = list.files("input/census/"),
  "path" = paste0("input/census/", file),
  "year" = 2000 + as.numeric(substr(file, 13, 14))
) %>%
  filter(
    year >= tdap_start
  )

# Load census files for all years up to 2020
home_fips <- map_st_cd(home_st)
denom_adu_1 <- map(
  files_to_read$path, 
  \(x) read_denom(x, home_fips),
  .progress = TRUE
) |> list_rbind() %>%
  filter(
    # filter to just 18+
    age >= 18
  ) %>%
  mutate(
    # add in variables included in the adolescent denom
    in_state = TRUE,
    state_cd = home_st
  )


# Re-use year 2020 single-year pop estimates for 2021 - end of data
y2021_plus <- 2021:year(exchg_end)
denom_adu <- denom_adu_1
for (yr in y2021_plus) {
  denom_adu_2 <- denom_adu_1 %>% filter(year == 2020) %>% mutate(year = yr)
  denom_adu <- denom_adu %>% bind_rows(denom_adu_2)
}


# Use iis total pop for ages 11 thru 17 as of Jan 1, for each year. 
# Given a year, this function counts up the population of 11-thru-17-yr-olds 
# as of Jan 1., aggregated to same level as census dataset.
identify_11_17 <- function(yr) {
  demog %>%
    mutate(
      # identify current in-state residents
      in_state = state_cd == home_st,
      # calculate age in years, as of Jan 1
      year = yr,
      age = floor(
        time_length(
          difftime(
            as.Date(paste0(year, '-01-01')), 
            dob
          ), 
          "years"
        )
      )
    ) %>%
    filter(
      # select 11 thru 17-yr-olds as of Jan 1
      age >= 11 & age <= 17
    ) %>%
    group_by(
      # aggregate
      year, in_state, state_cd, county_fips, sex, race_1, age
    ) %>%
    summarise(
      pop = n()
    )
}


# apply above function to every year since data exchange began, and pipe the
# result into a dataframe.
exchg_yrs <- year(exchg_start):year(exchg_end)
denom_adol <- map(
  exchg_yrs, 
  identify_11_17,
  .progress = TRUE
) |> list_rbind()


denom_tdap <- bind_rows(denom_adol, denom_adu)

# examine values of all variables in the denom: do they make sense? 
# There should be no NAs, except for county fips
denom_tdap %>% summary_as_factor()

#-----------------------------------------------------------------------------#
# create numerator
#-----------------------------------------------------------------------------#
# Tdap CVX is 115, add more if this ever changes
tdap_cvx <- c('115')

# given a year, this function identifies anyone initiated on their adolescent 
# Tdap series by a partner IIS as of Jan 1. (i.e. received 1+ Tdap on or after 
# their 10th birthday). 
find_tdap_init <- function(yr) {
  immz %>%
    filter(
      # select tdap shots admin as of Jan. 1
      cvx %in% tdap_cvx, 
      admin_date <= as.Date(paste0(yr, '-01-01'))
    ) %>% left_join(
      # pull in dob
      select(demog, person_id, dob), by = "person_id"
    ) %>% filter(
      # remove shots admin before the 10th bday
      time_length(difftime(admin_date, dob),"years") >= 10
    ) %>%
    arrange(
      # only one tdap per person per day (use first-reported shot)
      person_id, cvx, admin_date, report_date
    ) %>%
    group_by(person_id, cvx, admin_date) %>%
    mutate(nth = row_number()) %>%
    filter(nth == 1) %>%
    group_by(
      # count up total shots of each antigen admin per person, 
      # with and without partner iis shots
      person_id, cvx
    ) %>%
    summarise(
      n_w = n(),
      n_wo = sum(reporting_iis == 'N')
    ) %>% 
    mutate(
      # for each person, identify init status w/ and w/o partner iis
      init_w = n_w > 0,
      init_wo = n_wo > 0
    ) %>%
    mutate(
      # identify persons whose status is changed by data sharing
      init_diff = (init_w - init_wo),
      # include the year
      year = yr
    ) 
}

# apply above function to every year since data exchange began, and pipe the
# result into a dataframe. All ages are included.
tdap_shared <- map(
  exchg_yrs,
  find_tdap_init,
  .progress = TRUE
) |> list_rbind()

# create the numerator: number of 11+ year-olds initiated each year by 
# partner iis data, aggregated by the same variables as the denominator
num_tdap <- tdap_shared %>%
  left_join(
    # pull in sex, dob, race, current county, etc.
    demog, by = "person_id"
  ) %>%
  mutate(
    # identify current in-state residents
    in_state = state_cd == home_st,
    # calculate age in years as of Jan 1.
    age = floor(time_length(difftime(as.Date(paste0(year, '-01-01')), dob), "years")),
    # re-classify anyone 85+ as 85
    age = if_else(age > 85, 85, age)
  ) %>%
  filter(
    # remove anyone under 11 at start of year
    age >= 11
  ) %>%
  group_by(
    # aggregate
    in_state, state_cd, county_fips, sex, race_1, age, year
  ) %>%
  summarise(
    # total persons affected by data sharing in each demographic category
    init_diff = sum(init_diff, na.rm = TRUE)
  )

num_tdap %>% summary_as_factor()

#-----------------------------------------------------------------------------#
# QVIIa: How much does data sharing contribute to each year's
# coverage, among current residents? 
#-----------------------------------------------------------------------------#

tdap_state <- left_join(
  # aggregate num
  num_tdap %>% 
    group_by(in_state, year) %>% 
    summarise(init_diff_n = sum(init_diff)),
  # aggregate denom
  denom_tdap %>% 
    group_by(in_state, year) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2)
)

#-----------------------------------------------------------------------------#
# QVIIb: Stratify by current county of residence.
#-----------------------------------------------------------------------------#
tdap_cnty <- left_join(
  # aggregate num
  num_tdap %>% filter(in_state == TRUE) %>% 
    group_by(county_fips, year) %>% 
    summarise(init_diff_n = sum(init_diff)),
  # aggregate denom
  denom_tdap %>% filter(in_state == TRUE) %>% 
    group_by(county_fips, year) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  county_nm = map_cnty_fips(county_fips),
  init_diff_cvg = round(init_diff_n/pop*100, 2)
) 

#-----------------------------------------------------------------------------#
# QVIIc: Stratify by sex.
#-----------------------------------------------------------------------------#
tdap_sex <- left_join(
  # aggregate num
  num_tdap %>% filter(in_state == TRUE) %>% 
    group_by(sex, year) %>% 
    summarise(init_diff_n = sum(init_diff)),
  # aggregate denom
  denom_tdap %>% filter(in_state == TRUE) %>% 
    group_by(sex, year) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2)
)

#-----------------------------------------------------------------------------#
# QVIId: Stratify by race_1
#-----------------------------------------------------------------------------#
tdap_race1 <- left_join(
  # aggregate num
  num_tdap %>% filter(in_state == TRUE) %>% 
    group_by(race_1, year) %>% 
    summarise(init_diff_n = sum(init_diff)),
  # aggregate denom
  denom_tdap %>% filter(in_state == TRUE) %>% 
    group_by(race_1, year) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2)
)


#-----------------------------------------------------------------------------#
# QVIIe: Stratify by age group
#-----------------------------------------------------------------------------#
tdap_age <- left_join(
  # aggregate num
  num_tdap %>% filter(in_state == TRUE) %>% 
    mutate(age_grp = map_age_grp(age)) %>% 
    group_by(age_grp, year) %>% 
    summarise(init_diff_n = sum(init_diff)),
  # aggregate denom
  denom_tdap %>% filter(in_state == TRUE) %>% 
    mutate(age_grp = map_age_grp(age)) %>% 
    group_by(age_grp, year) %>% 
    summarise(pop = sum(pop))
) %>% mutate(
  init_diff_cvg = round(init_diff_n/pop*100, 2)
)

#-----------------------------------------------------------------------------#
# export results
#-----------------------------------------------------------------------------#
# Create the output (sub)directory
if (!dir.exists("output/3_coverage")) {
  dir.create("output/3_coverage", recursive = TRUE)
}
# export raw dataframes as csv files
write.csv(tdap_state, 
          file = paste0("output/3_coverage/tdap_state-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(tdap_cnty, 
          file = paste0("output/3_coverage/tdap_cnty-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(tdap_sex, 
          file = paste0("output/3_coverage/tdap_sex-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(tdap_race1, 
          file = paste0("output/3_coverage/tdap_race1-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(tdap_age, 
          file = paste0("output/3_coverage/tdap_age-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)

