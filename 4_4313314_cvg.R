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

# given a year, this function identifies 19-thru-35-month-olds as of Jan 1.
identify_19_35 <- function(yr) {
  demog %>%
    mutate(
      # identify current in-state residents
      in_state = state_cd == home_st,
      # calculate age in months, as of Jan 1
      year = yr,
      age = floor(
        time_length(
          difftime(
            as.Date(paste0(year, '-01-01')), 
            dob
          ), 
          "months"
        )
      )
    ) %>%
    filter(
      # select 19 thru 35 mo as of Jan 1
      age >= 19 & age <= 35
    )
}

# apply above function to every year since data exchange began, and pipe the
# result into a dataframe. Unlike other denominators (ex. denom_flu), 
# THIS IS A PERSON-LEVEL DENOMINATOR. Children who fall into the 19-35 age range
# for two consecutive years will be included twice.
exchg_yrs <- year(exchg_start):year(exchg_end)
denom_4313314 <- map(
  exchg_yrs, 
  identify_19_35,
  .progress = TRUE
) |> list_rbind()

#-----------------------------------------------------------------------------#
# 2. create numerator
#-----------------------------------------------------------------------------#
# load crosswalk of 4313314 CVXs with antigen names and total required doses
s4313314_crosswalk <- 
  read_delim("input/crosswalk_4313314.csv", col_types = 'c') %>%
  janitor::clean_names()

# given a year, this function identifies anyone brought UTD by a partner IIS 
# as of Jan 1., on the 4313314 series (i.e., person is UTD on ALL 7 antigens when
# partner shots are included but incomplete on at least one antigen when partner
# shots are excluded). All ages included.
find_4313314_UTD <- function(yr) {
  left_join(
    # select 4313314 CVXs, link to antigen & num required doses 
    s4313314_crosswalk, immz, by = "cvx", multiple = "all", relationship = "many-to-many"
  ) %>%
    filter(
      # shots admin as of Jan. 1
      admin_date <= as.Date(paste0(yr, '-01-01'))
    ) %>%
    arrange(
      # only one ag per person per day (use first-reported shot)
      person_id, ag, admin_date, report_date
    ) %>%
    group_by(person_id, ag, admin_date) %>%
    mutate(nth = row_number()) %>%
    filter(nth == 1) %>%
    group_by(
      # count up total shots of each antigen admin per person, 
      # with and without partner iis shots
      person_id, ag, req
    ) %>%
    summarise(
      n_w = n(),
      n_wo = sum(reporting_iis == 'N')
    ) %>% 
    mutate(
      # for each person and antigen, identify UTD status w/ and w/o partner iis
      UTD_w = n_w >= req,
      UTD_wo = n_wo >= req
    ) %>%
    pivot_wider(
      # create one row per person, 2 cols per antigen (status w/, status w/o)
      id_cols = c(person_id),
      names_from = c(ag),
      values_from = c(UTD_w, UTD_wo),
      values_fn = as.numeric,
      values_fill = 0
    ) %>% #ungroup() %>% rowwise() %>%
    mutate(
      # for each person, identify 4313314 UTD status w/ and w/o partner iis
      UTD_w_4313314 = sum(UTD_w_Dtap, UTD_w_Polio, UTD_w_MMR, 
                          UTD_w_Hib, UTD_w_HepB, UTD_w_Var, UTD_w_PCV) >= 7,
      UTD_wo_4313314 = sum(UTD_wo_Dtap, UTD_wo_Polio, UTD_wo_MMR, 
                           UTD_wo_Hib, UTD_wo_HepB, UTD_wo_Var, UTD_wo_PCV) >= 7,
      # persons brought UTD by partner iis on at least 1 antigen
      UTD_diff_4313314 = UTD_w_4313314 - UTD_wo_4313314,
      # include the year
      year = yr
    )
}

# apply above function to every year since data exchange began, and pipe the
# result into a dataframe. Unlike other numerators (ex. num_flu), 
# THIS IS A PERSON-LEVEL NUMERATOR. Anyone brought UTD as of Jan 1
# for two consecutive years will be included twice.
num_4313314 <- map(
  exchg_yrs,
  find_4313314_UTD,
  .progress = TRUE
) |> list_rbind()

# Create a combined denominator-numerator: select all 19-thru-35-month-olds as 
# of Jan 1.  each year from denom_4313314 and LEFT join their UTD status from 
# num_4313314--children outside the 19-35-mo age range will be excluded.
num_denom_4313314 <- left_join(
  denom_4313314, num_4313314, by = c("person_id", "year")
)

#-----------------------------------------------------------------------------#
# 3. How much does data sharing contribute to each year's
# coverage, among current residents? 
#-----------------------------------------------------------------------------#
s4313314_state <- num_denom_4313314 %>% 
  group_by(in_state, year) %>%
  summarise(
    pop = n_distinct(person_id),
    utd_diff_n = sum(UTD_diff_4313314, na.rm = TRUE)
  ) %>% 
  mutate(
    utd_diff_cvg = round(utd_diff_n/pop*100, 2)
  )

#-----------------------------------------------------------------------------#
# 4. Stratify by current county of residence.
#-----------------------------------------------------------------------------#
s4313314_cnty <- num_denom_4313314 %>% 
  filter(in_state == TRUE) %>% 
  group_by(year, county_fips) %>%
  summarise(
    pop = n_distinct(person_id),
    utd_diff_n = sum(UTD_diff_4313314, na.rm = TRUE)
  ) %>% mutate(
    county_nm = map_cnty_fips(county_fips),
    utd_diff_cvg = round(utd_diff_n/pop*100, 2)
  ) 

#-----------------------------------------------------------------------------#
# 5. Stratify by sex.
#-----------------------------------------------------------------------------#
s4313314_sex <- num_denom_4313314 %>% 
  filter(in_state == TRUE) %>%
  group_by(year, sex) %>%
  summarise(
    pop = n_distinct(person_id),
    utd_diff_n = sum(UTD_diff_4313314, na.rm = TRUE)
  ) %>% 
  mutate(
    utd_diff_cvg = round(utd_diff_n/pop*100, 2)
  )

# Stratify border counties only
s4313314_sex_bc <- num_denom_4313314 %>% 
  filter(in_state == TRUE, county_fips %in% c(border_fips)) %>%
  group_by(year, sex) %>%
  summarise(
    pop = n_distinct(person_id),
    utd_diff_n = sum(UTD_diff_4313314, na.rm = TRUE)
  ) %>% 
  mutate(
    utd_diff_cvg = round(utd_diff_n/pop*100, 2)
  )
#-----------------------------------------------------------------------------#
# 6. Stratify by race_1
#-----------------------------------------------------------------------------#
s4313314_race1 <- num_denom_4313314 %>% 
  filter(in_state == TRUE) %>%
  group_by(year, race_1) %>%
  summarise(
    pop = n_distinct(person_id),
    utd_diff_n = sum(UTD_diff_4313314, na.rm = TRUE)
  ) %>% 
  mutate(
    utd_diff_cvg = round(utd_diff_n/pop*100, 2)
  )

# Stratify border counties only
s4313314_race1_bc <- num_denom_4313314 %>% 
  filter(in_state == TRUE, county_fips %in% c(border_fips)) %>%
  group_by(year, race_1) %>%
  summarise(
    pop = n_distinct(person_id),
    utd_diff_n = sum(UTD_diff_4313314, na.rm = TRUE)
  ) %>% 
  mutate(
    utd_diff_cvg = round(utd_diff_n/pop*100, 2)
  )
#-----------------------------------------------------------------------------#
# 7. export results
#-----------------------------------------------------------------------------#
# Create the output (sub)directory
if (!dir.exists("output/3_coverage")) {
  dir.create("output/3_coverage", recursive = TRUE)
}

# export raw dataframes as csv files
write.csv(s4313314_state, 
          file = paste0("output/3_coverage/s4313314_state-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(s4313314_cnty, 
          file = paste0("output/3_coverage/s4313314_cnty-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(s4313314_sex, 
          file = paste0("output/3_coverage/s4313314_sex-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(s4313314_sex_bc, 
          file = paste0("output/3_coverage/s4313314_sex_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(s4313314_race1_bc, 
          file = paste0("output/3_coverage/s4313314_race1_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)

