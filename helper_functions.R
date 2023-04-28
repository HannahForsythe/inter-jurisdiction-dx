# This script contains helper functions that can be used in any of the analysis
# scripts. Must be sourced after libraries.R

#-----------------------------------------------------------------------------#
summary_as_factor <- function(tibble) {
  # a version of summary() that will treat character columns like factors
  tibble %>% 
    as.data.frame() %>% 
    mutate(across(
      where(is_character),
      as_factor
    )) %>% summary()
}

#-----------------------------------------------------------------------------#

# map a vector of 5-digit county fips codes to a vector of county names, 
# flagging the invalid ones.
# NOTE: this function is slow--best applied only to summarized data

# first, read in the codes-to-names crosswalk
crosswalk_fips <- read_csv(
  "input/mcir_fips_national_county.csv"
) %>%
  clean_names() %>% 
  transmute(
    state_cd = state_abbreviation,
    state_fips = state_code,
    county_fips = paste0(state_code, county_code),
    county_nm = str_replace(name, " County", "")
  )
# second, create the mapping function
map_cnty_fips <- function(code) {
  
  if (is.na(code)) {
    return(NA)
  } else if (code == 'UNK') {
    return('UNK') 
  } else if (code %in% crosswalk_fips$county_fips) {
    return(crosswalk_fips$county_nm[which(crosswalk_fips$county_fips == code)])
  } else {
    return('INVALID')
  }
  
}
# finally, vectorize the function (so it can operate over vectors, not just individual values)
map_cnty_fips <- Vectorize(map_cnty_fips, vectorize.args = "code")

#-----------------------------------------------------------------------------#

# map a vector of 2-digit state fips codes to a vector of state letter codes, 
# flagging the invalid ones.

# first, create fips-to-letter codes crosswalk from crosswalk_fips above
crosswalk_st_fips <- crosswalk_fips %>%
  select(state_cd, state_fips) %>%
  unique()

# second, create the mapping function
map_st_fips <- function(fips) {
  
  if (is.na(fips)) {
    return(NA)
  } else if (fips == 'UNK') {
    return('UNK') 
  } else if (sprintf("%02d", as.numeric(fips)) %in% crosswalk_st_fips$state_fips) {
    return(crosswalk_st_fips$state_cd[which(crosswalk_st_fips$state_fips == sprintf("%02d", as.numeric(fips)))])
  } else {
    return('INVALID')
  }
  
}

# finally, vectorize the function (so it can operate over vectors, not just individual values)
map_st_fips <- Vectorize(map_st_fips, vectorize.args = "fips")

#-----------------------------------------------------------------------------#

# map a vector of state fips codes to a vector of 2-digit state letter codes, 
# flagging the invalid ones (i.e., the mirror image of map_st_fips() above)

map_st_cd <- function(code) {
  
  if (is.na(code)) {
    return(NA)
  } else if (code == 'UNK') {
    return('UNK') 
  } else if (code %in% crosswalk_st_fips$state_cd) {
    return(crosswalk_st_fips$state_fips[which(crosswalk_st_fips$state_cd == code)])
  } else {
    return('INVALID')
  }
  
}

# vectorize the function 
map_st_cd <- Vectorize(map_st_cd, vectorize.args = "code")


#-----------------------------------------------------------------------------#

map_age_grp <- function(yrs) {
  # map a numeric vector of ages (in years) to an ordered factor of age groups
  factor(
    case_when(
      is.na(yrs) ~ 'UNK',
      yrs < 5 ~ '0-4',
      yrs < 12 ~ '5-11',
      yrs < 18 ~ '12-17',
      yrs < 30 ~ '18-29',
      yrs < 40 ~ '30-39',
      yrs < 50 ~ '40-49',
      yrs < 60 ~ '50-59',
      yrs < 70 ~ '60-69',
      TRUE ~ '70+'
    ),
    levels = c('0-4', '5-11', '12-17', '18-29', '30-39', '40-49', '50-59', '60-69', '70+', 'UNK')
  ) 
}

#-----------------------------------------------------------------------------#
# function for reading in census data found here:
# https://www.cdc.gov/nchs/nvss/bridged_race/data_documentation.htm#Vintage2020
# file definitions here:
# https://www.cdc.gov/nchs/data/nvss/bridged_race/Documentation-Bridged-PostcenV2020.pdf
#-----------------------------------------------------------------------------#
read_denom <- function(filename, home_st_fips) {
  read_fwf(
    (filename),
    fwf_cols(
      vintage = c(1,4),
      year = c(5,8),
      month = c(9, 9),
      state_fips = c(10, 11),
      county_fips = c(12, 14),
      age_yr = c(15, 16),
      br_race_sex = c(17, 17),
      hisp = c(18,18),
      pop = c(19, 26)
    ),
    col_types = cols('i', 'i', 'i', 'c', 'c', 'i', 'i', 'i', 'i'),
    num_threads = 8
  ) %>%
    filter(
      # filter out non-home state data
      state_fips == home_st_fips
    ) %>%
    mutate(
      # map raw values to pre-defined values for this minimum dataset
      age = age_yr,
      sex = case_when(
        br_race_sex %in% c(2, 4, 6, 8) ~ 'F', 
        br_race_sex %in% c(1, 3, 5, 7) ~ 'M'
      ),
      county_fips = paste0(state_fips, county_fips),
      ethnicity_1 = case_when(
        hisp == 1 ~ "NH",
        hisp == 2 ~ "Hispanic"
      ),
      race = case_when(
        br_race_sex %in% c(1, 2) ~ 'White', 
        br_race_sex %in% c(3, 4) ~ 'Black',
        br_race_sex %in% c(5, 6) ~ 'AI/AN', 
        br_race_sex %in% c(7, 8) ~ 'Asian/Hawaian/OPI', 
      ),
      race_1 = case_when(
        ethnicity_1 == "Hispanic" ~ ethnicity_1,
        ethnicity_1 == "NH" ~ paste(ethnicity_1, race),
        is.na(ethnicity_1) & !is.na(race) ~ paste("NH", race)
      )
    ) %>%
    select(
      # only select variables that will be needed
      sex, age, county_fips, race_1, pop, year
    )
}