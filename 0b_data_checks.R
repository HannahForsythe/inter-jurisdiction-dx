source("libraries.R")
source("helper_functions.R")
source("0a_parameters.R")
#-----------------------------------------------------------------------------#
# read in the source data
#-----------------------------------------------------------------------------#
immz <- read_delim("input/immz.zip") %>%
  janitor::clean_names() %>% 
  mutate(report_yr = year(report_date))

demog <- read_delim("input/demog.csv") %>%
  janitor::clean_names() 

# visually inspect your data. Does everything look right?
summary_as_factor(immz)
str(immz)
summary_as_factor(demog)
str(demog)

#-----------------------------------------------------------------------------#
# check that the data meets certain conditions
#-----------------------------------------------------------------------------#
# Condition 1: every person in immz.csv is present in demog.csv
missing <- anti_join(immz, demog, by = "person_id") %>% 
  select(person_id) %>% 
  distinct()
if (nrow(missing) > 0) {
  warning("The following persons are missing from demog.csv: ", missing)

} else {
  print("Condition 1 met")
}

# Condition 2: birth dates fall before admin dates
dob_after_admin <- left_join(immz, demog, by = "person_id") %>% 
  mutate(shot_age = difftime(admin_date, dob, units = "days")) %>%
  filter(shot_age < 0)
if (nrow(dob_after_admin) > 0) {
    warning(
      "There are ", 
      n_distinct(dob_after_admin$vax_event_id), 
      " shots administered before the DOB. Type view(dob_after_admin) to examine."
    )
} else {
  print("Condition 2 met")
}

# Condition 3: birth dates fall before today
dob_after_today <- demog %>%
  filter(dob > Sys.Date())
if (nrow(dob_after_today) > 0) {
  warning(
    "There are ", 
    n_distinct(dob_after_today$person_id), 
    " persons with a future DOB. Type view(dob_after_today) to examine."
  )
} else {
  print("Condition 3 met")
}

# Condition 4: admin dates fall before today
admin_after_today <- immz %>%
  filter(admin_date > Sys.Date())
if (nrow(admin_after_today) > 0) {
  warning(
    "There are ", 
    n_distinct(admin_after_today$vax_event_id), 
    " shots administered after today. Type view(admin_after_today) to examine."
  )
} else {
  print("Condition 4 met")
}

# Condition 5: exchanged shots all have report dates in or after the year your
# jurisdiction began sharing data (exchg_start)
rpt_before_exchg <- immz %>%
  filter(reporting_iis %in% c("P", "P1", "P2")) %>%
  filter(report_date < exchg_start)
if (nrow(rpt_before_exchg) > 0) {
  warning(
    "There are ", 
    n_distinct(rpt_before_exchg$vax_event_id), 
    " partner shots shared before data exchange began. Type view(rpt_before_exchg) to examine."
  )
} else {
  print("Condition 5 met")
}

# Condtiion 6: all state codes are valid
state_codes <- c("AK", "AL", "AR", "AS", "AZ", "CA", "CO", "CT", "DC", "DE", 
                 "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", 
                 "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MP", "MS", "MT", 
                 "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", 
                 "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UM", "UT", 
                 "VA", "VI", "VT", "WA", "WI", "WV", "WY", "XX")
bad_st_cd <- demog %>% 
  filter(!is.na(state_cd) & !state_cd %in% state_codes)
if (nrow(bad_st_cd) > 0) {
  warning(
    "There are ", 
    n_distinct(bad_st_cd$person_id), 
    " persons with invalid state codes. Type view(bad_st_cd) to examine."
  )
} else {
  print("Condition 6 met")
}

# Condition 7: all FIPS codes are valid
county_codes <- c("UNK", crosswalk_fips$county_fips)
bad_cnty_cd <- demog %>% 
  filter(!is.na(county_fips) & !county_fips %in% county_codes)
if (nrow(bad_cnty_cd) > 0) {
  warning(
    "There are ", 
    n_distinct(bad_cnty_cd$person_id), 
    " persons with invalid county fips codes. Type view(bad_cnty_cd) to examine. ",
    "Valid fips codes are listed in input/mcir_fips_national_county.csv"
  )
} else {
  print("Condition 7 met")
}

# Condition 8: all sex codes are valid
bad_sex_cd <- demog %>%
  filter(!sex %in% c('M', 'F', 'U'))
if (nrow(bad_sex_cd) > 0) {
  warning(
    "There are ", 
    n_distinct(bad_sex_cd$person_id), 
    " persons with invalid sex codes. Type view(bad_sex_cd) to examine."
  )
} else {
  print("Condition 8 met")
}

# Condition 9: all ethnicity codes are valid
bad_eth_cd <- demog %>%
  filter(!ethnicity_1 %in% c('H', 'NH', 'UNK'))
if (nrow(bad_eth_cd) > 0) {
  warning(
    "There are ", 
    n_distinct(bad_eth_cd$person_id), 
    " persons with invalid ethnicity codes. Type view(bad_eth_cd) to examine."
  )
} else {
  print("Condition 9 met")
}

# Condition 10: all race_1 codes are valid
bad_r1_cd <- demog %>%
  filter(!race_1 %in% c("Hispanic", "NH White", "NH Black", "NH AI/AN", "NH Asian/Hawaian/OPI", "Other", "UNK"))
if (nrow(bad_r1_cd) > 0) {
  warning(
    "There are ", 
    n_distinct(bad_r1_cd$person_id), 
    " persons with invalid ethnicity codes. Type view(bad_r1_cd) to examine."
  )
} else {
  print("Condition 10 met")
}

# Condition 11: race_2 codes are consistent with jurisdiction values defined in race_2_vals
bad_r2_cd <- demog %>%
  filter(!race_2 %in% race_2_vals)
if (nrow(bad_r2_cd) > 0) {
  warning(
    "There are ", 
    n_distinct(bad_r2_cd$person_id), 
    " persons with invalid ethnicity codes. Type view(bad_r2_cd) to examine."
  )
} else {
  print("Condition 11 met")
}

# Condition 12: all person IDs are unique
dup_person_ids <- demog %>% 
  arrange(person_id) %>%
  group_by(person_id) %>%
  summarise(n = n()) %>%
  filter(n > 1)
if (nrow(dup_person_ids) > 0) {
  warning(
    "There are ", 
    nrow(dup_person_ids), 
    " duplicate person IDs. Type view(dup_person_ids) to examine."
  )
} else {
  print("Condition 12 met")
}

# Condition 13: all CVX codes are valid
valid_cvx <- read_csv("input/crosswalk_CVX_nm.csv") %>%
  clean_names() 

bad_cvx <- immz %>%
  filter(
    !cvx %in% valid_cvx$cvx
  )
if (nrow(bad_cvx) > 0) {
  warning(
    "There are ", 
    n_distinct(bad_cvx$vax_event_id), 
    " shots with an invalid CVX. Type view(bad_cvx) to examine. ",
    "To view list of accepted CVX and names, type view(valid_cvx)"
  )
} else {
  print("Condition 13 met")
}

# Condition 14: no na values where they shouldn't be in the demographic file
demog_na <- demog %>%
  mutate(
    person_id_na = ifelse(is.na(person_id), 1, 0),
    sex_na = ifelse(is.na(sex), 1, 0),
    dob_na = ifelse(is.na(dob), 1, 0),
    ethnicity_1_na = ifelse(is.na(ethnicity_1), 1, 0),
    race_1_na = ifelse(is.na(race_1), 1, 0),
    race_2_na = ifelse(is.na(race_2), 1, 0),
    state_cd_na = ifelse(is.na(state_cd), 1, 0)
  ) %>% 
    filter(
      person_id_na > 0|
      sex_na > 0|
      dob_na > 0|
      ethnicity_1_na > 0|
      race_1_na > 0|
      race_2_na > 0|
      state_cd_na > 0
    )
if (nrow(demog_na) > 0) {
  warning(
    "There are ", 
    n_distinct(demog_na$person_id), 
    " persons with one or more missing required demographic values.",
    "Type view(demog_na) to examine. "
  )
} else {
  print("Condition 14 met")
}

# Condition 15: no na values where they shouldn't be in the immz file
immz_na <- immz %>%
  mutate(
    person_id_na = ifelse(is.na(person_id), 1, 0),
    vax_event_id_na = ifelse(is.na(vax_event_id), 1, 0),
    cvx_na = ifelse(is.na(cvx), 1, 0),
    admin_date_na = ifelse(is.na(admin_date), 1, 0),
    report_date_na = ifelse(is.na(report_date), 1, 0),
    reporting_iis_na = ifelse(is.na(reporting_iis), 1, 0)
  ) %>% 
  filter(
    person_id_na > 0|
      vax_event_id_na > 0|
      cvx_na > 0|
      admin_date_na > 0|
      report_date_na > 0|
      reporting_iis_na > 0
  )
if (nrow(immz_na) > 0) {
  warning(
    "There are ", 
    n_distinct(immz_na$person_id), 
    " persons with one or more missing required demographic values.",
    "Type view(immz_na) to examine. "
  )
} else {
  print("Condition 15 met")
}

