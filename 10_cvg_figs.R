source("libraries.R")
source("helper_functions.R")
source("0a_parameters.R")

#-----------------------------------------------------------------------------#
# 0. read in the raw data (skip if running all coverage analyses in same session)
#-----------------------------------------------------------------------------#
# Put the export date of each county-level dataset in  "~/output/3_coverage/" 
namedates <- list(
  c("s4313314_cnty", "20240108"),
  c("mmr_cnty", "20240108"),
  c("dtap_cnty", "20240108"),
  c("s132321_cnty", "20240108"),
  c("mcv_cnty", "20240108"),
  c("flu_cnty", "20240108"), 
  c("covid_cnty", "20240108"),
  c("tdap_cnty", "20240108")
)
for (namedate in namedates) {
  assign(
    namedate[1],
    read.csv(paste0("output/3_coverage/", namedate[1], "-", home_st, "-", namedate[2],".csv"))
  )
}

#-----------------------------------------------------------------------------#
# 1. county-level datasets
#-----------------------------------------------------------------------------#
cvg_cnty <- bind_rows(
  s4313314_cnty %>%
    ungroup() %>%
    mutate(increase =  utd_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = '4313314 UTD'),
  mmr_cnty %>%
    ungroup() %>%
    mutate(increase =  utd_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = 'MMR UTD'),
  dtap_cnty %>%
    ungroup() %>%
    mutate(increase =  utd_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = 'DTaP UTD'),
  s132321_cnty %>%
    ungroup() %>%
    mutate(increase =  utd_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = '132321 UTD'),
  mcv_cnty %>%
    ungroup() %>%
    mutate(increase =  utd_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = 'MenACWY UTD'),
  flu_cnty %>%
    ungroup() %>%
    mutate(year = season, increase =  utd_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = 'flu'),
  covid_cnty %>%
    ungroup() %>%
    mutate(year = 2023, increase =  utd_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = 'COVID UTD'),
  covid_cnty %>%
    ungroup() %>%
    mutate(year = 2023, increase =  init_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = 'COVID init'),
  tdap_cnty %>%
    ungroup() %>%
    mutate(increase =  init_diff_cvg) %>%
    select(year, county_fips, county_nm, pop, increase) %>%
    mutate(series = 'Tdap initiation')
) %>%
  filter(!county_nm %in% c(NA, 'UNK')) %>%
  mutate(
    # identify border counties
    border = county_fips %in% border_fips
  )

# total number of counties
n_counties <- cvg_cnty$county_nm %>% unique() %>% length()

# number of border counties
n_border_cntys <- length(border_fips)

#-----------------------------------------------------------------------------#
# 2a. 4313314 coverage increase in border counties
#-----------------------------------------------------------------------------#
cvg_cnty_4313314 <-
  ggplot(
    cvg_cnty %>% 
      filter(series == "4313314 UTD", border == TRUE), 
    aes(x = year, y = increase, group = year)
  ) +
  geom_boxplot(color = 'blue') +
  # geom_violin(fill = 'blue', color = 'blue') +
  coord_flip() +
  labs(
    title = str_wrap(
      paste(
        "Increase in 4313314 coverage among", 
        n_border_cntys, home_st, 
        "border counties"
      )
    ),
    y = "% of county population age 19 through 35 months brought up-to-date by a partner IIS"
  ) +
  theme_bw()

print(cvg_cnty_4313314)

#-----------------------------------------------------------------------------#
# 2b. Just MMR coverage increase in border counties
#-----------------------------------------------------------------------------#
cvg_cnty_mmr <-
  ggplot(
    cvg_cnty %>% 
      filter(series == "MMR UTD", border == TRUE), 
    aes(x = year, y = increase, group = year)
  ) +
  geom_boxplot(color = 'blue', fill = 'lightblue') +
  # geom_violin(fill = 'blue', color = 'blue') +
  coord_flip() +
  labs(
    title = str_wrap(
      paste(
        "Increase in MMR coverage among", 
        n_border_cntys, home_st, 
        "border counties"
      )
    ),
    y = "% of county population age 19 through 35 months brought up-to-date by a partner IIS"
  ) +
  theme_bw()

print(cvg_cnty_mmr)

#-----------------------------------------------------------------------------#
# 2c. Just DTaP coverage increase in border counties
#-----------------------------------------------------------------------------#
cvg_cnty_dtap <-
  ggplot(
    cvg_cnty %>% 
      filter(series == "DTaP UTD", border == TRUE), 
    aes(x = year, y = increase, group = year)
  ) +
  geom_boxplot(color = 'blue', fill = 'lightgray') +
  # geom_violin(fill = 'blue', color = 'blue') +
  coord_flip() +
  labs(
    title = str_wrap(
      paste(
        "Increase in DTaP coverage among", 
        n_border_cntys, home_st, 
        "border counties"
      )
    ),
    y = "% of county population age 19 through 35 months brought up-to-date by a partner IIS"
  ) +
  theme_bw()

print(cvg_cnty_dtap)
#-----------------------------------------------------------------------------#
# 3a. 132321 coverage increase in border counties
#-----------------------------------------------------------------------------#
cvg_cnty_132321 <-
  ggplot(
    cvg_cnty %>% 
      filter(series == "132321 UTD", border == TRUE), 
    aes(x = year, y = increase, group = year)
  ) +
  geom_boxplot(color = 'red') +
  # geom_violin(fill = 'red', color = 'red') +
  coord_flip() +
  labs(
    title = str_wrap(
      paste(
        "Increase in 132321 coverage among", 
        n_border_cntys, home_st, 
        "border counties"
      )
    ),
    y = "% of county population age 13 through 17 brought up-to-date by a partner IIS"
  ) +
  theme_bw()

print(cvg_cnty_132321)

#-----------------------------------------------------------------------------#
# 3b. Just MenACWY (MCV) coverage increase in border counties
#-----------------------------------------------------------------------------#
cvg_cnty_mcv <-
  ggplot(
    cvg_cnty %>% 
      filter(series == "MenACWY UTD", border == TRUE), 
    aes(x = year, y = increase, group = year)
  ) +
  geom_boxplot(color = 'red', fill = "pink") +
  # geom_violin(fill = 'red', color = 'red') +
  coord_flip() +
  labs(
    title = str_wrap(
      paste(
        "Increase in MenACWY coverage among", 
        n_border_cntys, home_st, 
        "border counties"
      )
    ),
    y = "% of county population age 13 through 17 brought up-to-date by a partner IIS"
  ) +
  theme_bw()

print(cvg_cnty_mcv)
#-----------------------------------------------------------------------------#
# 4. Flu coverage increase in border counties
#-----------------------------------------------------------------------------#

# Start of the flu season when data exchange first began (used in title)
flu_start <- if_else(
  month(exchg_start) >= 7, year(exchg_start),
  year(exchg_start)-1    
) 

cvg_cnty_flu <-
  ggplot(
    cvg_cnty %>% 
      filter(series == "flu", border == TRUE) %>% 
      mutate(season = paste0(year, "-", year + 1 - 2000)), 
    aes(x = season, y = increase, group = season)
  ) +
  geom_boxplot(color = 'purple') +
  # geom_violin(fill = 'purple', color = 'purple') +
  coord_flip() +
  labs(
    title = str_wrap(
      paste(
        "Increase in seasonal flu coverage among", 
        n_border_cntys, home_st, 
        "border counties,", 
        flu_start, "- present"
      )
    ),
    y = "% of county population age 18+ vaccinated by a partner IIS"
  ) +
  theme_bw()

print(cvg_cnty_flu)

#-----------------------------------------------------------------------------#
# 5. Covid coverage increase in border counties
#-----------------------------------------------------------------------------#

cvg_cnty_covid <-
  ggplot(
    cvg_cnty %>% 
      filter(series %in% c('COVID init', 'COVID UTD'), border == TRUE), 
    aes(x = series, y = increase, group = series)
  ) +
  geom_boxplot(color = 'dark green') +
  # geom_violin(fill = 'green', color = 'green') +
  coord_flip() +
  labs(
    title = str_wrap(
      paste(
        "Increase in COVID initiation and booster coverage among", 
        n_border_cntys, home_st, 
        "border counties,"
      )
    ),
    y = "% of county population vaccinated by a partner IIS",
    x = ""
  ) +
  theme_bw()

print(cvg_cnty_covid)

#-----------------------------------------------------------------------------#
# 6. Tdap coverage increase in border counties
#-----------------------------------------------------------------------------#
cvg_cnty_tdap <-
  ggplot(
    cvg_cnty %>% 
      filter(series == "Tdap initiation", border == TRUE), 
    aes(x = year, y = increase, group = year)
  ) +
  geom_boxplot(color = 'brown') +
  # geom_violin(fill = 'red', color = 'red') +
  coord_flip() +
  labs(
    title = str_wrap(
      paste(
        "Increase in 1+ Tdap coverage among", 
        n_border_cntys, home_st, 
        "border counties"
      )
    ),
    y = "% of county population age 11+ brought up-to-date by a partner IIS"
  ) +
  theme_bw()

print(cvg_cnty_tdap)
#-----------------------------------------------------------------------------#
# 7. export
#-----------------------------------------------------------------------------#
ggsave(filename = paste0("output/3_coverage/s4313314_cnty_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = cvg_cnty_4313314)
ggsave(filename = paste0("output/3_coverage/MMR_cnty_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = cvg_cnty_mmr)
ggsave(filename = paste0("output/3_coverage/DTaP_cnty_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = cvg_cnty_dtap)

ggsave(filename = paste0("output/3_coverage/s132321_cnty_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = cvg_cnty_132321)
ggsave(filename = paste0("output/3_coverage/MCV_cnty_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = cvg_cnty_mcv)

ggsave(filename = paste0("output/3_coverage/flu_cnty_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = cvg_cnty_flu)

ggsave(filename = paste0("output/3_coverage/COVID_cnty_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = cvg_cnty_covid)

ggsave(filename = paste0("output/3_coverage/Tdap_cnty_bc-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = cvg_cnty_tdap)
