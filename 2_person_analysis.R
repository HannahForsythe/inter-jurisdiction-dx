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
# 1. Clients with 1+ partner record, compared to total client base
#-----------------------------------------------------------------------------#
n_shared <- immz %>%
  distinct(person_id) %>%
  nrow()

n_total <- demog %>%
  distinct(person_id) %>%
  nrow()

pr_topline <- tibble(
  "jurisdiction" = home_st,
  "total clients" = n_total,
  "clients with shared record(s)" = n_shared,
  "percent" = round(n_shared/n_total*100, 2)
) 

pr_topline %>% flextable()
#-----------------------------------------------------------------------------#
# 2. Proportion currently residing in-state
#-----------------------------------------------------------------------------#
pr_shared <- immz %>% 
  distinct(person_id) %>%
  left_join(demog, by = "person_id") %>%
  mutate(in_state = state_cd == home_st)


pr_state <- pr_shared %>%
  group_by(in_state, state_cd) %>%
  summarise(
    persons = n(), 
    percent = round(n()/n_shared*100, 2)
  ) %>% ungroup()

pr_state_tbl <- pr_state %>%
  select(state_cd:percent) %>%
  mutate(
    percent = ifelse(percent < 1.0, '< 1.0', paste(percent))
  ) %>%
  arrange(desc(persons)) %>%
  rename(state = state_cd) %>%
  adorn_totals() %>%
  flextable() %>%
  set_caption(
    paste0("Current residence of persons having at least one record reported to ", 
           home_st, 
           " by partner IIS (", 
           paste(partner_st, collapse = ', '),
           ")"
           )
  ) %>%
  add_footer_lines(paste("Note: At time of vaccination, all recipients resided within", home_st))
pr_state_tbl

#-----------------------------------------------------------------------------#
# 3. Stratify by resident's current county, 
# report as a share of each county's total client base
#-----------------------------------------------------------------------------#
pr_demog <- pr_shared %>%
  select(person_id) %>%
  mutate(shared = 1) %>%
  right_join(demog, by = "person_id") %>%
  replace_na(list(shared = 0))

pr_cnty <- pr_demog %>%
  group_by(county_fips) %>%
  summarise(
    persons = sum(shared, na.rm = TRUE),
    cnty_total = n(),
    percent = round(persons/cnty_total*100, 2)
  ) %>%
  mutate(county_nm = map_cnty_fips(county_fips))

# format as a table
pr_cnty_tbl <- pr_cnty %>%
  mutate(
    percent = ifelse(percent < 1.0, '< 1.0', paste(percent))
  ) %>%
  relocate(county_nm) %>%
  arrange(county_fips) %>%
  rename(
    FIPS = county_fips, 
    County = county_nm,
    `County total` = cnty_total
  ) %>%
  replace_na(list(County = "Moved")) %>%
  adorn_totals() %>%
  flextable() %>%
  set_caption(
    paste0("Number of current ", 
           home_st, 
           " residents who received at least one immunization in ", 
           paste(partner_st, collapse = ', '),
           ", as a share of each county's total client base"
           )
  ) %>%
  add_footer_lines(
    paste(
      "Note: n = ", 
      sum(pr_state$persons[pr_state$in_state == FALSE], na.rm = TRUE), 
      "persons moved out of", 
      home_st, 
      "since vaccination"
    )
  )
pr_cnty_tbl

# Map of % Population w/ Shared Shots
pr_cnty_2 <- subset(pr_cnty, select = c("county_fips","percent")) %>%
  rename("fips"="county_fips")

pr_cnty_map <- plot_usmap(data=pr_cnty_2, values="percent", include=home_st) + 
  scale_fill_continuous(
    name="% Persons w/ Shared Imms", 
    label=scales::comma,
    # names chosen from: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
    low = "wheat1", high = "orange3"
  ) +
  labs(
    title="Percent of population with a shared shot", 
    # subtitle=paste("Partner IIS:", paste(partner_st, collapse = ', ')),
    subtitle=paste0("Statewide: ", pr_topline$percent, "%")
  ) + 
  theme(legend.position="right")
pr_cnty_map

#-----------------------------------------------------------------------------#
# 4. Stratify by demographic variables (sex, race, current age)
# Compare age/sex/race distribution among persons with vs. without a shared shot
#-----------------------------------------------------------------------------#

pr_age <- 
  pr_demog %>%
  mutate(age_today = round(time_length(difftime(Sys.Date(), dob), "years"), 2)) %>%
  mutate(age_grp = map_age_grp(age_today)) %>%
  group_by(shared, age_grp) %>%
  summarise(
    persons = n()
  ) %>%
  arrange(shared, age_grp)

pr_age_fig <- ggplot(
  pr_age %>% mutate(shared = ifelse(shared == 1, 'with', 'without')), 
  aes(x = shared, y = persons, fill = age_grp)
) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "",
    y = "proportion of persons",
    title = str_wrap(
      "Current age of persons with/without at least one shared record",
      45
    ),
    subtitle = paste0(
      "with (left): N = ", n_shared, 
      "\n", "without (right): N = ", n_total - n_shared
    ),
    caption = paste("Ages calculated as of ", Sys.Date())
  ) +
  theme(legend.title = element_blank())
pr_age_fig

  
pr_sex <- 
  pr_demog %>%
  group_by(shared, sex) %>%
  summarise(
    persons = n()
  )

pr_sex_fig <- ggplot(
  pr_sex %>% mutate(shared = ifelse(shared == 1, 'with', 'without')), 
  aes(x = shared, y = persons, fill = sex)
) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "",
    y = "proportion of persons",
    title = str_wrap(
      "Sex at birth of persons with/without at least one shared record",
      45
    ),
    subtitle = paste0(
      "with (left): N = ", n_shared, 
      "\n", "without (right): N = ", n_total - n_shared
    )
  )
pr_sex_fig

pr_race1 <- 
  pr_demog %>%
  group_by(shared, race_1) %>%
  summarise(
    persons = n()
  )

pr_race1_fig <- ggplot(
  pr_race1 %>% mutate(shared = ifelse(shared == 1, 'with', 'without')),  
  aes(x = shared, y = persons, fill = race_1)
) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "",
    y = "proportion of persons",
    title = str_wrap(
      "Racial distribution of persons with/without at least one shared record",
      45
    ),
    subtitle = paste0(
      "with (left): N = ", n_shared, 
      "\n", "without (right): N = ", n_total - n_shared
    )
  ) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.title = element_blank())
pr_race1_fig

pr_race2 <- 
  pr_demog %>%
  group_by(shared, race_2) %>%
  summarise(
    persons = n()
  )

pr_race2_fig <- ggplot(
  pr_race2 %>% mutate(shared = ifelse(shared == 1, 'with', 'without')), 
  aes(x = shared, y = persons, fill = race_2)
) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "",
    y = "proportion of persons",
    title = str_wrap(
      "Racial distribution of persons with/without at least one shared record",
      45
    ),
    subtitle = paste0(
      "with (left): N = ", n_shared, 
      "\n", "without (right): N = ", n_total - n_shared
    )
  ) +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.title = element_blank())
pr_race2_fig


pr_eth <- 
  pr_demog %>%
  group_by(shared, ethnicity_1) %>%
  summarise(
    persons = n()
  )

pr_eth_fig <- ggplot(
  pr_eth %>% mutate(shared = ifelse(shared == 1, 'with', 'without')), 
  aes(x = shared, y = persons, fill = ethnicity_1)
) +
  geom_bar(position = "fill", stat = "identity") +
  labs(
    x = "",
    y = "proportion of persons",
    title = str_wrap(
      "Hispanic ethnicity of persons with/without at least one shared record",
      45
    ),
    subtitle = paste0(
      "with (left): N = ", n_shared, 
      "\n", "without (right): N = ", n_total - n_shared
    )
  ) +
  scale_fill_brewer(palette="Set1") +
  theme(legend.title = element_blank())
pr_eth_fig

#-----------------------------------------------------------------------------#
# 5. export results
#-----------------------------------------------------------------------------#
# Create the output (sub)directory
if (!dir.exists("output/2_persons")) {
  dir.create("output/2_persons", recursive = TRUE)
}

# export raw dataframes as csv files
write.csv(pr_topline, 
          file = paste0("output/2_persons/pr_topline-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(pr_state, 
          file = paste0("output/2_persons/pr_state-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(pr_cnty, 
          file = paste0("output/2_persons/pr_cnty-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(pr_age, 
          file = paste0("output/2_persons/pr_age-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(pr_sex, 
          file = paste0("output/2_persons/pr_sex-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(pr_race1, 
          file = paste0("output/2_persons/pr_race1-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(pr_race2, 
          file = paste0("output/2_persons/pr_race2-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(pr_eth, 
          file = paste0("output/2_persons/pr_eth-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)


# export formatted tables as word docs
save_as_docx(pr_state_tbl,
             path = paste0("output/2_persons/pr_state-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".docx"))
save_as_docx(pr_cnty_tbl,
             path = paste0("output/2_persons/pr_cnty-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".docx"))

# export figures as png files
ggsave(filename = paste0("output/2_persons/pr_age-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = pr_age_fig)
ggsave(filename = paste0("output/2_persons/pr_sex-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = pr_sex_fig)
ggsave(filename = paste0("output/2_persons/pr_race1-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = pr_race1_fig)
ggsave(filename = paste0("output/2_persons/pr_race2-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = pr_race2_fig)
ggsave(filename = paste0("output/2_persons/pr_eth-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = pr_eth_fig)
ggsave(filename = paste0("output/2_persons/pr_cnty_map-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = pr_cnty_map)
