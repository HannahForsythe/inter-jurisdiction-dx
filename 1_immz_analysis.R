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

immz_shared <- immz %>%
  filter(reporting_iis %in% c("P", "P1", "P2"))

crosswalk_cvx <- read_csv(
  "input/crosswalk_CVX_nm.csv"
) %>%
  clean_names()
#-----------------------------------------------------------------------------#
# 1. Number of records added to the native IIS, per year
#-----------------------------------------------------------------------------#

iz_yr <- immz_shared %>%
  group_by(report_yr) %>%
  summarise(n = n_distinct(vax_event_id))

iz_yr_tbl <- iz_yr %>%
  adorn_totals() %>%
  rename(`year reported` = report_yr) %>%
  head() %>%
  flextable() %>%
  autofit() %>%
  set_caption(
    paste0("Immunizations reported to ", home_st, " by ", paste(partner_st, collapse = ', '))
  )
iz_yr_tbl

#-----------------------------------------------------------------------------#
# 2. Stratify by recipient county
#-----------------------------------------------------------------------------#

iz_cnty <- left_join(immz_shared, demog, by = "person_id") %>%
  group_by(report_yr, county_fips) %>%
  summarise(n_shots = n_distinct(vax_event_id)) %>%
  mutate(county_nm = map_cnty_fips(county_fips))

iz_cnty_tbl <- iz_cnty %>%
  pivot_wider(names_from = report_yr, values_from = n_shots) %>%
  arrange(county_fips) %>%
  rename(FIPS = county_fips, County = county_nm) %>%
  replace_na(list(County = "Moved")) %>%
  adorn_totals() %>%
  # head() %>%
  flextable() %>%
  # autofit() %>%
  set_caption(
    paste0("Immunizations reported to ", 
           home_st, 
           " by ", 
           paste(partner_st, collapse = ', '), 
           " each year, by recipient's current county of residence.")
  ) %>%
  add_footer_lines(paste("Note: At time of vaccination, all recipients resided within", home_st))
iz_cnty_tbl

# Create a map
# Sum shots across all years
iz_cnty_2 <- left_join(immz_shared, demog, by = "person_id") %>%
  group_by(county_fips) %>%
  summarise(n_shots = n_distinct(vax_event_id)) %>%
  mutate(county_nm = map_cnty_fips(county_fips)) %>%
  rename("fips"="county_fips")

iz_cnty_map <- plot_usmap(data=iz_cnty_2, values="n_shots", include=home_st) +
  scale_fill_continuous(
    name="# Shared Shots", 
    label=scales::comma,
    # names chosen from: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
    low = "skyblue", high = "steelblue4"
  ) +
  labs(
    title=paste("Count of Shared Shots by", home_st, "County"), 
    subtitle=paste("Partner IIS:", paste(partner_st, collapse = ', '))
  ) +
  theme(legend.position="right")

iz_cnty_map

#-----------------------------------------------------------------------------#
# 3. stratify by category and antigen group
#-----------------------------------------------------------------------------#
# this is the raw data, by cvx
iz_cvx <- immz_shared %>%
  left_join(crosswalk_cvx, by = "cvx") %>%
  group_by(vax_cat, vax_cat_antigen, cvx) %>%
  summarise(n = n_distinct(vax_event_id)) 

# create a table by broad category (seasonal, child, adol, adu, other)
iz_cat <- iz_cvx %>%
  group_by(vax_cat) %>%
  summarise(n = sum(n)) %>%
  arrange(-n)

iz_cat_tbl <- iz_cat %>%
  adorn_totals() %>%
  rename(`category` = vax_cat) %>%
  # head() %>%
  flextable() %>%
  autofit() %>%
  set_caption(
    paste0("Out-of-state immunizations reported to ", home_st)
  )
iz_cat_tbl

# create a graph crossing broad vaccine categories with antigen type
iz_type <- iz_cvx %>%
  group_by(vax_cat, vax_cat_antigen) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  arrange(vax_cat, -n) %>%
  mutate(
    order = row_number()
  )
  
# create a palette with 17 colors
Dark2_17 <- c("#1B9E77", "#6E8243", "#D55E00", "#B96439", "#8D6B86", "#8A62AB",
              "#BC4399", "#DE3083", "#A66753", "#6E9E24", "#CEAA07", "#D59D08", 
              "#666666", "#96A713", "darkgreen", "#9D7426", "#0072B2")

iz_type_fig <- iz_type %>%
  ggplot(
    aes(y = vax_cat, 
        x = n, 
        fill = reorder(vax_cat_antigen, -order))
  ) +
  geom_col(color = "black") +
  labs(
    title = "Vaccinations received out of state, by category",
    x = "",
    y = ""
  ) +
  # scale_fill_discrete(name = "Vaccine antigen(s)") +
  theme_bw() +
  scale_fill_manual(values = Dark2_17, name = "Vaccine antigen(s)")
iz_type_fig

#-----------------------------------------------------------------------------#
# 4. Average reporting delay
#-----------------------------------------------------------------------------#
rpt_delay <- immz %>%
  filter(Sys.Date() > admin_date) %>%
  mutate(report_delay = as.numeric(paste(report_date - admin_date)))

rpt_delay_hist_N <- rpt_delay %>%
  filter(reporting_iis == 'N') %>%
  ggplot(aes(x = report_delay)) +
  geom_histogram(color = "darkblue", fill = "darkblue", alpha = 0.5, binwidth = 7) +
  theme_classic() +
  labs(
    title = str_wrap(
      paste0(
        "Delay between administration and reporting, for shots administered within ",
        home_st),
      45
    ), 
    subtitle = paste0(min(rpt_delay$admin_date), " to ", max(rpt_delay$admin_date), ""), 
    x = "number of days"
    )
rpt_delay_hist_N

rpt_delay_hist_P <- rpt_delay %>%
  filter(reporting_iis != 'N') %>%
  ggplot(aes(x = report_delay, fill = reporting_iis, color = reporting_iis)) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 7) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  theme_classic() +
  labs(
    title = str_wrap(
      paste0(
        "Delay between administration and reporting, for shots administered within ",
        paste(partner_st, collapse = ', ')
      ),
      45
    ), 
    subtitle = paste0(min(rpt_delay$admin_date), " to ", max(rpt_delay$admin_date), ""), 
    x = "number of days"
  ) +
  theme(legend.position="bottom")
rpt_delay_hist_P


#-----------------------------------------------------------------------------#
# 5. export results
#-----------------------------------------------------------------------------#
# Create the output (sub)directory
if (!dir.exists("output/1_immz")) {
  dir.create("output/1_immz", recursive = TRUE)
}

# export raw dataframes as csv files
write.csv(iz_yr, 
          file = paste0("output/1_immz/iz_yr-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(iz_cnty, 
          file = paste0("output/1_immz/iz_cnty-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(iz_cvx, 
          file = paste0("output/1_immz/iz_cvx_cat_antigen-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)
write.csv(rpt_delay, 
          file = paste0("output/1_immz/rpt_delay-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".csv"),
          row.names = FALSE)

# export formatted tables as word docs
save_as_docx(iz_yr_tbl,
             path = paste0("output/1_immz/iz_yr-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".docx"))
save_as_docx(iz_cnty_tbl,
             path = paste0("output/1_immz/iz_cnty-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".docx"))
save_as_docx(iz_cat_tbl,
             path = paste0("output/1_immz/iz_cat-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".docx"))

# export figures as png files
ggsave(filename = paste0("output/1_immz/iz_cnty_map-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = iz_cnty_map)
ggsave(filename = paste0("output/1_immz/iz_type_fig-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = iz_type_fig)
ggsave(filename = paste0("output/1_immz/rpt_delay-", home_st, "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = rpt_delay_hist_N)
ggsave(filename = paste0("output/1_immz/rpt_delay-", paste0(partner_st, collapse = '-'), "-", format(Sys.Date(), "%Y%m%d"),".png"),
       plot = rpt_delay_hist_P)

