# What is the state code for your home jurisdiction?
home_st <- 'MI'
# What are the state codes of the jurisdictions you exchange data with?
partner_st <- c('WI')
# What are the race_2 categories in your demographic file (input/demog.zip)?
race_2_vals <- c("Hispanic", "NH Black", "NH White", "AI/AN", 
                 "NH Asian/Hawaian/OPI", "NH MENA", "Other", "UNK")
# When did data exchange begin (earliest date)?
exchg_start <- as.Date('2019-01-01')
# When did data exchange end?
exchg_end <- as.Date('2023-01-01')
# What are the fips codes of border counties in your state?
border_fips <- c(26041, 26043, 26053, 26071, 26109, 26131)
