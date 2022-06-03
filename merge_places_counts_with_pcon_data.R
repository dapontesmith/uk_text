# SCRIPT TO MERGE PLACES-COUNTS DATA WITH VARIOUS DATA AT CONSTITUENCY LEVEL

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
library(tidyverse)
library(parlitools)
library(Hmisc)

# read in and deal with places data 
df01 <- read_csv("uk_geography/places_output/out2001.csv")
df05 <- read_csv("uk_geography/places_output/out2005.csv")
df10 <- read_csv("uk_geography/places_output/out2010.csv")
df15 <- read_csv("uk_geography/places_output/out2015.csv")
df17 <- read_csv("uk_geography/places_output/out2017.csv")

election2015 <- as.Date("2015-05-07")
election2010 <- as.Date("2010-05-06")
election2005 <- as.Date("2005-05-05")
election2001 <- as.Date("2001-06-07")

full <- bind_rows(
  df01, df05, df10, df15, df17
) %>% 
  rename(count = place_mentions_in_constit) %>% 
  mutate(prop = count / terms) %>% 
  mutate(government = case_when(
    party == "Lab" & date < election2010 ~ 1, 
    party == "Con" & date > election2010 ~ 1, 
    party == "LibDem" & date > election2010 & date < election2015 ~ 1,
    TRUE ~ 0
  ))

# merge in data on size of MP's majority
majorities <- bes_2015 %>% 
  select(ons_const_id, constituency_name, starts_with("majority")) %>% 
  left_join(., 
            bes_2017 %>% select(ons_const_id, constituency_name, majority_17),
            by = c("ons_const_id","constituency_name")) %>% 
  pivot_longer(cols = starts_with('majority'), 
               names_to = "parliament", 
               values_to = "majority") %>% 
  mutate(parliament = str_remove(parliament, "majority_"),
         parliament = as.numeric(paste("20", parliament, sep = ""))) 


majorities_1997_2005 <- read_csv("results2005clean.csv") %>% 
  select(constituency_name = Name, majority) %>% 
  mutate(parliament = 2005) %>% 
  bind_rows(.,
            read_csv("results2001clean.csv") %>% 
              select(constituency_name = Name, majority) %>% 
              mutate(parliament = 2001)
  ) %>% 
  bind_rows(.,
    read_csv("results1997clean.csv") %>% 
      select(constituency_name = Name, majority) %>% 
      mutate(parliament = 1997)
  ) %>% 
  mutate(majority = 100*majority)

majorities <- bind_rows(majorities, majorities_1997_2005) %>% 
  arrange(constituency_name, parliament) %>% 
  group_by(constituency_name) %>% 
  mutate(majority_prev = lag(majority)) %>% 
  mutate(constituency_name = case_when(
    constituency_name == "Wrekin, The" ~ "The Wrekin",
    constituency_name == "Ayr Carrick and Cumnock" ~ "Ayr, Carrick and Cumnock",
    constituency_name == "Surrey South West" ~ "South West Surrey",
    constituency_name == "Worcestershire West" ~ "West Worcestershire",
    constituency_name == "Essex North" ~ "North Essex",
    constituency_name == "Worcestershire Mid" ~ "Mid Worcestershire",
    constituency_name == "Derbyshire North East" ~ "North East Derbyshire",
    constituency_name == "Devon East" ~ "East Devon",
    constituency_name == "Shropshire North" ~ "North Shropshire",
    constituency_name == "Torridge and Devon West" ~ "Torridge and West Devon",
    constituency_name == "Dorset West" ~ "West Dorset",
    constituency_name == "Sussex Mid" ~ "Mid Sussex",
    constituency_name == "Chester, City of" ~ "City of Chester",
    constituency_name == "Cambridgeshire North West" ~ "North West Cambridgeshire",
    constituency_name == "Hampshire North West" ~ "North West Hampshire",
    constituency_name == "Fife North East" ~ "North East Fife",
    constituency_name == "Tayside North" ~ "North Tayside",
    constituency_name == "Carmarthen West and Pembrokeshire South" ~ "Carmarthen West and South Pembrokeshire",
    constituency_name == "Cambridgeshire South" ~ "South Cambridgeshire",
    constituency_name == "Derbyshire West" ~ "West Derbyshire",
    constituency_name == "Norfolk Mid" ~ "Mid Norfolk",
    constituency_name == "Thanet North" ~ "North Thanet",
    constituency_name == "York, City of" ~ "City of York",
    constituency_name == "Durham, City of" ~ "City of Durham",
    constituency_name == "Chester, City of" ~ "City of Chester",
    constituency_name == "York, City Of" ~ "City of York",
    constituency_name == "Durham, City Of" ~ "City of Durham",
    constituency_name == "Chester, City Of" ~ "City of Chester",
    constituency_name == "Dorset South" ~ "South Dorset",
    constituency_name == "Suffolk South" ~ "South Suffolk",
    constituency_name == "Wiltshire North" ~ "North Wiltshire",
    constituency_name == "Cornwall South East" ~ "South East Cornwall",
    constituency_name == "Leicestershire North West" ~ "North West Leicestershire",
    constituency_name == "Richmond" ~ "Richmond (Yorks)",
    constituency_name == "Norfolk South" ~ "South Norfolk",
    constituency_name == "Hertfordshire North East" ~ "North East Hertfordshire",
    constituency_name == "Norfolk North" ~ "North Norfolk",
    constituency_name == "Dorset North" ~ "North Dorset",
    constituency_name == "Hertfordshire South West" ~ "South West Hertfordshire",
    constituency_name == "Cambridgeshire South East" ~ "South East Cambridgeshire",
    constituency_name == "Cornwall North" ~ "North Cornwall",
    constituency_name == "Warwickshire North" ~ "North Warwickshire",
    constituency_name == "Durham North West" ~ "North West Durham",
    constituency_name == "Derbyshire South" ~ "South Derbyshire",
    constituency_name == "Yorkshire East" ~ "East Yorkshire",
    constituency_name == "Dorset Mid and Poole North" ~ "Mid Dorset and North Poole",
    constituency_name == "Devon North" ~ "North Devon",
    constituency_name == "Maldon and Chelmsford East" ~ "Maldon and East Chelmsford",
    constituency_name == "Hampshire East" ~ "East Hampshire",
    constituency_name == "Bedfordshire Mid" ~ "Mid Bedfordshire",
    constituency_name == "Norfolk South West" ~ "South West Norfolk",
    constituency_name == "Manchester Central " ~ "Manchester Central",
    constituency_name == "Lancashire" ~ "West Lancashire",
    constituency_name == "Kingston upon Hull West and Hessle " ~ "Kingston upon Hull West and Hessle",
    constituency_name == "Norfolk North West" ~ "North West Norfolk",
    constituency_name == "Durham North" ~ "North Durham",
    constituency_name == "Tyneside North" ~ "North Tyneside",
    constituency_name == "Surrey East" ~ "East Surrey",
    constituency_name == "Hampshire North East" ~ "North East Hampshire",
    constituency_name == "Bedfordshire North East" ~ "North East Bedfordshire",
    constituency_name == "Thanet South" ~ "South Thanet",
    constituency_name == "Faversham and Kent Mid" ~ "Faversham and Mid Kent",
    constituency_name == "Weston-super-Mare" ~ "Weston-Super-Mare",
    constituency_name == "Suffolk West" ~ "West Suffolk",
    constituency_name == "Devon South West" ~ "South West Devon",
    constituency_name == "Staffordshire South" ~ "South Staffordshire",
    constituency_name == "Milton Keynes North East" ~ "North East Milton Keynes",
    constituency_name == "Fife Central" ~ "Central Fife",
    constituency_name == "Durham, City of" ~ "City of Durham",
    constituency_name == "Swindon South" ~ "South Swindon",
    constituency_name == "Cambridgeshire North East" ~ "North East Cambridgeshire",
    constituency_name == "Ribble South" ~ "South Ribble",
    constituency_name == "Renfrewshire West" ~ "West Renfrewshire",
    constituency_name == "Bedfordshire South West" ~ "South West Bedfordshire",
    constituency_name == "Worthing East and Shoreham" ~ "East Worthing and Shoreham",
    constituency_name == "Southwark North and Bermondsey" ~ "North Southwark and Bermondsey",
    constituency_name == "Middlesbrough South and Cleveland East" ~ "Middlesbrough South and East Cleveland",
    constituency_name == "Chelmsford West" ~ "West Chelmsford",
    constituency_name == "Swindon North" ~ "North Swindon",
    constituency_name == "Inverness Nairn Badenoch and Strathspey" ~ "Inverness, Nairn, Badenoch and Strathspey",
    constituency_name == "Ross Skye and Lochaber" ~ "Ross, Skye and Lochaber",
    constituency_name == "Caithness Sutherland and Easter Ross" ~ "Caithness, Sutherland and Easter Ross",
    constituency_name == "Ealing Acton and Shepherd's Bush" ~ "Ealing, Acton and Shepherd's Bush",
    constituency_name == "Dunbartonshire West" ~ "West Dunbartonshire",
    constituency_name == "Ayrshire Central" ~ "Central Ayrshire",
    constituency_name == "Renfrewshire East" ~ "East Renfrewshire",
    constituency_name == "Ayrshire North and Arran" ~ "North Ayrshire and Arran",
    constituency_name == "Dunbartonshire East" ~ "East Dunbartonshire",
    constituency_name == "Romsey and Southampton North " ~ "Romsey and Southampton North",
    constituency_name == "Lancashire West" ~ "West Lancashire",
    TRUE ~ constituency_name
  ), 
  constituency_name = str_replace(constituency_name, "Birmingham ", "Birmingham, "),
  constituency_name =str_replace(constituency_name, "Liverpool ", "Liverpool, "),
  constituency_name =str_replace(constituency_name, "Manchester ", "Manchester, "),
  constituency_name =str_replace(constituency_name, "Plymouth ", "Plymouth, "),
  constituency_name =str_replace(constituency_name, "Southampton ", "Southampton, "))

full <- full %>% 
  select(date:government) %>% 
  left_join(., majorities, 
                  by = c("constit" = "constituency_name",
                         "parliament"))

# read in median income data 
annual_pay <- read_csv("uk_geography/pcon_data/median_incomes/annual_pay_2001_2021_clean.csv") %>% 
  mutate(area = str_replace(area, "&","and"),
         area = str_replace(area, "Birmingham ", "Birmingham, ")) %>% 
  mutate(area = case_when(
    str_detect(area, "Ynys") == TRUE ~ "Ynys Mon",
    TRUE ~ area
  )) %>% 
  distinct(area, year, median, median_scale, pcon_nat_median_diff, pcon_nat_median_diff_scale) %>% 
  group_by(area) %>% 
  # some values are missing because they do not appear in the annual pay csvs - figure out some way of filling them in?
  # i will simply interpolate with the last known value
  mutate(median = na.approx(median, na.rm = FALSE, rule = 2),
         median_scale = na.approx(median_scale, na.rm = FALSE, rule = 2),
         pcon_nat_median_diff = na.approx(pcon_nat_median_diff, na.rm = FALSE, rule = 2),
         pcon_nat_median_diff_scale = na.approx(pcon_nat_median_diff_scale, na.rm = FALSE, rule = 2),
         ) %>% 
  filter(!is.na(median))

# join to the full data
full <- full %>% 
  left_join(., annual_pay, 
            by = c("constit" = "area","year"))

# join in census data from 2011 census 
cen <- census_11 %>% 
  select(ons_const_id, constituency_name, region, constituency_type, 
         population, population_density, house_owned,
         ethnicity_white_british, starts_with("industry")) %>% 
  mutate(white_british_pct = ethnicity_white_british / population) %>% 
  mutate(constituency_name = as.character(constituency_name)) %>% 
  mutate(constituency_name = ifelse(constituency_name == "Carmarthen West and Pembrokeshire South",
                                    "Carmarthen West and South Pembrokeshire", constituency_name), 
         census_year = 2011)

full <- full %>% 
  mutate(census_year = case_when(
    year >= 2011 ~ 2011, 
    TRUE ~ 2001
  )) %>% 
  left_join(., cen, by = c("constit" = "constituency_name", "census_year"))



# group by constituency nad month?
grouped_constit_month <- full %>% 
  group_by(constit, median, median_scale, party, government, majority, my, year, 
           region, population_density, constituency_type, white_british_pct) %>% 
  dplyr::summarize(prop = sum(count) / sum(terms)) 

fullmod <- felm(data = full, scale(prop)[,1] ~scale(majority)[,1] +
                  median_scale + scale(population_density)[,1] + 
                  scale(white_british_pct)[,1]  | 
                  party + year| 0 | party+year)
groupedmod <- (felm(data = grouped_constit_month, scale(prop)[,1] ~  scale(majority)[,1] +
               median_scale + scale(population_density)[,1] + 
                 scale(white_british_pct)[,1] | 
               region + year | 0 | region + year))




modelsummary(list("full" = fullmod, "grouped" = groupedmod), 
             stars = TRUE)
