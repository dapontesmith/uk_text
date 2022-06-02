# SCRIPT TO MERGE PLACES-COUNTS DATA WITH VARIOUS DATA AT CONSTITUENCY LEVEL

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
library(tidyverse)
library(parlitools)

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
         parliament = as.numeric(paste("20", parliament, sep = ""))) %>% 
  group_by(ons_const_id) %>% 
  mutate(majority_previous = lag(majority))

test <- full %>% 
  select(date:government) %>% 
  left_join(., majorities, 
                  by = c("constit" = "constituency_name",
                         "parliament"))
###### next - need to merge in the 2005-1997 results



summary(felm(data = test, prop ~ government + majority | 
               party + year | 0 | 0))
