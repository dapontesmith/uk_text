setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data")
library(tidyverse)

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
