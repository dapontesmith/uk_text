setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/shapefiles/")
library(sf)
library(lwgeom)
library(tidyverse)
sf_use_s2(FALSE)
# define point of westminster
point <- tibble(
  long = -0.124605,
  lat = 51.499798
)
point_sf <- st_as_sf(point, coords = c("long","lat"), crs = "WGS84")

# function to get distance between westminster and centroids of all constituencies, in meters 
get_distance <- function(path, year){
  shp <- st_read(path)
  shp <- st_transform(shp, "+proj=longlat +datum=WGS84")
  out <- st_distance(point_sf, shp) %>% 
    t() %>% as_tibble() %>% 
    rename(distance = value)
  full_out <- bind_cols(shp, out) %>% 
    arrange(distance) %>% 
    mutate(distance = as.numeric(distance)) %>% 
    select(starts_with("pcon"), starts_with("NAME"), distance) %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    mutate(year = year)
  
  names <- tolower(names(full_out))
  names <- ifelse(str_detect(names, "cd") == TRUE, "code", names)
  names <- ifelse(str_detect(names, "nm") == TRUE, "constituency_name", names)
  names <- ifelse(str_detect(names, "name") == TRUE, "constituency_name", names)
  names(full_out) <- names
  return(full_out)
}

out2017 <- get_distance("const2017/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK.shp", 2017)
out2015 <- get_distance("const2015/Westminster_Parliamentary_Constituencies_(December_2015)_Boundaries.shp", 2015)
out2010 <- get_distance("const2010/westminster_const_region.shp", 2010) 
out2005 <- get_distance("const2001_2005/PCON_DEC_2005_GB_BFE.shp", 2005)
out2001 <- get_distance("const2001_2005/PCON_DEC_2001_GB_BFE.shp", year = 2001)


full <- bind_rows(out2001, out2005, out2010, out2015, out2017) %>% 
  mutate(constituency_name = str_remove(constituency_name, " Co Const"),
         constituency_name = str_remove(constituency_name, " Boro Const"),
         constituency_name = str_remove(constituency_name, " Burgh Const")) %>% 
  arrange(constituency_name) %>% 
  select(-code) 

setwd("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/pcon_data")
write.csv(full, "constituencies_distance.csv")


  

