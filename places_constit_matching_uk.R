library(tidyverse)
library(sf)

setwd('C:/Users/dapon/Dropbox/Harvard/dissertation/data')

places <- read_csv("uk_geography/uk_placenames_wiki.csv") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>% # get rid of NAs - this we must do 
  as_tibble() %>%
  select(-coordinates) %>% 
  mutate(index = seq(1, nrow(.), 1)) #create index for merging later

#read in shapefile 
shp <- st_read("uk_geography/Westminster_Parliamentary_Constituencies__December_2017__Boundaries_UK.shp")
shp_crs <- st_crs(shp)



#transform coordinates of places into sf file
places_sf <- sf::st_as_sf(places, coords=c("longitude", "latitude"), crs = "WGS84")
places_sf <- st_transform(places_sf, crs = shp_crs) 


#the col.id column corresponds to constituencies by their row placement in shp
inter <- sf::st_intersects(places_sf, shp, sparse = TRUE) %>% as.data.frame() %>% as_tibble()
#note that some places don't have constituencies (entry 9, for instance)


#get vector of constituency names
constits <- cbind(shp$pcon17cd, shp$pcon17nm) %>% as.data.frame() %>%
  rename(code = V1, name = V2)

#loop through inter$col.id to match the numbers to constituency names 
const <- matrix(nrow = nrow(inter), ncol = 2)

#get corresponding constituency codes and names 
for( i in 1:nrow(inter)) {
  print(i)
  const[i,1] <- constits[inter$col.id[i],1] # this is constituency code
  const[i,2] <- constits[inter$col.id[i],2] # constituency name
}
const <- const %>% as_tibble() %>%
  rename(code = V1, name = V2)

#put the index in there for merging
const$index <- (inter$row.id)

#places <- places %>% mutate(index = as.character(index))

#join the datasets 
full <- left_join(places, const, by = "index")

#write csv 
write.csv(full, file = "uk_geography/places_constits_matched_2017.csv")



########################################################################
############# 2015 CONSTITUENCIES

places <- read_csv("uk_geography/uk_placenames_wiki.csv") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>% # get rid of NAs - this we must do 
  as_tibble() %>%
  select(-coordinates) %>% 
  mutate(index = seq(1, nrow(.), 1)) #create index for merging later

#read in shapefile 
shp <- st_read("uk_geography/shapefiles/const2010/westminster_const_region.shp")
shp_crs <- st_crs(shp)

#transform coordinates of places into sf file
places_sf <- sf::st_as_sf(places, coords=c("longitude", "latitude"), crs = "WGS84")
places_sf <- st_transform(places_sf, crs = shp_crs) 

#the col.id column corresponds to constituencies by their row placement in shp
inter <- sf::st_intersects(places_sf, shp, sparse = TRUE) %>% as.data.frame() %>% as_tibble()
#note that some places don't have constituencies (entry 9, for instance)

shp$NAME <- str_replace_all(shp$NAME, c(" Co Const"," Boro Const"), "")
#get vector of constituency names
constits <- as_tibble(shp$NAME) %>% rename(name = value)
#loop through inter$col.id to match the numbers to constituency names 
const <- NULL ; const_out <- NULL

#get corresponding constituency codes and names 
for( i in 1:nrow(inter)) {
  print(i)
  const[i] <- constits[inter$col.id[i],][[1]]
  const_out[i] <- const[[i]]
  # this is constituency name
}
const_out <- const_out %>% 
  as_tibble() %>%
  mutate(value = str_remove(value, " Co Const"),
         index = inter$row.id) 

const2010 <- left_join(places, const_out, by = "index") %>%
  select(location:longitude, constit = value)

write.csv(const2010, file = "uk_geography/places_constits_matched_2010.csv")

  
  

##########
# do this for 2015 constituencies 
places <- read_csv("uk_geography/uk_placenames_wiki.csv") %>%
  filter(!is.na(latitude), !is.na(longitude)) %>% # get rid of NAs - this we must do 
  as_tibble() %>%
  select(-coordinates) %>% 
  mutate(index = seq(1, nrow(.), 1)) #create index for merging later

#read in shapefile 
shp <- st_read("uk_geography/shapefiles/const2015/Westminster_Parliamentary_Constituencies_(December_2015)_Boundaries.shp")
shp_crs <- st_crs(shp)

#transform coordinates of places into sf file
places_sf <- sf::st_as_sf(places, coords=c("longitude", "latitude"), crs = "WGS84")

places_sf <- st_transform(places_sf, crs = shp_crs) 
#the col.id column corresponds to constituencies by their row placement in shp
inter <- sf::st_intersects(places_sf, shp, sparse = TRUE) %>% as.data.frame() %>% as_tibble()
#note that some places don't have constituencies (entry 9, for instance)

#get vector of constituency names
constits <- cbind(shp$pcon15cd, shp$pcon15nm) %>% as.data.frame() %>%
  rename(code = V1, name = V2)

#loop through inter$col.id to match the numbers to constituency names 
const <- matrix(nrow = nrow(inter), ncol = 2)

#get corresponding constituency codes and names 
for( i in 1:nrow(inter)) {
  print(i)
  const[i,1] <- constits[inter$col.id[i],1] # this is constituency code
  const[i,2] <- constits[inter$col.id[i],2] # constituency name
}
const <- const %>% as_tibble() %>%
  rename(code = V1, name = V2) %>% 
  mutate(index = inter$row.id)


#places <- places %>% mutate(index = as.character(index))

#join the datasets 
out15 <- left_join(places, const, by = "index")

write.csv(out15, file = "uk_geography/places_constits_matched_2015.csv")


############################
## 2001 
shp <- st_read("uk_geography/shapefiles/const2001_2005/PCON_DEC_2001_GB_BFE.shp")
shp_crs <- st_crs(shp)
#transform coordinates of places into sf file
places_sf <- sf::st_as_sf(places, coords=c("longitude", "latitude"), crs = "WGS84")
places_sf <- st_transform(places_sf, crs = shp_crs) 
inter <- sf::st_intersects(places_sf, shp, sparse = TRUE) %>% as.data.frame() %>% as_tibble()
constits <- cbind(shp$PCON01CD, shp$PCON01NM) %>% as.data.frame() %>%
  rename(code = V1, name = V2)
#loop through inter$col.id to match the numbers to constituency names 
const <- matrix(nrow = nrow(inter), ncol = 2)
#get corresponding constituency codes and names 
for( i in 1:nrow(inter)) {
  print(i)
  const[i,1] <- constits[inter$col.id[i],1] # this is constituency code
  const[i,2] <- constits[inter$col.id[i],2] # constituency name
}
const <- const %>% as_tibble() %>%
  rename(code = V1, name = V2) %>% 
  mutate(index = inter$row.id)

#join the datasets 
out01 <- left_join(places, const, by = "index")

write.csv(out01, file = "uk_geography/places_constits_matched_2001.csv")


#########
# 2005
## 
shp <- st_read("uk_geography/shapefiles/const2001_2005/PCON_DEC_2005_GB_BFE.shp")
shp_crs <- st_crs(shp)
#transform coordinates of places into sf file
places_sf <- sf::st_as_sf(places, coords=c("longitude", "latitude"), crs = "WGS84")
places_sf <- st_transform(places_sf, crs = shp_crs) 
inter <- sf::st_intersects(places_sf, shp, sparse = TRUE) %>% as.data.frame() %>% as_tibble()
constits <- cbind(shp$PCON05CD, shp$PCON05NM) %>% as.data.frame() %>%
  rename(code = V1, name = V2)
#loop through inter$col.id to match the numbers to constituency names 
const <- matrix(nrow = nrow(inter), ncol = 2)
#get corresponding constituency codes and names 
for( i in 1:nrow(inter)) {
  print(i)
  const[i,1] <- constits[inter$col.id[i],1] # this is constituency code
  const[i,2] <- constits[inter$col.id[i],2] # constituency name
}
const <- const %>% as_tibble() %>%
  rename(code = V1, name = V2) %>% 
  mutate(index = inter$row.id)

#join the datasets 
out05 <- left_join(places, const, by = "index")
write.csv(out05, file = "uk_geography/places_constits_matched_2005.csv")
