# load libraries
library(magrittr)
library(tidyverse)
library(helminthR)
library(tidygeocoder)

# get helminthR data
x <- helminthR::loadData()
# remove humans (just because its 50k records we don't need)
x %<>% dplyr::filter(hostSpecies!="Homo sapiens")
data(locations)
# remove locations that are not georeferenced (creates many-to-many issues on join)
# it appears to be when a location has both a real lat/long and an NA lat/long in the locations data
locations %<>% tidyr::drop_na(Latitude)

# Reverse geocode to find location names
chkLocs <- locations %>% reverse_geocode(lat=Latitude,long=Longitude,method="osm",full_results=T)
chkLocsTerse <- chkLocs %>% dplyr::select(Location,Latitude,Longitude,state,country)
chkLocsTerse %<>% tidyr::drop_na(country)
