library(magrittr)
library(tidyverse)
library(helminthR)
library(tidygeocoder)


h <- helminthR::loadData()
loc <- helminthR::locations

test <- tibble(loc$Location[6])
names(test) <- "addr"
test %<>% dplyr::mutate(name="test$addr")
tidygeocoder::geocode(test)

library(dplyr, warn.conflicts = FALSE)
sample_addresses %>%
  slice(1:2) %>%
  geocode(addr, method = "arcgis")


library(CoordinateCleaner)

#country centroids
countryCentroids <-CoordinateCleaner::countryref
countryCentroids %<>% dplyr::filter(source=="geolocate") %>% dplyr::distinct()
countryCentroids %<>% dplyr::group_by(name) %>%
  dplyr::reframe(centroid.lat=mean(centroid.lat),centroid.lon=mean(centroid.lon))
countryCentroids %<>% dplyr::rename(Location=name)
#capital centroids
capitalCentroids <-CoordinateCleaner::countryref
capitalCentroids %<>% dplyr::filter(source=="geolocate") %>% dplyr::distinct()
capitalCentroids %<>% dplyr::group_by(capital) %>%
  dplyr::reframe(centroid.lat=mean(centroid.lat),centroid.lon=mean(centroid.lon))
capitalCentroids %<>% dplyr::rename(Location=capital)

x <- readr::read_csv("locationData.csv",locale=locale(encoding="Latin1"))

x %<>% dplyr::filter(`Quality check`=="error")

x %<>% dplyr::left_join(.,capitalCentroids)



y <- CoordinateCleaner::countryref

###which victoria
h.vic <- h %>% dplyr::filter(country=="Victoria")
g.eMed <- h %>% dplyr::filter(country=="Eastern Mediterranean")
