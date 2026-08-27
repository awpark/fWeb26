#load libraries
library(magrittr)
library(tidyverse)
library(here)
library(helminthR)
library(taxize)
#library(tidygeocoder) #referenced here but not used explicitly

#read benesh data
benesh <- here::here("CLC_database_hosts.csv")
b <- readr::read_csv(benesh,
                     locale=locale(encoding="latin1"))

#remove all (parnthetical) [text] from Parasite.species column
pattern <- "\\s*\\([^)]*\\)|\\s*\\[[^]]*\\]"
b %<>% dplyr::mutate(Parasite.species=
                           stringr::str_remove_all(Parasite.species,pattern))

#drop taxa not identified to species (sp., cf., a/b)
b %<>% dplyr::filter(stringr::str_detect(Parasite.species,"sp\\.")==F)
b %<>% dplyr::filter(stringr::str_detect(Parasite.species,"cf\\.")==F)
b %<>% dplyr::filter(stringr::str_detect(Parasite.species,"\\/")==F)

#remove "3rd" words (subspecies, sensu stricto etc.)
b %<>% dplyr::mutate(Parasite.species=stringr::word(Parasite.species,1,2))

#remove unintended white space
b %<>% dplyr::mutate(Parasite.species=stringr::str_squish(Parasite.species))

#remove duplicates
b %<>% dplyr::distinct()

#how many habitats per parasite from benesh data?
bHab <- b %>% dplyr::select(Parasite.species,Host.habitat) %>% distinct()

#keep only rows where parasite appears 2 or more times
bHab2 <- bHab %>% dplyr::add_count(Parasite.species) %>%
  dplyr::filter(n >= 2) %>%
  dplyr::select(-n) 

bHab2 %<>%
  dplyr::mutate(present = 1) %>% 
  tidyr::pivot_wider(
    names_from = Host.habitat,
    values_from = present,
    values_fill = 0
  )

#for now, filter out these "multihabitat" parasites
b %<>% dplyr::filter(!Parasite.species %in% bHab2$Parasite.species)

#reduce to a df where each parasite is referenced once and
#calc chain length as max(host.no) per parasite species
y <- b %>% dplyr::group_by(Parasite.species) %>% dplyr::reframe(Chain.length=max(Host.no))
#Downgrade chain length of parasite genera per "big chain query" investigation
y %<>% dplyr::mutate(Chain.length=if_else(stringr::str_detect(Parasite.species,"Contracaecum")==T,3,Chain.length))
y %<>% dplyr::mutate(Chain.length=if_else(stringr::str_detect(Parasite.species,"Diphyllobothrium")==T,3,Chain.length))
y %<>% dplyr::mutate(Chain.length=if_else(stringr::str_detect(Parasite.species,"Eustrongylides")==T,3,Chain.length))
y %<>% dplyr::mutate(Chain.length=if_else(stringr::str_detect(Parasite.species,"Pseudoterranova")==T,4,Chain.length))
y %<>% dplyr::mutate(Chain.length=if_else(stringr::str_detect(Parasite.species,"Raphidascaris")==T,3,Chain.length))
y %<>% dplyr::mutate(Chain.length=if_else(stringr::str_detect(Parasite.species,"Spirometra")==T,3,Chain.length))

#check no parasite species entered twice under different chain lengths
y %<>% dplyr::distinct()
tmp <- y %>% dplyr::group_by(Parasite.species) %>% dplyr::reframe(n=n())

#add in habitat data
y %<>% dplyr::left_join(.,bHab)

#obtain helminthR parasites and location data
h <- helminthR::loadData()
loc <- helminthR::locations
h %<>% dplyr::as_tibble()
h %<>% dplyr::rename(Parasite.species=parasiteScientificName)

#cleave off parasite higher taxonomic data to join later with h and y
pTax <- h %>% dplyr::select(starts_with("parasite",ignore.case=T)) %>% dplyr::distinct()
pTax %<>% dplyr::select(-c(parasiteStatus,parasiteSuperfamily,parasiteSpecies)) %>% dplyr::distinct()
#establish parasites in common (our data and helminthR)
commonParas <- base::intersect(y$Parasite.species,h$Parasite.species)

#reduce helminthR to commonParas
h %<>% dplyr::filter(Parasite.species %in% commonParas)

#based on a reverse geocoding exercise, several locations were found to have
#incorrect lat/long, these are corrected here...
locations <- here::here("locationData.csv")
newLoc <- readr::read_csv(locations,
                     locale=locale(encoding="latin1"))
newLoc %<>% drop_na(`revised Lat`)

for (i in 1:dim(loc)[1]){
  if (loc$Location[i] %in% newLoc$Location){
    idx <- which(newLoc$Location==loc$Location[i])
    loc$Latitude[i] <- newLoc$`revised Lat`[idx];
    loc$Longitude[i] <- newLoc$`revised Long`[idx]
  }
}

#next, other locations were erroneous but vague such that geocoding didn't work
#here, we flag those records for removal (done a bit later...)
newLoc2 <- readr::read_csv(locations,
                          locale=locale(encoding="latin1"))
newLoc2 %<>% dplyr::filter(`Quality check` %in% c("drop","error"))
newLoc2 %<>% dplyr::filter(!Location %in% newLoc$Location)

#generate succinct helminthR (spp,"country") without duplication
h %<>% dplyr::select(Parasite.species,country) %>% dplyr::distinct()
h %<>% dplyr::rename(Location=country)
#remove locations we are not confident in
h %<>% dplyr::filter(!Location %in% newLoc2$Location)

#remove locations with NA lat/long (and any duplicates)
loc %<>% tidyr::drop_na()
loc %<>% dplyr::distinct()

#join lat/long (which includes fixes)
h %<>% dplyr::left_join(.,loc)

#drop NA introduced by locations not having coords
h %<>% tidyr::drop_na()

#average spatial data in h to overwrite master list
h.av <- h %>% dplyr::group_by(Parasite.species) %>%
  dplyr::reframe(minLat=min(Latitude),meanLat=mean(Latitude),maxLat=max(Latitude),
                 minLon=min(Longitude),meanLon=mean(Longitude),maxLon=max(Longitude))

#prep for update
h.av %<>% dplyr::rename(
  Max.lat=maxLat,
  Min.lat=minLat,
  Avg.lat=meanLat,
  Max.long=maxLon,
  Min.long=minLon,
  Avg.long=meanLon,
)

#add averaged georef data to y
y %<>% dplyr::left_join(.,h.av)

#remove parasite species without georef data
y %<>% tidyr::drop_na()

#add habitat and chain.length to h
chainHab4join <- y %>% dplyr::select(Parasite.species,Chain.length,Host.habitat)
h %<>% dplyr::left_join(.,chainHab4join)

#swap out synonym for certain parasites in y and h
#https://de.wikipedia.org/wiki/Subulura_suctoria
y %<>% dplyr::mutate(Parasite.species=if_else(Parasite.species=="Allodapa suctoria","Subulura suctoria",Parasite.species))
h %<>% dplyr::mutate(Parasite.species=if_else(Parasite.species=="Allodapa suctoria","Subulura suctoria",Parasite.species))
#https://www.marinespecies.org/aphia.php?p=taxdetails&id=100598&from=rss
y %<>% dplyr::mutate(Parasite.species=if_else(Parasite.species=="Echinorhynchus borealis","Echinorhynchus cinctulus",Parasite.species))
h %<>% dplyr::mutate(Parasite.species=if_else(Parasite.species=="Echinorhynchus borealis","Echinorhynchus cinctulus",Parasite.species))


#get vec of all parasite species
paraSpp <- y %>% pull(Parasite.species)

#taxQuery <- classification(paraSpp, db = "gbif")
#save(taxQuery,file="get_taxQuery.Rda")
load("get_taxQuery.Rda")

taxQueryDf <- rbind(taxQuery) %>%
  dplyr::filter(rank %in% c("kingdom","phylum","class","order","family","genus","species")) %>%
  dplyr::select(query,rank,name) %>%
  tidyr::pivot_wider(names_from=rank,values_from=name) %>%
  dplyr::rename(
    Parasite.species=query,
    parasiteKingdom=kingdom,
    parasitePhylum=phylum,
    parasiteClass=class,
    parasiteOrder=order,
    parasiteFamily=family,
    parasiteGenus=genus
  )


#pull out genera where sister species taxonomy copying is available
tax.Anoplocephala <- taxQueryDf %>%
  dplyr::filter(stringr::str_detect(Parasite.species,"Anoplocephala "))

tax.Cyrnea <- taxQueryDf %>%
  dplyr::filter(stringr::str_detect(Parasite.species,"Cyrnea "))

tax.Tetrameres <- taxQueryDf %>%
  dplyr::filter(stringr::str_detect(Parasite.species,"Tetrameres "))

#temporarily remove these species from main data frame
taxQueryDf %<>%  dplyr::filter(!stringr::str_detect(Parasite.species,"Anoplocephala "))
taxQueryDf %<>%  dplyr::filter(!stringr::str_detect(Parasite.species,"Cyrnea "))
taxQueryDf %<>%  dplyr::filter(!stringr::str_detect(Parasite.species,"Tetrameres "))

#copy taxonomic info among genus
tax.Anoplocephala %<>% tidyr::fill(starts_with("parasite"),.direction="down")
tax.Cyrnea %<>% tidyr::fill(starts_with("parasite"),.direction="up")
tax.Tetrameres %<>% tidyr::fill(starts_with("parasite"),.direction="down")

#add restored genera back to main data frame
taxQueryDf %<>% bind_rows(.,tax.Anoplocephala,tax.Cyrnea,tax.Tetrameres) %>% dplyr::arrange(Parasite.species)

#fix the missing classes of Trichinellida and Dioctophymatida parasites
#https://en.wikipedia.org/wiki/Trichinellidae; https://en.wikipedia.org/wiki/Dioctophymidae
taxQueryDf %<>% dplyr::mutate(parasiteClass=if_else(parasiteOrder=="Trichinellida","Enoplea",parasiteClass))
taxQueryDf %<>% dplyr::mutate(parasiteClass=if_else(parasiteOrder=="Dioctophymatida","Enoplea",parasiteClass))

#check no NA values remaining (except for "species" which was diagnostic)
taxQueryDf %>% dplyr::summarize(across(everything(), ~ sum(is.na(.x))))

#remove "species" as we already have "parasite.Species"
taxQueryDf %<>% dplyr::select(-species)

#add taxonomic data to h and y
h %<>% dplyr::left_join(.,taxQueryDf)
y %<>% dplyr::left_join(.,taxQueryDf)

#create csv files of h and y
write_csv(h,file="paraData_disAgg.csv")
write_csv(y,file="paraData_agg.csv")
