# plot foodWebs & parasites together

# load libraries
library(magrittr)
library(tidyverse)
library(here)
library(geosphere)
library(gt)
library(patchwork)

# source file for para data
para <- here::here("dataPrep.R")
source(para)

# source file for foodWeb data
fWeb <- here::here("foodWebAgg.R")
source(fWeb)

#invoke world map
worldMap <- ggplot2::map_data("world")
#Build the map and overlay the color-coded coordinates
pfMap <- ggplot() +
  # Draw the background map
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group), 
               fill="grey80",color="white") +
  # Add the parasite coordinates and color them by the 'habitat' category
  geom_point(data=h,aes(x=Longitude,y=Latitude,color=Host.habitat), 
             size=2,alpha=0.3) +
  # add foob web coords and color them by habitat
  geom_point(data=fwAgg,aes(x=meanLon,y=meanLat,color=Habitat), 
             size=3,alpha=1,pch=21) +
  # Keep correct geographic map proportions
  coord_fixed(1.3) + 
  # Apply clean styling
  theme_minimal() +
  labs(x = "Longitude", 
       y = "Latitude",
       color = "Habitat")+scale_color_manual(values=c("darkgreen","darkblue","darkorange"))+
  theme(axis.text.x=element_text(size=14),
        axis.title.x=element_text(size=18),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=18))



# calc distance to nearest habitat matching fweb for each parasite location
# note: there are a number of ways to do this; because we use average parasite
# lat in the analysis, then average parasite location seems appropriate
# we also use the citation-aggregated food web location data &
# distributions and medians are evaluated on a per-parasite basis

# unique parasite locations
u <- y %>% dplyr::select(Avg.lat,Avg.long) %>% dplyr::distinct()

#create dummy col for nearest food web
u %<>% dplyr::mutate(fweb0=-1)

for (i in 1:dim(u)[1]){
  pLat <- u$Avg.lat[i]
  pLon <- u$Avg.long[i]
  dist=999999999999999
  for (j in 1:dim(fwAgg)[1]){
    thisDist <- geosphere::distGeo(c(u$Avg.long[i],u$Avg.lat[i]),c(fwAgg$meanLon[j],fwAgg$meanLat[j]))
    if (thisDist<dist){dist <- thisDist}
  }
  u$fweb0[i] <- dist
}

#add dist in km
u %<>% dplyr::mutate(fweb0.km=fweb0/1000)

# add dist to mean parasite location data
y %<>% dplyr::left_join(.,u)

# plot distro of distances by habitat

#distPlot <- y %>% ggplot(.,aes(x=fweb0.km,fill=Host.habitat))+geom_density(alpha=0.3)+scale_fill_manual(values=c("darkgreen","darkblue","darkorange"))+xlab("Nearest foodweb")+ylab("Density")
#ggsave("distPlot.png",distPlot,dpi=300,width=6,height=4,units="in")

# summary stats of distances & reshape and create the gt table
distTable <- y %>% dplyr::group_by(Host.habitat) %>% reframe(probs=c(0.25,0.5,0.75),range=quantile(fweb0.km,prob=probs)) %>%
  tidyr::pivot_wider(
    names_from = probs, 
    values_from = range,
    names_prefix = "prob_"
  ) %>%
  gt::gt() %>%
  gt::fmt_number(columns=starts_with("prob_"),decimals=1) %>%
  gt::row_order(prob_0.5,reverse=F) %>%
  gt::tab_header(
    title = "Quantiles of nearest foodweb by habitat (km)"
  ) %>%
  gt::cols_label(
    Host.habitat = "Host Habitat",
    prob_0.25 = "0.25",
    prob_0.5 = "0.5",
    prob_0.75 = "0.75"
  ) %>%
  gt::tab_options(table.font.size = px(12))

distTbl <- as_gtable(distTable)

pfMap + 
  inset_element(distTbl,
                left=0.6,
                bottom=0.24,
                right=0.65,
                top=0.26,
                align_to="plot",
                clip=F)

