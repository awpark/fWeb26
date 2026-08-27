#load libraries
library(magrittr)
library(tidyverse)

#read in our parasite data
paraDisAgg <- here::here("paraData_disAgg.csv")
h <- readr::read_csv(paraDisAgg,
                     locale=locale(encoding="latin1"))

#invoke world map
worldMap <- ggplot2::map_data("world")
#Build the map and overlay the color-coded coordinates
paraMap <- ggplot() +
  # Draw the background map
  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group), 
               fill = "grey60", color = "white") +
  # Add the coordinates and color them by the 'region' category
  geom_point(data = h, aes(x =Longitude,y=Latitude,color=Host.habitat), 
             size = 3, alpha = 0.2) +
  # Keep correct geographic map proportions
  coord_fixed(1.3) + 
  # Apply clean styling
  theme_minimal() +
  labs(title = "Parasite locations by habitat and chain length",
       x = "Longitude", 
       y = "Latitude",
       color = "Habitat")+scale_color_manual(values=c("darkgreen","darkblue","darkorange"))+
  #facet
  facet_wrap(~Chain.length)

ggsave("paraMap.png",paraMap,dpi=300,width=12,height=8,units="in")



