## aggregate food web data by citation

library(tidyverse)
library(magrittr)
library(here)


fWebs <- here::here("Final_Foodweb_Dataset_Complete.csv")

f <- read_csv(file=fWebs,
              locale=locale(encoding="latin1"))


fwAgg <- f %>% dplyr::group_by(Citation,Habitat) %>%
  dplyr::reframe(
    meanLat=mean(Latitude),
    meanLon=mean(Longitude),
    meanNodes=mean(Nodes),
    meanLinks=mean(Links),
    meanLinkDensity=mean(Link_Density),
    meanGenerality=mean(Generality),
    meanNormGen=mean(Norm_Gen),
    meanVulnerability=mean(Vulnerability),
    meanNormVul=mean(Norm_Vul),
    meanFracBasal=mean(Fraction_Basal),
    meanFracInt=mean(Fraction_Intermediate),
    meanFracTop=mean(Fraction_Top),
    meanMeanFoodChain=mean(Mean_Food_Chain),
    meanModularity=mean(Modularity)
  )


write_csv(fwAgg,file="foodWebs_Agg.csv")
