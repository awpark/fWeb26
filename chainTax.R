#load libraries
library(magrittr)
library(tidyverse)
library(here)
library(patchwork)

#get para data
paraData <- here::here("paraData_agg.csv")
chain <- readr::read_csv(paraData,
                       locale=locale(encoding="latin1"))


#FW
#meanCV
fw.cv.genus <- chain %>% dplyr::filter(Host.habitat=="freshwater") %>% group_by(parasiteGenus) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
fw.cv.family <- chain %>% dplyr::filter(Host.habitat=="freshwater")%>% group_by(parasiteFamily) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
fw.cv.order <- chain %>% dplyr::filter(Host.habitat=="freshwater")%>% group_by(parasiteOrder) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
fw.cv.class <- chain %>% dplyr::filter(Host.habitat=="freshwater")%>% group_by(parasiteClass) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
fw.cv.phylum <- chain %>% dplyr::filter(Host.habitat=="freshwater")%>% group_by(parasitePhylum) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))

#MAR
#meanCV
mar.cv.genus <- chain %>% dplyr::filter(Host.habitat=="marine") %>% group_by(parasiteGenus) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
mar.cv.family <- chain %>% dplyr::filter(Host.habitat=="marine")%>% group_by(parasiteFamily) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
mar.cv.order <- chain %>% dplyr::filter(Host.habitat=="marine")%>% group_by(parasiteOrder) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
mar.cv.class <- chain %>% dplyr::filter(Host.habitat=="marine")%>% group_by(parasiteClass) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
mar.cv.phylum <- chain %>% dplyr::filter(Host.habitat=="marine")%>% group_by(parasitePhylum) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))

#TER
#meanCV
ter.cv.genus <- chain %>% dplyr::filter(Host.habitat=="terrestrial") %>% group_by(parasiteGenus) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
ter.cv.family <- chain %>% dplyr::filter(Host.habitat=="terrestrial")%>% group_by(parasiteFamily) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
ter.cv.order <- chain %>% dplyr::filter(Host.habitat=="terrestrial")%>% group_by(parasiteOrder) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
ter.cv.class <- chain %>% dplyr::filter(Host.habitat=="terrestrial")%>% group_by(parasiteClass) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))
ter.cv.phylum <- chain %>% dplyr::filter(Host.habitat=="terrestrial")%>% group_by(parasitePhylum) %>% reframe(l=sd(Chain.length)/mean(Chain.length)) %>% summarize(mean(l,na.rm=T))

#create tibble for plotting mean CV
x.cv <- tibble(taxScale=character(0),hab=character(0),avCV=numeric(0))
#populate from data above
x.cv %<>% add_case(taxScale="Genus",hab="fw",avCV=as.numeric(fw.cv.genus))
x.cv %<>% add_case(taxScale="Family",hab="fw",avCV=as.numeric(fw.cv.family))
x.cv %<>% add_case(taxScale="Order",hab="fw",avCV=as.numeric(fw.cv.order))
x.cv %<>% add_case(taxScale="Class",hab="fw",avCV=as.numeric(fw.cv.class))
x.cv %<>% add_case(taxScale="Phylum",hab="fw",avCV=as.numeric(fw.cv.phylum))
x.cv %<>% add_case(taxScale="Genus",hab="mar",avCV=as.numeric(mar.cv.genus))
x.cv %<>% add_case(taxScale="Family",hab="mar",avCV=as.numeric(mar.cv.family))
x.cv %<>% add_case(taxScale="Order",hab="mar",avCV=as.numeric(mar.cv.order))
x.cv %<>% add_case(taxScale="Class",hab="mar",avCV=as.numeric(mar.cv.class))
x.cv %<>% add_case(taxScale="Phylum",hab="mar",avCV=as.numeric(mar.cv.phylum))
x.cv %<>% add_case(taxScale="Genus",hab="ter",avCV=as.numeric(ter.cv.genus))
x.cv %<>% add_case(taxScale="Family",hab="ter",avCV=as.numeric(ter.cv.family))
x.cv %<>% add_case(taxScale="Order",hab="ter",avCV=as.numeric(ter.cv.order))
x.cv %<>% add_case(taxScale="Class",hab="ter",avCV=as.numeric(ter.cv.class))
x.cv %<>% add_case(taxScale="Phylum",hab="ter",avCV=as.numeric(ter.cv.phylum))
#reorder tax levels
x.cv %<>% dplyr::mutate(taxScale=factor(taxScale,levels=c("Genus",
                                                          "Family",
                                                          "Order",
                                                          "Class",
                                                          "Phylum")))
#plot mean CV
plot.cv.fw <- x.cv %>% dplyr::filter(hab=="fw") %>% ggplot(.,aes(x=taxScale,y=avCV))+geom_col(fill="darkgreen")+ylab("Mean CV (chain length)")+xlab("Taxonomic rank")+coord_flip()+ggtitle("Freshwater")+ylim(0,0.275)
plot.cv.mar <- x.cv %>% dplyr::filter(hab=="mar") %>% ggplot(.,aes(x=taxScale,y=avCV))+geom_col(fill="darkblue")+ylab("Mean CV (chain length)")+xlab("Taxonomic rank")+coord_flip()+ggtitle("Marine")+ylim(0,0.275)
plot.cv.ter <- x.cv %>% dplyr::filter(hab=="ter") %>% ggplot(.,aes(x=taxScale,y=avCV))+geom_col(fill="darkorange")+ylab("Mean CV (chain length)")+xlab("Taxonomic rank")+coord_flip()+ggtitle("Terrestrial")+ylim(0,0.275)

(plot.cv.fw/plot.cv.mar/plot.cv.ter)
