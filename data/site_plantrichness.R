#### Script to determine woody plant richness of all sites

library(dplyr)
library(tidyr)
library(stringr)

datalocation<-"C:/Users/dnarango/Desktop/plantrichness"

plants_to_remove<-c("",".","0","unknown","unknown ornamental shrub",
                    "unknown (vine)","unknown (ilex)", "unknown (conifer)","unknown shrub",
                    "unknown (pine)")

plants_to_remove<-c("snag","telephone pole","herbaceous","rosemary","no shrubs","sage",
                    "ground cover","yucca","cactus"," ",".","0","")

ID_to_remove<-c("angeparva1 - 2013","buisnorva1 - 2013","burdrobva1 - 2013","camprosmd1 - 2013",
                   "champarva1 - 2013","cronchrva2 - 2013","eganmarmd1 - 2016","fishferva1 - 2013",
                   "giraangmd1 - 2013","hillthrva1 - 2013","hopkbarmd1 - 2013","hulljefva1 - 2013",
                   "jacosydmd1 - 2013","lafomelva1 - 2013","lindhanmd1 - 2013","lyonlynva1 - 2013",
                   "marrpetmd2 - 2013","mcnakrimd1 - 2013","morrchrva1 - 2013","myerbrumd1 - 2013",
                   "omalmadva1 - 2013","raunerimd2 - 2013","rohrsalmd1 - 2013","ruttjormd1 - 2013",
                   "singdadmd1 - 2013","sollchrmd1 - 2013","stewbetmd1 - 2013","sturcarmd1 - 2013",
                   "traupatva2 - 2013","versbetmd1 - 2013","vongjenmd1 - 2013","wildannva1 - 2013",
                   "wisecelmd1 - 2013","younelimd1 - 2013")

sites_to_remove<-c("angeparva1","champarva1","cameparva1","hillthrva1","ohagkerva1","ohagkarva1")


## pass data in
mastersheet<-read.csv(paste0(datalocation,"/mastersheet_7aug2017.csv", collapse =","))

trees<-read.csv(paste0(datalocation,"/finaltrees_7aug2017.csv", collapse =",")) %>%
        mutate(type="tree") %>%
        mutate(ID=paste(site,"-",year)) %>%
        mutate(plantspecies=tolower(treespp_format)) %>%
        left_join(mastersheet, by="plantspecies") %>%
        select(site, ID, plantspecies,type, taxa.origin) %>%
        filter(!plantspecies %in% plants_to_remove)
        
        
shrubs<-read.csv(paste0(datalocation,"/finalshrubs_7aug2017.csv", collapse =",")) %>%
        mutate(type="shrub") %>%
        mutate(ID=paste(site,"-",year)) %>%
         mutate(plantspecies=tolower(shrub_format)) %>%
        left_join(mastersheet, by="plantspecies") %>%
        select(site, ID, plantspecies,type, taxa.origin) %>%
        filter(!plantspecies %in% plants_to_remove)


site_plantrichness<-trees %>%
                    rbind(shrubs) %>%
                    group_by(site, ID, plantspecies, taxa.origin) %>%
                    summarise(plant_count=n()) %>%
                    group_by(site,ID,taxa.origin) %>%
                    summarise(species_count=n()) %>%
                    spread(taxa.origin, species_count, fill=0) %>%
                    rename(nonnative="non-native") %>%
                    mutate(total_plant_taxa=native+nonnative+unknown) %>%
                    mutate(proportion_nonnative=nonnative/total_plant_taxa) %>%
                   filter(!ID %in% ID_to_remove)  %>%   ## remove sites sampled in >1 year
                    filter(!site %in% sites_to_remove) 
                    


