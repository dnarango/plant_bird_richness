---
output: html_notebook
---
# Species richness
  
```{r echo = FALSE, warnings = FALSE, message = FALSE}
  library(jagsUI)
  library(dplyr)
  library(tidyr)
  knitr::opts_chunk$set(dpi = 600, warning = FALSE, message = FALSE)
```

## Read in plant information from each site 

```{r}
library(dplyr)
library(tidyr)
library(stringr)

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

# read data in
mastersheet<-read.csv("../data/mastersheet_7aug2017.csv")

trees<-read.csv("../data/finaltrees_7aug2017.csv") %>%
        mutate(type="tree") %>%
        mutate(ID=paste(site,"-",year)) %>%
        mutate(plantspecies=tolower(treespp_format)) %>%
        left_join(mastersheet, by="plantspecies") %>%
        select(site, ID, plantspecies,type, taxa.origin) %>%
        filter(!plantspecies %in% plants_to_remove)
        
        
shrubs<-read.csv("../data/finalshrubs_7aug2017.csv") %>%
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

site_plantrichness$site <- as.character(site_plantrichness$site)
```

```{r}
# Slightly edit a few study site names
site_plantrichness$site[which(site_plantrichness == "andrkenmd1")]<-"andrkenva1"
site_plantrichness$site[which(site_plantrichness == "huffchrva1-1")]<-"huffchrva1"
site_plantrichness$site[which(site_plantrichness == "mazujenmd2")]<-"mazujenmd1"
site_plantrichness$site[which(site_plantrichness == "wiltinva1")]<-"willtinva1"

# Sort the dataframe so that the sites are in ABC order
site_plantrichness <- site_plantrichness[order(site_plantrichness$site),]

StudySites <- site_plantrichness$site
```

## Generate variables of interest 
```{r}
# standardize the values of interest #
#TotalPlantSpp <- (site_plantrichness$total_plant_taxa-mean(site_plantrichness$total_plant_taxa))/sd(site_plantrichness$total_plant_taxa)
#PropNonNative <- (site_plantrichness$proportion_nonnative-mean(site_plantrichness$proportion_nonnative))/sd(site_plantrichness$proportion_nonnative)

TotalPlantSpp <- site_plantrichness$total_plant_taxa
PropNonNative <- site_plantrichness$proportion_nonnative
```

## Read in the data
```{r}
# Read in the data
countdata <- read.csv("../data/finalpointcounts_28march2017.csv", na.strings = ".", stringsAsFactors=FALSE)
NNcountdata <- read.csv("../data/final_NN_pointcounts_23april2017.csv", na.strings = ".", stringsAsFactors=FALSE)
```

## Combine the data sets and remove unused columns
```{r}
# Variables of interest 

vars <- c("id","site","year","month",
          "observer","juldate","timeformat",
          "species_format",
          "dist1","dist2","dist3","dist4","dist5",
          "total",
          "detection","detection_format")

# Keep only those variables in that order #
countdata <- countdata[,vars]

NNcountdata <- NNcountdata[,vars]

countdata <- rbind(countdata,NNcountdata)
```
## format 2016 data to match previous years

```{r}
NNcount2016 <- read.csv("../data/2016_NN_pointcounts_30may2017.csv",na.strings = ".")
NNcount2016$month <- format(as.POSIXct(NNcount2016$date, format = "%m/%d/%Y"),"%m")
NNcount2016$day <- format(as.POSIXct(NNcount2016$date, format = "%m/%d/%Y"),"%d")
NNcount2016$year <- format(as.POSIXct(NNcount2016$date, format = "%m/%d/%Y"),"%Y")
NNcount2016$juldate <- format(as.POSIXct(NNcount2016$date, format = "%m/%d/%Y"),"%j")
NNcount2016$species_format <- tolower(NNcount2016$species_format)

NNcount2016_format<-NNcount2016 %>%
                    mutate(site=tolower(site)) %>%
                    mutate(id=paste(site,"-",juldate,"-", year,"-", timePc)) %>%
                    group_by(id, site, observerPc, date, day, month, year, juldate, startTimePc, timePc, speciesPc, species_format, distancePc, detectionPc)%>%
                    summarise(count=sum(countPc))%>%
                    spread(distancePc, count, fill=0 ) %>%
                    rename(observer=observerPc, start_time=startTimePc,time_interval=timePc, species=speciesPc)
                    
  names(NNcount2016_format)[13:18]<-c("detection","dist1","dist2","dist3","dist4","dist5")
 
NNcount2016_format$total <- apply(NNcount2016_format[,14:18],1,sum, na.rm = TRUE)
NNcount2016_format$detection_format <- tolower(substr(NNcount2016_format$detection,1,1))
NNcount2016_format$timeformat <- as.numeric(gsub(x = NNcount2016_format$start_time,pattern = ":", replacement = ""))

# reorder to match other years #
NNcount2016_format <- NNcount2016_format[,vars]
```

```{r}
# combine the two data sets

countdata <- rbind(countdata,NNcountdata,NNcount2016_format)

countdata$month <- as.numeric(countdata$month)
countdata$month[countdata$month == 8]<-7
```

## Structure the data 
```{r}

### Format data to array including every bird species and every visit

birdsurvey <-countdata %>%
        filter(detection != "flyover" | detection != ">50" )%>%
        select(id,site, year, month,juldate, timeformat, species_format,total) %>%
        group_by(site, year, month, species_format) %>%
        summarise(total=sum(total)) %>%
        mutate(present=1) %>%
        mutate(visit=paste(year,"-",month)) %>%
        ungroup() %>%
        select(-month, -year, -total) %>%
        spread(species_format, present, fill=0) %>%
        gather(species_format, present,3:114) %>%
        spread(visit, present, fill=NA) 

# Determine which years were surveyed
siteYr <- group_by(countdata,site)%>%
            summarize(minYr = min(year),
                      maxYr = max(year))

siteYr <- siteYr[siteYr$site %in% StudySites,]

siteVisits <- group_by(countdata,site,year,month)%>%
               summarize(n())

F.yr <- siteYr$minYr
L.yr <- siteYr$maxYr

years <- c(2013:2016)
n.years <- length(years)

# change to integer years
for(i in 1:n.years){
F.yr[F.yr == years[i]]<-i
L.yr[L.yr == years[i]]<-i
}

# Save only the count data for sites where vegetation surveys were conducted 

birdsurvey <- birdsurvey[birdsurvey$site %in% StudySites,]
birdsurvey <- birdsurvey[order(birdsurvey$site),]

PropNonNative <- PropNonNative[StudySites %in% birdsurvey$site]
TotalPlantSpp <- TotalPlantSpp[StudySites %in% birdsurvey$site]
```


```{r}
# Create vectors from the data to help summarize the data 
sites <- unique(birdsurvey$site)
visits <- length(unique(countdata$month))
n.spp <- unique(countdata$species_format)
``` 

## Convert occurence data to array format
```{r}
### Convert to array format 
spp.array <- array(NA,c(length(sites),4,n.years,length(n.spp)))

dimnames(spp.array)[[1]]<- sites
dimnames(spp.array)[[2]]<- c("Apr","May","June","July")
dimnames(spp.array)[[3]]<- c(2013:2016)
dimnames(spp.array)[[4]]<- n.spp

for(s in 1:length(n.spp)){
temp <- subset(birdsurvey,species_format == n.spp[s])
for(y in 1:n.years){
if(y == 1){
  spp.array[,2:4,y,s]<-as.matrix(temp[,3:5])
}
  if(y == 2){
    spp.array[,1:4,y,s]<-as.matrix(temp[,6:9])
  }
  if(y == 3){
    spp.array[,1:4,y,s]<-as.matrix(temp[,10:13])
  }
  if(y == 4){
    spp.array[,1:4,y,s]<-as.matrix(temp[,14:17])
  }
}
}


# Remove species that are unlikey to breed or vegetation is not relevant at the scale of the yard
S.O.I <- dget("../Data/SpeciesToKeep.txt")
S.O.I[]<- lapply(S.O.I, as.character)

spp.array <- spp.array[,,,which(dimnames(spp.array)[[4]] %in% S.O.I$V1)]

# order the species to make it easier down the road 
spp.array <- spp.array[,,,match(S.O.I$V1,dimnames(spp.array)[[4]])]

n.spp <- dim(spp.array)[4]
```


## Read in BCI information needed for analysis 
```{r}
guild <- read.csv("../data/oconnel_BCI_guilds.csv")

rank <- read.csv("../data/oconnel_BCI_rank.csv")

# Identify any species that is in the spp.array that is an insectivore #
insectEater <- guild$species[grep("insectivore",guild$guild)]
insectEater <- match(dimnames(spp.array)[[4]],insectEater)

# create binary array for model #
insectEater[is.na(insectEater)]<-0
insectEater[insectEater > 0]<-1

rep.row<-function(x,n){
   matrix(rep(x,each=n),nrow=n)
}

insectivores <- rep.row(insectEater,dim(spp.array)[1])

## Functional
omnivore<- guild %>%
      filter(response=="omnivore")%>% mutate(omnivore=1) %>% select(species, omnivore)
bark_prober<- guild %>%
      filter(response=="bark_prober")%>% mutate(bark_prober=1) %>% select(species, bark_prober)  
ground_gleaner<- guild %>%
      filter(response=="ground_gleaner")%>% mutate(ground_gleaner=1) %>% select(species, ground_gleaner)
upper_canopy<- guild %>%
      filter(response=="upper_canopy")%>% mutate(upper_canopy=1) %>% select(species, upper_canopy)
lower_canopy<- guild %>%
      filter(response=="lower_canopy")%>% mutate(lower_canopy=1) %>% select(species, lower_canopy)

##Compositional
predator_parasite<- guild %>%
      filter(response=="predator_parasite")%>% mutate(predator_parasite=1) %>% select(species,       predator_parasite)
exotic<- guild %>%
      filter(response=="exotic")%>% mutate(exotic=1) %>% select(species, exotic)
resident<- guild %>%
      filter(response=="resident")%>% mutate(resident=1) %>% select(species, resident)
temperate_migrant<- guild %>%
      filter(response=="temperate_migrant")%>% mutate(temperate_migrant=1) %>% select(species, temperate_migrant)
single_brood<- guild %>%
      filter(response=="single-brood")%>% mutate(single_brood=1) %>% select(species, single_brood)

##Structural
forest_ground<- guild %>%
      filter(response=="forest_ground")%>% mutate(forest_ground=1) %>% select(species, forest_ground)
open_ground<- guild %>%
      filter(response=="open_ground")%>% mutate(open_ground=1) %>% select(species, open_ground)
shrub<- guild %>%
      filter(response=="shrub")%>% mutate(shrub=1) %>% select(species, shrub)
canopy<- guild %>%
      filter(response=="canopy_nester")%>% mutate(canopy=1) %>% select(species, canopy)
forest_generalist<- guild %>%
      filter(response=="forest_generalist")%>% mutate(forest_generalist=1) %>% select(species, forest_generalist)
interior_forest<- guild %>%
      filter(response=="interior_forest")%>% mutate(interior_forest=1) %>% select(species, interior_forest)




# BCI.Rmd is run here #

# Format the guilds and rank conditions to use in the JAGS model to calculate BCI #
Functional <- array(NA, c(length(sites),n.spp,5)) # 5 'guilds'
for(i in 1:n.spp){
Functional[,i,1]<-ifelse(dimnames(spp.array)[[4]][i] %in% omnivore$species,1,0)
Functional[,i,2]<-ifelse(dimnames(spp.array)[[4]][i] %in% bark_prober$species,1,0)
Functional[,i,3]<-ifelse(dimnames(spp.array)[[4]][i] %in% ground_gleaner$species,1,0)
Functional[,i,4]<-ifelse(dimnames(spp.array)[[4]][i] %in% lower_canopy$species,1,0)
Functional[,i,5]<-ifelse(dimnames(spp.array)[[4]][i] %in% upper_canopy$species,1,0)
}

Compositional <- array(NA, c(length(sites),n.spp,5)) # 5 'guilds'
for(i in 1:n.spp){
Compositional[,i,1]<-ifelse(dimnames(spp.array)[[4]][i] %in% predator_parasite$species,1,0)
Compositional[,i,2]<-ifelse(dimnames(spp.array)[[4]][i] %in% exotic$species,1,0)
Compositional[,i,3]<-ifelse(dimnames(spp.array)[[4]][i] %in% resident$species,1,0)
Compositional[,i,4]<-ifelse(dimnames(spp.array)[[4]][i] %in% temperate_migrant$species,1,0)
Compositional[,i,5]<-ifelse(dimnames(spp.array)[[4]][i] %in% single_brood$species,1,0)
}

Structural <- array(NA, c(length(sites),n.spp,6)) # 6 'guilds'
for(i in 1:n.spp){
Structural[,i,1]<-ifelse(dimnames(spp.array)[[4]][i] %in% forest_ground$species,1,0)
Structural[,i,2]<-ifelse(dimnames(spp.array)[[4]][i] %in% open_ground$species,1,0)
Structural[,i,3]<-ifelse(dimnames(spp.array)[[4]][i] %in% shrub$species,1,0)
Structural[,i,4]<-ifelse(dimnames(spp.array)[[4]][i] %in% canopy$species,1,0)
Structural[,i,5]<-ifelse(dimnames(spp.array)[[4]][i] %in% forest_generalist$species,1,0)
Structural[,i,6]<-ifelse(dimnames(spp.array)[[4]][i] %in% interior_forest$species,1,0)
}
```

    
# Analyze in Bayesian Framework
## Initial values
```{r}
# Inital values #
# Function to pass inital z values to jags in parallel
Zint<-function(x){
suppressWarnings(zst <- apply(x,c(1,4,3),max, na.rm = TRUE))# Observed occurrence as starting values
  
#z.all <- zst
 
#zst[,,1:F.yr]<-NA
#zst[,,L.yr:4]<-NA
 
#zst[,,F.yr:L.yr] <- z.all[,,F.yr:L.yr]
  
zst[zst==-Inf]<-NA

return(list(z = zst,
            w = zst))
}
```

```{r}
set.seed(04823)

inits <- function() list(z = Zint(spp.array)$z,
                         w = Zint(spp.array)$w)

nchains<-3

# Compile data #

win.data<-list(y = spp.array,
               nsites = dim(spp.array)[1],
               nspp = dim(spp.array)[4],
               nmonth = dim(spp.array)[2],
               nyears = dim(spp.array)[3],
               F.yr = as.numeric(F.yr),
               L.yr = as.numeric(L.yr),
               Functional = Functional,
               Compositional = Compositional,
               Structural = Structural,
               Insectivores = insectivores)


# Parameters to monitor #

params<-c("Nsite","max.spp","lpsi","lp","omega",
          "BCI","BCI.score", "max.insect")


##################################################################################
#
# Start Model 
#
##################################################################################
library(jagsUI)

ptm<-proc.time()
outj1000<- jags.basic(model.file ="../ModelFiles/SppOcc.txt",
            data=win.data,
            parallel = TRUE,
            seed=04823,
            inits=inits, 
            parameters.to.save=params, 
            n.iter=1000,
            n.burnin=500,
            n.thin=1, 
            n.chains=3,
            save.model = TRUE)
proc.time()-ptm

source("../Functions/sims.list.R")

d<-process.output(outj1000[[1]],Rhat = TRUE,params.omit = "z")

plot(d$mean$BCI.score~PropNonNative,ylim = c(25,55),pch = 19, col = "black", cex = 1.25)
abline(lm(d$mean$BCI.score~PropNonNative))
segments(x0 = PropNonNative,y0 = d$q2.5$BCI.score,
         x1 = PropNonNative,y1 = d$q97.5$BCI.score, col = "gray88")
points(d$mean$BCI.score~PropNonNative,pch = 19, col = "black",cex = 1.25)


plot(d$mean$max.spp~TotalPlantSpp)
abline(lm(d$mean$max.spp~TotalPlantSpp))
segments(x0 = PropNonNative,y0 = d$q2.5$max.spp,
         x1 = PropNonNative,y1 = d$q97.5$max.spp, col = "gray88")
points(d$mean$max.spp~PropNonNative,pch = 19, col = "black",cex = 1.25)
```