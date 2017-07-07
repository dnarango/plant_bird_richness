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

```{r}
# Create vectors from the data to help summarize the data 
sites <- unique(countdata$site)
visits <- length(unique(countdata$month))
n.spp <- unique(countdata$species_format)
years <- c(2013:2016)
n.years <- length(years)
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

siteVisits <- group_by(countdata,site,year,month)%>%
               summarize(n())

F.yr <- siteYr$minYr
L.yr <- siteYr$maxYr

# change to integer years
for(i in 1:n.years){
F.yr[F.yr == years[i]]<-i
L.yr[L.yr == years[i]]<-i
}
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
    spp.array[,1:2,y,s]<-as.matrix(temp[,14:15])
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

guild <- guilds[guilds$species %in% S.O.I$V1,]

rank <- read.csv("../data/oconnel_BCI_rank.csv")
head(rank)
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
Zint<-function(x,F.yr,L.yr){
suppressWarnings(zst <- apply(x,c(1,4,3),max, na.rm = TRUE))# Observed occurrence as starting values
  
# z.all <- zst
# 
# zst[,,1:F.yr]<-NA
# zst[,,L.yr:4]<-NA
# 
# zst[,,F.yr:L.yr] <- z.all[,,F.yr:L.yr]
zst[zst==-Inf]<-NA

return(list(z = zst,
            w = zst))
}
```


```{r}
set.seed(04823)

inits <- function() list(z = Zint(spp.array,F.yr,L.yr)$z,
                         w = Zint(spp.array,F.yr,L.yr)$w)

nchains<-3

# Compile data #

win.data<-list(y = spp.array,
               nsites = dim(spp.array)[1],
               nspp = dim(spp.array)[4],
               nmonth = 4,
               nyears = 4,
               F.yr = as.numeric(F.yr),
               L.yr = as.numeric(L.yr),
               Functional = Functional,
               Compositional = Compositional,
               Structural = Structural)


# Parameters to monitor #

params<-c("Nsite","max.spp","lpsi","lp","z","omega",
          "BCI","BCI.score")


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
            n.iter=10000,
            n.burnin=5000,
            n.thin=5, 
            n.chains=3,
            save.model = TRUE)
proc.time()-ptm

source("../Functions/sims.list.R")

d<-process.output(outj1000[[1]],Rhat = TRUE,params.omit = "z")

plot(d$mean$BCI.score,
     pch = 20,
     col = "black",
     ylim = c(25,55))
segments(c(1:dim(spp.array)[1]),d$q2.5$BCI.score,
         c(1:dim(spp.array)[1]),d$q97.5$BCI.score)
```