library("mapr")
library("spocc")
library("rglobi")

spp <- c('Danaus plexippus', 'Asclepias syriaca')
dat <- occ(query = spp, from = 'gbif', has_coords = TRUE, limit = 5000)
map_leaflet(dat)

library("spocc")
spp <- c('Danaus plexippus', 'Asclepias syriaca')
dat <- occ(spp, from = 'gbif', limit = 50, has_coords = TRUE,
           gbifopts = list(country = 'US'))
map_plot(dat)




Asc<-read.csv("Asclepias_USA.csv", header = T)
Asc_clean<-na.omit(Asc)

#Déterminer les états avec les coordonnées géographiques

library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


#Nos states
coord<-data.frame(x=Asc_clean$long,y=Asc_clean$lat)
states<-latlong2state(coord)

#Day of the year and week of the year
date<-as.Date(Asc_clean$date)
time<-data.frame(DOY=as.POSIXlt(date, format = "20%y-%m-%d")$yday+1,
                 WOY=as.numeric(format(date, format = "%W"))+1)

Asc_clean$state <- states
Asc_clean$DOY <- time$DOY
Asc_clean$WOY <- time$WOY

View(Asc_clean)

#Import table csv (import dans environnement et cocher première ligne comme header)
Danaus=read.csv("DanausPlexippusUSA.csv", header=T)

Danaus.na=na.omit(Danaus)
nrow(Danaus)
nrow(Danaus.na)

#longitude and latitude transformation
install.packages("maps")
install.packages("maptools")
library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


# Longitude Latitude
testPoints <- data.frame(x = Danaus.na$longitude, y = Danaus.na$latitude)
state=latlong2state(testPoints)

############################

#Day of the year

date=as.Date(Danaus.na$date)
DOY=as.POSIXlt(date, format="20%y-%m-%d")$yday+1
head(DOY)

#Week of the year
dates=as.Date(Danaus.na$date)
Week <- data.frame(Dates = dates, Week = format(dates, format = "%W"))
head(Week, 10)



####### CREATE MAPS FOR  DATA Asclepias tuberosa, speciosa syriaca 

data = read.csv("/Users/vlemieuxlabonte/Documents/Summer_school_ecological_data_synthesis/Asclepias_MIN_WOY.csv")
attach(data)

View(data)
library(maps)
library(ggplot2)

all_states <- map_data("state")
View(all_states)

## Change column name to region
colnames(data)[6] = "region"

## New dataframe for map 
head(all_states)
Total <- merge(all_states[,-4], data, by="region")
dim(Total)
Total2016 <- Total[Total$year >= 2012 & Total$year <= 2016 ,]
Total <- Total[Total$region!="district of columbia",]

plant.name = unique(Total2016$name)

library(gridExtra)
library(ggplot2)
Total2016 <- Total[Total$year>= 2013 & Total$year <= 2016,]
Total2012 <- Total[Total$year>= 2009 & Total$year <= 2012,]
Total2008 <- Total[Total$year>= 2005 & Total$year <= 2008,]
Total2004 <- Total[Total$year>= 2000 & Total$year <= 2004,]
head(Total2011)

###Syriaca
pdf("Syriaca.pdf")
r <- ggplot()
r <- r + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
r = r + geom_polygon(data=Total2016[Total2016$name=="Asclepias syriaca",], aes(x=long.x, y=lat.x, group = group, fill=Total2016[Total2016$name=="Asclepias syriaca",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred", na.value="white",guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "Syriaca 2013-2016", x="", y="")

s <- ggplot()
s <- s + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
s = s + geom_polygon(data=Total2012[Total2012$name=="Asclepias syriaca",], aes(x=long.x, y=lat.x, group = group, fill=Total2012[Total2012$name=="Asclepias syriaca",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "Syriaca 2009-2012", x="", y="")

t <- ggplot()
t <- t + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
t = t + geom_polygon(data=Total2008[Total2008$name=="Asclepias syriaca",], aes(x=long.x, y=lat.x, group = group, fill=Total2008[Total2008$name=="Asclepias syriaca",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "Syriaca 2005-2008", x="", y="")

q <- ggplot()
q <- q + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
q = q + geom_polygon(data=Total2004[Total2004$name=="Asclepias syriaca",], aes(x=long.x, y=lat.x, group = group, fill=Total2004[Total2004$name=="Asclepias syriaca",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "Syriaca 2000-2004", x="", y="")
grid.arrange(q,t,s,r, ncol=2, nrow=2)
dev.off()
############################################
#Speciosa
pdf("Speciosa.pdf")
r1 <- ggplot()
r1 <- r1 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
r1 = r1 + geom_polygon(data=Total2016[Total2016$name=="Asclepias speciosa",], aes(x=long.x, y=lat.x, group = group, fill=Total2016[Total2016$name=="Asclepias speciosa",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "Speciosa 2013-2016", x="", y="")

s1 <- ggplot()
s1 <- s1 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
s1 = s1 + geom_polygon(data=Total2012[Total2012$name=="Asclepias speciosa",], aes(x=long.x, y=lat.x, group = group, fill=Total2012[Total2012$name=="Asclepias speciosa",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "Speciosa 2009-2012", x="", y="")

t1 <- ggplot()
t1 <- t1 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
t1 = t1 + geom_polygon(data=Total2008[Total2008$name=="Asclepias speciosa",], aes(x=long.x, y=lat.x, group = group, fill=Total2008[Total2008$name=="Asclepias speciosa",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "Speciosa 2005-2008", x="", y="")

q1 <- ggplot()
q1 <- q1 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
q1 = q1 + geom_polygon(data=Total2004[Total2004$name=="Asclepias speciosa",], aes(x=long.x, y=lat.x, group = group, fill=Total2004[Total2004$name=="Asclepias speciosa",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "Speciosa 2000-2004", x="", y="")
grid.arrange(q1,t1,s1,r1, ncol=2, nrow=2)
dev.off()


#Tuberosa
pdf("Tuberosa.pdf")
r2 <- ggplot()
r2 <- r2 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
r2 = r2 + geom_polygon(data=Total2016[Total2016$name=="Asclepias tuberosa",], aes(x=long.x, y=lat.x, group = group, fill=Total2016[Total2016$name=="Asclepias tuberosa",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "tuberosa 2013-2016", x="", y="")

s2 <- ggplot()
s2 <- s2 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
s2 = s2 + geom_polygon(data=Total2012[Total2012$name=="Asclepias tuberosa",], aes(x=long.x, y=lat.x, group = group, fill=Total2012[Total2012$name=="Asclepias tuberosa",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "tuberosa 2009-2012", x="", y="")

t2 <- ggplot()
t2 <- t2 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
t2 = t2 + geom_polygon(data=Total2008[Total2008$name=="Asclepias tuberosa",], aes(x=long.x, y=lat.x, group = group, fill=Total2008[Total2008$name=="Asclepias tuberosa",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "tuberosa 2005-2008", x="", y="")

q2 <- ggplot()
q2 <- q2 + geom_polygon(data=all_states, aes(x=long, y=lat, group = group, fill=-1),colour="black")
q2 = q2 + geom_polygon(data=Total2004[Total2004$name=="Asclepias tuberosa",], aes(x=long.x, y=lat.x, group = group, fill=Total2004[Total2004$name=="Asclepias tuberosa",]$Min_WOY),colour="black"
) + scale_fill_continuous(low = "thistle2", high = "darkred",na.value="white", guide="colorbar",limits=c(0,50)) +labs(fill = "Week" ,title = "tuberosa 2000-2004", x="", y="")
grid.arrange(q2,t2,s2,r2, ncol=2, nrow=2)
dev.off()





#### Texas
#A. tuberosa Texas ******
Astubtexas <- data[data$region == "texas"& data$name=="Asclepias tuberosa" &data$year>2000,]
View(Astubtexas)
summary(lm(Astubtexas$Min_WOY ~ Astubtexas$year))
plot(Astubtexas$Min_WOY ~ Astubtexas$year)



cath_theme<-function(base_size = 12, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      legend.title=element_blank(),
      panel.background=element_rect(fill="white",colour="black"),
      panel.grid = element_blank(),
      legend.key=element_blank()  
    )
}

Scatter1 <- ggplot(Astubtexas, aes(x=year, y=Min_WOY)) + 
  geom_point(size=2) + cath_theme() +
  xlab("Year") + ylab ("First week of occurrence") +
  ggtitle ("Texas A. tuberosa") +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,50)

Scatter1



#A. speciosa Texas
Asspetexas <- data[data$region == "texas"& data$name=="Asclepias speciosa",]
View(Asspetexas)
summary(lm(Asspetexas$Min_WOY ~ Asspetexas$year))
plot(Asspetexas$Min_WOY ~ Asspetexas$year)
#A. syriaca Texas
Assyrtexas <- data[data$region == "texas" & data$name=="Asclepias syriaca",]
View(Assyrtexas)
summary(lm(Assyrtexas$Min_WOY ~ Assyrtexas$year))
plot(Assyrtexas$Min_WOY ~ Assyrtexas$year)


#### california
#A. tuberosa california
tubcalifornia <- data[data$region == "california" & data$name=="Asclepias tuberosa",]
View(tubcalifornia)#Only one data...
#A. speciosa california
specalifornia <- data[data$region == "california" & data$name=="Asclepias speciosa",]
View(specalifornia)
summary(lm(specalifornia$Min_WOY ~ specalifornia$year))
plot(specalifornia$Min_WOY ~ specalifornia$year,ylim=c(0,50))

Scatter <- ggplot(specalifornia, aes(x=year, y=Min_WOY)) + 
  geom_point(size=2) + cath_theme() + ylim(0,50) +
  xlab("Year") + ylab ("First week of occurrence") +
  ggtitle ("California A. speciosa") +
  theme(plot.title = element_text(hjust = 0.5))

Scatter
#A. syriaca california
syrcalifornia <- data[data$region == "california" & data$name=="Asclepias syriaca",]
View(syrcalifornia)#only one data...


###### Boxplot

As_WOY_clean=read.csv("/Users/vlemieuxlabonte/Documents/Summer_school_ecological_data_synthesis/Asc_state_WOY_clean.csv")


# function for number of observations 
give.n <- function(x){
  return(c(y = median(x)*1.3, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

###Texas boxplot
#A. tuberosa texas
boxtubtex <- ggplot(na.omit(As_WOY_clean[As_WOY_clean$states_Asc=="texas" & As_WOY_clean$name=="Asclepias tuberosa",]), aes(x=year, y=WOY, group=year)) + 
  geom_boxplot(outlier.color = "blue") + ggtitle("Tuberosa Texas") + cath_theme() + xlab("Year") + ylab ("Week of Occurrence") + theme(plot.title = element_text(hjust = 0.5)) + stat_summary(fun.data = give.n, geom = "text", fun.y = median) + 
  ylim(0,50)
boxtubtex



###California boxplot
#A. speciosa California
boxspeca <- ggplot(na.omit(As_WOY_clean[As_WOY_clean$states_Asc=="california" & As_WOY_clean$name=="Asclepias speciosa" & As_WOY_clean$year>=1900,]), aes(x=year, y=WOY,group=year)) + 
  geom_boxplot(outlier.color = "blue") + ggtitle("Speciosa California") + cath_theme() + xlab("Year") + ylab ("Week of Occurrence") +
  ggtitle ("California") + theme(plot.title = element_text(hjust = 0.5)) + ylim(0,50)
boxspeca 

cath_theme() +
  xlab("Year") + ylab ("Week of Occurrence") +
  ggtitle ("California") +
  theme(plot.title = element_text(hjust = 0.5))


grid.arrange(boxspeca, boxtubtex,ncol=2)


## extra code to select for the year

& As_WOY_clean$year>=2000,

