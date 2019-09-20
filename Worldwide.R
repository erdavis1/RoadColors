#-----------you need to install the following packages. this only needs to be done once.
install.packages(c('sf', 'foreign', 'tidyverse', 'stringi', 'lwgeom'))

#-----------initialize libraries. This needs to be done for each new R session 
library(sf)
library(foreign)
library(tidyverse)
library(lwgeom)
library(stringi)
options(stringsAsFactors = FALSE)

#-----------download files
#pick a region and download/unzip the .shp.zip file: http://download.geofabrik.de/

#-----------set the working directory to wherever you unzipped the downloaded files to
setwd("C:/Users/Erin/Documents/DataViz/!Complete/IndivRoads/OSM_Data/London/")


#-----------set some basic info about the city you're mapping
city <- "london"
lat <- 51.508420 #center point latitude
long <- -0.112730 #center point longitude
rad <- 20000 #radius, in meters, around the center point to map
crs <- 102013 #ESRI projection for mapping. I found mine here: https://spatialreference.org/ref/esri/europe-albers-equal-area-conic/


#-----------set up the road types you want to plot and what colors they should be
plottypes <-  c('Road', 'Street', 'Avenue', 'Lane', 'Close', 'Way', 'Place', 'Embankment')
plotcolors <-  c('Road' = '#59c8e5', 'Street' = '#fed032', 'Avenue' ='#4cb580', 'Lane' = '#fe4d64', 'Close' = '#0a7abf',
                 'Way' = '#2e968c', 'Place' = '#fe9ea5', 'Embankment' = '#fe9ea5', 'Motorway' = "#ff9223", 'Other' = '#cccccc')


#-----------get to plotting
#import  road geography
filename <- "gis_osm_roads_free_1"
allroads <- read_sf(".", filename)

#subset the roads into a circle.
pt <- data.frame(lat = lat, long = long)
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>%  st_transform(crs) 
circle <- st_buffer(pt, dist = rad)
circle <- circle %>% st_transform(st_crs(allroads))
allroads <- st_intersection(circle, allroads)

#remove unnamed footpaths
allroads <- allroads[!(allroads$fclass  == "footway" & is.na(allroads$name)),]

#add in length 
allroads$len <- st_length(allroads)

#-----derive road suffixes-----
#run this line if your suffixes are at the END of the name (e.g. Canal Street)
allroads$TYPE <- substr(allroads$name, stri_locate_last(allroads$name, regex = " ")[, 1] + 1,  nchar(allroads$name)) %>% stri_trans_general(id = "Title")

#run this line if your "suffixes" are at the BEGINNING of the name (e.g. Calle de los Gatos)
allroads$TYPE <- substr(allroads$name, 1,  str_locate(allroads$name, " ")[, 1] -1)  %>% stri_trans_general(id = "Title")   #for road prefixes

#--------uncomment and run this code to get the top roads by length.
#--------i usually run this to decide what road types to plot
#plottype <- allroads %>% select(TYPE,len)
#plottype$geometry <- NULL
#plottype <- subset(plottype, !is.na(TYPE))
#plottype <- plottype %>% group_by(TYPE) %>% summarise(Length = sum(len)) %>% arrange(-Length) 


#rename motorways that don't have some other designation
allroads$TYPE[allroads$fclass == 'motorway' & !(allroads$TYPE %in% plottypes)] <- "Motorway"

#put other roads into their own dataframe
allroads$TYPE[!(allroads$TYPE %in% plottypes) & allroads$TYPE != 'Motorway'] <- "Other"
otherroads <- allroads[(allroads$TYPE  == "Other"),]
allroads <- allroads[(allroads$TYPE  != "Other"),]

#plot it
blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent")) + 
  geom_sf(data=otherroads, size = .8, aes(color=TYPE)) + 
  geom_sf(data=allroads, size =1, aes(color=TYPE)) + 
  scale_color_manual(values = plotcolors, guide = "legend") 

ggsave(paste0(".", city, ".png"), plot = last_plot(),
       scale = 1, width = 24, height = 36, units = "in",
       dpi = 500)






