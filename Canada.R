
#--------------before doing this the first time, you need to download these packages. You only need to do this once.
install.packages(c('sf', 'foreign', 'tidyverse', 'lwgeom'))


#-------------every time you open a new R session you'll want to load in the necessary libraries.-----
library(sf)
library(foreign)
library(tidyverse)
library(lwgeom)
options(stringsAsFactors = FALSE)


#-------------------getting set up. Fill in the blanks here before running the code below----------------
#download + unzip SHP file of whatever province you're interested in. I chose Saskatchewan
#https://open.canada.ca/data/en/dataset/82efb454-3241-4440-a5d4-8b03a42f4df8

#set the directory to work out of. You'll want this to be the folder you unzipped the files to. Make sure to use / instead of \ in the path.
setwd("C:/Users/Erin/Documents/DataViz/Canada/")

#change this to the name of the files you downloaded (sans suffix)
filename <- "grnf047r09a_e"

#pick a lat/long to be at the center of your map. I chose a point in Saskatoon
lat <- 52.139224
long <- -106.646308
  
#pick a radius to plot the roads in (in meters)
rad <- 10000

#pick the road types you'd like to have colored in the plot
plottypes <-  c('HWY', 'RD', 'AVE', 'ST', 'DR', 'CRES', 'TRAIL', 'LINE')

#set colors for each road type
plotcolors <-  c('HWY' = '#FE4D64', 'RD' = '#4cb580', 'AVE' ='#59c8e5', 'ST' = '#fed032', 'DR' = '#a7abfe', 
                 'CRES' = '#fe9ea5', 'TRAIL' = '#2e968c', 'LINE' = '#ff9223', 'Other' = '#cccccc')


#---------------code---------------
#import  road geography
allroads <- read_sf(".", filename)
allroads$len <- st_length(allroads)

#subset the roads into a circle. You can remove this if you want to plot the entire province.
pt <- data.frame(lat = lat, long = long)
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>%  st_transform(32195) 
circle <- st_buffer(pt, dist = rad)
circle <- circle %>% st_transform(st_crs(allroads))
allroads <- st_intersection(circle, allroads)



#put other roads into their own dataframe
allroads$TYPE[!(allroads$TYPE %in% plottypes)] <- "Other"
otherroads <- allroads[(allroads$TYPE  == "Other"),]
allroads <- allroads[(allroads$TYPE  != "Other"),]

#plot it
blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

#depending on the city/radius you're mapping you might need to adjust the size variables below. 
#I just do trial/error till I like the way it looks when it's saved
ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent")) + 
  geom_sf(data=otherroads, size = 1.5, aes(color=TYPE)) + 
  geom_sf(data=allroads, size = 1.5, aes(color=TYPE)) + 
  scale_color_manual(values = plotcolors, guide = FALSE) 

ggsave("myplot.png", plot = last_plot(),
       scale = 1, width = 24, height = 36, units = "in",
       dpi = 500)



