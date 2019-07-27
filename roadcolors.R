library(sf)
library(foreign)
library(tidyverse)


options(stringsAsFactors = FALSE)

pt <- data.frame(lat = YOURLATHERE, long = YOURLONHERE)
city <- 'CITYNAMEHERE'
geoid <-  c('GEOID1', 'GEOID2', 'GEOIDN')

#-------import road files----------
files <- list.files(path="./FeatNames", pattern="*.dbf", full.names=TRUE, recursive=FALSE) %>% as.data.frame
names(files) <- c("path")
files$GEOID <- substr(files$path, 21, 25)
files <- subset(files, GEOID %in% geoid)

allroads <-NULL

#----------combine em all and add suffixes--------------
for (i in 1:nrow(files)) {
  #read in the feature names file, which has road suffixes in it
  featname <- read.dbf(files$path[i],  as.is = TRUE)
  featname$SUFTYPABRV[is.na(featname$SUFTYPABRV)] <- featname$PRETYPABRV[is.na(featname$SUFTYPABRV)]
  featname <- featname %>% dplyr::select(LINEARID, SUFTYPABRV) %>% unique
  
  #read in the roads shapefile as a simple features dataframe
  roads <- read_sf("./Roads", paste0("tl_2018_", files$GEOID[i], "_roads"))
  roads$len <- st_length(roads)
  
  #join the two 
  temp <- inner_join(roads, featname, by = "LINEARID") 
  
  #merge em all
  if (i==1) {
    allroads <- temp
  }else {
    allroads <- do.call(rbind, list(temp, allroads))
  }
}

#---------subset the roads into a circle 15 miles wide-------
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(2163)
circle <- st_buffer(pt, dist = 24140.2)
circle <- circle %>% st_transform(st_crs(temp))
allroads <- st_intersection(circle, allroads)

#-----------figure out plot colors automagically------
plottype <- allroads %>% select(SUFTYPABRV,len) 
plottype$geometry <- NULL
plottype <- subset(plottype, !is.na(SUFTYPABRV))
plottype <- plottype %>% group_by(SUFTYPABRV) %>% summarise(Length = sum(len)) %>% arrange(-Length) %>% head(8)

#these ones I want to set always
plotcolors <- c('Other' = '#cccccc')

#get what's leftover
findcolors <- plottype$SUFTYPABRV
colors <- c('#59c8e5', '#fed032',  '#4cb580', '#fe9ea5', '#fe4d64', '#0a7abf', '#ff9223', '#2e968c')

if(length(findcolors[findcolors=="Ave"])>0) {
  plotcolors <- c(plotcolors, "Ave" = '#59c8e5')
  findcolors <- findcolors[findcolors !="Ave"]
  colors <- colors[colors !="#59c8e5"]
}

if(length(findcolors[findcolors=="St"])>0) {
  plotcolors <- c(plotcolors, "St" = '#fed032')
  findcolors <- findcolors[findcolors !="St"]
  colors <- colors[colors !="#fed032"]
}

if(length(findcolors[findcolors=="Rd"])>0) {
  plotcolors <- c(plotcolors, "Rd" = '#4cb580')
  findcolors <- findcolors[findcolors !="Rd"]
  colors <- colors[colors !="#4cb580"]
}


if(length(findcolors[findcolors=="I-"])>0) {
  plotcolors <- c(plotcolors, "I-" = '#fe4d64')
  findcolors <- findcolors[findcolors !="I-"]
  colors <- colors[colors !="#fe4d64"]
}

if(length(findcolors[findcolors=="Dr"])>0) {
  plotcolors <- c(plotcolors, "Dr" = '#0a7abf')
  findcolors <- findcolors[findcolors !="Dr"]
  colors <- colors[colors !="#0a7abf"]
}

if(length(findcolors[findcolors=="Blvd"])>0) {
  plotcolors <- c(plotcolors, "Blvd" = '#2e968c')
  findcolors <- findcolors[findcolors !="Blvd"]
  colors <- colors[colors !="#2e968c"]
}

if(length(findcolors[findcolors=="US Hwy"])>0) {
  plotcolors <- c(plotcolors, "US Hwy" = '#ff9223')
  findcolors <- findcolors[findcolors !="US Hwy"]
  colors <- colors[colors !="#ff9223"]
}

if(length(findcolors[findcolors=="State Hwy"])>0 &length(colors[colors=="#ff9223"])>0) {
  plotcolors <- c(plotcolors, "State Hwy" = '#ff9223')
  findcolors <- findcolors[findcolors !="State Hwy"]
  colors <- colors[colors !="#ff9223"]
}

if(length(findcolors[findcolors=="Hwy"])> 0 & length(colors[colors=="#ff9223"])>0) {
  plotcolors <- c(plotcolors, "Hwy" = '#ff9223')
  findcolors <- findcolors[findcolors !="Hwy"]
  colors <- colors[colors !="#ff9223"]
}


#go thru and assign the rest in order
for (i in 1:length(colors)) {
  tempnames <- names(plotcolors)
  plotcolors <- c(plotcolors, colors[i]) 
  names(plotcolors) <- c(tempnames, findcolors[i])
}


#-----------plot----------
suff <- plottype$SUFTYPABRV
allroads$SUFTYPABRV[!(allroads$SUFTYPABRV %in% suff)] <- "Other"

otherroads <- allroads[(allroads$SUFTYPABRV  == "Other"),]
allroads <- allroads[(allroads$SUFTYPABRV  != "Other"),]

blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent")) + 
  geom_sf(data=otherroads, size = .45, aes(color=SUFTYPABRV)) + 
  geom_sf(data=allroads, size = .55, aes(color=SUFTYPABRV)) + 
  scale_color_manual(values = plotcolors) 

ggsave(paste0("./IndivRoads/", city, ".png"), plot = last_plot(),
       scale = 1, width = 24, height = 24, units = "in",
       dpi = 500)





