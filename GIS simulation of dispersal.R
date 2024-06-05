



# Clear workspace
rm(list=ls())


##  =====  Some Additional Handy GIS Packages  =====
##  ================================================
library(maptools)   		## Read shape files
library(raster)     		## Read raster files and clipping
library(PBSmapping) 		## GIS_like geospatial object manipulation / analysis including poly
library(rgdal)     		## *** For map projection, CRS *** 
library(SDMTools)   		## Species Distribution Modelling Tools		 
library(RSAGA)      		## Geoprocessing and Terrain Analysis in R
library(spatstat)   		## Spatial statistics in R
library(rgeos)
library(FedData)
library(rasterVis)
library(sp)
library(reshape2)
library(treemapify)
library(ggplot2)
library(kableExtra)
library(animation)
library(scales)


library(elevatr)
library(tigris) 
library(viridis)
library(mapproj)


library(gdistance)
library(tidyverse)
library(spocc)
library(ggthemes)
library(sf)
library(tmap)




#Attempting rasters of canopy
wd <-  ("//boilerad.purdue.edu/cos/Userdata/jenki129/Documents/GIS_Practice_Directory")


setwd(wd)

##############################################################################
#########================ Creating Raster Map======================###########
##############################################################################

#import raster for 2011 landcover
cover <- "//boilerad.purdue.edu/cos/Userdata/jenki129/Documents/GIS_Practice_Directory/NLCD_2011_Land_Cover_L48_20210604_NN75mJAcaEgJCDNvGLD6.tiff"

lncov <- raster(cover) #assign raster


projection(lncov) #get projection data
plot(lncov) #view the raster


states <- states() #import state shapefiles from tigris


se <- states %>%
  subset(REGION == "2") #subset the region for midwest



IN <- se %>%     # Subsetting the data to Indiana
  subset(NAME %in% "Indiana")


#Chickadee sites
my.sites <- read.csv("sampling points.csv", header = T, row.names = 1) #import the sampling points

#make sure the projection is the same for all data used, otherwise it won't run
prj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 \
        +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 \
        +datum=NAD83 +units=m +no_defs'

ch_sp <- my.sites %>%            # Grabbing the Darwin-core data from the spocc object
  dplyr::select(longitude,                             # Keep locations and year, discard the rest
                latitude) %>%                   
  filter(!duplicated(round(longitude, 2),              # Remove duplicate records using rounded decimals (this removes points very near to one-another)       
                     round(latitude, 2)) == TRUE) %>%  # >> See notes below about ^^
  dplyr::mutate(lon = scale(longitude),                # Remove points far outside the cluster of occurrences
                lat = scale(latitude)) %>%             # >> See notes below about ^^
  dplyr::filter(!abs(lon)>2) %>%
  dplyr::filter(!abs(lat)>2) %>%
  dplyr::select(longitude,
                latitude) %>%
  SpatialPoints(proj4string = crs("+proj=longlat +datum=WGS84")) 



ch_coords <- ch_sp@coords                                                  
ch_chull <- chull(ch_sp@coords)                           # Creating convex hull

ch_chull_ends <- ch_sp@coords[c(ch_chull, ch_chull[1]),]  # generate the end points of polygon. 
ch_poly <- SpatialPolygons(
  list(Polygons(
    list(Polygon(ch_chull_ends)), ID=1)),
  proj4string = crs("+proj=longlat +datum=WGS84"))       # convert coords to SpatialPolygons 


ch_poly_buff <- gBuffer(ch_poly, width = 0.05, byid=T)




legend <- pal_nlcd() #import nlcd landcover legend
legend        

vals<-unique(lncov[[1]])
df<-legend[legend$code %in% vals,]


#Recognize it as a categorical raster using ratify(): 
rat<-ratify(lncov[[1]])

#  custom legend:
myKey <- list(rectangles=list(col = df$color),
              text=list(lab=df$description),
              space='left',
              columns=1,
              size=2,
              cex=.6)

#Plot 
levelplot(lncov, att='ID', 
          col.regions=df$color,
          par.settings = list(axis.line = list(col = "transparent"), 
                              strip.background = list(col = 'transparent'), 
                              strip.border = list(col = 'transparent')), 
          scales = list(col = "transparent"),
          colorkey=F,
          key=myKey)   


st_crs(lncov)
st_crs(IN)

#already done, but if for some reason r forgets, which it has before, run this again
prj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 \
        +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 \
        +datum=NAD83 +units=m +no_defs'


coordinates(ch_poly_buff) <- ~longitude+latitude
proj4string(ch_poly_buff) <- CRS("+proj=longlat +datum=WGS84")
ch_poly_buff <- spTransform(ch_poly_buff, CRS(prj))



#Cut Indiana from the raster
IN <- st_transform(
  IN,
  crs = prj)


r <- crop(lncov, extent(IN))
dlm.in <- mask(r, IN)

plot(dlm.in)

#Now isolate the study area
r2 <- crop(lncov, extent(ch_poly_buff))
dlm <- mask(r2, ch_poly_buff)

plot(dlm)




# And here we plot it minus the axes, ticks and labels. 
levelplot(dlm, att='ID', 
          col.regions=df$color,
          par.settings = list(axis.line = list(col = "transparent"), 
                              strip.background = list(col = 'transparent'), 
                              strip.border = list(col = 'transparent')), 
          scales = list(col = "transparent"),
          colorkey=F,
          key=myKey)   


coordinates(my.sites) <- ~longitude+latitude
proj4string(my.sites) <- CRS("+proj=longlat +datum=WGS84")
sites <- spTransform(my.sites, CRS(prj))


sites <- read.csv("coordinates.csv")

image(dlm, zlim=c(30,70))
col <- terrain.colors(5)
image(dlm, zlim=c(30,70), main="Digital Landcover Model (DEM)", col=col)



# force the legend to fit next to the plot.
par(xpd = TRUE)

#add a legend - & make it appear outside of the plot
legend(x = "right", inset=c(-0.3,0),
       legend =  myKey$text$lab, 
       fill = col)

points(sites, pch=16) #add sampling points
text(x=sites2$longitude, sites2$latitude, sites2$X, cex=0.6, pos=3, offset(0.5)) #name the ponits




#####====  Reclassify Raster ====#####
resistance.cells <- c(1,11,50,12,40,20,53,95,20,41,52,50) #reclassify the cells from the legend making forest, urban,  waterways 50 and all else 20
res.matrix <- matrix(resistance.cells, ncol=3, nrow=4, byrow=T) #turn cells to matrix
res.matrix

forest.res <- reclassify(dlm, res.matrix) #reclassifying the dlm
forest.res
plot(forest.res)

col1 <- c("grey58", "green", "green") #change the colors to something more useful

plot(forest.res,
     col=col1, legend =F)

points(sites, pch=16) #add sites and text
text(x=sites2$longitude, sites2$latitude, sites2$X, cex=1.2, pos=4, offset(0.5))



##############################################################################
#########=============== Generating Simulations ===================###########
##############################################################################



ch_sample <- sites


ch_combn <- combn(nrow(sites),2) %>%
  t() %>%
  as.matrix()



#The raster is fairly large and for some computers it may be too large to run
#If this is the case you can aggregate it to make it smaller, I'd recommend this anyway
#because it speeds up the time and doesn't lose much in the way of relative cell size
forest.res.small <- aggregate(forest.res, fact=5)
forest.res.small

plot(forest.res.small)

#when shrinking the raster it reclassifies the cells so you must do so again.
resistance.cells2 <- c(1,5,20,5,5,5,20,50,50)
res.matrix2 <- matrix(resistance.cells2, ncol=3, nrow=3, byrow=T)
res.matrix2
forest.res.small <- reclassify(forest.res.small, res.matrix2)
forest.res.small
plot(forest.res.small)


col2 <- c("grey58", "green", "green")

plot(forest.res.small,
     col=col1, main="Tree Cover Small", legend =F)

par(xpd = TRUE)

points(sites10, pch=16)
text(x=sites5$longitude, sites5$latitude, sites5$X, cex=0.6, pos=3, offset(0.5))



#Creating the transition function for the simulation
for_tr <- transition(forest.res.small, transitionFunction = mean, 16) %>%
  geoCorrection(type="c",multpl=F)  

tobler <- function(x){(6*exp(3.5*abs(x+0.05)))*1000} #for generation of steps to get from one site to another

#Making sure it reads the raster directions
for.adj <- adjacent(x=forest.res.small, cells=1:ncell(forest.res.small), direction=16)
speed <- for_tr
speed[for.adj] <- tobler(for_tr[for.adj])

plot(raster(speed))
conduct <- geoCorrection(speed, scl=FALSE)

plot(raster(conduct))


locations <- SpatialPoints(rbind(ch_sample[ch_combn[,1],1:2],     # create origin points
                                 ch_sample[ch_combn[,2],1:2]),   # create destination (or goal) points, to traverse
                           crs(prj))


com.dis <- commuteDistance(for_tr, locations) #random walk distance
com.dis


cost.dis <- costDistance(for_tr, locations) #least cost distance
cost.dis




#library(MASS)
write.matrix(com.dis, file="matrix.canopyriver.f50.u20.east.csv")

write.matrix(cost.dis, file="matrix.canopyriver.cost.f50.u20.east.csv")

##############################################################################
#########================ Mapping Simulation ======================###########
##############################################################################

#### Code inspired by Alex Baecher https://www.alexbaecher.com/post/connectivity-script/

minValue(forest.res)
maxValue(forest.res)
range(forest.res)
my.list <- list(getValues(forest.res.small))
my.list <- as.data.frame(my.list)
my.list <- na.omit(my.list)
my.list


passages <- list()                                                     # Create a list to store the passage probability rasters in
system.time(                                                           # Keep track of how long this takes
  for (i in 1:nrow(ch_combn)) {           
    locations <- SpatialPoints(rbind(ch_sample[ch_combn[i,1],1:2],     # create origin points
                                     ch_sample[ch_combn[i,2],1:2]),   # create destination (or goal) points, to traverse
                               crs(prj))
    passages[[i]] <- passage(for_tr,                                   # run the passage function 
                             origin=locations[1],                 # set orgin point
                             goal=locations[2],                   # set goal point
                             theta = 0)                             # set theta (If 0 then it's a random walk if 1 then it's least cost, it can also be set in between)
    print(paste((i/nrow(ch_combn))*100, "% complete"))
  }
)




passages <- stack(passages)                                            # create a raster stack of all the passage probabilities
passages_overlay <- sum(passages)/nrow(ch_combn)                       # calculate average




colors <- c("lightgrey", viridis_pal(option="inferno", begin = 0.3, end = 1)(20))

ggplot(as.data.frame(passages_overlay, xy=T)) + geom_raster(aes(x=x,y=y,fill=layer)) +
  scale_fill_gradientn(colors = colors, na.value = NA) + 
  theme_map() +   theme(legend.position = "right")


writeRaster(passages_overlay, "f20.u50.passages.tif")

#plotting the simulation
pcover <- "//boilerad.purdue.edu/cos/Userdata/jenki129/Documents/GIS_Practice_Directory/f20.u50.passages.tif"

passlncov <- raster(pcover)
plot(passlncov)

resistance.cells3 <- c(0,0.0011,0,0.0011,0.06,10,0.06,0.5,10)
res.matrix3 <- matrix(resistance.cells3, ncol=3, nrow=3, byrow=T)
res.matrix3
forest.passage <- reclassify(passlncov, res.matrix3)
forest.passage

plot(forest.passage)
