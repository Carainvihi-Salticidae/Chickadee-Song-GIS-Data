
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

library(landscapemetrics)
library(dplyr)
library(terra)


#Attempting rasters of canopy
wd <- ("C:/Users/jenki129/OneDrive - purdue.edu/GIS_Practice_Directory")


setwd(wd)

##############################################################################
#########==================== 2011 Data ==========================###########
##############################################################################

#import raster for 2011 landcover
cover <- "C:/Users/jenki129/OneDrive - purdue.edu/GIS_Practice_Directory/NLCD_2011_Land_Cover_L48_20210604_NN75mJAcaEgJCDNvGLD6.tiff"

lncov <- raster(cover)


projection(lncov)


states <- states() #import state shapefiles from tigris


se <- states %>%
  subset(REGION == "2") #subset the region for midwest



IN <- se %>%     # Subsetting the data to Indiana
  subset(NAME %in% "Indiana")


#Chickadee sites
my.sites <- read.csv("sampling points.csv", header = T, row.names = 1) #import the sampling points


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




legend <- pal_nlcd()
legend        


st_crs(lncov)
st_crs(IN)


prj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 \
        +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 \
        +datum=NAD83 +units=m +no_defs'


coordinates(ch_poly_buff) <- ~longitude+latitude
proj4string(ch_poly_buff) <- CRS("+proj=longlat +datum=WGS84")
ch_poly_buff <- spTransform(ch_poly_buff, CRS(prj))




IN <- st_transform(
  IN,
  crs = prj)


r <- crop(lncov, extent(IN))
dlm.in <- mask(r, IN)


r2 <- crop(lncov, extent(ch_poly_buff))
dlm <- mask(r2, ch_poly_buff)


#####====  Reclassify Raster ====#####
resistance.cells <- c(1,40,0,40,44,1,44,100,0)
res.matrix <- matrix(resistance.cells, ncol=3, nrow=3, byrow=T)
res.matrix

forest.res <- reclassify(dlm, res.matrix)
forest.res
plot(forest.res)


sp.dlm <- rast(forest.res)

sp.forest.res <- classify(sp.dlm, res.matrix)
sp.forest.res
plot(sp.forest.res)

yf <- patches(sp.forest.res, zeroAsNA=TRUE)

max(zf$patches)
ff <-freq(yf)
head(ff)

zf = cellSize(yf,unit="km") |> zonal(yf, sum)
head(zf) |> round(2)

write.csv(zf, "C:/Users/jenki129/OneDrive - purdue.edu/GIS_Practice_Directory\\Patch Sizes - Forest Total.csv", row.names=FALSE)

sp.forest.res

plot(yf)
cuts= c(0,20000,40000)
pal <- c("blue","red")



plot(yf,breaks=cuts,col=pal)
points(sites, pch=16) #add sampling points
text(x=sites$longitude, sites$latitude, sites$X, cex=0.6, pos=3, offset(0.5)) #name the ponits



#####====  North ====#####

#Chickadee sites
my.sites.n <- read.csv("sampling points north.csv", header = T, row.names = 1) #import the sampling points


ch_sp.n <- my.sites.n %>%            # Grabbing the Darwin-core data from the spocc object
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



ch_coords.n <- ch_sp.n@coords                                                  
ch_chull.n <- chull(ch_sp.n@coords)                           # Creating convex hull

ch_chull_ends.n <- ch_sp.n@coords[c(ch_chull.n, ch_chull.n[1]),]  # generate the end points of polygon. 
ch_poly.n <- SpatialPolygons(
  list(Polygons(
    list(Polygon(ch_chull_ends.n)), ID=1)),
  proj4string = crs("+proj=longlat +datum=WGS84"))       # convert coords to SpatialPolygons 


ch_poly_buff.n <- gBuffer(ch_poly.n, width = 0.05, byid=T)




coordinates(ch_poly_buff.n) <- ~longitude+latitude
proj4string(ch_poly_buff.n) <- CRS("+proj=longlat +datum=WGS84")
ch_poly_buff.n <- spTransform(ch_poly_buff.n, CRS(prj))


r <- crop(lncov, extent(IN))
dlm.in <- mask(r, IN)


r2n <- crop(lncov, extent(ch_poly_buff.n))
dlm.n <- mask(r2n, ch_poly_buff.n)


resistance.cells <- c(1,40,0,40,44,1,44,100,0)
res.matrix <- matrix(resistance.cells, ncol=3, nrow=3, byrow=T)
res.matrix

forest.res.n <- reclassify(dlm.n, res.matrix)
forest.res.n
plot(forest.res.n)


#####==== South ====#####

#Chickadee sites
my.sites.s <- read.csv("sampling points south.csv", header = T, row.names = 1) #import the sampling points


ch_sp.s <- my.sites.s %>%            # Grabbing the Darwin-core data from the spocc object
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



ch_coords.s <- ch_sp.s@coords                                                  
ch_chull.s <- chull(ch_sp.s@coords)                           # Creating convex hull

ch_chull_ends.s <- ch_sp.s@coords[c(ch_chull.s, ch_chull.s[1]),]  # generate the end points of polygon. 
ch_poly.s <- SpatialPolygons(
  list(Polygons(
    list(Polygon(ch_chull_ends.s)), ID=1)),
  proj4string = crs("+proj=longlat +datum=WGS84"))       # convert coords to SpatialPolygons 


ch_poly_buff.s <- gBuffer(ch_poly.s, width = 0.05, byid=T)




coordinates(ch_poly_buff.s) <- ~longitude+latitude
proj4string(ch_poly_buff.s) <- CRS("+proj=longlat +datum=WGS84")
ch_poly_buff.s <- spTransform(ch_poly_buff.s, CRS(prj))


r2s <- crop(lncov, extent(ch_poly_buff.s))
dlm.s <- mask(r2s, ch_poly_buff.s)

forest.res.s <- reclassify(dlm.s, res.matrix)
forest.res.s
plot(forest.res.s)


#####==== Fragmentation ====#####

num.patch <- get_patches(forest.res,class = "1", directions = 8)
num.patch
forest.patch <- lsm_c_area_mn(forest.res,directions=8)
forest.patch #13.8 hectares
forest.patch.sd <- lsm_c_area_sd(forest.res,directions=8)
forest.patch.sd #94.4

  

num.patch.n <- get_patches(forest.res.n,class = "1", directions = 8)
num.patch.n
forest.patch.n <- lsm_c_area_mn(forest.res.n,directions=8)
forest.patch.n #8.20 hectares
forest.patch.n.sd <- lsm_c_area_sd(forest.res.n,directions=8)
forest.patch.n.sd #27.8


num.patch.s <- get_patches(forest.res.s,class = "1", directions = 8)
num.patch.s
forest.patch.s <- lsm_c_area_mn(forest.res.s,directions=8)
forest.patch.s #22.0 hectares
forest.patch.s.sd <- lsm_c_area_sd(forest.res.s,directions=8)
forest.patch.s.sd



forest.freq <- freq(forest.res)
forest.freq
(2038814/9781274)*100  #20.84%


forest.freq.n1 <- freq(forest.res.n)
forest.freq.n1
(625686/4995259)*100   #12.53%


forest.freq.s1 <- freq(forest.res.s)
forest.freq.s1
(1268537/3614472)*100  #35.09%



#cohesion
forest.coh <- lsm_c_cohesion(forest.res, directions = 8)
forest.coh

forest.coh.n <- lsm_c_cohesion(forest.res.n, directions = 8)
forest.coh.n

forest.coh.s <- lsm_c_cohesion(forest.res.s, directions = 8)
forest.coh.s

forest.complexity <- lsm_l_condent(forest.res, neighbourhood = 8) #conditional entropy
forest.complexity

forest.complexity.n <- lsm_l_condent(forest.res.n, neighbourhood = 8)
forest.complexity.n

forest.complexity.s <- lsm_l_condent(forest.res.s, neighbourhood = 8)
forest.complexity.s



######===== Compare ======#####
library(ecolTest)

div.data <- read.csv("North South diversity.csv", header = T) #import the data
View(my.data)


Hutcheson_t_test(
  div.data$North,
  div.data$South,
  shannon.base = exp(1),
  alternative = "two.sided",
  difference = 0
)
