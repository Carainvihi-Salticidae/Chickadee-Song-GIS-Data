

# clear the R workspace
rm(list = ls())

library(gdistance)
library(tidyverse)
library(rgeos)
library(elevatr)
library(ggplot2)  
library(tigris)  
library(spocc)
library(raster)
library(viridis)
library(ggthemes)
library(sf)
library(tmap)
library(rgdal)


states <- states()

se <- states %>%
  subset(REGION == "2") 



IN <- se %>%     # Subsetting the data to Indiana
  subset(NAME %in% "Indiana")



#Chickadee sites
my.sites <- read.csv("sampling points.csv", header = T) #import the data

colnames(se)


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
  SpatialPoints(proj4string = crs(se)) 



ggplot() + 
  theme_void() +
  geom_sf(data=se, fill = "grey80", aes(geometry = geometry)) +
  geom_sf(data=IN, fill="lightblue", aes(geometry = geometry))


ch_coords <- ch_sp@coords                                                  
ch_chull <- chull(ch_sp@coords)                           # Creating convex hull

ch_chull_ends <- ch_sp@coords[c(ch_chull, ch_chull[1]),]  # generate the end points of polygon. 
ch_poly <- SpatialPolygons(
  list(Polygons(
    list(Polygon(ch_chull_ends)), ID=1)),
  proj4string = crs(se))       # convert coords to SpatialPolygons 


ch_poly_buff <- gBuffer(ch_poly, width = 0.05, byid=T)


ggplot() + 
  theme_void() +
  geom_sf(data=IN, fill="lightblue", aes(geometry = geometry)) +
  geom_polygon(data=ch_poly_buff, aes(x=long, y=lat, grou=group), col="grey40", fill="pink") +
  geom_point(data=my.sites, aes(x=longitude, y=latitude), size=0.01)



ggplot() + 
  theme_void() +
  geom_polygon(data=ch_poly_buff, aes(x=long, y=lat, grou=group), col="grey40", fill="pink") +
  geom_point(data=my.sites, aes(x=longitude, y=latitude), size=3)


site_shapes <- geom_polygon(data=ch_poly_buff, aes(x=long, y=lat, grou=group), col="white", fill="grey")


library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)

my.coords <- "
1              Ross -87.06919 40.40991
2              Mart -87.03893 40.43215
3              Step -86.62048 40.64901
4              MoGo -86.73590 40.61429
5              NelS -86.98500 39.75799
6              CrGr -87.32664 40.36734
7              CaCl -87.41800 40.02649
8              Ferg -86.47653 40.34317
9              WhHr -87.27200 40.23732
10             WaFo -86.89265 40.06255
11             McCl -86.68235 39.82733
12             TkRn -87.20052 39.88718
13             Nard -87.28634 39.74225
14             Wasr -86.79117 40.50830
15             BgWt -86.77728 39.79676
16             Finl -87.02113 39.53162
17             Fern -86.96316 39.61072
18             Chan -86.75217 39.37161
19             RaLa -87.07616 39.75045"


nodes <- read.delim(text = my.coords, header = FALSE,
                    quote = "'", sep = "",
                    col.names = c('id','name', 'lon', 'lat'))


set.seed(123)  # set random generator state for the same output
N_EDGES_PER_NODE_MIN <- 1
N_EDGES_PER_NODE_MAX <- 18
N_CATEGORIES <- 9


edges <- read.csv("Song SNA by forest Complexity.csv", header = T) #import the data
edges
edges <- edges %>% mutate(category = as.factor(category))

g <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)


edges_for_plot <- edges %>%
  dplyr::inner_join(nodes %>% dplyr::select(id, lon, lat), by = c('from' = 'id')) %>%
  dplyr::rename(x = lon, y = lat) %>%
  dplyr::inner_join(nodes %>% dplyr::select(id, lon, lat), by = c('to' = 'id')) %>%
  dplyr::rename(xend = lon, yend = lat)


nodes$weight = degree(g)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))


ggplot(nodes) + site_shapes


mapcoords <- coord_fixed(xlim = c(-86, -88), ylim = c(39, 41))

ggplot(nodes) + site_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = category, size = weight),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) + # scale for edge widths
  geom_point(aes(x = lon, y = lat, size = weight),           # draw nodes
             shape = 21, fill = 'white',
             color = 'black', stroke = 0.5) +
  scale_size_continuous(guide = FALSE, range = c(1, 6)) +    # scale for node size
  geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 3, color = "white", fontface = "bold") +
  mapcoords + maptheme


