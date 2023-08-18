
setwd("~/osoyoos/map")

# Libraries
library(dplyr); library(ggplot2)
library(mapdata); library(maptools)
library(rgdal); library(ggspatial)
library(sf); library(pgirmess)

# Map of Canada.
can <- map_data("worldHires", "Canada")

# Map of Shingle Creek.
# Because this is a kml, easiest to add prior to shapefiles.
shingle <- st_read("ShingleCreek.kml")      

# For specifying elevation raster boundaries.
(j <- pgirmess::bbox2sf(n = 49.6, s = 49.0,
                        w = -120, e = -119,
                        crs = 4326))


# Read in and process elevation data.
e <- elevatr::get_elev_raster(locations = j, z = 9); plot(e)
elevation_data <- raster::as.data.frame(e, xy = TRUE)
colnames(elevation_data)[3] <- "elevation"


# Map of bounding box with scale bar and Shingle Creek. 
(map <- ggplot() +    
    geom_polygon(data = can, 
                 aes(x = long, y = lat,
                     group = group),
                     fill = "white", 
                 alpha = 1/10,
                 color = "black") + 
    geom_raster(data = elevation_data, 
                aes(x = x, y = y,
                    fill = elevation),
                alpha = 3/4) +
    scale_fill_gradient2(low = "white",
                         mid = "gray90",
                        high = "black",
                        midpoint = 1800) +
    theme(axis.ticks = element_blank(),
          panel.background = element_rect(colour = "black", fill = NA),
          panel.grid = element_blank(),
          legend.position = "none") +
    geom_sf(data = shingle,
            color = "blue4") +
    coord_sf(xlim = c(-120, -119),
             ylim = c(49.02,49.6))  +
    ggspatial::annotation_scale(location = "tr",pad_y = unit(1.75, "cm")) +
    ggspatial::annotation_north_arrow(location = "tr",pad_y = unit(1/4, "cm"),
               style = ggspatial::north_arrow_fancy_orienteering()))

# Read in River shapefiles.
rivers <- spTransform(readOGR(dsn = ".", 
                        stringsAsFactors = FALSE,
               "FWRVRSPL_polygon",
               dropNULLGeometries = TRUE),
               CRS("+proj=longlat + datum=WGS84"))

# Read in lake shapefiles.
lakesall <- spTransform(readOGR(dsn = ".", 
                      stringsAsFactors = FALSE,
                  "250_WATPS_polygon",
                  dropNULLGeometries = TRUE),
                  CRS("+proj=longlat + datum=WGS84")) 

# Only include lakes over a select area.
lakes <- lakesall[lakesall@data$AREA_SQM > 4.8e5,]

# Add above shapes on original map.
(lakemap <- map + 
    geom_polygon(data = lakes,
                 aes(x = long, y = lat, group = group),
                 color = "blue4", fill = "lightblue") +
    geom_polygon(data = rivers,
                 aes(x = long, y = lat, group = group),
                 color = "blue4", fill = "blue4") +
    xlab(expression("Longitude"~(degree*W))) +
    ylab(expression("Latitude"~(degree*N))))

# Read in place coordinates.
places <- read.csv("coords.csv")

# Read in labels.
labs <- read.csv("map_labels.csv") %>% 
  filter(group == "label") %>% select(2:ncol(.))

# Subset of above labels for water bodies only.
water <- read.csv("map_labels.csv") %>% 
  filter(group == "water") %>% select(2:ncol(.))

# Line coordinates for labelling map.
lines <- read.csv("connectors.csv") %>% 
  filter(shape == "line") %>% select(2:ncol(.))

# Coordinates for arrows.
arrows <- read.csv("connectors.csv") %>% 
  filter(shape == "arrow") %>% select(2:ncol(.))

# Add above shapefiles to previous map.
(labmap <- lakemap +
    xlab("") +
    ylab("") +
    geom_segment(data = lines, 
                 aes(x = x, xend = xend,
                     y = y, yend = yend)) +
    geom_segment(data = arrows, 
                 aes(x = x, xend = xend,
                     y = y, yend = yend),
                 arrow = arrow(length = unit(0.15, "cm")),
                 color = "navy") +
    geom_point(data = places,
               aes(x = x, y = y),
               shape = 21, size = 2.5,
               color = "black",
               fill = "gray60") +
    geom_text(data=labs, hjust = "left",
              aes(x = x, y = y,
                  label = text))) +
    geom_text(data = water, hjust = "left",
              aes(x = x, y = y, label = text,
                  fontface = "italic"), 
              color = "dodgerblue3") +
    geom_text(data = places, 
              aes(x = x + 0.08, 
                  y = y, hjust = "left", 
                  label = Name))

ggsave("plots/oso_map.png", 
       width = 2000,
       height = 2000,
       units = "px")
