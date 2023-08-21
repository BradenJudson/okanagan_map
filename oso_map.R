
setwd("~/osoyoos/map")

# Libraries
library(dplyr); library(ggplot2)
library(mapdata); library(maptools)
library(rgdal); library(ggspatial)
library(sf); library(pgirmess)
library(osmdata); library(bcmaps)
library(raster)

# Map of Canada.
can <- map_data("worldHires", "Canada")


# Map of Shingle Creek.
# Because this is a kml, easiest to add prior to other shapefiles.
shingle <- st_read("ShingleCreek.kml")      


# For specifying elevation raster boundaries.
# Helps to go just outside of intended plot boundary.
(j <- pgirmess::bbox2sf(n = 50, s = 48.0,
                        w = -121, e = -118,
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
    theme(panel.background = element_rect(colour = "black", fill = NA),
          panel.grid = element_blank(),
          legend.position = "none",
          plot.margin = margin(0.1, 0.1, -1/3, 0.1, "cm")) +
    labs(x = "", y = "") +
    geom_sf(data = shingle,
            color = "blue4") +
    coord_sf(xlim = c(-120, -119),
             ylim = c(49,49.6))  +
    ggspatial::annotation_scale(location = "tl",pad_y = unit(1.75, "cm")) +
    ggspatial::annotation_north_arrow(location = "tl", 
                                      pad_y = unit(1/4, "cm"),
                                      pad_x = unit(3/5, "cm"),
               style = ggspatial::north_arrow_fancy_orienteering()))


# Obtain major roadway data.
(okhi <- opq(j) %>% 
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "primary_link",
                            "motorway_link", "secondary", "secondary_link", 
                            "trunk")) %>% 
  osmdata_sf())


# Plot original map with major roads.
(mapR <- map + geom_sf(data = okhi$osm_lines,
              inherit.aes = FALSE, alpha = 1/2) +
  coord_sf(xlim = c(-120, -119),
           ylim = c(49,49.6)))


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


# Only include lakes over a select area/size.
lakes <- lakesall[lakesall@data$AREA_SQM > 4.8e5,]


# Add above shapes on original map.
(lakemap <- mapR + 
    geom_polygon(data = lakes,
                 aes(x = long, y = lat, group = group),
                 color = "blue4", fill = "lightblue") +
    geom_polygon(data = rivers,
                 aes(x = long, y = lat, group = group),
                 color = "blue4", fill = "blue4"))

# Clear up memory.
rm(mapR); rm(lakesall); gc()

# Read in place coordinates.
places <- read.csv("coords.csv")

# Read in labels.
labs <- read.csv("map_labels.csv") %>% 
  filter(group == "label") %>% dplyr::select(2:ncol(.))

# Subset of above labels for water bodies only.
water <- read.csv("map_labels.csv") %>% 
  filter(group == "water") %>% dplyr::select(2:ncol(.))

# Line coordinates for labelling map.
lines <- read.csv("connectors.csv") %>% 
  filter(shape == "line") %>% dplyr::select(2:ncol(.))

# Coordinates for arrows.
arrows <- read.csv("connectors.csv") %>% 
  filter(shape == "arrow") %>% dplyr::select(2:ncol(.))


# Add above shapefiles to previous map.
(labmap <- lakemap +
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
                  label = text)) +
    geom_text(data = water, hjust = "left",
              aes(x = x, y = y, label = text,
                  fontface = "italic"), 
              color = "dodgerblue3") +
    geom_text(data = places, 
              aes(x = x + 0.09, 
                  y = y, hjust = "left", 
                  label = Name)))

ggsave("plots/oso_map.png", 
       width = 2000,
       height = 2000,
       units = "px")


# Inset -------------------------------------------------------------------

# Read in Canada and US base data.
ca <- map_data("world", "Canada")
us <- map_data("state")

# Download shape of Alaska. 
us <- raster::getData("GADM", country = "USA", level = 1)
alaska <- us[us$NAME_1 %in% "Alaska", c(1:3)]

# Download BC boundary data. 
bcp <- st_geometry(bc_bound()); plot(bcp)

# Inset plot (BC and some surrounding USA).
(ins <- ggplot() +
  geom_polygon(data = us,
               aes(x = long, y = lat,
                   group = group),
               fill = "gray95",
               colour = "black",
               linewidth = 1/4) +
  geom_polygon(data = ca, 
               aes(x = long, y = lat, group = group),
               colour = "black", fill = "gray90")+
  geom_sf(data = bcp, color = "black") +
  geom_polygon(data = alaska,
               aes(x = long, y = lat, group = group),
               fill = "gray95",
               colour = "black",
               linewidth = 1/4) +
  annotate("rect", fill = NA, color = "black",
           xmin = -119, xmax = -120,
           ymin = 49, ymax = 49.6) +
  coord_sf(ylim = c(45, 59),
           xlim = c(-133, -113),
           crs = "WGS84")  +
  geom_segment(aes(xend = -119, x = -115,
                   yend = 49.7, y = 53),
               lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(0.25, "cm")),
               colour = "red2")  +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid =  element_blank(),
        panel.background = element_rect(fill = alpha("skyblue", 1/10)),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = margin(0,0,0,0),
        plot.background = element_rect(fill = NA, color = NA)) +
  labs(x = "", y = ""))

ggsave("plots/inset.png", units = "px",
       width = 1250, height = 2000)

# Add inset --------------------------------------------------------------------

library(cowplot)

ggdraw(plot = labmap) +
  draw_plot(
    {
      ins
    },
    x= 0.67,
    y = 0.55,
    width = 0.3,
    height = 0.4
  )

ggsave("plots/map_w_inset.png", units = "px",
       width = 2000, height = 2000)

