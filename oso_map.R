setwd("~/osoyoos/map")

library(dplyr); library(ggplot2)
library(mapdata); library(maptools)
library(rgdal); library(ggspatial)
library(sf)

can <- map_data("worldHires", "Canada")

shingle <- st_read("ShingleCreek.kml")      

(map <- ggplot() +    
    geom_polygon(data = can, 
                 aes(x = long, y = lat,
                     group = group),
                     fill = "white", 
                 color = "black") + 
    geom_sf(data = shingle,
            color = "blue4") +
    coord_sf(xlim = c(-120, -119),
             ylim = c(49,49.6)) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "gray90")) +
    ggsn::scalebar(y.min = 49.0, y.max = 49.6,
                   x.min = -120, x.max = -119,
                   transform = TRUE, dist = 5,
                   dist_unit = "km", height = 0.01,
                   model = "WGS84", location = "topright",
                   border.size = 0.5, st.dist = 0.05, 
                   st.color ="white"))

rivers <- spTransform(readOGR(dsn = ".", 
                        stringsAsFactors = FALSE,
               "FWRVRSPL_polygon",
               dropNULLGeometries = TRUE),
               CRS("+proj=longlat + datum=WGS84"))

lakesall <- spTransform(readOGR(dsn = ".", 
                      stringsAsFactors = FALSE,
                  "250_WATPS_polygon",
                  dropNULLGeometries = TRUE),
                  CRS("+proj=longlat + datum=WGS84")) 
lakes <- lakesall[lakesall@data$AREA_SQM > 4.8e5,]

(lakemap <- map + 
    geom_polygon(data = lakes,
                 aes(x = long, y = lat, group = group),
                 color = "blue4", fill = "lightblue") +
    geom_polygon(data = rivers,
                 aes(x = long, y = lat, group = group),
                 color = "blue4", fill = "blue4") +
    xlab(expression("Latitude"~(degree*W))) +
    ylab(expression("Latitude"~(degree*N))))

places <- read.csv("coords.csv")

labs <- read.csv("map_labels.csv") %>% 
  filter(group == "label") %>% select(2:ncol(.))

water <- read.csv("map_labels.csv") %>% 
  filter(group == "water") %>% select(2:ncol(.))

lines <- read.csv("connectors.csv") %>% 
  filter(shape == "line") %>% select(2:ncol(.))


(labmap <- lakemap +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "gray90")) +
    geom_segment(data = lines, 
                 aes(x = x, xend = xend,
                     y = y, yend = yend)) +
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
              color = "skyblue2") +
    geom_text(data = places, 
              aes(x = x + 0.08, 
                  y = y, hjust = "left", 
                  label = Name))

ggsave("oso_map.png", width = 2000, height = 2000,
      units = "px")
