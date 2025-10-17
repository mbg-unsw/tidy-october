# SPHRAS Tidy Tuesday challenge - October 2025
# Malcolm Gillies <malcolm.gillies@unsw.edu.au>

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-02-18/readme.md

library(readr)

agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')

library(tmap)
library(dplyr)
library(lubridate)

agencies$yr <- as.factor(year(agencies$nibrs_start_date))

# Convert agencies data set to a "simple features" object
ag_sf = st_as_sf(na.omit(agencies, cols=c("longitude", "latitude")), coords = c("longitude", "latitude"),
                 crs = 4269, agr = "constant")

# Pick out US from world
US = World[World$sovereignt == "United States of America", ]

# Bounding box of area of continental US
bb <- st_bbox(c(xmin = -124.8, xmax = -66.9, ymax = 49.4, ymin = 24.7), crs = st_crs(4269))

ag_tm <- tm_shape(land[US]) + # clip/subset land data to US
tm_raster("cover_cls") + # add raster layer with land use colours
  tm_shape(US, is.main=TRUE, bbox=bb) +
  #tm_basemap("OpenTopoMap") + # couldn't work out how to clip basemaps
     tm_borders() +
  tm_shape(ag_sf) +
  tm_symbols(fill="red", size=0.2) + # add dots for agency locations
  tm_crs("EPSG:4269") + # NAD83 coordinate system
  tm_legend(show=FALSE) +
  tm_title("US Law Enforcement Agency Locations",
           position = tm_pos_out(cell.h = "center", cell.v = "top", pos.h = "center")) +
  tm_animate(frames="yr")

tmap_animation(ag_tm, filename="agencies_anim.gif")
