# SPHRAS Tidy Tuesday challenge - October 2025
# Malcolm Gillies <malcolm.gillies@unsw.edu.au>

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-02-18/readme.md

library(readr)

agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')

library(tmap)
library(dplyr)
library(lubridate)

agencies$yr <- as.factor(year(agencies$nibrs_start_date))
ag_sf = st_as_sf(na.omit(agencies, cols=c("longitude", "latitude")), coords = c("longitude", "latitude"),
                 crs = 4269, agr = "constant")

US = World[World$sovereignt == "United States of America", ]

bb <- st_bbox(c(xmin = -124.8, xmax = -66.9, ymax = 49.4, ymin = 24.7), crs = st_crs(4269))

ag_tm <- tm_shape(land[US]) +
tm_raster("cover_cls") +
  tm_shape(US, is.main=TRUE, bbox=bb) +
  #tm_basemap("OpenTopoMap") +
     tm_borders() +
  tm_shape(ag_sf) +
  tm_symbols(fill="red", size=0.2) +
  tm_crs("EPSG:4269") +
  tm_legend(show=FALSE) +
  tm_title("US Law Enforcement Agency Locations",
           position = tm_pos_out(cell.h = "center", cell.v = "top", pos.h = "center")) +
  tm_animate(frames="yr")

tmap_animation(ag_tm, filename="agencies_anim.gif")
