## HexBin Maps

setwd('/home/hp-nunes/dev/repo/R_dataViz')

# library
library(tidyverse)
library(devtools)
library(geojsonio) # install_version('geojsonio', '0.9.2')
library(RColorBrewer)
library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(geogrid)
library(classInt)
##################################
purpleAir2 <- read.csv("assets/purpleAir_byUniqueLocations.csv")
baaqmdSF <- readOGR(dsn="assets/shp/", layer="baaqmdSF")
nrow(purpleAir2)
colnames(purpleAir2)
### Choropleth
plot(baaqmdSF)
names(baaqmdSF)

pal <- brewer.pal(5,"OrRd")  # we select 5 colors from the palette

# determine the breaks
breaks.qt <- classIntervals(baaqmdSF$PM25, n = 5, style = "sd") 

# add a very small value to the top breakpoint, and subtract from the bottom
  # for symmetry
br <- breaks.qt$brks
offs <- 1e-07
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs

spplot(baaqmdSF,"PM25",col.regions = pal, at = br, 
       main = "PM2.5 Estimations (BAAAQMD)")


#####################################################################
library(tidyverse)
library(h3jsr) ## https://github.com/obrl-soil/h3jsr
library(mapdeck)
library(sf)

H3_RESOLUTION <-4

## A helper:
lon_lat_to_point <- function(lon, lat) {
  purrr::map2(
    lon,
    lat,
    ~ sf::st_point(c(.x, .y))
  ) %>%
    sf::st_sfc(crs = 4326)
}

purpleAir_hexbinned <-
  purpleAir2 %>%
  mutate(
    location = lon_lat_to_point(
      Longitude,
      Latitude
    ),
    h3_index = point_to_h3(
      location,
      H3_RESOLUTION
    )
  ) %>%
  group_by(h3_index) %>%
  summarise(n_crashes = n()) %>%
  mutate(outline = h3jsr::h3_to_polygon(h3_index)) %>%
  st_sf(crs = 4326) ## lon lat Coordinate Reference System

MAPBOX_TOKEN <- "pk.eyJ1IjoiaHAtbnVuZXMiLCJhIjoiY2pqNHAxaHIxMDA3aTNrbW15OGx2NW4ybiJ9.pHzT2FAtpO-Xhnc3PzJsFA"

purpleAir_hexbinned %>%
  mutate(log_crashes = log(n_crashes)) %>% ## makes hex colours a bit more interesting
  mapdeck(
    style = mapdeck_style("dark"),
    token = Sys.getenv(MAPBOX_TOKEN),
    height = "100vh"
  ) %>%
  add_polygon(fill_colour = "log_crashes")