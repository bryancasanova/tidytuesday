library(tidyverse)
library(ggmap)
library(lubridate)

dat <- read.csv("data/post_offices.txt") %>% 
  as_tibble()

dat <- dat %>% 
  select(name, state, county1, established,discontinued, coordinates, latitude, longitude)

# us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
datcor <- dat %>% 
  select(latitude, longitude,established, discontinued) %>% 
  na.omit() %>% 
  filter(established >= discontinued,
         established > 1000, discontinued > 1000) %>% 
  filter(latitude > 25.76, latitude < 49,
         longitude > -125, longitude < -67) %>% 
  mutate(start = ymd(paste0(established,"-01-01")),
         # end = ymd(paste0(discontinued, "-12-31"))) %>% 
         end = ymd("2000-12-31")) %>% 
    select(latitude, longitude, start, end)

# leaflet for timeline
library(leaflet)
library(leaftime)
datcor$end %>% range()
shp <- geojsonio::geojson_json(datcor,lat="latitude",lon="longitude")

leaflet() %>%
  addTiles() %>%
  setView(-95,38,4) %>%
  addTimeline(data = shp,
              sliderOpts = sliderOptions(
                position = "bottomleft",
                showTicks = FALSE))
