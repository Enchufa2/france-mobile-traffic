library(tidyverse)
library(leaflet)
library(leaflet.extras)

voronoi_polys <- function(bs) {
  bs <- bs %>%
    group_by(lon, lat) %>%
    summarise(bs_id=sort(bs_id)[1]) %>%
    ungroup() %>%
    arrange(bs_id)
  
  w <- deldir::tile.list(deldir::deldir(bs$lon, bs$lat))
  
  lapply(seq_along(w), function(i) {
    tmp <- cbind(w[[i]]$x, w[[i]]$y)
    sp::Polygons(list(sp::Polygon(rbind(tmp, tmp[1,]))), ID=i)
  }) %>% sp::SpatialPolygons()
}

get_slice <- function(df, target_ts) df %>%
  filter(ts == target_ts) %>%
  select(-ts) %>%
  right_join(paris_bs) %>%
  mutate_if(is.numeric, coalesce, 0) %>%
  group_by(lon, lat) %>%
  summarise(bs_id = sort(bs_id)[1], 
            uplink = sum(uplink), 
            downlink = sum(downlink)) %>%
  ungroup() %>%
  arrange(bs_id)

# read basic data, compute voronoi
cat <- read_csv("categorization_dataframe_sess_courts_and_longs_updated.csv")
paris_bs <- read_csv("paris_lonlat.csv")
center <- list(lon = mean(range(paris_bs$lon)), lat = mean(range(paris_bs$lat)))
voronoi <- voronoi_polys(paris_bs)
