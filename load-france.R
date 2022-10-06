library(sp)
library(rgeos)
library(raster)

.path <- "data/france.RData"

if (file.exists(.path)) load(.path) else {
  voronoi_polys <- function(bs, proj4string) {
    bs <- bs %>%
      arrange(bs_id) %>%
      group_by(lon, lat) %>%
      slice(1) %>%
      ungroup()
    
    w <- deldir::tile.list(deldir::deldir(bs$lon, bs$lat))
    
    lapply(seq_along(w), function(i) {
      tmp <- cbind(w[[i]]$x, w[[i]]$y)
      Polygons(list(Polygon(rbind(tmp, tmp[1,]))), ID=i)
    }) %>%
      SpatialPolygons(proj4string=proj4string) %>%
      SpatialPolygonsDataFrame(as.data.frame(bs))
  }
  
  shp <- file.path("data", "CONTOURS-IRIS_2-1__SHP__FRA_2017-06-30",
                   "CONTOURS-IRIS", "1_DONNEES_LIVRAISON_2016",
                   "CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2016", "CONTOURS-IRIS.shp")
  
  france.iris <- shapefile(shp) %>% gBuffer(byid=TRUE, width=0)
  france.insee <- aggregate(france.iris, by="INSEE_COM")
  france <- aggregate(france.iris)
  
  france.bs <- read_csv("data/TCPSess_300_2017_nidt_xy_lonlat.csv") %>%
    filter(hasdata) %>%
    voronoi_polys(crs("+proj=longlat +datum=WGS84")) %>%
    spTransform(crs(france.iris)) %>%
    intersect(france)
  
  save(france, france.iris, france.insee, france.bs, file=.path)
}
