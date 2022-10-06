setwd("/zompopo/iucar")
suppressWarnings(suppressMessages(source("paris-init.R")))

# collect an application
dbdir <- "TCPSessCourtParisLyon_300_2016_monetdb"
paris_db <- tbl(MonetDBLite::src_monetdblite(dbdir), "paris")
paris_youtube <- paris_db %>%
  filter(category == "s1") %>%
  select(-category) %>%
  collect() %>%
  filter(ts < as.numeric(anytime::anytime("2016-09-15")))
max_downlink <- paris_db %>%
  summarise(downlink = max(downlink, na.rm=TRUE)) %>%
  pull()
vec_ts <- seq(min(paris_youtube$ts), max(paris_youtube$ts), by=300)

# bins, palette
#bins <- c(0, 10*2^seq(0, ceiling(log2(max(paris_youtube$downlink/10/1e6)))))
#pal <- colorBin("YlOrRd", domain = range(bins), bins = bins)
pal <- colorBin("YlOrRd", domain = c(0, 1), na.color = "white", pretty = FALSE)

# print maps
parallel::mclapply(vec_ts, function(target_ts) {
  if (any(grepl(target_ts, Sys.glob("*.png")))) return(NULL)
  
  paris_youtube_slice <- get_slice(paris_youtube, target_ts) %>%
    mutate(downlink = log(downlink) / log(max_downlink))
  voronoi_df <- SpatialPolygonsDataFrame(voronoi, data = paris_youtube_slice)
  
  m <- leaflet(voronoi_df) %>%
    setView(center$lon, center$lat, zoom = 13) %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    #addCircles(data = paris_bs, lng = ~lon, lat = ~lat) %>%
    addLegend(pal = pal, values = bins, opacity = 0.7,
              title = anytime::anytime(target_ts), position = "bottomright") %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.5,
                fillColor = ~pal(downlink), fillOpacity = 0.6,
                highlight = highlightOptions(fillOpacity = 1, bringToFront = TRUE),
                label = sprintf(
                  "<strong>%s</strong><br/>%g MB", 
                  paris_youtube_slice$bs_id, paris_youtube_slice$downlink
                ) %>% lapply(htmltools::HTML))
  
  #m
  
  filename <- paste0("paris_youtube_", as.numeric(target_ts), ".png")
  mapview::mapshot(m, file = path.expand(filename))
}, mc.cores = 32)  %>% invisible
