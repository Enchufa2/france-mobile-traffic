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

# print maps
parallel::mclapply(vec_ts, function(target_ts) {
  if (any(grepl(target_ts, Sys.glob("*.png")))) return(NULL)
  
  paris_youtube_slice <- get_slice(paris_youtube, target_ts) %>%
    mutate(downlink = log(downlink) / log(max_downlink))
  
  m <- leaflet(paris_youtube_slice) %>%
    setView(center$lon, center$lat, zoom = 13) %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    #addCircles(data = paris_bs, lng = ~lon, lat = ~lat) %>%
    addHeatmap(lng = ~lon, lat = ~lat, intensity = ~downlink, blur=15, radius=15)
  
  #m
  
  filename <- paste0("paris_youtube_", as.numeric(target_ts), ".png")
  mapview::mapshot(m, file = path.expand(filename))
}, mc.cores = 32) %>% invisible
