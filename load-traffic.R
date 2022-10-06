library(tidyverse)
library(data.table)

name <- "TCPSess_300_2017_0"
.path <- paste0("data/", name, ".hourly.merged.fst")

if (file.exists(.path)) df <- fst::read.fst(.path, as.data.table=TRUE) else {
  files <- Sys.glob(file.path("data", name, "*.hourly.fst"))
  
  df <- rbindlist(pbapply::pblapply(files, function(x) {
    df <- fst::read.fst(x, as.data.table=TRUE)
    df[, category:=strsplit(basename(x), "\\.")[[1]][1]]
  }))
  
  dups <- read_csv("data/TCPSess_300_2017_nidt_xy_lonlat.csv") %>%
    filter(hasdata) %>%
    arrange(bs_id) %>%
    group_by(lon, lat) %>%
    summarise(bs_id = list(bs_id)) %>%
    pull() %>%
    purrr::compact(~.x[-1])
  
  df[, bs_id := bs]
  setkey(df, bs)
  for (x in dups) df[x[-1], bs_id := x[1]]
  
  df[, bs := NULL]
  df <- df[, .(uplink=sum(uplink), downlink=sum(downlink)),
           by = .(ts, bs_id, category)]
  
  fst::write.fst(df, .path)
}
