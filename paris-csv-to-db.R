library(tidyverse)
library(DBI)

setwd("/zompopo/iucar")
col_names <- c("ts", "bs_id", "category", "uplink", "downlink")

bsids <- read_csv("ParisLyon_bs.csv", col_names="bs_id")$bs_id
bs <- read_delim("nidt_lonlat.csv", ";") %>% filter(bs_id %in% bsids)
paris_bs <- filter(bs, lat > 46.5)
lyon_bs <- filter(bs, lat < 46.5)
write_csv(paris_bs, "paris_lonlat.csv")
write_csv(lyon_bs, "lyon_lonlat.csv")

db <- dbConnect(MonetDBLite::MonetDBLite(), dbname="TCPSessCourtParisLyon_300_2016_monetdb")

for (f in Sys.glob("TCPSessCourtParisLyon_300_2016_*.csv.gz")) {
  message("Processing ", f, "...")
  month <- data.table::fread(paste("zcat", f))
  colnames(month) <- col_names
  paris <- month[month$bs_id %in% paris_bs$bs_id,]
  lyon <- month[month$bs_id %in% lyon_bs$bs_id,]
  message("Writing ", f, " to database...")
  dbWriteTable(db, "paris", paris, append=TRUE)
  dbWriteTable(db, "lyon", lyon, append=TRUE)
}

dbDisconnect(db)
rm(month, paris, lyon)
gc()
