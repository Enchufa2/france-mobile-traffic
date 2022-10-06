library(DBI)

name <- "TCPSess_300_2017"
col_names <- c("ts", "bs", "category", "uplink", "downlink")
col_classes <- c("integer", "character", "character", "double", "double")

tmp <- "/run/media/iucar/82484f7d-dd7d-4eea-89d5-8f7a73e98295/spark/tmp.csv"

db <- MonetDBLite::ml(paste0("data/", name, "_monetdb"))
name <- dbQuoteIdentifier(db, name)

MonetDBLite::monetdb.read.csv(
  db, tmp, name, header=FALSE, col.names=col_names, sep=";")

dbDisconnect(db, shutdown=TRUE)
