library(data.table)
library(fst)

name <- "TCPSess_300_2017_0"
col_names <- c("ts", "bs", "uplink", "downlink")
col_classes <- c("integer", "character", "double", "double")

cl <- parallel::makeForkCluster(2)

pbapply::pbsapply(Sys.glob(file.path("data", name, "*.csv.gz")), function(x) {
  ptm <- proc.time()
  
  x.name <- strsplit(basename(x), "\\.")[[1]][1]
  df <- fread(x, col.names=col_names, colClasses=col_classes)
  
  df[, ts := anytime::anytime(ts)]
  setkey(df, ts, bs)
  write.fst(df, file.path("data", name, paste0(x.name, ".fst")))
  
  df.hourly <- df[, .(uplink=sum(uplink), downlink=sum(downlink)),
                  by = .(ts=lubridate::floor_date(ts, unit="hours"), bs)]
  write.fst(df.hourly, file.path("data", name, paste0(x.name, ".hourly.fst")))
  
  (proc.time() - ptm)[["elapsed"]]
}, cl=cl) -> x

parallel::stopCluster(cl)
