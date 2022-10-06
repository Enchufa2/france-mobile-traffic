library(data.table)
library(fst)

col_names <- c("ts", "commune", "port", "uplink", "downlink")
col_classes <- c("integer", "character", "integer", "double", "double")

week <- rbindlist(lapply(Sys.glob("week/TCPSess_300_2016_09_*.csv.gz"), function(x) {
  message("Processing ", x, "...")
  fread(x, col.names=col_names, colClasses=col_classes)
}))

week[, ts := anytime::anytime(ts)]
setkey(week, ts, commune, port)

write.fst(week, "week/week.fst")

week.hourly <- week[, ts := lubridate::floor_date(ts, unit="hours")][, .(
  uplink = sum(uplink),
  downlink = sum(downlink)
), by = .(ts, commune, port)]

write.fst(week.hourly, "week/week.hourly.fst")
