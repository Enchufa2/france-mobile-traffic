library(DBI)

col_names <- c("ts", "commune", "port", "uplink", "downlink")
col_classes <- c("integer", "character", "integer", "double", "double")

db <- dbConnect(MonetDBLite::MonetDBLite(), dbname="TCPSess_300_2016_09_monetdb")

system.time(for (f in Sys.glob("TCPSess_300_2016_09_*.csv.gz")) {
  message("Processing ", f, "...")
  day <- data.table::fread(paste("zcat", f), col.names=col_names, colClasses=col_classes)
  colnames(day) <- col_names
  day$ts <- anytime::anytime(day$ts)
  #dbWriteTable(db, "temp", day, append=TRUE)
  dbWriteTable(db, "week", day, append=TRUE)
})

#dbCreateTable(db, "week", day)
#system.time(dbSendStatement(db, "insert into week select * from temp order by ts"))
#dbRemoveTable(db, "temp")

# hourly aggregate (~20 min)
system.time(week.hourly <- dbGetQuery(
  db, "select ts, commune, port, uplink, downlink from (select 
         min(ts) as ts, commune, port, 
         sum(uplink) as uplink, sum(downlink) as downlink,
         extract(month from ts) as m, extract(day from ts) as d, extract(hour from ts) as h
       from week
       group by commune, port, m, d, h) as temp"
))
week.hourly <- week.hourly[order(week.hourly$ts), ]
week.hourly$ts <- lubridate::floor_date(week.hourly$ts, unit="hours")
dbWriteTable(db, "week_hourly", week.hourly)

dbDisconnect(db)
rm(day, db)
gc()
