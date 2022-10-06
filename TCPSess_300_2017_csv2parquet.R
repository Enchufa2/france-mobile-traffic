library(sparklyr)
library(dplyr)

name <- "TCPSess_300_2017"
col_names <- c("ts", "bs", "category", "uplink", "downlink")
col_classes <- c("integer", "character", "character", "double", "double")

tmp <- "-Djava.io.tmpdir=/run/media/iucar/82484f7d-dd7d-4eea-89d5-8f7a73e98295/spark"
config <- spark_config()
config$`sparklyr.shell.driver-java-options` <- tmp
config$spark.driver.extraJavaOptions <- tmp
config$spark.executor.extraJavaOptions <- tmp
config$`sparklyr.shell.driver-memory` <- "60G"
config$`sparklyr.shell.executor-memory` <- "4G"

################################################################################

sc <- spark_connect(master="local", config=config)

df <- sdf_bind_rows(lapply(Sys.glob(paste0("data/", name, "*.csv.gz")), function(x) {
  spark_read_csv(sc, name, x, columns=col_names, delimiter=";")
}))

spark_write_parquet(df, paste0("data/", name, "_parquet"))

rm(df); spark_disconnect(sc)

################################################################################

sc <- spark_connect(master="local", config=config)

df <- spark_read_parquet(sc, name, paste0("data/", name, "_parquet"), memory=FALSE)

df %>%
  mutate(ts = from_unixtime(ts)) %>%
  mutate(month = month(ts), day = day(ts), hour = hour(ts)) %>%
  group_by(month, day, hour, bs, category) %>%
  summarise(uplink = sum(uplink, na.rm=TRUE), downlink = sum(downlink, na.rm=TRUE)) %>%
  spark_write_parquet(paste0("data/", name, "_hourly_parquet"))
