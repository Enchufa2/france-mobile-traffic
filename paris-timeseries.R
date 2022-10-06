setwd("/zompopo/iucar")
suppressWarnings(suppressMessages(source("init.R")))

library(data.table)
library(mvtsplot)

dbdir <- "TCPSessCourtParisLyon_300_2016_monetdb"
paris_db <- tbl(MonetDBLite::src_monetdblite(dbdir), "paris")
paris_ts <- paris_db %>%
  group_by(bs_id, ts) %>%
  summarise(bytes = sum(uplink + downlink, na.rm=TRUE)) %>%
  ungroup() %>%
  collect()

ggplot(paris_ts) + aes(bs_id, bytes) + geom_boxplot() + coord_flip()

x <- as.data.table(paris_ts) %>%
  dcast(ts ~ bs_id, value.var="bytes", fill=0)

ord <- order(apply(x, 2, median, na.rm = TRUE))
x2 <- as.matrix(x)[, ord]
colm <- apply(x2, 2, function(x) grDevices::boxplot.stats(x)$stats)
colm <- colm[, ord]

plot(colm[3,], log="y")


mvtsplot(as.matrix(x[,-1]), xtime=anytime::anytime(x$ts), sort=median)





paris_youtube <- paris_db %>%
  filter(category == "s1") %>%
  select(-category) %>%
  collect() %>%
  filter(ts < as.numeric(anytime::anytime("2016-09-15")))
max_downlink <- paris_db %>%
  summarise(downlink = max(downlink)) %>%
  pull()
vec_ts <- seq(min(paris_youtube$ts), max(paris_youtube$ts), by=300)

ggplot(paris_youtube) + aes(ts, downlink, group=bs_id) + geom_line(alpha=.3)



suma <- paris_db %>%
  group_by(bs_id) %>%
  summarise(
    downlink = mean(downlink, na.rm=TRUE),
    uplink = mean(uplink, na.rm=TRUE)) %>%
  ungroup() %>%
  collect() %>%
  gather("link", "bytes", uplink, downlink)

plot(cumsum(sort(suma$bytes, decreasing=TRUE)), log="y")
abline(h=sum(suma$bytes[suma$bytes > quantile(suma$bytes, 0.9)]))

ggplot(suma) + aes(bytes) + stat_ecdf() + scale_x_log10()

ggplot(suma) + aes(bytes, fill=link) + geom_histogram(position="identity") + scale_x_log10()

suma <- paris_youtube %>%
  count(bs_id, wt=downlink, sort=TRUE)

x <- as.data.table(paris_youtube) %>%
  dcast(ts ~ bs_id, value.var="downlink", fill=0)

mvtsplot(as.matrix(x[,-1]), xtime=anytime::anytime(x$ts), sort=median)
