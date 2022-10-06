library(tidyverse)
library(leaflet)
library(fst)

DEC <- readxl::read_excel("data/BASE_TD_FILO_DEC_IRIS_2014.xls", skip = 5)
DISP <- readxl::read_excel("data/BASE_TD_FILO_DISP_IRIS_2014.xls", skip = 5)
income <- left_join(DEC, DISP) %>%
  dplyr::select(1:4, DEC_MED14, DISP_MED14) %>%
  rename(CODE_IRIS=IRIS, NOM_IRIS=LIBIRIS, INSEE_COM=COM, NOM_COM=LIBCOM) %>%
  na.omit()

source("load-france.R")
france.iris$area <- area(france.iris)
france.bs$area <- area(france.bs)
france.iris.f <- subset(france.iris, CODE_IRIS %in% income$CODE_IRIS)
france.bs.f <- france.bs %>%
  subset(as.logical(colSums(gIntersects(., france.iris.f, byid=TRUE))))

lcrs <- function(x) spTransform(x, crs("+init=epsg:4326"))
leaflet() %>%
  addPolygons(data=lcrs(france), weight=1, fill=FALSE, color="black") %>%
  addPolygons(data=lcrs(france.bs.f), weight=1) %>%
  addPolygons(data=lcrs(france.iris.f), weight=1, fill=FALSE, color="red")

path <- "data/france.bs.iris.RData"
if (file.exists(path)) load(path) else {
  france.bs.iris <- intersect(france.bs.f, france.iris.f)
  france.bs.iris$area <- area(france.bs.iris)
  save(france.bs.iris, file=path)
}

leaflet() %>%
  addPolygons(data=lcrs(france.bs.iris), weight=1, fill=FALSE)

bs.iris <- france.bs.iris@data %>%
  mutate(ratio = area / area.1) %>%
  dplyr::select(bs_id, ratio, CODE_IRIS, TYP_IRIS) %>%
  left_join(dplyr::select(income, CODE_IRIS, DISP_MED14))

head(bs.iris)

name <- "TCPSess_300_2017_0"
path <- paste0("data/", name, ".hourly.iris.fst")
if (file.exists(path)) df <- read.fst(path, as.data.table=TRUE) else {
  source("load-traffic.R")
  setkey(df, bs_id)
  df <- df[france.bs.f$bs_id]; gc()
  setkey(df, ts); df # any NAs?
  
  df <- split(df, by="category")
  for (i in names(df)) df[[i]] <- 
    merge(df[[i]], bs.iris, by="bs_id", allow.cartesian=TRUE)[
      , `:=`(uplink=ratio*uplink, downlink=ratio*downlink, ratio=NULL)][
      , .(uplink=sum(uplink), downlink=sum(downlink)), 
      by=.(ts, CODE_IRIS, TYP_IRIS, category)]
  df <- rbindlist(df); gc()
  
  write.fst(df, path)
}

################################################################################

library(data.table)




df[, `:=`(
  uplink = NULL,
  hour = lubridate::hour(ts),
  date = as.Date(ts),
  ts = NULL
)]
df.feat <- dcast(df, ... ~ hour, value.var="downlink", fill=0)

df.feat[, `:=`(
  CODE_IRIS = factor(CODE_IRIS),
  category = factor(category),
  wday = lubridate::wday(date, label=TRUE, week_start=1),
  hday = date %in% anytime::anydate(c("2017-05-25", "2017-06-05"))
)]

clust <- Hmisc::varclus(as.matrix(df.feat[,4:27]))

df.feat.wday <- df.feat[hday==FALSE & (wday %in% levels(wday)[1:5])]
df.feat.wend <- df.feat[hday==FALSE & (wday %in% levels(wday)[6:7])]

clust.wday <- Hmisc::varclus(as.matrix(df.feat.wday[,4:27]))
clust.wend <- Hmisc::varclus(as.matrix(df.feat.wend[,4:27]))

df.feat.agg <- df.feat[hday==FALSE, .(
  
  CODE_IRIS = factor(CODE_IRIS),
  category = factor(category),
  wday = lubridate::wday(date, label=TRUE, week_start=1),
  hday = date %in% anytime::anydate(c("2017-05-25", "2017-06-05"))
)]
