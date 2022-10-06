library(dplyr, warn.conflicts = FALSE)
library(raster)
library(rgeos)

load("data/france.RData")

shp <- file.path("data", "qp-politiquedelaville-shp", "QP_METROPOLE_LB93.shp")

france.qp <- shapefile(shp) %>% gBuffer(byid=TRUE, width=0)
france.qp
plot(france.qp)

france.qp$area.qp <- area(france.qp)
france.bs$area.bs <- area(france.bs)

path <- "data/france.bs.qp.RData"

if (file.exists(path)) load(path) else {
  library(furrr)
  plan(multisession, workers=4)
  
  intersects <- gIntersects(france.bs, france.qp, byid=TRUE, returnDense=FALSE)
  
  n <- 1000
  nr <- nrow(france.bs)
  france.bs.s <- split(france.bs, rep(1:ceiling(nr/n), each=n, length.out=nr))
  
  france.bs.qp <- bind(future_map(france.bs.s, function(x) {
    yids <- unique(unlist(intersects[as.numeric(row.names(x))]))
    intersect(x, france.qp[yids,])
  }, .progress = TRUE))
  
  france.bs.qp$area <- area(france.bs.qp)
  
  save(france.bs.qp, file=path)
  rm(intersects, france.bs.s)
}

bs.qp <- france.bs.qp@data %>%
  mutate(weight = area / area.bs) %>%
  dplyr::select(CODE_QP, weight, lon, lat) %>%
  left_join(
    filter(readr::read_csv("data/TCPSess_300_2017_nidt_xy_lonlat.csv"), hasdata)) %>%
  dplyr::select(bs_id, CODE_QP, weight)

save(bs.qp, file="data/bs.qp.RData")
bs.qp

################################################################################

rm(list=ls())
invisible(gc())

load("data/bs.qp.RData")
load("data/france.RData")
shp <- file.path("data", "qp-politiquedelaville-shp", "QP_METROPOLE_LB93.shp")
france.qp <- shapefile(shp) %>% gBuffer(byid=TRUE, width=0)

target.qp.2017 <- "data/REVN_2017_V4.xls" %>%
  readxl::read_excel(skip=5, progress=FALSE) %>%
  slice(-1) %>%
  dplyr::select(CODGEO, DISP_Q2) %>%
  na.omit() %>%
  pull(CODGEO)

target.qp.2019 <- "data/REVN_2019_V1.xls" %>%
  readxl::read_excel(skip=5, progress=FALSE) %>%
  slice(-1) %>%
  dplyr::select(CODGEO, DISP_Q2) %>%
  na.omit() %>%
  pull(CODGEO)

target.qp <- intersect(target.qp.2017, target.qp.2019)
france.qp.f <- subset(france.qp, CODE_QP %in% target.qp)
bs.qp.f <- subset(bs.qp, CODE_QP %in% target.qp)
france.bs.f <- subset(france.bs, bs_id %in% bs.qp.f$bs_id)

library(data.table)

cat.byid <- "data/TCPSess_300_2017_service_categ2.csv" %>%
  readr::read_csv(col_names=c("id", "name", "x", "y"), quote="") %>%
  dplyr::select(id, name) %>%
  distinct() %>%
  bind_rows(list(id="other", name="Other")) %>%
  arrange(id) %>%
  pull(name)

name <- "TCPSess_300_2017_0"
path <- paste0("data/", name, ".hourly.qp.fst")

if (file.exists(path)) DT <- fst::read.fst(path, as.data.table=TRUE) else {
  library(furrr)
  plan(multisession, workers=4)
  
  files <- Sys.glob(file.path("data", name, "*.hourly.fst"))
  
  DT <- rbindlist(future_map(files, function(x) {
    # load and set category
    DT <- fst::read.fst(x, as.data.table=TRUE)[
      , category := strsplit(basename(x), "\\.")[[1]][1]]
    
    # rename and filter BSs
    setnames(DT, "bs", "bs_id")
    DT <- DT[bs_id %in% bs.qp.f$bs_id,]
    
    # interpolate traffic into qp zones
    merge(DT, bs.qp.f, by="bs_id", allow.cartesian=TRUE)[
      , `:=`(uplink=weight*uplink, downlink=weight*downlink, weight=NULL)][
        , .(uplink=sum(uplink), downlink=sum(downlink)), 
        by=.(ts, CODE_QP, category)]
  }, .progress = TRUE))
  
  DT[, category := factor(category)]
  setattr(DT$category, "levels", cat.byid)
  setkey(DT, ts)
  
  fst::write.fst(DT, path)
}

################################################################################

rm(list=ls())
invisible(gc())

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(data.table)

name <- "TCPSess_300_2017_0"
path <- paste0("data/", name, ".hourly.qp.fst")
DT <- fst::read.fst(path, as.data.table=TRUE)

target.qp.2017 <- "data/REVN_2017_V4.xls" %>%
  readxl::read_excel(skip=5, progress=FALSE) %>%
  slice(-1) %>%
  dplyr::select(CODGEO, DISP_Q2) %>%
  rename(DISP_Q2_2017 = DISP_Q2)

target.qp.2019 <- "data/REVN_2019_V1.xls" %>%
  readxl::read_excel(skip=5, progress=FALSE) %>%
  slice(-1) %>%
  dplyr::select(CODGEO, DISP_Q2) %>%
  rename(DISP_Q2_2019 = DISP_Q2)

inc.pop <- "data/DEMO_2017_V7.xls" %>%
  readxl::read_excel(skip=5, progress=FALSE) %>%
  slice(-1) %>%
  left_join(target.qp.2017) %>%
  left_join(target.qp.2019) %>%
  mutate(DISP_MED14 = DISP_Q2_2019 - DISP_Q2_2017) %>%
  rename(CODE_QP = CODGEO, P15_POP = POP_MUN) %>%
  dplyr::select(CODE_QP, DISP_MED14, P15_POP) %>%
  na.omit()

DT <- subset(DT, CODE_QP %in% inc.pop$CODE_QP)
inc.pop <- subset(inc.pop, CODE_QP %in% DT$CODE_QP)

DT <- merge(DT, inc.pop, by="CODE_QP")[, `:=`(
  uplink=uplink/P15_POP, downlink=downlink/P15_POP, P15_POP=NULL)]

is.home.hour <- function(ts, start=18, end=7) {
  hday <- as.Date(c("2017-05-25", "2017-06-05")) # French holidays
  wday <- lubridate::wday(ts, label=TRUE, week_start=1)
  hour <- lubridate::hour(ts)
  
  in.interval <- if (start <= end)
    (hour >= start) & (hour <= end)
  else (hour >= start) | (hour <= end)
  
  in.interval & (wday %in% levels(wday)[1:5]) & !(as.Date(ts) %in% hday)
}

DT <- subset(DT, is.home.hour(ts, 20)) # takes too much time

DT <- DT[, .(uplink=sum(uplink), downlink=sum(downlink)),
         by = .(date=as.Date(ts), CODE_QP, category, DISP_MED14)]

path <- paste0("data/", name, ".homeagg.qp.fst")
fst::write.fst(DT, path)

################################################################################

library(dplyr, warn.conflicts = FALSE)
library(data.table)
library(ggplot2)
theme_set(theme_classic())

name <- "TCPSess_300_2017_0"
path <- paste0("data/", name, ".homeagg.qp.fst")
DT <- fst::read.fst(path, as.data.table=TRUE)

# aggregates for each zone and category
DT <- DT[, .(uplink=median(uplink), downlink=median(downlink)),
         by=.(CODE_QP, category, DISP_MED14)]

# T_i, T_j, RCA
DT[, `:=`(uplink_zon=sum(uplink), downlink_zon=sum(downlink)), by=.(CODE_QP)]
DT[, `:=`(uplink_cat=sum(uplink), downlink_cat=sum(downlink)), by=.(category)]
DT[, `:=`(
  uplink = (uplink / uplink_zon) / (uplink_cat / sum(uplink)),
  downlink = (downlink / downlink_zon) / (downlink_cat / sum(downlink))
)]
DT[, `:=`(uplink_cat=NULL, downlink_cat=NULL)]
DT[, `:=`(uplink=NULL, uplink_zon=NULL)]
DT <- dcast(DT, ... ~ category, value.var="downlink", fill=0)

names(DT) <- names(DT) %>%
  sub("King", "Candy Crush (game)", .) %>%
  sub("Supercell", "Clash of Clans (game)", .) %>%
  sub("What's App", "WhatsApp", .) %>%
  sub("SnapChat", "Snapchat", .)

handpicked <- c("Pokemon Go",
                "No info", "Other", "Others",
                "Ads",
                "Updates",
                "Encrypted web", "Generic web")

sel <- c(names(DT)[c(1:3)], handpicked)

ff <- paste0("DISP_MED14 ~ `", paste(names(DT[,-..sel]), collapse="`+`"), "`")
fit <- ccv.glmnet(ff, DT, DT$downlink_zon)

perf(fit, size=2)
std_coef(fit)
