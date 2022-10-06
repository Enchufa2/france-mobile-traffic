library(tidyverse)
library(maptools)
library(leaflet)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

france <- rgdal::readOGR("CONTOURS-IRIS_2-1__SHP__FRA_2017-06-30/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2016/CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2016/CONTOURS-IRIS.shp")

DEC <- readxl::read_excel("BASE_TD_FILO_DEC_IRIS_2014.xls", skip = 5)
DISP <- readxl::read_excel("BASE_TD_FILO_DISP_IRIS_2014.xls", skip = 5)
income <- left_join(DEC, DISP) %>%
  select(1:4, DEC_MED14, DISP_MED14) %>%
  # same names as in CONTOURS
  rename(CODE_IRIS=IRIS, NOM_IRIS=LIBIRIS, INSEE_COM=COM, NOM_COM=LIBCOM)

# count bytes
#dbdir <- "TCPSess_300_2016_09_monetdb"
#week.db <- tbl(MonetDBLite::src_monetdblite(dbdir), "week_hourly")
#week.hourly <- collect(week.db)
week.hourly <- fst::read.fst("week/week.hourly.fst") %>%
  mutate(has_income = commune %in% unique(income$INSEE_COM))

# communes covered by our data
france.filt.traffic <- france[france@data$INSEE_COM %in% unique(week.hourly$commune),]
france.filt.traffic <- rgeos::gBuffer(france.filt.traffic, byid=TRUE, width=0)
france.filt.traffic.u <- unionSpatialPolygons(france.filt.traffic, france.filt.traffic@data$INSEE_COM)

plot(france.filt.traffic.u, col="black", border=NA)
sum(unique(france@data$INSEE_COM) %in% unique(week.hourly$commune)) / length(unique(france@data$INSEE_COM))

# communes covered by income
france.filt.income <- france[france@data$INSEE_COM %in% income$INSEE_COM,]
france.filt.income <- rgeos::gBuffer(france.filt.income, byid=TRUE, width=0)
france.filt.income.u <- unionSpatialPolygons(france.filt.income, france.filt.income@data$INSEE_COM)

plot(france.filt.income.u, col="black", border=NA)
sum(unique(france@data$INSEE_COM) %in% income$INSEE_COM) / length(unique(france@data$INSEE_COM))

leaflet(sp::spTransform(france.filt.income.u, sp::CRS("+init=epsg:4326"))) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addPolygons()

# percentage of communes in our data with no income
sum(!(unique(week.hourly$commune) %in% unique(income$INSEE_COM))) / length(unique(week.hourly$commune))
# percentage of communes with income with no traffic data
sum(!(unique(income$INSEE_COM) %in% unique(week.hourly$commune))) / length(unique(income$INSEE_COM))

week.hourly %>%
  group_by(has_income) %>%
  summarise(uplink = sum(uplink), downlink = sum(downlink))

# variability per insee
ggplot(income) + aes(fct_reorder(INSEE_COM, DEC_MED14), DEC_MED14) + geom_boxplot() + coord_flip()
plot(ecdf(count(income, INSEE_COM)$n))

################################################################################

week.hourly
