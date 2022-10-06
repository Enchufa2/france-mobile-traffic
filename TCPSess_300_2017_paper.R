library(dplyr, warn.conflicts = FALSE)
library(data.table)
library(ggplot2)
theme_set(theme_classic())

name <- "TCPSess_300_2017_0"
path <- paste0("data/", name, ".homeagg.iris.fst")
#path <- paste0("data/", name, ".homeagg-18.iris.fst")
#path <- paste0("data/", name, ".homeagg-wend.iris.fst")
DT <- fst::read.fst(path, as.data.table=TRUE)

# aggregates for each zone and category
DT <- DT[, .(uplink=median(uplink), downlink=median(downlink)),
         by=.(CODE_IRIS, category, DISP_MED14)]

# T_i, T_j, RCA
DT[, `:=`(uplink_zon=sum(uplink), downlink_zon=sum(downlink)), by=.(CODE_IRIS)]
DT[, `:=`(uplink_cat=sum(uplink), downlink_cat=sum(downlink)), by=.(category)]
DT[, `:=`(
  uplink = (uplink / uplink_zon) / (uplink_cat / sum(uplink)),
  downlink = (downlink / downlink_zon) / (downlink_cat / sum(downlink))
)]
DT <- DT[, c("CODE_IRIS", "category", "DISP_MED14", "downlink", "downlink_zon")]
DT <- dcast(DT, ... ~ category, value.var="downlink", fill=0)

inc <- 
  readxl::read_excel("data/BASE_TD_FILO_DISP_IRIS_2014.xls", skip=5, progress=FALSE) %>%
  dplyr::select(IRIS, DISP_D114, DISP_D214, DISP_D314, DISP_D414, DISP_D614, 
                DISP_D714, DISP_D814, DISP_D914) %>%
  na.omit()

DT <- merge(DT, inc, by.x="CODE_IRIS", by.y="IRIS")
DT <- DT[, c(1, 3, 44:47, 2, 48:51, 4:43)]

names(DT) <- names(DT) %>%
  sub("King", "Candy Crush (game)", .) %>%
  sub("Supercell", "Clash of Clans (game)", .) %>%
  sub("What's App", "WhatsApp", .) %>%
  sub("SnapChat", "Snapchat", .) %>%
  sub("Gameloft", "Gameloft (gaming)", .) %>%
  sub("Pokemon Go", "Pokemon Go (game)", .)

POP <- readxl::read_excel("data/base-ic-evol-struct-pop-2015.xls", skip=5, progress=FALSE)
POP <- POP[, c(1, 13, 15:23, 82, 56, 61, 53, 32, 30)]
# 13 -> P15_POP total population
# 14:23 -> P15_POPxxyy tramos
# 82 -> P15_POP_IMM immigrants
# 56 -> C15_POP15P_CS3 estudios superiores
# 61 -> C15_POP15P_CS8 desempleados
# 53 -> C15_POP15P mayores de 15
# 32 -> P15_POP65P menores de 65
# 30 -> P15_POP0019 menores de 20

# Variables divididas por población total
POP[, 3:(ncol(POP)-5)] <- POP[, 3:(ncol(POP)-5)] / POP[[2]]
# Desempleados divididos por gente entre 16 y 64 años
POP[, ncol(POP)-3] <- POP[, ncol(POP)-3] / (POP[[ncol(POP)-2]]-POP[[ncol(POP)-1]])
# Estudios superiores dividos por gente mayor de 19 años
POP[, ncol(POP)-4] <- POP[, ncol(POP)-4] / (POP[[2]]-POP[[ncol(POP)]])

DT <- merge(DT, POP[,c(1:2, 4:(ncol(POP)-3))], by.x="CODE_IRIS", by.y="IRIS")

handpicked <- c("Pokemon Go (game)",
                "No info", "Other", "Others",
                "Ads",
                "Updates",
                "Encrypted web", "Generic web")

sel <- c(names(DT)[c(1:11, 52, 62:63)], handpicked)
pop <- names(DT)[53:60]
imm <- names(DT)[61]

# checks
mc <- mctest::mctest(lm(DT$DISP_MED14 ~ as.matrix(DT[,c(12:31, 33:51)])), "i")
mean(mc$idiags[,1])   # 27
median(mc$idiags[,1]) # 4
mc <- mctest::mctest(lm(DT$DISP_MED14 ~ as.matrix(DT[,-c(..sel, ..pop, ..imm)])), "i")
mean(mc$idiags[,1])   # 2
median(mc$idiags[,1]) # 2

load("data/france.RData")
rm(france.bs, france.insee)
france.iris <- subset(france.iris, CODE_IRIS %in% DT$CODE_IRIS)
france.iris <- france.iris[order(france.iris$CODE_IRIS),]

# add urbanization code 1=urban, 2=periurban, 3=rural
sf.france.dgurba <- sf::st_read("data/DGURBA-2018-01M-SH/DGURBA_2018_01M.shp") %>%
  subset(grepl("^FR_", GISCO_ID)) %>%
  sf::st_transform(sf::st_crs(france.iris))
france.iris.dgurba <- sf::st_as_sf(france.iris) %>%
  sf::st_intersection(sf.france.dgurba) %>%
  as.data.table() %>%
  subset(TRUE, c("CODE_IRIS", "DGURBA", "SHAPE_AREA")) %>%
  dplyr::mutate_at(vars(SHAPE_AREA), as.numeric) %>%
  dplyr::group_by(CODE_IRIS, DGURBA) %>%
  dplyr::summarise(SHAPE_AREA = sum(SHAPE_AREA)) %>%
  dplyr::group_by(CODE_IRIS) %>%
  dplyr::summarise(DGURBA = DGURBA[which.max(SHAPE_AREA)]) %>%
  dplyr::add_row(CODE_IRIS="763510102", DGURBA=1L) %>% # missing
  dplyr::arrange(CODE_IRIS) %>%
  as.data.table()
  
all(DT$CODE_IRIS == france.iris.dgurba$CODE_IRIS)
DT <- merge(DT, france.iris.dgurba)

################################################################################

library(ggfortify)

path <- paste0("data/", name, ".hourly.iris.fst")
DT.hourly <- fst::read.fst(path, as.data.table=TRUE)
DT.hourly[, hour := lubridate::hour(ts)]

x <- DT.hourly[, lapply(.SD, sum), by=.(hour, category), .SDcols="downlink"]
x <- dcast(x, hour ~ category, value.var="downlink")
autoplot(prcomp(x[, -1]), data=x, colour="hour", label=TRUE, shape=FALSE) +
  geom_vline(xintercept=0, linetype=2) + geom_hline(yintercept=0, linetype=2)
ggsave("img/fig_hour_pca.pdf", width=9, height=6.7, scale=0.7, device=cairo_pdf)

################################################################################

is.wday <- function(ts) {
  hday <- as.Date(c("2017-05-25", "2017-06-05")) # French holidays
  wday <- lubridate::wday(ts, label=TRUE, week_start=1)
  
  (wday %in% levels(wday)[1:5]) & !(as.Date(ts) %in% hday)
}

DT.paris <- DT[grepl("^7510", CODE_IRIS)]
rich <- DT.paris[DISP_MED14 > quantile(DISP_MED14, 0.95)]$CODE_IRIS
poor <- DT.paris[DISP_MED14 < quantile(DISP_MED14, 0.05)]$CODE_IRIS

path <- paste0("data/", name, ".hourly.iris.fst")
DT.hourly <- fst::read.fst(path, as.data.table=TRUE)
DT.hourly <- DT.hourly[category %in% c("News", "Facebook")]
DT.hourly.paris <- DT.hourly[grepl("^7510", CODE_IRIS)]
DT.hourly.paris <- DT.hourly.paris[is.wday(ts)]
DT.hourly.paris <- DT.hourly.paris[CODE_IRIS %in% c(rich, poor)]
DT.hourly.paris[, area := "5% richest"][CODE_IRIS %in% poor, area := "5% poorest"]
DT.hourly.paris <- merge(DT.hourly.paris, DT[, c("CODE_IRIS", "P15_POP")], by="CODE_IRIS")
DT.hourly.paris <- DT.hourly.paris[
  , .(downlink = median(downlink/P15_POP)), by=.(ts, category, area)]
DT.hourly.paris[, day := as.Date(ts)]
DT.hourly.paris[, hour := hms::as_hms(ts)]
DT.hourly.paris <- rbindlist(list(
  DT.hourly.paris,
  DT.hourly.paris[hour < 14*3600][, hour := hms::as_hms(hour + 24*3600)]))
#DT.hourly.paris[hour < 7*3600, hour := hms::as_hms(hour + 24*3600)]

#ggplot(DT.hourly.paris[hour < (24+7)*3600]) +
ggplot(DT.hourly.paris) +
  aes(hour, downlink) +
  facet_grid(category~area, scales="free_y") +
  geom_line(aes(group=interaction(area, day)), color="lightgray") +
  geom_smooth(method="gam") +
  scale_x_time(breaks=seq(0, 24, 5)*3600, labels=seq(0, 24, 5))

colnf <- c("#D6604D", "#4393C3")

ggplot(dcast(DT.hourly.paris, ts+area+day+hour~category, value.var="downlink")) +
  aes(hour, linetype=area) +
  geom_smooth(aes(y=Facebook/1e3, color="Facebook"), method="gam", se=FALSE) +
  geom_smooth(aes(y=News*5/1e3, color="News"), method="gam", se=FALSE) +
  annotate("rect", xmin=7*3600, xmax=20*3600, ymin=-Inf, ymax=Inf, fill="white", alpha=0.6) +
  geom_vline(xintercept=20*3600) +
  labs(x="Time [h]", y="Downlink traffic\n[kB/inhabitant]", color=NULL) +
  scale_x_time(breaks=seq(0+10, 24+10, 5)*3600, labels=seq(0, 24, 5)[c(3:5, 1:2)]) +
  coord_cartesian(xlim=c(7, 31)*3600, expand=FALSE) +
  scale_y_continuous(sec.axis=sec_axis(~./5)) +
  scale_color_manual(values=colnf) + guides(linetype=FALSE) +
  theme(legend.position=c(.99, .99), legend.justification=c(1, 1)) +
  theme(axis.line.y.left=element_line(color=colnf[1], size=1)) +
  theme(axis.ticks.y.left=element_line(color=colnf[1])) +
  theme(axis.line.y.right=element_line(color=colnf[2], size=1)) +
  theme(axis.ticks.y.right=element_line(color=colnf[2]))

ggsave("img/fig1.svg", width=9, height=3.75, scale=0.65)

ggplot(dcast(DT.hourly.paris, ts+area+day+hour~category, value.var="downlink")) +
  aes(hour) +
  facet_wrap("area", ncol=1) +
  geom_smooth(aes(y=Facebook/1e3, color="Facebook"), method="gam", se=FALSE) +
  geom_smooth(aes(y=News*5/1e3, color="News"), method="gam", se=FALSE) +
  labs(x="Time [h]", y="Downlink traffic [kB/inhabitant]", color=NULL) +
  annotate("rect", xmin=7*3600, xmax=20*3600, ymin=-Inf, ymax=Inf, fill="white", alpha=0.6) +
  geom_vline(xintercept=20*3600) +
  scale_x_time(breaks=seq(0+10, 24+10, 5)*3600, labels=seq(0, 24, 5)[c(3:5, 1:2)]) +
  coord_cartesian(xlim=c(7, 31)*3600, expand=FALSE) +
  scale_y_continuous(sec.axis=sec_axis(~./5)) +
  scale_color_manual(values=colnf) + guides(linetype=FALSE) +
  theme(legend.position=c(.99, .99), legend.justification=c(1, 1)) +
  theme(axis.line.y.left=element_line(color=colnf[1], size=1)) +
  theme(axis.ticks.y.left=element_line(color=colnf[1])) +
  theme(axis.line.y.right=element_line(color=colnf[2], size=1)) +
  theme(axis.ticks.y.right=element_line(color=colnf[2]))

ggsave("img/fig1_alt.svg", width=12, height=9, scale=0.65)

################################################################################

path <- paste0("data/", name, ".homeagg.iris.fst")
DT2 <- fst::read.fst(path, as.data.table=TRUE)
DT2 <- DT2[, .(uplink=median(uplink), downlink=median(downlink)),
           by=.(CODE_IRIS, category, DISP_MED14)]
DT2[, `:=`(uplink=NULL)]
DT2 <- dcast(DT2, ... ~ category, value.var="downlink", fill=0)
names(DT2) <- names(DT2) %>%
  sub("King", "Candy Crush (game)", .) %>%
  sub("Supercell", "Clash of Clans (game)", .) %>%
  sub("What's App", "WhatsApp", .) %>%
  sub("SnapChat", "Snapchat", .) %>%
  sub("Gameloft", "Gameloft (gaming)", .) %>%
  sub("Pokemon Go", "Pokemon Go (game)", .)

corder <- corrplot::corrMatOrder(cor(DT[,12:51]), "FPC")
labeldf <- data.frame(Var1=names(DT[,12:51][,..corder]), Var2=names(DT[,12:51][,..corder]), value=0)
col <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
                          "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                          "#4393C3", "#2166AC", "#053061"))(200)

all.cor <- cor(DT[,12:51][,..corder])
# all.cor.plot <- all.cor
# DT2.cor <- cor(DT2[,3:42][,..corder])
all.p.mat <- ggcorrplot::cor_pmat(DT[,12:51][,..corder])
# DT2.p.mat <- ggcorrplot::cor_pmat(DT2[,3:42][,..corder])
# 
# for (i in 1:40) for (j in i:40) {
#   all.cor[i, j] <- DT2.cor[i, j]
#   all.p.mat[i, j] <- DT2.p.mat[i, j]
# }

ggcorrplot::ggcorrplot(
  all.cor[, 40:1], type="full", pch.cex=2, colors=c("red", "white", "blue"), 
  show.diag=TRUE, p.mat=all.p.mat) + 
  scale_fill_gradientn(colours=col, limit=c(-1, 1)) + labs(fill=NULL) +
  coord_fixed(xlim=c(1, 44)) + theme(
    axis.text.x=element_blank(), panel.grid=element_blank(),
    legend.position=c(1, 1), legend.justification=c(1, .985),
    legend.key.height=unit(.1946, "npc"))

ggsave("img/fig_cor.pdf", width=9, height=6.7, scale=1.3, device=cairo_pdf)
ggsave("img/fig_cor.png", width=9, height=6.7, scale=1.3)

all.cor.plot[all.p.mat > 0.05] <- 0
all.cor.plot[t(all.p.mat) > 0.05] <- 0
g <- igraph::graph_from_adjacency_matrix(all.cor.plot, "undirected", weighted=TRUE, diag=FALSE)
gst <- igraph::mst(g, weights=1 - igraph::E(g)$weight^2)
set.seed(1234)
gly <- igraph::layout_with_lgl(gst, maxiter=1000)

ggraph::ggraph(g, pr$x[, 1:2]) +
  ggraph::geom_edge_link(aes(color=weight, alpha=abs(weight), edge_width=5*abs(weight))) + 
  ggraph::geom_node_point() +
  ggraph::geom_node_label(aes(label=name)) +
  ggraph::scale_edge_color_gradientn(colours=col, limit=c(-1, 1), guide=ggraph::guide_edge_colourbar())

network_plot(all.cor.plot, 0, curved=FALSE, colors=col) +
  theme(legend.position=c(0.08, 0.14), legend.justification=c(0, 0))
ggsave("img/fig_net.pdf", width=9, height=7, scale=1, device=cairo_pdf)
ggsave("img/fig_net.png", width=9, height=7, scale=1)

################################################################################

library(furrr)
plan(multisession, workers=12)

params <- future_pmap(DT[,3:11], function(...) {
  dec <- seq(0.1, 0.9, 0.1)
  suppressMessages(suppressWarnings(
    rriskDistributions::get.lnorm.par(dec, c(...), show.output=FALSE, plot=FALSE)))
})

nrmsdiqr <- function(x, y)
  sqrt(mean((qlnorm(seq(0.1, 0.9, 0.1), x[1], x[2]) - y)^2)) / diff(qlnorm(c(0.25, 0.75), x[1], x[2]))
x <- sapply(seq_len(nrow(DT)), function(i) nrmsdiqr(params[[i]], as.numeric(DT[i, 3:11])))
summary(x) # ~ RMSE is ~4% of IQR in average
hist(x)

mae <- function(x, y) mean(abs(qlnorm(seq(0.1, 0.9, 0.1), x[1], x[2]) - y))
x <- sapply(seq_len(nrow(DT)), function(i) mae(params[[i]], as.numeric(DT[i, 3:11])))
summary(x)
hist(x)

qq <- do.call(rbind, lapply(seq_len(nrow(DT)), function(i) data.frame(
  theoretical = qlnorm(seq(0.1, 0.9, 0.1), params[[i]][1], params[[i]][2]),
  empirical = as.numeric(DT[i, 3:11]),
  weights = as.numeric(DT[i, 52]), n = i)))

ggplot(qq) + aes(theoretical, empirical, group=n, alpha=weights/max(weights)) +
  geom_point(alpha=0.1, size=.3) + #geom_line() +
  geom_abline(intercept=0, slope=1, color="red") +
  labs(x="Theoretical quantiles", y="Empirical quantiles") +
  theme(legend.position=c(1, 0), legend.justification=c(1, 0))

ggsave("img/fig8.pdf", width=9, height=6, scale=0.7, device=cairo_pdf)

DT$DISP_var <- purrr::map_dbl(params, ~ (exp(.x[2]^2) - 1) * exp(2*.x[1] + .x[2]^2))
DT$DISP_gini <- purrr::map_dbl(params, ~ 2 * pnorm(.x[2]/2 * sqrt(2)) - 1) # erf(s/2)
DT$DISP_theil <- purrr::map_dbl(params, ~ .x[2]^2/2)
nona <- !is.na(DT$DISP_var)

cor(DT[, c("DISP_MED14", "DISP_var", "DISP_gini", "DISP_theil")][nona,])
sel <- c(sel, c("DISP_var", "DISP_gini", "DISP_theil"))

################################################################################

vars <- list(
  Population = c(pop, imm),
  Traffic = setdiff(names(DT), c(sel, pop, imm, "DGURBA")),
  All = setdiff(names(DT), c(sel, "DGURBA")))
conf <- list(
  `Median income` = list(y="DISP_MED14", family=Gamma("log"), weights="NULL"),
  `Higher education ratio` = list(y="C15_POP15P_CS3", family=quasibinomial, weights="P15_POP"),
  `Local inequality` = list(y="DISP_gini", family="Beta", weights="NULL"))

colscale <- function(data, cols) {
  x <- unlist(data[, cols, with=FALSE])
  scale <- sqrt(sum(x^2)/(length(x)-1))
  data[, cols] <- data[, cols, with=FALSE] / scale
  data
}

filt <- nona & with(DT, downlink_zon > quantile(downlink_zon, 0.05)) #& DT$DGURBA==1
DT.filt <- subset(DT, filt)
DT.filt <- colscale(DT.filt, vars$Traffic)
DT.filt <- colscale(DT.filt, vars$Population)
france.iris.filt <- subset(france.iris, filt)
nb <- spdep::poly2nb(france.iris.filt)
lw <- spdep::nb2listw(nb, zero.policy=TRUE)

set.seed(42)

#ff <- paste0(y, " ~ CITY + `", paste(x, collapse="`+`"), "`")
#ccv.glmnet(ff, cbind(DT, CITY=substr(DT$CODE_IRIS, 1, 3)), DT$downlink_zon)

# first pass
fit <- lapply(vars, function(x) lapply(conf, function(param) {
  ff <- paste0(param$y, " ~ `", paste(x, collapse="`+`"), "`")
  ff <- as.formula(ff)
  weights <- DT.filt[[param$weights]]
  
  if (!identical(param$family, "Beta"))
    glm(ff, family=param$family, data=DT.filt, weights=weights, maxit=100)
  else betareg::betareg(ff, data=DT.filt)
}))

# second pass with spatial filtering
fit[["All+SF"]] <- mapply(function(model, param) {
  DT.filt$err <- spdep::lag.listw(lw, resid(model), zero.policy=TRUE)
  
  ff <- paste0(param$y, " ~ `", paste(vars$All, collapse="`+`"), "`")
  ff <- as.formula(paste(ff, "+ err"))
  weights <- DT.filt[[param$weights]]
  
  if (!identical(param$family, "Beta"))
    glm(ff, family=param$family, data=DT.filt, weights=weights, maxit=100)
  else betareg::betareg(ff, data=DT.filt)
}, fit$All, conf, SIMPLIFY=FALSE)

# spatial autocorrelation checks
sapply(fit, function(i) sapply(i, function(j) {
  spdep::moran.mc(resid(j), lw, 999, zero.policy=TRUE)$p.value
}))
sapply(fit, function(i) sapply(i, function(j) {
  unname(round(spdep::moran.test(resid(j), lw, 999, zero.policy=TRUE)$estimate[1], 2))
}))
sapply(fit[[1]], function(i) {
  unname(round(spdep::moran.test(i$y, lw, 999, zero.policy=TRUE)$estimate[1], 2))
})

# diagnostics
rstandard.betareg <- function(model, ...) residuals(model, type="pearson")

diag <- lapply(setNames(nm=names(conf)), function(var) {
  diag <- lapply(fit, function(m) performance::check_model(m[[var]]))
  
  alpha <- 0.2
  color <- "blue"
  
  hg <- bind_rows(lapply(diag, "[[", "HOMOGENEITY"), .id="model") %>%
    mutate(model = factor(model, levels=names(fit))) %>%
    ggplot() + aes(x, y) + facet_grid("model") +
    geom_point(alpha=alpha) + geom_smooth(method="loess", color=color) +
    labs(x="Fitted values", y=expression(sqrt("|Std. residuals|"))) +
    theme(strip.background=element_blank(), strip.text=element_blank())
  
  qq <- bind_rows(lapply(diag, "[[", "QQ"), .id="model") %>%
    mutate(model = factor(model, levels=names(fit))) %>%
    ggplot() + aes(sample=y) + facet_grid("model") +
    geom_qq(alpha=alpha) + geom_qq_line(color=color) +
    labs(x="Theoretical Quantiles", y="Sample Quantiles")
  
  patchwork::wrap_plots(hg, qq, ncol=2) + patchwork::plot_annotation(title=var)
})

for (i in seq_along(diag)) {
  #ggsave(paste0("img/fig_diag_", i, ".pdf"), diag[[i]], width=18, height=15, scale=0.7, device=cairo_pdf)
  ggsave(paste0("img/fig_diag_", i, ".png"), diag[[i]], width=9, height=8, scale=0.7)
}

# performance
plot(performance::compare_performance(lapply(fit, "[[", names(conf)[1]), rank=TRUE))
plot(performance::compare_performance(lapply(fit, "[[", names(conf)[2]), rank=TRUE))
plot(performance::compare_performance(lapply(fit, "[[", names(conf)[3]), rank=TRUE))

fit.perf <- do.call(rbind, mapply(function(i, iname) mapply(function(j, jname) {
  r2 <- summary(lm(j$y ~ fitted(j)))$adj.r.squared
  maesd <- mean(abs(resid(j, "response"))) / mean(abs(j$y))
  data.frame(Response=jname, Regressors=iname, r2=r2, maesd=maesd)
}, i, names(i), SIMPLIFY=FALSE), fit, names(fit)))
fit.perf <- tidyr::gather(fit.perf, meas, Value, 3:4) %>%
  mutate_at(vars(Response, Regressors, meas), forcats::fct_inorder)

ggplot(fit.perf) +
  aes(Regressors, Value, color=Response, fill=Response) +
  facet_grid(~meas, labeller=labeller(
    meas=as_labeller(c(r2="pseudo-R[adj]^2", maesd="Std.MAE"), label_parsed))) +
  geom_segment(aes(xend=Regressors, yend=0), size=4, alpha=0.1) +
  geom_line(aes(group=Response)) +
  geom_point(pch=15, size=4, color="white", show.legend=FALSE) +
  geom_point() +
  scale_y_continuous(limits=c(0, 1), sec.axis=sec_axis(~ .)) +
  theme(legend.position=c(.99, .99), legend.justification=c(1, 1)) +
  theme(axis.line.x=element_blank())

p.fit <- last_plot()
#ggsave("img/fig2.pdf", width=9, height=5, scale=0.7, device=cairo_pdf)
#ggsave("img/fig2_wend.png", width=9, height=5, scale=0.7)

################################################################################

lbreaks <- list(
  c(5, 17, 20, 24, 30, 38, 50, 65)*1e3,
  seq(0.1, 0.6, 0.1),
  seq(0.1, 0.6, 0.1))

places <- list(
  #Marseille = c(5.41, 43.31),
  #Lyon = c(4.85, 45.75),
  #Bordeaux = c(-0.59, 44.83),
  #Toulouse = c(1.44, 43.6),
  Paris = c(2.34, 48.86))
view <- c(0.083, 0.047)

places <- lapply(places, function(place) {
  bbox_i <- place + c(-view, view) * 2.25
  
  filt_bbox <- sf::st_bbox(
    setNames(bbox_i, c("xmin", "ymin", "xmax", "ymax")), crs="+init=epsg:4326") %>%
    sf::st_as_sfc() %>% sf::st_transform(sf::st_crs(france.iris.filt))
  
  roads <- filt_bbox %>%
    sf::st_transform(crs="+init=epsg:4326") %>%
    sf::st_bbox() %>%
    osmdata::opq() %>%
    osmdata::add_osm_feature("highway", c("motorway", "trunk", "primary")) %>%
    osmdata::osmdata_sf()
  
  list(bbox=filt_bbox, roads=roads)
})

examples <- mapply(function(model, breaks) {
  df <- cbind(DT.filt[, c("CODE_IRIS")], Observed=model$y, Predicted=fitted(model))
  france.iris.filt@data <- merge(france.iris.filt@data, df, by="CODE_IRIS")
  france.iris.filt.sf <- sf::st_as_sf(france.iris.filt)
  
  lapply(places, function(place) {
    do.call(rbind, lapply(c("Observed", "Predicted"), function(var) {
      france.iris.filt.sf %>%
        sf::st_intersection(place$bbox) %>%
        #oldtmaptools::smooth_map(var, breaks=c(0, breaks)) %>%
        #.$polygons %>%
        mutate(level=cut(.data[[var]], breaks=c(0, breaks), dig.lab=10)) %>%
        mutate(key=var)
    }))
  })
}, fit$`All+SF`, lbreaks, SIMPLIFY=FALSE)

examples.p <- mapply(function(response, scale_option) {
  example.p <- lapply(names(examples[[response]]), function(city) {
    df <- examples[[response]][[city]]
    
    response <- sub("Median income", "Median\nincome [k€]", response, fixed=TRUE)
    response <- sub("Higher education ", "Higher\neducation\n", response, fixed=TRUE)
    response <- sub("Local inequality", "Local\ninequality", response, fixed=TRUE)
    levels(df$level) <- gsub(",000", "", levels(df$level), fixed=TRUE)
    levels(df$level) <- gsub("000", "", levels(df$level), fixed=TRUE)
    levels(df$level) <- gsub("(", "", levels(df$level), fixed=TRUE)
    levels(df$level) <- gsub("]", "", levels(df$level), fixed=TRUE)
    levels(df$level) <- gsub(",", " to ", levels(df$level), fixed=TRUE)
    
    roads <- places[[city]]$roads$osm_lines %>%
      sf::st_transform(sf::st_crs(places[[city]]$bbox)) %>%
      sf::st_intersection(places[[city]]$bbox)
    
    p <- ggplot(df) +
      geom_sf(data=places[[city]]$bbox, fill="#222222") +
      geom_sf(aes(fill=level), color=NA, alpha=1) +
      geom_sf(data=roads, size=.2, alpha=.65, color="white") +
      facet_grid(~key) +
      scale_fill_viridis_d(drop=FALSE, option=scale_option) +
      coord_sf(crs="+init=epsg:4326") +
      #coord_sf(view[c(1, 3)], view[c(2, 4)], crs="+init=epsg:4326") +
      ggspatial::annotation_scale(
        style="ticks", line_col="white", text_col="white",
        line_width=2, text_face="bold",
        pad_x=unit(0.08, "npc"), pad_y=unit(0.08, "npc")) +
      labs(fill=response) +
      theme(axis.text=element_blank(), axis.ticks=element_blank(),
            axis.line=element_blank(), strip.background=element_blank(),
            legend.key.size=unit(15, "pt"))
    if (!grepl("income", response))
      p <- p + theme(strip.text=element_blank())
    p
  })
  patchwork::wrap_plots(example.p[order(names(examples[[1]]))])
}, names(examples), c("D", "E", "A"), SIMPLIFY=FALSE)

(p.map <- patchwork::wrap_plots(examples.p, ncol=1) +
  patchwork::plot_layout(guides="collect") &
  theme(plot.margin=unit(c(0, -1, -3.6, -3.5), "mm")))
p.both <- patchwork::wrap_plots(
  patchwork::wrap_elements(p.fit + theme(plot.margin=unit(c(0, 1, 0, 0), "mm"))), 
  patchwork::wrap_elements(p.map), ncol=1, heights=c(1, 2.5))

ga <- ggplot() + theme_void() +
  annotate("text", 0, 0, label="a", fontface="bold", size=4.8)
gb <- ggplot() + theme_void() +
  annotate("text", 0, 0, label="b", fontface="bold", size=4.8)
ggplot() + theme_void() + xlim(0, 1) + ylim(0, 1) +
  annotation_custom(patchwork::patchworkGrob(p.both)) +
  annotation_custom(ggplotGrob(ga), -Inf, 0.02, 1, 1.04) +
  annotation_custom(ggplotGrob(gb), -Inf, 0.02, 0.66, 0.76)

ggsave("img/fig_perf.pdf", width=9, height=15, scale=0.7, device=cairo_pdf)
ggsave("img/fig_perf.png", width=9, height=14, scale=0.7)

################################################################################

pop_rename <- function(x) {
  x <- sapply(strsplit(x, "POP"), "[", 2)
  x <- ifelse(x=="_IMM", "immigrant", gsub('^(.{2})(.+)$', '\\1-\\2', x))
  paste("Pop. ratio", x)
}

df.fit <- rbindlist(lapply(fit$`All+SF`, sjPlot::get_model_data, "est"), idcol=TRUE)
df.fit[, .id := forcats::fct_inorder(.id)]
df.fit[, term := as.character(term)]
df.fit[, group := "Traffic"]
df.fit[grepl("POP", term), `:=`(group = "Population", term = pop_rename(term))]
df.fit[grepl("err", term), `:=`(group = "SF", term = "Spatial error")]
df.fit[, group := forcats::fct_inorder(group)]
df.fit[, cat := .id]
df.fit[p.value >= 0.05, .id := NA]

stripe_color <- c("#ffffff00", "#0000001a")
stripe_size <- 5.5

lvl <- levels(with(df.fit, reorder(
  term, rep(estimate[grepl("edu", cat)], 3), median, na.rm=TRUE)))
#lvl <- levels(with(df.fit, reorder(term, estimate, median, na.rm=TRUE)))
lvl[grepl("^Pop", lvl)] <- rev(sort(lvl[grepl("^Pop", lvl)]))

ggplot(df.fit) +
  aes(estimate, factor(term, levels=lvl), xmin=conf.low, xmax=conf.high, color=.id) +
  facet_grid("group", scales="free_y", space="free_y") +
  geom_vline(xintercept=1, color="black", linetype="dashed") +
  geom_errorbarh(height=0, size=1) + geom_point(size=2) +
  scale_x_log10(breaks=c(0.2, 0.5, 1, 2, 5)) +
  scale_color_discrete(
    breaks=levels(df.fit$.id),
    na.value=grDevices::adjustcolor("grey50", 0.5)) +
  labs(y=NULL, x="Estimate", color="Response") +
  theme(legend.position=c(.01, .99), legend.justification=c(0, 1)) +
  theme(legend.background=element_rect(fill="#ffffff88")) +
  theme(panel.border=element_rect(fill=NA)) +
  theme(axis.ticks.y=element_line(color=stripe_color, size=stripe_size)) +
  theme(axis.ticks.length.y=unit(4.5, "cm")) +
  theme(axis.text.y=element_text(margin=margin(r=-4.3, unit="cm"))) +
  theme(panel.grid.major.x=element_line(color="white")) +
  theme(panel.grid.major.y=element_line(color=stripe_color, size=stripe_size))

ggsave("img/fig_est.pdf", width=9, height=10.5, scale=0.7, device=cairo_pdf)
ggsave("img/fig_est_wend.png", width=9, height=10.5, scale=0.7)

################################################################################

france.cities <- france.iris %>%
  subset(CODE_IRIS %in% DT$CODE_IRIS) %>%
  `$<-`("clust", factor(substr(.$INSEE_COM, 1, 3)))

# ggplot() + theme_void() + theme(legend.position="none") +
#   geom_sf(data=sf::st_as_sf(france), fill=NA) +
#   geom_sf(aes(fill=factor(clust, levels=sample(levels(clust)))),
#           sf::st_as_sf(france.cities), color=NA)

ggplot() + theme_void() + theme(legend.position="none") +
  geom_sf(data=sf::st_as_sf(france), color="gray", fill=NA) +
  geom_sf(data=sf::st_as_sf(france.cities), color=NA, fill="black")

ggsave("img/fig6.pdf", width=9, height=9, scale=0.7, device=cairo_pdf)
ggsave("img/fig6.png", width=9, height=9, scale=0.7)
