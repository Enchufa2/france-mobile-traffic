library(data.table)
library(fst)

name <- "TCPSess_300_2017_0"
files <- Sys.glob(file.path("data", name, "*.hourly.fst"))

df <- rbindlist(pbapply::pblapply(files, function(x) {
  df <- read.fst(x, as.data.table=TRUE)
  df[, category:=strsplit(basename(x), "\\.")[[1]][1]]
}))

df.bs <- df[, .(uplink=sum(uplink), downlink=sum(downlink)), by = bs]
df.cat <- df[, .(uplink=sum(uplink), downlink=sum(downlink)), by = category]
df.type <- rbind(
  df.bs[, .(uplink, downlink, type="BS")], 
  df.cat[, .(uplink, downlink, type="category")]
)
df.sp <- df[, .(uplink=sum(uplink), downlink=sum(downlink)), by = .(bs, category)]
df.ts <- df[, .(uplink=sum(uplink), downlink=sum(downlink)), by = .(ts, category)]

beepr::beep("mario")

################################################################################

library(ggplot2)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)

catLookup <- readr::read_csv("data/TCPSess_300_2017_service_categ2.csv", 
                             col_names=c("id", "name", "x", "y"), quote="") %>%
  select(id, name) %>%
  distinct() %>%
  mutate(name = setNames(name, id)) %>%
  pull() %>%
  c(other = "Other")

df.type %>%
  ggplot(aes(downlink/1e12, uplink/1e12)) + 
  facet_wrap("type", scales="free") + geom_point()

# eCDF
df.type %>%
  gather(link, bytes, uplink, downlink) %>%
  ggplot(aes(bytes/1e12, color=link)) + stat_ecdf() + #scale_x_log10() +
  facet_grid("type", scales="free_x") + labs(y="eCDF", x="Traffic [TB]")

# ePDF
df.type %>%
  gather(link, bytes, uplink, downlink) %>%
  ggplot(aes(bytes, fill=link)) + stat_density() + scale_x_log10() +
  facet_grid("type", scales="free") + labs(x="Traffic [B]")

################################################################################

library(qqplotr)
library(ggpmisc)

# Q-Q plot
df.type %>%
  ggplot(aes(sort(downlink)/1e12, sort(uplink)/1e12)) + geom_point() +
  facet_wrap("type", scales="free") + geom_smooth(method="lm") +
  labs(y="uplink sample [TB]", x="downlink sample [TB]", title="Q-Q plot") +
  stat_poly_eq(aes(label=paste(stat(eq.label), stat(adj.rr.label), sep = "~~~~")), 
               formula=y~x, parse=TRUE, rr.digits=3, coef.digits=2,
               label.x.npc = "right", label.y.npc = "bottom")

dist <- "weibull"; df.type %>%
  gather(link, bytes, uplink, downlink) %>%
  ggplot(aes(sample=bytes/1e12)) + stat_qq_band(distribution=dist) +
  stat_qq_line(distribution=dist) + stat_qq_point(distribution=dist) +
  labs(x="Theoretical Quantiles", y="Sample Quantiles", title="Q-Q plot") +
  facet_wrap(link~type, scales="free")
df.type %>%
  gather(link, bytes, uplink, downlink) %>%
  ggplot(aes(sample=bytes/1e12)) + stat_pp_band(distribution=dist) +
  stat_pp_line() + stat_pp_point(distribution=dist) +
  labs(x="Cumulative Probability", y="Probability Points", title="P-P plot") +
  facet_wrap(link~type, scales="free")

dist <- "qweibull"; dparams <- list(shape=0.95); df.type %>%
  gather(link, bytes, uplink, downlink) %>%
  ggplot(aes(sample=bytes/1e12)) + stat_qq(distribution=dist, dparams=dparams) +
  ggplot2::stat_qq_line(distribution=dist, dparams=dparams) +
  labs(x="Theoretical Quantiles", y="Sample Quantiles", title="Q-Q plot (Weibull, shape=0.95)") +
  facet_wrap(link~type, scales="free")

df.type %>%
  gather(link, bytes, uplink, downlink) %>%
  group_by(type, link) %>%
  do(broom::tidy(MASS::fitdistr(.$bytes/1e12, "weibull", shape=0.95, lower=1e-3)))

################################################################################

df.cat %>%
  mutate(uplink = uplink / sum(uplink), downlink = downlink / sum(downlink)) %>%
  mutate(category = catLookup[category]) %>%
  mutate(category = forcats::fct_reorder(factor(category), downlink)) %>%
  gather(link, bytes, uplink, downlink) %>%
  arrange(link, category) %>%
  ggplot(aes(category, bytes*100)) + facet_grid("link") + coord_flip() +
  geom_col(aes(fill=rep(c(0, 1), 40))) + labs(x=NULL, y="Traffic [%]") +
  theme_minimal() + theme(panel.grid.minor.x=element_blank()) + guides(fill=FALSE)

################################################################################

library(corrplot)

sp <- df.sp %>%
  complete(bs, category, fill=list(uplink=0, downlink=0)) %>%
  select(-uplink) %>%
  mutate(category = catLookup[category]) %>%
  spread(category, downlink) %>%
  select(-bs)

corrplot(cor(sp), order="FPC", method="color", tl.cex=.6, pch.cex=.6,
         p.mat=cor.mtest(sp)$p, insig="pch", sig.level=.01)

ts <- df.ts %>%
  complete(ts, category, fill=list(uplink=0, downlink=0)) %>%
  select(-uplink) %>%
  mutate(category = catLookup[category]) %>%
  spread(category, downlink) %>%
  select(-ts)

corrplot(cor(ts), order="FPC", method="color", tl.cex=.6, pch.cex=.6,
         p.mat=cor.mtest(ts)$p, insig="pch", sig.level=.01)
