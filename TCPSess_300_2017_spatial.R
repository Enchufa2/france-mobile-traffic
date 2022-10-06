colscale <- function(data, cols) {
  x <- unlist(data[, cols, with=FALSE])
  scale <- sqrt(sum(x^2)/(length(x)-1))
  data[, cols] <- data[, cols, with=FALSE] / scale
  data
}

DT <- colscale(DT, vars$Traffic)
DT <- colscale(DT, vars$Population)

#x <- vars$Traffic
#x <- vars$Population
x <- vars$All
y <- response$`A) Median income`
ff <- paste0(y, " ~ `", paste(x, collapse="`+`"), "`")

DT$lag <- spdep::lag.listw(lw, DT$DISP_MED14, zero.policy=TRUE)
ff <- paste(ff, "+ scale(lag, center=FALSE)")

fit.glm1 <- glm(as.formula(ff), family=Gamma("log"), data=DT)
DT$err <- spdep::lag.listw(lw, resid(fit.glm1), zero.policy=TRUE)
fit.glm <- glm(as.formula(paste(ff, "+ err")), family=Gamma("log"), data=DT)
err <- resid(fit.glm)
qqnorm(err)
qqline(err)
plot(fitted(fit.glm), DT$DISP_MED14)
abline(0, 1)
summary(lm(DT$DISP_MED14~fitted(fit.glm)))
sjPlot::plot_model(fit.glm)
spdep::moran.mc(residuals(fit.glm), lw, 999, zero.policy=TRUE)

################################################################################

library(spatialreg)
set.VerboseOption(TRUE)
set.coresOption(parallel::detectCores())
set.mcOption(FALSE)
cl <- parallel::makeCluster(get.coresOption())
set.ClusterOption(cl)

france.iris <- subset(france.iris, CODE_IRIS %in% DT$CODE_IRIS)
france.iris <- france.iris[order(france.iris$CODE_IRIS),]
nb <- spdep::poly2nb(france.iris)
lw <- spdep::nb2listw(nb, zero.policy=TRUE)
spdep::moran.mc(residuals(fit.glm), lw, 999, zero.policy=TRUE)

.sendtg("inicio ME")
fit.me <- ME(as.formula(ff), family=quasipoisson, data=DT, listw=lw, zero.policy=TRUE)
.sendtg("fin ME")

################################################################################

library(spatialreg)

m1s <- lagsarlm(DISP_MED14 ~ 1, data=DT, lw)
m1s <- lagsarlm(ff, data=DT, lw, zero.policy=TRUE)
m1s <- errorsarlm(DISP_MED14 ~ 1, data=DT, lw)
m1s <- errorsarlm(ff, data=DT, lw, zero.policy=TRUE)
summary(m1s)
spdep::moran.mc(residuals(m1s), lw, 999, zero.policy=TRUE)
summary(lm(DT$DISP_MED14~fitted(m1s)))
err <- resid(m1s)
qqnorm(err)
qqline(err)
cor(fitted(m1s), DT$DISP_MED14) # 0.84 para ~ 1
plot(fitted(m1s), DT$DISP_MED14)
barplot(coef(m1s)[-(1:2)], horiz=TRUE, las=2)

################################################################################

library(spatialreg)

# takes ages
sfiltw <- SpatialFiltering(ff, data=DT, nb=nb, style="W")

# SpatialFiltering with RSpectra
lw <- spdep::listw2U(lw) # ??
#lw <- similar.listw(lw) # make symmetric
S <- spdep::listw2mat(lw)
S <- nrow(S)/sum(S) * S
isSymmetric(S, check.attributes=FALSE) # TRUE
xsar <- model.matrix(as.formula(ff), DT)
mx <- diag(1, nrow(S)) - xsar %*% qr.solve(crossprod(xsar), t(xsar))
S <- mx %*% S %*% mx
k <- 200
eigens <- RSpectra::eigs_sym(S, k); .sendtg("eigs_sym finished") # 30 min

netfit <- ccv.glmnet(
  paste(ff, "+", paste0("V", 1:k, collapse="+")),
  cbind(DT, eigens$vectors), DT$downlink_zon)
err <- fitted(netfit) - netfit$y
qqnorm(err)
qqline(err)
spdep::moran.mc(err, lw, 999)

# moran eigen approach
meig <- spmoran::meigen(cmat=spdep::nb2mat(nb)); # 20 min
#coords <- st_coordinates(st_centroid(st_as_sf(france.iris)))
#meig <- spmoran::meigen_f(coords); # muy rápido, pero no reduce mucho la correl

netfit <- ccv.glmnet(
  paste(ff, "+", paste0("V", 1:ncol(meig$sf), collapse="+")),
  cbind(DT, meig$sf), weights=DT$downlink_zon,
  penalty.factor=c(rep(1, length(x)), rep(0, ncol(meig$sf))))
fit <- lm(
  paste(ff, "+", paste0("V", 1:ncol(meig$sf), collapse="+")),
  cbind(DT, meig$sf), weights=DT$downlink_zon)
err <- fitted(netfit) - netfit$y
qqnorm(err)
qqline(err)
spdep::moran.mc(err, lw, 999)

# mem approach
# https://cran.r-project.org/web/packages/adespatial/vignettes/tutorial.html
memv <- adespatial::mem(lw) # 6 min
# memv.sel <- adespatial::mem.select(DT$DISP_MED14, lw, MEM.all=TRUE) # way too much
scalo <- adespatial::scalogram(DT$DISP_MED14, memv, nblocks=20)
plot(scalo)

