## ----options, include=F-------------------------------------------------------
library(knitr)
library(xtable)
options(scipen = 999) # to avoid scientic notation in knitr outputs
opts_chunk$set(size = 'small',
               cache.path = 'collin_cache/collin-',
               fig.align = 'center')

## ----echo=T, eval=F-----------------------------------------------------------
#  install.packages("collin")

## ----echo=T-------------------------------------------------------------------
library(collin)

## ----eval=F-------------------------------------------------------------------
#  help(package = "collin")

## ----eval=F-------------------------------------------------------------------
#  browseVignettes("collin")

## ----nsimseed-----------------------------------------------------------------
mynsim <- 100    # number of simulations
myseed <- 23984  # seed

## ----lib----------------------------------------------------------------------
library(nlme)     # lme
library(dlnm)
library(splines)  # ns

## ----corpm25------------------------------------------------------------------
# data summary:
summary(mempm25)

# exposure with lags matrix:
pm25lags <- 0:7
nlagspm25 <- length(pm25lags)
E <- paste0("pm25y", pm25lags)
Qpm25 <- as.matrix(mempm25[, E])

# exposure pairwise correlations:
corQpm25 <- cor(Qpm25, use = "complete.obs")
rownames(corQpm25) <- colnames(corQpm25) <- E

## ----printcorQpm25, eval=FALSE------------------------------------------------
#  print(corQpm25, digits = 3)

## ----printcorQpm25fancy, results="asis", echo=FALSE---------------------------
xtab <- xtable(corQpm25,
               caption = "Correlation between \\PM\\ concentrations at different lags.",
               label = "tab:corQpm25")
names(xtab) <- paste("Year", pm25lags)
names(xtab)[1] <- "Pregnancy"
rownames(xtab) <- names(xtab)
print(xtab, size = 'footnotesize', booktabs = TRUE)
rm(xtab)

## ----fitsinglelagmempm25, cache=TRUE------------------------------------------
# set the exposure increase:
pm25change <- 10

# data.frame to store effects and CI:
pm25effects <- data.frame(lower = rep(NA, nlagspm25),
                          estimate = rep(NA, nlagspm25),
                          upper = rep(NA, nlagspm25))

# fit models:
for (i in 1:nlagspm25) {
  # select exposure lag:
  Ei <- Qpm25[, i]
  # fit model for that single lag:
  modi <- lme(wmemo ~ Ei + sex + agecen + educ + resses,
              data = mempm25,
              weights = ~ wei,
              random = ~ 1|school/id,
              na.action = na.omit,
              control = lmeControl(opt = "optim"))
  # get effect estimate (for Echange units increase):
  pm25effects[i, ] <- pm25change * intervals(modi)$fixed["Ei", ]
}
rm(Ei, modi)

## ----plotsinglelagmempm25, eval=FALSE-----------------------------------------
#  par(las = 1)
#  xvalues <- 0:(nlagspm25 - 1)
#  with(pm25effects,
#       plot(xvalues, estimate, ylim = range(pm25effects), pch = 19,
#            xlab = "Year", ylab = "Change in mean working memory"))
#  with(pm25effects, segments(xvalues, lower, xvalues, upper))
#  abline(h = 0, lty = 2)

## ----plotsinglelagmempm25b, fig.width=8, fig.height=5, out.width='0.65\\textwidth', fig.align= 'center', echo = F----
par(las = 1)
xvalues <- 0:(nlagspm25 - 1)
with(pm25effects,
     plot(xvalues, estimate, ylim = range(pm25effects), pch = 19,
          xlab = "Year", ylab = "Change in mean working memory"))
with(pm25effects, segments(xvalues, lower, xvalues, upper))
abline(h = 0, lty = 2)

## ----fitdlnmmempm25-----------------------------------------------------------
# create crossbasis:
df <- 5
ekn <- equalknots(x = c(0, nlagspm25 - 1),
                  nk = NULL,
                  fun = "bs",
                  df = df,
                  degree = 2,
                  intercept = TRUE)
cbpm25 <- crossbasis(x = Qpm25,
                     lag = c(0, nlagspm25 - 1),
                     argvar = list(fun = "lin"),
                     arglag = list(fun = "bs", degree = 2, df = df, knots = ekn))

# fit model:
modmempm25 <- lme(wmemo ~ cbpm25 + sex + agecen + educ + resses,
                  data = mempm25,
                  weights = ~ wei,
                  random = ~ 1|school/id,
                  na.action = na.exclude,
                  control = lmeControl(opt = "optim"))

# predict effects at different lags
predmempm25 <- crosspred(basis = cbpm25, model = modmempm25, cen = 0, at = pm25change)

## ----plotdlnmmempm25, eval=FALSE----------------------------------------------
#  par(las = 1)
#  plot(predmempm25, var = pm25change, xlim = c(0, nlagspm25 - 1), main = "",
#       xlab = "Year", ylab = "Change in mean working memory")

## ----plotdlnmmempm25b, fig.width=8, fig.height=5, out.width='0.65\\textwidth', fig.align= 'center', echo = F----
par(las = 1)
plot(predmempm25, var = pm25change, xlim = c(0, nlagspm25 - 1), main = "",
     xlab = "Year", ylab = "Change in mean working memory")

## ----setsimmempm25conseff-----------------------------------------------------
# constant effect (divide cumulative by number of lags):
(conseffpm25 <- rep(predmempm25$allfit / nlagspm25, nlagspm25))

## ----simmempm25conseff, cache=TRUE--------------------------------------------
simconseffpm25 <- collindlnm(model = modmempm25,    # the original fitted model
                             x = Qpm25,             # matrix with PM2.5 values at each lag
                             cb = cbpm25,           # the crossbasis included in the model
                             at = pm25change,       # increase in PM2.5 to compute effects
                             effect = conseffpm25,  # hypothetical effect
                             nsim = mynsim,
                             seed = myseed)

## ----plotsimconseffpm25, eval=FALSE-------------------------------------------
#  par(las = 1)
#  plot(simconseffpm25, xlab = "Year", ylab = "Change in mean working memory")

## ----plotsimconseffpm25b, fig.width=8, fig.height=5, out.width='0.65\\textwidth', fig.align= 'center', echo = F----
par(las = 1)
plot(simconseffpm25, xlab = "Year", ylab = "Change in mean working memory")

## ----setsimmempm25lag1and6----------------------------------------------------
lag1and6effpm25 <- rep(0, nlagspm25)
lag1and6effpm25[c(2, 7)] <- 1.5 * predmempm25$allfit
round(lag1and6effpm25, 2)

## ----simmempm25lag1and6, cache=TRUE-------------------------------------------
simlag1and6effpm25 <- collindlnm(model = modmempm25,
                                 x = Qpm25,
                                 cb = cbpm25,
                                 at = pm25change,
                                 effect = lag1and6effpm25,
                                 nsim = mynsim,
                                 seed = myseed)

## ----plotsimlag1and6effpm25, eval=FALSE---------------------------------------
#  par(las = 1)
#  plot(simlag1and6effpm25, xlab = "Year", ylab = "Change in mean working memory")

## ----plotsimlag1and6effpm25b, fig.width=8, fig.height=5, out.width='0.65\\textwidth', fig.align='center', echo=FALSE----
par(las = 1)
plot(simlag1and6effpm25, xlab = "Year", ylab = "Change in mean working memory")

## ----setsimmempm25lag5--------------------------------------------------------
lag5seffpm25 <- rep(0, nlagspm25)
lag5seffpm25[6] <- 4 * predmempm25$allfit
round(lag5seffpm25, 2)

## ----simmempm25lag5, cache=TRUE-----------------------------------------------
simlag5effpm25 <- collindlnm(model = modmempm25,
                             x = Qpm25,
                             cb = cbpm25,
                             at = pm25change,
                             effect = lag5seffpm25,
                             nsim = mynsim,
                             seed = myseed)

## ----plotsimlag5effpm25, eval=FALSE-------------------------------------------
#  par(las = 1)
#  plot(simlag5effpm25, xlab = "Year", ylab = "Change in mean working memory")

## ----plotsimlag5effpm25b, fig.width=8, fig.height=5, out.width='0.65\\textwidth', fig.align='center', echo=FALSE----
par(las = 1)
plot(simlag5effpm25, xlab = "Year", ylab = "Change in mean working memory")

## ----rhospno2-----------------------------------------------------------------
summary(rhospno2)

## ----corrrhospno2-------------------------------------------------------------
# create matrix with lagged data:
nlagsno2 <- 15  # number of lags considered (14 + 1)

Qno2 <- matrix(NA, nrow = dim(rhospno2)[1], ncol = nlagsno2)
for (i in 1:nlagsno2)
  Qno2[, i] <- lagpad(x = rhospno2$no2, k = i - 1)

# correlation betweeen exposures:
corQno2 <- cor(Qno2, use = "complete.obs")
rownames(corQno2) <- colnames(corQno2) <- paste0("lag", 0:(nlagsno2 - 1))

## ----printcorQno2, eval=FALSE-------------------------------------------------
#  print(corQno2, digits = 2)

## ----printcorQno2fancy, results="asis", echo=FALSE----------------------------
xtab <- xtable(corQno2,
               caption = "Correlation between \\NO\\ concentrations at different lags.",
               label = "tab:corQno2")
names(xtab) <- paste0("-", 0:(nlagsno2 - 1), " d.")
names(xtab)[1] <- "Given day"
rownames(xtab) <- names(xtab)
print(xtab, size = 'tiny', booktabs = TRUE)
rm(xtab)

## ----fitsinglelagrhosppm25, cache=TRUE----------------------------------------
# crossbasis for temperature

# Fixing the knots at equally spaced values of temperature and at equally spaced
# log-values of lag. From:
# https://github.com/gasparrini/2010_gasparrini_StatMed_Rcode/blob/master/Rcode.R

ktemp <- equalknots(rhospno2$temp, nk = 4)
nlagstemp <- 22  # maximum lag for temperature + 1
klag <- logknots(nlagstemp - 1, nk = 3)

cbtemp <- crossbasis(x = rhospno2$temp,
                     argvar = list(knots = ktemp),
                     arglag = list(knots = klag),
                     lag = nlagstemp - 1)

# number of years for the time spline:
nyears <- diff(range(rhospno2$year)) + 1

# get beta coefficients and CI for each model:
coefsno2single <- data.frame(estimate = rep(NA, nlagsno2),
                             lower = rep(NA, nlagsno2),
                             upper = rep(NA, nlagsno2))

for (i in 1:nlagsno2) {
  # select exposure lag:
  Ei <- Qno2[, i]
  # fit model:
  modi <- glm(hresp ~ Ei + cbtemp + ns(t, 7 * nyears) + dow,
              data = rhospno2,
              family = quasipoisson,
              na.action = na.exclude)
  # get beta estimates and CI:
  ints <- confint.default(modi)
  coefsno2single$lower[i] <- ints["Ei", "2.5 %"]
  coefsno2single$estimate[i] <- summary(modi)$coefficients["Ei", "Estimate"]
  coefsno2single$upper[i] <- ints["Ei", "97.5 %"]
}

# set the exposure increase:
no2change <- 10

# compute effects (RRs):
effectno2single <- exp(no2change * coefsno2single)

## ----plotsinglelagrhospno2, eval=FALSE----------------------------------------
#  par(las = 1)
#  xvalues <- 0:(nlagsno2 - 1)
#  with(effectno2single,
#       plot(xvalues, estimate, ylim = range(effectno2single), pch = 19, xlab = "Lag", ylab = "RR"))
#  with(effectno2single, segments(xvalues, lower, xvalues, upper))
#  abline(h = 1, lty = 2)

## ----plotsinglelagrhospno2b, fig.width=8, fig.height=5, out.width='0.65\\textwidth', fig.align= 'center', echo = F----
par(las = 1)
xvalues <- 0:(nlagsno2 - 1)
with(effectno2single,
     plot(xvalues, estimate, ylim = range(effectno2single), pch = 19, xlab = "Lag", ylab = "RR"))
with(effectno2single, segments(xvalues, lower, xvalues, upper))
abline(h = 1, lty = 2)

## ----cbno2--------------------------------------------------------------------
# crossbasis for NO2 (linear effect):
lagknots <- logknots(nlagsno2 - 1, nk = 3)
cbno2 <- crossbasis(x = rhospno2$no2,
                    lag = c(0, (nlagsno2 - 1)),
                    argvar = list(fun = "lin"),
                    arglag = list(fun = "ns", knots = lagknots))

## ----cbnames------------------------------------------------------------------
colnames(cbtemp)
colnames(cbno2)
all(colnames(cbno2) %in% colnames(cbtemp))

## ----cbtempnames--------------------------------------------------------------
# change the names of the crossbassis for temperature:
aux <- as.data.frame(cbtemp)
ncbtemp <- dim(cbtemp)[2]
crosstempnames <- paste0("crosstemp", 1:ncbtemp)
names(aux) <- crosstempnames
rhospno2 <- cbind(rhospno2, aux)
rm(aux)
names(rhospno2)

## ----fitdlnmrhospno2----------------------------------------------------------
# model formula:
formhosp <- paste0("hresp ~ cbno2 + ",
                   paste(crosstempnames, collapse = " + "),
                   " + ns(t, 7 * nyears) + dow")
(formhosp <- as.formula(formhosp))

# fit model:
modrhospno2 <- glm(formhosp, family = quasipoisson, na.action = na.exclude, data = rhospno2)

# predict effects at different lags:
predrhospno2 <- crosspred(basis = cbno2, model = modrhospno2, cen = 0, at = no2change)

## ----plotdlnmrhospno2, eval=FALSE---------------------------------------------
#  par(las = 1)
#  plot(predrhospno2, var = no2change, xlim = c(0, nlagsno2 - 1), main = "", xlab = "Day",
#       ylab = "RR of hospital admission")

## ----plotdlnmrhospno2b, fig.width=8, fig.height=5, out.width='0.65\\textwidth', fig.align= 'center', echo = F----
par(las = 1)
plot(predrhospno2, var = no2change, xlim = c(0, nlagsno2 - 1), main = "", xlab = "Day",
     ylab = "RR of hospital admission")

## ----setsimrhospno2lag0-------------------------------------------------------
# Effect (RRs) only at lags 0, same as observed
RRveclag0 <- rep(1, nlagsno2)
RRveclag0[1] <- predrhospno2$matRRfit[, "lag0"]
RRveclag0

## ----simrhospno2lag0, cache=TRUE----------------------------------------------
simlag0effno2 <- collindlnm(model = modrhospno2,
                            x = Qno2,
                            cb = cbno2,
                            at = no2change,
                            effect = RRveclag0,
                            type = "risk",
                            nsim = mynsim,
                            seed = myseed)

## ----plotsimlag0effno2, eval=FALSE--------------------------------------------
#  par(las = 1)
#  plot(simlag0effno2, xlab = "Day", ylab = "RR of hospital admission")

## ----plotsimlag0effno2b, fig.width=8, fig.height=5, out.width='0.65\\textwidth', fig.align='center', echo=FALSE----
par(las = 1)
plot(simlag0effno2, xlab = "Day", ylab = "RR of hospital admission")

## ----chicago------------------------------------------------------------------
chica <- chicagoNMMAPS[, c("date", "time", "year", "dow", "death", "temp", "pm10")]
summary(chica)

## ----corQtemp-----------------------------------------------------------------
# create matrix with lagged data:
nlagstemp <- 31  # number of lags considered (30 + 1)

Qtemp <- matrix(NA, nrow = dim(chica)[1], ncol = nlagstemp)
for (i in 1:nlagstemp) {
  Qtemp[, i] <- lagpad(x = chica$temp, k = i - 1)
}
colnames(Qtemp) <- paste0("lag", 0:(nlagstemp - 1))

# correlation betweeen exposures
corQtemp <- cor(Qtemp, use = "complete.obs")
rownames(corQtemp) <- colnames(corQtemp) <- paste0("lag", 0:(nlagstemp - 1))

## ----<cbpm10------------------------------------------------------------------
# crossbasis for PM10:
cbpm10 <- crossbasis(x = chica$pm10,
                     lag = 1,
                     argvar = list(fun = "lin"),
                     arglag = list(fun = "strata"))

# problems with models with 2 crossbases because of names. Rename:
chica$baspm <- cbpm10
rm(cbpm10)

## ----fitsinglelagchica, cache=TRUE--------------------------------------------
# reference value of temperature for effects calculation:
centemp <- 21

# evaluation points (values of temperature):
attemp <- c(-20, 0, 33)

# get beta coefficients and CI for each model:
coefs <- lower <- upper <- matrix(NA, nrow = dim(Qtemp)[2], ncol = length(attemp))

# number of years for time spline:
nyearschica <- diff(range(chica$year, na.rm = TRUE)) + 1


for (i in 1:nlagstemp) {
  Ei <- Qtemp[, i]
  # crossbasis for lag i of temperature:
  cbi <- onebasis(Ei, fun = "bs", knots = ktemp, degree = 2)
  # fit model:
  modi <- glm(death ~ cbi + baspm + ns(time, 7 * nyearschica) + dow,
              data = chica,
              family = quasipoisson)
  # get effect estimates and CI:
  predi <- crosspred(basis = cbi, model = modi, at = attemp, cen = centemp)
  lower[i, ] <- t(predi$matRRlow)
  coefs[i, ] <- t(predi$matRRfit)
  upper[i, ] <- t(predi$matRRhigh)
}

## ----plotsinglelagchica, eval=FALSE-------------------------------------------
#  par(las = 1, mfrow = c(3, 1), mar = c(4, 4, 0, 2) + 0.1)
#  for (i in 1:length(attemp)) {
#    plot(0:(nlagstemp - 1), coefs[, i], pch = 19, ylim = c(min(lower[, i]), max(upper[, i])),
#         xlab = "", ylab = "RR")
#    segments(0:(nlagstemp - 1), lower[, i], 0:(nlagstemp - 1), upper[, i])
#    abline(h = 1, lty = 2)
#    legend("topright", paste0("Temp = ", attemp[i]))
#    mtext("Lag", side = 1, line = 2, cex = 0.7)
#  }

## ----plotsinglelagchicab, fig.width=8, fig.height=8, out.width='0.65\\textwidth', fig.align= 'center', echo = F----
par(las = 1, mfrow = c(3, 1), mar = c(4, 4, 0, 2) + 0.1)
for (i in 1:length(attemp)) {
  plot(0:(nlagstemp - 1), coefs[, i], pch = 19, ylim = c(min(lower[, i]), max(upper[, i])),
       xlab = "", ylab = "RR")
  segments(0:(nlagstemp - 1), lower[, i], 0:(nlagstemp - 1), upper[, i])
  abline(h = 1, lty = 2)
  legend("topright", paste0("Temp = ", attemp[i]))
  mtext("Lag", side = 1, line = 2, cex = 0.7)
}

## ----fitdlnmchica-------------------------------------------------------------
# fixing the knots at equally spaced log values of lag:
klag <- logknots(nlagstemp - 1, nk = 3)

# crossbasis matrix for temperature:
cbtemp <- crossbasis(x = chica$temp,
                     argvar = list(fun = "bs", knots = ktemp),
                     arglag = list(knots = klag),
                     lag = nlagstemp - 1)

# fit model:
modtemp <- glm(death ~ cbtemp + baspm + ns(time, 7 * nyearschica) + dow,
               data = chica,
               family = quasipoisson)

# effect estimates:
predtemp <- crosspred(basis = cbtemp, model = modtemp, at = attemp, cen = centemp)

## ----plotdlnmchica, eval=FALSE------------------------------------------------
#  par(las = 1, mfrow = c(3, 1), mar = c(4, 4, 0, 2) + 0.1)
#  plot(predtemp, var = attemp[1])
#  legend("topright", paste0("Temp = ", attemp[1]))
#  
#  plot(predtemp, var = attemp[2], yaxt = "n", ylim = c(0.94, 1.05))
#  axis(2, at = c(0.96, 0.98, 1, 1.02, 1.04))
#  legend("topright", paste0("Temp = ", attemp[2]))
#  
#  plot(predtemp, var = attemp[3])
#  legend("topright", paste0("Temp = ", attemp[3]))

## ----plotdlnmchicab, fig.width=8, fig.height=8, out.width='0.65\\textwidth', fig.align= 'center', echo = F----
par(las = 1, mfrow = c(3, 1), mar = c(4, 4, 0, 2) + 0.1)
plot(predtemp, var = attemp[1])
legend("topright", paste0("Temp = ", attemp[1]))

plot(predtemp, var = attemp[2], yaxt = "n", ylim = c(0.94, 1.05))
axis(2, at = c(0.96, 0.98, 1, 1.02, 1.04))
legend("topright", paste0("Temp = ", attemp[2]))

plot(predtemp, var = attemp[3])
legend("topright", paste0("Temp = ", attemp[3]))

## ----setsimchicalag0null------------------------------------------------------
RRmattemp <- predtemp$matRRfit
round(RRmattemp, 2)

attempc <- as.character(attemp)
attempc

# all effects null from lag 8 included
RRmattemp[, paste0("lag", 6:(nlagstemp - 1))] <- 1

# at temp 1:
RRmattemp[attempc[1], paste0("lag", 0:2)] <- 1
RRmattemp[attempc[1], paste0("lag", 3:5)] <- c(1.07, 1.12, 1.06)

# at temp 2:
RRmattemp[attempc[2], paste0("lag", 0:2)] <- 1
RRmattemp[attempc[2], paste0("lag", 3:5)] <- c(1.08, 1.03, 1.01)

# at temp 3:
RRmattemp[attempc[3], paste0("lag", 0:5)] <- c(1.15, 1.20, 1.22, 1.15, 1.10, 1.04)

RRmattemp

## ----simchicalag0null, cache=TRUE---------------------------------------------
simchicalag0null <- collindlnm(model = modtemp,
                               x = chica$temp,
                               cb = cbtemp,
                               at = attemp,
                               cen = centemp,
                               effect = RRmattemp,
                               type = "risk",
                               shape = "nonlinear",
                               nsim = mynsim,
                               seed = myseed)

## ----plotsimchicalag0null, eval=FALSE-----------------------------------------
#  par(las = 1, mfrow = c(3, 1), mar = c(4, 4, 2, 2))
#  plot(simchicalag0null, varlegend = "Temperature")

## ----plotsimchicalag0nullb, fig.width=8, fig.height=8, out.width='0.65\\textwidth', fig.align='center', echo=FALSE----
par(las = 1, mfrow = c(3, 1), mar = c(4, 4, 2, 2))
plot(simchicalag0null, varlegend = "Temperature")

## ----plotsimchicalag0nullzoom, eval=FALSE-------------------------------------
#  par(las = 1)
#  plot(simchicalag0null, lags = 0:8, show = "auto", varlegend = "Temperature")

## ----plotsimchicalag0nullzoomb, fig.width=8, fig.height=8, out.width='0.65\\textwidth', fig.align='center', echo=FALSE----
par(las = 1)
plot(simchicalag0null, lags = 0:8, show = "auto", varlegend = "Temperature")

