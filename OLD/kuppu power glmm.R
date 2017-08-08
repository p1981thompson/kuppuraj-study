library(lme4)
packageVersion("lme4")
library(plyr)
library(ggplot2)
theme_set(theme_bw())


N=100
expdat <- expand.grid(indiv = factor(1:N), dist = factor(1:2), pred = factor(1:2), ttt = c("adult","child"))


set.seed(1981)
nsim <- 100
beta <- c(qlogis(0.9), -0.2)
theta <- c(0.1,0.1,0.1)
ss <- simulate(~ttt + (1 | indiv) + (1 | dist) + (1 | pred), nsim = nsim, family = binomial,
weights = rep(25, nrow(expdat)), newdata = expdat, newparams = list(theta = theta,
beta = beta))

expdat$resp <- ss[, 1]

fit1 <- glmer(resp ~ ttt + (1 | indiv) + (1 | dist) + (1 | pred), family = binomial, weights = rep(25, nrow(expdat)), data = expdat)
fit1B <- refit(fit1, ss[[2]])
fitsim <- function(i) {
coef(summary(refit(fit1, ss[[i]])))["tttchild", ]
}
################################################################################
t1 <- system.time(fitAll <- lapply(seq(nsim), function(i) fitsim(i)))
## you can use .progress='text' to get a progress indicator ...
fitAll <- setNames(as.data.frame(fitAll), c("est", "stderr", "zval", "pval"))
###POWER####################
mean(unlist(fitAll)[1:100*4] < 0.05)
