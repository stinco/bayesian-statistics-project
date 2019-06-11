#####
# 8. Prove rstanarm
# 11/06/2019
#####


# Libraries ####

library(rstan)
library(rstanarm)
library(GGally)
library(arm)
library(gridExtra)
library(tidyverse)
library(plotly)
library(mgcv)       # For GAM model
# library(gamm4)      # For GAM model
library(loo)        # For bayesian model comparison
library(lme4)
library(bayesplot)

theme_set(theme_bw())

SEED = 1234


# Prove rstanarm ####

# from example(gamm4, package = "gamm4"), prefixing gamm4() call with stan_
dat <- mgcv::gamSim(1, n = 400, scale = 2) ## simulate 4 term additive truth
#> Gu & Wahba 4 term additive model
## Now add 20 level random effect `fac'...
dat$fac <- fac <- as.factor(sample(1:20, 400, replace = TRUE))
dat$y <- dat$y + model.matrix(~ fac - 1) %*% rnorm(20) * .5

br <- stan_gamm4(y ~ s(x0) + x1 + s(x2), data = dat, random = ~ (1 | fac),
                 chains = 1, iter = 200) # for example speed

print(br)

# summary(br)

plot_nonlinear(br)

plot(br)


br2 <- stan_gamm4(y ~ s(x0) + x1 + s(x2) + fac, data = dat,
                  chains = 1, iter = 200) # for example speed

print(br2)

summary(br2)

plot_nonlinear(br2)

plot(br2)


p1 <- plot(br)
p2 <- plot(br2)

grid.arrange(p1, p2)

loo(br)
loo(br2)

compare(loo(br), loo(br2))