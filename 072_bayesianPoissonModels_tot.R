#####
# 7.2 Bayesian poisson models for mort.tot
# 14/06/2019
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
library(knitr)

theme_set(theme_bw())

SEED = 1234
# CHAINS = 4
# CORES = 4
# ITER = 2000
CHAINS = 1
CORES = 1
# ITER = 500
ITER = 200


# Model for mort  without interaction (best) ####

# Model fitting
fit_gam_bayes_pois_best <- stan_gamm4(tot.mort ~ offset(log(pop_m)) + s(mean.temp) + s(day.of.year) + factor(year) + factor(weekend),
                                 family = neg_binomial_2, data = mort,
                                 chains = CHAINS, iter = ITER, seed = SEED, cores = CORES)
# The default prior for coefficients is:
# normal(location = 0, scale = NULL)
# that means an improper prior
# it is a non-informative prior


print(fit_gam_bayes_pois_best)
summary(fit_gam_bayes_pois_best)

plot(fit_gam_bayes_pois_best)
plot(fit_gam_bayes_pois_best,
     prob = c(.5,.99))

plot(fit_gam_bayes_pois_best, pars = str_c("factor(year)", 1:9)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-.1, .1)) #+
# labs(y = "Variables") +
# theme(axis.title.y = element_text(family = "sans", size = 15, margin=margin(0,10,0,0))) +
# theme(plot.margin = margin(.5, .5, .5, 2, "cm"),
#       axis.text = element_text(colour = "red", size = rel(1.5),
#                                margin = margin(t = 0, r = 100, b = 0, l = 0, unit = "pt")))


p1 <- plot(fit_gam_bayes_pois_best, pars = c(str_c("factor(year)", 1:9), "factor(weekend)TRUE"),
           prob = c(.95,.99)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-.1, .1)) #+
# theme(plot.margin = margin(.5, .5, .5, 2, "cm"))

p2 <- plot(fit_gam_bayes_pois_best, pars = str_c("s(mean.temp).", 1:9),
           prob = c(.95,.99)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-6, 6))

p3 <- plot(fit_gam_bayes_pois_best, pars = str_c("s(day.of.year).", 1:9),
           prob = c(.95,.99)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-6, 6))

grid.arrange(p1, p2, p3,
             ncol = 1)


# Table with estimated coefficients
coef_est <- as.matrix(fit_gam_bayes_pois_best) %>% 
  apply(FUN = mean, MARGIN = 2)

as_tibble(as.matrix(fit_gam_bayes_pois_best))

coeff_int1 <- posterior_interval(fit_gam_bayes_pois_best, prob = .90)
coeff_int2 <- posterior_interval(fit_gam_bayes_pois_best, prob = .95)
coeff_int3 <- posterior_interval(fit_gam_bayes_pois_best, prob = .99)
coeff_int4 <- posterior_interval(fit_gam_bayes_pois_best, prob = .999)


coeff_int_tbb <- as_tibble(coeff_int1) %>%
  cbind(as_tibble(coeff_int2)) %>% 
  cbind(as_tibble(coeff_int3)) %>%
  cbind(as_tibble(coeff_int4)) %>%
  mutate(coeff = rownames(coeff_int1)) %>% 
  select(coeff,
         `0.05%`, `0.5%`, `2.5%`, `5%`,
         everything()) %>% 
  # `95%`, `97.5%`, `99.5%`) %>% 
  # select(coeff, everything()) %>% 
  mutate(signif_90 = as.logical((sign(`5%`)*sign(`95%`) + 1) / 2 ),
         signif_95 = as.logical((sign(`2.5%`)*sign(`97.5%`) + 1) / 2 ),
         signif_99 = as.logical((sign(`0.5%`)*sign(`99.5%`) + 1) / 2 ),
         signif_999 = as.logical((sign(`0.05%`)*sign(`99.95%`) + 1) / 2 )) %>% 
  mutate(mean = coef_est)


coeff_int_tbb %>% 
  mutate(signif = ifelse(signif_999, "***",
                         ifelse(signif_99, "**",
                                ifelse(signif_95, "*",
                                       ifelse(signif_90, ".",
                                              ""))))) %>% 
  # select(-signif_90, -signif_95, -signif_99) %>% 
  select(coeff, `2.5%`, mean, `97.5%`, signif) %>% 
  knitr::kable(digits = 2)



# plot(fit_gam_bayes_pois_best$jam)
# plot(fit_gam_bayes_pois_best$jam)

plot_nonlinear(fit_gam_bayes_pois_best)

par(mfrow = c(1,2))
plot(fit_gam_bayes_pois_best$jam, shade = TRUE)
par(mfrow = c(1,1))


# Model checking best ####


# Overlay plot
y_rep <- posterior_predict(fit_gam_bayes_pois_best, draws = ITER/2)
y_rep
str(y_rep)
dim(y_rep)

ppc_dens_overlay(mort$tot.mort, y_rep)


# Stats plot

# Mean
ppc_stat(
  y = mort$tot.mort,
  yrep = y_rep,
  stat = 'mean'
)

ppc_stat_grouped(
  y = mort$tot.mort,
  yrep = y_rep,
  group = mort$year,
  stat = 'mean'
)

# Standard deviation
ppc_stat(
  y = mort$tot.mort,
  yrep = y_rep,
  stat = 'sd'
)

ppc_stat_grouped(
  y = mort$tot.mort,
  yrep = y_rep,
  group = mort$year,
  stat = 'sd'
)

# !!! Qui c'è qualcosa di strano

ppc_intervals(
  y = mort$tot.mort,
  yrep = y_rep,
  x = mort$day.of.year
)

ppc_intervals_grouped(
  y = mort$tot.mort,
  yrep = y_rep,
  x = mort$day.of.year,
  group = mort$year,
  facet_args = list(nrow = 2))


res <- (apply(y_rep, FUN = mean, MARGIN = 2) - mort$tot.mort) / apply(y_rep, FUN = sd, MARGIN = 2)

plot(res)

























