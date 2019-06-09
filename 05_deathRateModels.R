#####
# 5. Death rate models
# 10/06/2019
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

theme_set(theme_bw())

# Model on tot_mort_prob ignoring time ####

mort


fit_gam_noday <- gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP),
                     family = gaussian,
                     data = mort)

summary(fit_gam_noday)

par(mfrow = c(2,2))
plot(fit_gam_noday,
     residuals = TRUE, shade = TRUE)
par(mfrow = c(1,1))


fit_gam_noday_2 <- gam(tot_mort_prob ~ s(mean.temp, rel.humid) + s(SO2_log) + s(TSP),
                       family = gaussian,
                       data = mort)

summary(fit_gam_noday_2)

# par(mfrow = c(2,2))
layout(matrix(c(1,1,2,3), 2, 2))
# plot(fit_gam_noday_2,
#      residuals = TRUE, shade = TRUE, scheme = 2)
plot(fit_gam_noday_2,
     shade = TRUE, scheme = 2)
par(mfrow = c(1,1))



fit_gam_noday_3 <- gam(tot_mort_prob ~ s(mean.temp, rel.humid) + SO2_log + s(TSP),
                       family = gaussian,
                       data = mort)

summary(fit_gam_noday_3)

par(mfrow = c(1,2))
plot(fit_gam_noday_3,
     shade = TRUE, scheme = 2)
par(mfrow = c(1,1))


fit_gam_noday_4 <- gam(tot_mort_prob ~ s(mean.temp, rel.humid) + s(SO2_log, TSP),
                       family = gaussian,
                       data = mort)

summary(fit_gam_noday_4)

par(mfrow = c(1,2))
plot(fit_gam_noday_4,
     shade = TRUE, scheme = 2)
par(mfrow = c(1,1))


AIC(fit_gam_noday)
AIC(fit_gam_noday_2)
AIC(fit_gam_noday_3)
AIC(fit_gam_noday_4)



# Model on tot_mort_prob taking into account time ####

gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year),
    family = gaussian, data = mort) %>% 
  summary()

# gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + s(day.num),
#     family = gaussian, data = mort) %>% 
#   summary()

gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year),
    family = gaussian, data = mort) %>% 
  summary()

# gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year) + day.of.week,
#     family = gaussian, data = mort) %>% 
#   summary()


mort <- mort %>% 
  mutate(weekend = day.of.week %in% c(6,7)) %>% 
  select(day.num, day.of.year, year, day.of.week, weekend, holiday, everything())

gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year) + weekend,
    family = gaussian, data = mort) %>% 
  summary()

# Il weekend la mortalità è più bassa
# Meno smog? Meno incidenti?

# gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year) + holiday,
#     family = gaussian, data = mort) %>% 
#   summary()


# Tolgo variabili che non hanno un effetto significativo

gam(tot_mort_prob ~ s(mean.temp) + s(day.of.year) + factor(year) + weekend,
    family = gaussian, data = mort) %>% 
  summary()

fit_gam_best <- gam(tot_mort_prob ~ s(mean.temp) + s(day.of.year) + factor(year) + factor(weekend),
                    family = gaussian, data = mort)

summary(fit_gam_best)

par(mfrow = c(2,2))
plot(fit_gam_best, residuals = TRUE, all.terms = TRUE, shade = TRUE)
par(mfrow = c(1,1))

fit_gam_best$coefficients



# Con interazione

fit_gam_int <- gam(tot_mort_prob ~ s(day.of.year, mean.temp) + factor(year) + factor(weekend),
                   family = gaussian, data = mort)
summary(fit_gam_int)



# par(mfrow = c(2,2))
layout(matrix(c(1,2,1,3), 2, 2))
plot(fit_gam_int, scheme=2,
     residuals = TRUE, all.terms = TRUE, shade = TRUE)
par(mfrow = c(1,1))

# Compare models with and without interaction

AIC(fit_gam_best) # AIC più basso
AIC(fit_gam_int)


layout(matrix(c(1,2,3,3), 2, 2))
plot(fit_gam_best, residuals = TRUE, shade = TRUE)
plot(fit_gam_int, scheme = 2)
par(mfcol = c(1,1))









