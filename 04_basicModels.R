#####
# 4. Basic models
# 6/06/2019
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

# Model on tot.mort ignoring time ####

mort

mort %>% 
  select(mean.temp, rel.humid, SO2_log, TSP, tot.mort)

fit1 <- lm(tot.mort ~ mean.temp + rel.humid + SO2_log + TSP,
           data = mort)

summary(fit1)



fit_gam <- lm(tot.mort ~ mean.temp + rel.humid + SO2_log + TSP,
              data = mort)



summary(mort)



# GAM


fit_gam_1 <- gam(tot.mort ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP),
                 family = gaussian,
                 data = mort)

summary(fit_gam_1)





# Model on tot.mort taking into account time ####

fit_gam_time_1 <- gam(tot.mort ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year),
                      family = gaussian, data = mort)

summary(fit_gam_time_1)


fit_gam_time_2 <- gam(tot.mort ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + s(day.num),
                      family = gaussian, data = mort)


summary(fit_gam_time_2)


gam(tot.mort ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year),
    family = gaussian, data = mort) %>% 
  summary()

gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year),
    family = gaussian, data = mort) %>% 
  summary()

gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year) + day.of.week,
    family = gaussian, data = mort) %>% 
  summary()

gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year) + weekend,
    family = gaussian, data = mort) %>% 
  summary()

# Il weekend la mortalità è più bassa
# Meno smog? Meno incidenti?


mort <- mort %>% 
  mutate(weekend = day.of.week %in% c(6,7)) %>% 
  select(day.num, day.of.year, year, day.of.week, weekend, holiday, everything())

gam(tot_mort_prob ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year) + holiday,
    family = gaussian, data = mort) %>% 
  summary()


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


# Provo a riprodurrei grafici
# Non funziona

str_detect(names(fit_gam_best$coefficients), "temp")
str_detect(names(fit_gam_best$coefficients), "day.of.year")

coef_temp <- fit_gam_best$coefficients[str_detect(names(fit_gam_best$coefficients), "temp")]

x_temp = seq(min(mort$mean.temp) - 10, max(mort$mean.temp) + 10, by = .1)

mat_temp <- matrix(c(x_temp, x_temp^2, x_temp^3, x_temp^4, x_temp^5, x_temp^6, x_temp^7, x_temp^8, x_temp^9),
       ncol = 9)

plot(x_temp, as.vector(mat_temp %*% matrix(coef_temp)),
     type = "l")


fit_gam_best$coefficients[str_detect(names(fit_gam_best$coefficients), "day.of.year")]



fit_gam_best_pois <- gam(tot_mort_prob ~ s(mean.temp) + s(day.of.year) + factor(year) + factor(weekend),
    family = poisson, data = mort)

par(mfrow = c(2,2))
plot(fit_gam_best_pois, residuals = TRUE, all.terms = TRUE, shade = TRUE)
par(mfrow = c(1,1))


# AIC(fit_gam_best)
# AIC(fit_gam_best_pois)




# Residuals

par(mfrow = c(1,2))

hist(fit_gam_best$residuals)


plot(fit_gam_best$fitted.values,
     fit_gam_best$residuals / sqrt(fit_gam_best$sig2))
abline(h = c(-2,2))

par(mfrow = c(1,1))


sum(abs(fit_gam_best$residuals / sqrt(fit_gam_best$sig2)) > 2) / length(fit_gam_best$residuals)


fit_gam_best$deviance
fit_gam_best$df.residual

AIC(fit_gam_best)


# Macro trends ####

# Pollution

ggplot(data = mort,
       aes(x = day.num, y = SO2_log)) +
  geom_point() +
  geom_smooth(method = "lm")
  
ggplot(data = mort,
       aes(x = day.num, y = TSP)) +
  geom_point() +
  geom_smooth(method = "lm")

# SO2 è andato diminuendo
# TSP è sostanzialmente costante


# Death
ggplot(data = mort,
       aes(x = day.num, y = tot.mort)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = mort,
       aes(x = day.num, y = resp.mort)) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(data = mort,
       aes(x = day.num, y = tot_mort_prob)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = mort,
       aes(x = day.num, y = resp_mort_prob)) +
  geom_point() +
  geom_smooth(method = "lm")




# Harvesting effect ####

acf(fit_gam_best$residuals,
    lag.max = 30)
pacf(fit_gam_best$residuals,
     lag.max = 30)


# Prove fit_gam_best ####

plot(fit_gam_best$linear.predictors)


fit_gam_best$smooth


fit_gam_best$smooth[[1]]



# Inserisco l'interazione tra mean.temp e day.of.year ####

fit_gam_int <- gam(tot_mort_prob ~ s(day.of.year, mean.temp) + factor(year) + weekend,
    family = gaussian, data = mort)
summary(fit_gam_int)

plot(fit_gam_int)


plot(fit_gam_int, scheme=1)

plot(fit_gam_int, scheme=2)



par(mfrow = c(1,2))
plot(fit_gam_int, scheme=1)
plot(fit_gam_int, scheme=2)
par(mfrow = c(1,1))




