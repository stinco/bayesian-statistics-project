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



# GAM


fit_gam_noday <- gam(tot.mort ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP),
                 family = gaussian,
                 data = mort)

summary(fit_gam_noday)

par(mfrow = c(2,2))
plot(fit_gam_noday,
     residuals = TRUE, shade = TRUE)
par(mfrow = c(1,1))


fit_gam_noday_2 <- gam(tot.mort ~ s(mean.temp, rel.humid) + s(SO2_log) + s(TSP),
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



fit_gam_noday_3 <- gam(tot.mort ~ s(mean.temp, rel.humid) + SO2_log + s(TSP),
                       family = gaussian,
                       data = mort)

summary(fit_gam_noday_3)

par(mfrow = c(1,2))
plot(fit_gam_noday_3,
     shade = TRUE, scheme = 2)
par(mfrow = c(1,1))


fit_gam_noday_4 <- gam(tot.mort ~ s(mean.temp, rel.humid) + s(SO2_log, TSP),
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

fit_gam_int <- gam(tot_mort_prob ~ s(day.of.year, mean.temp) + factor(year) + factor(weekend),
    family = gaussian, data = mort)
summary(fit_gam_int)

# plot(fit_gam_int)

# par(mfrow = c(1,2))
# plot(fit_gam_int, scheme=1)
# plot(fit_gam_int, scheme=2)
# par(mfrow = c(1,1))

plot(fit_gam_int, scheme=2)


par(mfrow = c(2,2))
plot(fit_gam_int, scheme=2,
     residuals = TRUE, all.terms = TRUE, shade = TRUE)
par(mfrow = c(1,1))


# Compare models with and without interaction


AIC(fit_gam_best) # AIC più basso
AIC(fit_gam_int)


plot(mort$SO2_log,
     fit_gam_best$residuals)



gam(tot_mort_prob ~ s(day.of.year, mean.temp) + factor(year) + factor(weekend),
    family = gaussian, data = mort) %>% 
  summary()




layout(matrix(c(1,2,3,3), 2, 2))
plot(fit_gam_best, residuals = TRUE, shade = TRUE)
plot(fit_gam_int, scheme = 2)
par(mfcol = c(1,1))



# How to manually make graphs
# https://stats.stackexchange.com/questions/166553/creating-marginal-plots-when-using-mgcv-gam-package-in-r

# mort
# 
# doy_dat <- tibble(day.of.year = seq(min(mort$day.of.year), max(mort$day.of.year), length.out = 100),
#                   mean.temp = mean(mort$mean.temp),
#                   year = 0,
#                   weekend = FALSE
#                   )
# 
# doy_fit1 <- predict(fit_gam_best, newdata = doy_dat, type = "response", se = TRUE) 
# doy_fit2 <- predict(fit_gam_int, newdata = doy_dat, type = "response", se = TRUE) 
# 
# par(mfrow = c(2,2))
# plot(fit_gam_best, residuals = TRUE, shade = TRUE)
# 
# plot(tot.mort ~ day.of.year, data = mort, pch = ".")
# lines(doy_dat$day.of.year, doy_fit1$fit) 
# 
# plot(tot.mort ~ day.of.year, data = mort, pch = ".")
# lines(doy_dat$day.of.year, doy_fit2$fit) 






# grid_doy <- seq(min(mort$day.of.year), max(mort$day.of.year), length.out = 100)
grid_doy <- 1:365
grid_temp <- seq(min(mort$mean.temp), max(mort$mean.temp), length.out = 100)

grid <- expand.grid(grid_doy, grid_temp)
names(grid) <- c("day.of.year", "mean.temp")

dat_grid <- as_tibble(grid)

dat_grid <- dat_grid %>% 
  mutate(year = 0,
         weekend = FALSE)

dat_grid_fit_best <- predict(fit_gam_best, newdata = dat_grid, type = "response", se = TRUE)
dat_grid_fit_int <- predict(fit_gam_int, newdata = dat_grid, type = "response", se = TRUE)


# filled.contour(x = grid_doy,
#                y = grid_temp,
#                z = matrix(dat_grid_fit$fit,
#                           ncol= 100))

# contour(x = grid_doy,
#         y = grid_temp,
#         z = matrix(dat_grid_fit$fit,
#                    ncol = 100),
#         add = T)

# points(mean.temp ~ day.of.year, data = mort, pch = ".")



# lim = c(min(dat_grid_fit_best$fit,
#             dat_grid_fit_int$fit),
#         max(dat_grid_fit_best$fit,
#             dat_grid_fit_int$fit))
# 
# lim = c(10, 40)


gam_temp_doy <- gam(formula = mean.temp ~ s(day.of.year),
                    data = mort)

# plot(gam_temp_doy)


alpha = 4

dat_grid <- dat_grid %>% 
  mutate(cloud_lw = predict(gam_temp_doy, data.frame(day.of.year)) - alpha*sqrt(gam_temp_doy$sig2),
         cloud_up = predict(gam_temp_doy, data.frame(day.of.year)) + alpha*sqrt(gam_temp_doy$sig2),
         cloud = (cloud_lw < mean.temp & mean.temp < cloud_up))

dat_grid_1 <- dat_grid %>% 
  mutate(fit = ifelse(cloud,
                      dat_grid_fit_best$fit,
                      NA))
dat_grid_2 <- dat_grid %>% 
  mutate(fit = ifelse(cloud,
                      dat_grid_fit_int$fit,
                      NA))

lim <- c(min(dat_grid_1$fit,
             dat_grid_2$fit,
             na.rm = T),
         max(dat_grid_1$fit,
             dat_grid_2$fit,
             na.rm = T))

p1 <- dat_grid_1 %>% 
  ggplot(aes(x = day.of.year, y = mean.temp)) +
  geom_tile(aes(fill = fit)) +
  geom_contour(aes(z = fit),
               color = "white", alpha = 0.5) +
  scale_fill_distiller(palette="Spectral", na.value="white",
                       limits = lim) +
  geom_point(data = mort,
             aes(x = day.of.year, y = mean.temp),
             size = .5) +
  labs(title = "Model without interaction")

p2 <- dat_grid_2 %>% 
  ggplot(aes(x = day.of.year, y = mean.temp)) +
  geom_tile(aes(fill = fit)) +
  geom_contour(aes(z = fit),
               color = "white", alpha = 0.5) +
  scale_fill_distiller(palette="Spectral", na.value="white",
                       limits = lim) +
  geom_point(data = mort,
             aes(x = day.of.year, y = mean.temp),
             size = .5) +
  labs(title = "Model with interaction")

grid.arrange(p1,p2,
             ncol = 1)






