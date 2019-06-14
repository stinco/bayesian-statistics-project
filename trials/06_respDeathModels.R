#####
# 6. Respiration death models
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


fit_gam_noday <- gam(resp.mort ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP),
                     family = poisson, offset = log(pop_m),
                     data = mort)

summary(fit_gam_noday)

par(mfrow = c(2,2))
# plot(fit_gam_noday,
#      residuals = TRUE, shade = TRUE)
plot(fit_gam_noday, shade = TRUE)
par(mfrow = c(1,1))


fit_gam_noday_2 <- gam(resp.mort ~ s(mean.temp, rel.humid) + s(SO2_log) + s(TSP),
                       family = poisson, offset = log(pop_m),
                       data = mort)

summary(fit_gam_noday_2)

# par(mfrow = c(2,2))
layout(matrix(c(1,1,2,3), 2, 2))
# plot(fit_gam_noday_2,
#      residuals = TRUE, shade = TRUE, scheme = 2)
plot(fit_gam_noday_2,
     shade = TRUE, scheme = 2)
par(mfrow = c(1,1))



fit_gam_noday_3 <- gam(resp.mort ~ s(mean.temp, rel.humid) + SO2_log + s(TSP),
                       family = poisson, offset = log(pop_m),
                       data = mort)

summary(fit_gam_noday_3)

par(mfrow = c(1,2))
plot(fit_gam_noday_3,
     shade = TRUE, scheme = 2)
par(mfrow = c(1,1))


fit_gam_noday_4 <- gam(resp.mort ~ s(mean.temp, rel.humid) + s(SO2_log, TSP),
                       family = poisson, offset = log(pop_m),
                       data = mort)

summary(fit_gam_noday_4)

par(mfrow = c(1,2))
plot(fit_gam_noday_4,
     shade = TRUE, scheme = 2)
par(mfrow = c(1,1))


AIC(fit_gam_noday)   # The best
AIC(fit_gam_noday_2)
AIC(fit_gam_noday_3)
AIC(fit_gam_noday_4)




fit_gam_noday_e1 <- gam(resp.mort ~ s(mean.temp) + s(rel.humid) + SO2_log + s(TSP),
                     family = poisson, offset = log(pop_m),
                     data = mort)

fit_gam_noday_e2 <- gam(resp.mort ~ s(mean.temp) + rel.humid + s(SO2_log) + s(TSP),
                        family = poisson, offset = log(pop_m),
                        data = mort)

fit_gam_noday_e3 <- gam(resp.mort ~ s(mean.temp) + rel.humid + SO2_log + s(TSP),
                        family = poisson, offset = log(pop_m),
                        data = mort)

fit_gam_noday_e4 <- gam(resp.mort ~ s(mean.temp) + rel.humid + SO2_log,
                        family = poisson, offset = log(pop_m),
                        data = mort)

AIC(fit_gam_noday_e1)
AIC(fit_gam_noday_e2)
AIC(fit_gam_noday_e3) # The best
AIC(fit_gam_noday_e4)

fit_resp_gam_noday_best <- fit_gam_noday_e3

summary(fit_resp_gam_noday_best)



par(mfrow = c(2,2))
plot(fit_resp_gam_noday_best,
     shade = TRUE, all.terms = TRUE)
par(mfrow = c(1,1))



# Residuals

par(mfrow = c(1,2))

hist(fit_resp_gam_noday_best$residuals)

plot(fit_resp_gam_noday_best$fitted.values,
     fit_resp_gam_noday_best$residuals / sqrt(fit_resp_gam_noday_best$sig2))
abline(h = c(-2,2))
par(mfrow = c(1,1))


sum(abs(fit_resp_gam_noday_best$residuals / sqrt(fit_resp_gam_noday_best$sig2)) > 2) / length(fit_resp_gam_noday_best$residuals)



# Model on resp.mort taking into account time ####

gam(resp.mort ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year),
    family = poisson, offset = log(pop_m), data = mort) %>% 
  summary()

gam(resp.mort ~ s(mean.temp) + s(SO2_log) + s(TSP) + s(day.of.year),
    family = poisson, offset = log(pop_m), data = mort) %>% 
  summary()

# gam(resp.mort ~ s(mean.temp) + s(SO2_log) + s(TSP) + s(day.of.year) + s(day.num),
#     family = poisson, offset = log(pop_m), data = mort) %>%
#   summary()

gam(resp.mort ~ s(mean.temp) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year),
    family = poisson, offset = log(pop_m), data = mort) %>% 
  summary()

gam(resp.mort ~ s(mean.temp) + s(SO2_log) + s(day.of.year) + factor(year),
    family = poisson, offset = log(pop_m), data = mort) %>% 
  summary()


# gam(resp.mort ~ s(mean.temp) + s(SO2_log) + s(day.of.year) + factor(year) + day.of.week,
#     family = poisson, offset = log(pop_m), data = mort) %>%
#   summary()



mort <- mort %>% 
  mutate(weekend = day.of.week %in% c(6,7)) %>% 
  select(day.num, day.of.year, year, day.of.week, weekend, holiday, everything())

# gam(resp.mort ~ s(mean.temp) + s(rel.humid) + s(SO2_log) + s(TSP) + s(day.of.year) + factor(year) + weekend,
#     family = poisson, offset = log(pop_m), data = mort) %>%
#   summary()


# gam(resp.mort ~ s(mean.temp) + s(SO2_log) + s(day.of.year) + factor(year) + holiday,
#     family = poisson, offset = log(pop_m), data = mort) %>% 
#   summary()


# Tolgo variabili che non hanno un effetto significativo



fit_gam_day_1 <- gam(resp.mort ~ s(mean.temp) + s(SO2_log) + s(day.of.year) + factor(year),
                     family = poisson, offset = log(pop_m), data = mort)

par(mfrow = c(2,2))
plot(fit_gam_day_1, residuals = TRUE, all.terms = TRUE, shade = TRUE)
par(mfrow = c(1,1))


fit_gam_day_2 <- gam(resp.mort ~ s(mean.temp) + SO2_log + s(day.of.year) + factor(year),
                     family = poisson, offset = log(pop_m), data = mort)

par(mfrow = c(2,2))
plot(fit_gam_day_2, residuals = TRUE, all.terms = TRUE, shade = TRUE)
par(mfrow = c(1,1))

AIC(fit_gam_day_1)
AIC(fit_gam_day_2) # Better



fit_resp_gam_best <- fit_gam_day_2

summary(fit_resp_gam_best)

par(mfrow = c(2,2))
plot(fit_resp_gam_best, residuals = TRUE, all.terms = TRUE, shade = TRUE)
par(mfrow = c(1,1))



# Con interazione

fit_resp_gam_int <- gam(resp.mort ~ s(day.of.year, mean.temp) + SO2_log + factor(year),
                   family = poisson, offset = log(pop_m), data = mort)
summary(fit_resp_gam_int)


# par(mfrow = c(2,2))
layout(matrix(c(1,2,1,3), 2, 2))
plot(fit_resp_gam_int, scheme=2,
     residuals = TRUE, all.terms = TRUE, shade = TRUE)
par(mfrow = c(1,1))

# Compare models with and without interaction

AIC(fit_resp_gam_best)
AIC(fit_resp_gam_int)  # The one with interaction is better

layout(matrix(c(1,2,3,3), 2, 2))
plot(fit_resp_gam_best, residuals = TRUE, shade = TRUE)
plot(fit_resp_gam_int, scheme = 2)
par(mfcol = c(1,1))



# Harvesting effect

acf(fit_resp_gam_int$residuals,
    lag.max = 30)
pacf(fit_resp_gam_int$residuals,
     lag.max = 30)


# grid_doy <- seq(min(mort$day.of.year), max(mort$day.of.year), length.out = 100)
grid_doy <- 1:365
grid_temp <- seq(min(mort$mean.temp), max(mort$mean.temp), length.out = 100)

grid <- expand.grid(grid_doy, grid_temp)
names(grid) <- c("day.of.year", "mean.temp")

dat_grid <- as_tibble(grid)

dat_grid <- dat_grid %>% 
  mutate(SO2_log = mean(mort$SO2_log),
         year = 0,
         pop_m = mean(mort$pop_m))

dat_grid_fit_best <- predict(fit_resp_gam_best, newdata = dat_grid, type = "response", se = TRUE)
dat_grid_fit_int <- predict(fit_resp_gam_int, newdata = dat_grid, type = "response", se = TRUE)



gam_temp_doy <- gam(formula = mean.temp ~ s(day.of.year),
                    data = mort)


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
               color = "white", alpha = 0.5, size = 1) +
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
               color = "white", alpha = 0.5, size = 1) +
  scale_fill_distiller(palette="Spectral", na.value="white",
                       limits = lim) +
  geom_point(data = mort,
             aes(x = day.of.year, y = mean.temp),
             size = .5) +
  labs(title = "Model with interaction")

grid.arrange(p1,p2,
             ncol = 1)




plot(resp_mort_prob ~ SO2_log,
     data = mort)

ggplot(data = mort,
       aes(x = SO2_log, y = resp_mort_prob)) +
  geom_point(alpha = .4) +
  geom_smooth()

ggplot(data = mort,
       aes(x = SO2_log, y = resp_mort_prob)) +
  geom_point(alpha = .3) +
  geom_smooth(se = F, size = 1.5, method = "lm") +
  geom_smooth(aes(color = SO2_log>3),
              se = F, size = 1.5, method = "lm")

# The 2 lines are basically the same
# There is no leverage effect for that point




