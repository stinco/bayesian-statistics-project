#####
# 7. Bayesian models for mort_tot_prob
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
ITER = 500

# Model for mort  without interaction (best) ####

# # Model fitting
# fit_gam_bayes_best <- stan_gamm4(tot_mort_prob ~ s(mean.temp) + s(day.of.year) + factor(year) + factor(weekend),
#                                  family = gaussian, data = mort,
#                                  chains = CHAINS, iter = ITER, seed = SEED, cores = CORES)
# # The default prior for coefficients is:
# # normal(location = 0, scale = NULL)
# # that means an improper prior
# # it is a non-informative prior


print(fit_gam_bayes_best)
summary(fit_gam_bayes_best)

plot(fit_gam_bayes_best)
plot(fit_gam_bayes_best,
     prob = c(.5,.99))

plot(fit_gam_bayes_best, pars = str_c("factor(year)", 1:9)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-2.2, 2.2)) #+
  # labs(y = "Variables") +
  # theme(axis.title.y = element_text(family = "sans", size = 15, margin=margin(0,10,0,0))) +
  # theme(plot.margin = margin(.5, .5, .5, 2, "cm"),
  #       axis.text = element_text(colour = "red", size = rel(1.5),
  #                                margin = margin(t = 0, r = 100, b = 0, l = 0, unit = "pt")))


p1 <- plot(fit_gam_bayes_best, pars = c(str_c("factor(year)", 1:9), "factor(weekend)TRUE"),
           prob = c(.95,.99)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-2.2, 2.2)) #+
  # theme(plot.margin = margin(.5, .5, .5, 2, "cm"))

p2 <- plot(fit_gam_bayes_best, pars = str_c("s(mean.temp).", 1:9),
           prob = c(.95,.99)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-90, 90))

p3 <- plot(fit_gam_bayes_best, pars = str_c("s(day.of.year).", 1:9),
           prob = c(.95,.99)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-70, 70))

grid.arrange(p1, p2, p3,
             ncol = 1)


# Table with estimated coefficients
coef_est <- as.matrix(fit_gam_bayes_best) %>% 
  apply(FUN = mean, MARGIN = 2)

as_tibble(as.matrix(fit_gam_bayes_best))

coeff_int1 <- posterior_interval(fit_gam_bayes_best, prob = .90)
coeff_int2 <- posterior_interval(fit_gam_bayes_best, prob = .95)
coeff_int3 <- posterior_interval(fit_gam_bayes_best, prob = .99)
coeff_int4 <- posterior_interval(fit_gam_bayes_best, prob = .999)


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



# plot(fit_gam_bayes_best$jam)
# plot(fit_gam_bayes_best$jam)

plot_nonlinear(fit_gam_bayes_best)

par(mfrow = c(1,2))
plot(fit_gam_bayes_best$jam, shade = TRUE)
par(mfrow = c(1,1))


# Model checking best ####


# Overlay plot
y_rep <- posterior_predict(fit_gam_bayes_best, draws = ITER/2)
y_rep
str(y_rep)
dim(y_rep)

ppc_dens_overlay(mort$tot_mort_prob, y_rep)


# Stats plot

# Mean
ppc_stat(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  stat = 'mean'
  )

ppc_stat_grouped(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  group = mort$year,
  stat = 'mean'
  )

# Standard deviation
ppc_stat(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  stat = 'sd'
  )

ppc_stat_grouped(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  group = mort$year,
  stat = 'sd'
)

# !!! Qui c'è qualcosa di strano

ppc_intervals(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  x = mort$day.of.year
)

ppc_intervals_grouped(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  x = mort$day.of.year,
  group = mort$year,
  facet_args = list(nrow = 2))


# Model with interaction (int) ####

# # Model fitting
# fit_gam_bayes_int <- stan_gamm4(tot_mort_prob ~ s(day.of.year, mean.temp) + factor(year) + factor(weekend),
#                                 family = gaussian, data = mort,
#                                 chains = CHAINS, iter = ITER, seed = SEED, cores = CORES)


print(fit_gam_bayes_int)
summary(fit_gam_bayes_int)

plot(fit_gam_bayes_int)
# plot(fit_gam_bayes_int,
#      prob = c(.5,.99))

p1 <- plot(fit_gam_bayes_int, pars = c(str_c("factor(year)", 1:9), "factor(weekend)TRUE"),
           prob = c(.95,.99)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-2.2, 2.2)) #+
# theme(plot.margin = margin(.5, .5, .5, 2, "cm"))

p2 <- plot(fit_gam_bayes_int, pars = str_c("s(day.of.year,mean.temp).", 1:29),
           prob = c(.95,.99)) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-50, 50))

grid.arrange(p1, p2,
             ncol = 1)



coef_est <- as.matrix(fit_gam_bayes_int) %>% 
  apply(FUN = mean, MARGIN = 2)

as_tibble(as.matrix(fit_gam_bayes_int))

coeff_int1 <- posterior_interval(fit_gam_bayes_int, prob = .90)
coeff_int2 <- posterior_interval(fit_gam_bayes_int, prob = .95)
coeff_int3 <- posterior_interval(fit_gam_bayes_int, prob = .99)
coeff_int4 <- posterior_interval(fit_gam_bayes_int, prob = .999)


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



# plot(fit_gam_bayes_best$jam)
# plot(fit_gam_bayes_int$jam)

plot_nonlinear(fit_gam_bayes_int)

layout(matrix(c(1,2,3,3), 2, 2))
plot(fit_gam_bayes_best$jam, shade = TRUE)
plot(fit_gam_bayes_int$jam, scheme = 2)
par(mfcol = c(1,1))


# Model comparison with 3D plot

grid_doy <- 1:365
grid_temp <- seq(min(mort$mean.temp), max(mort$mean.temp), length.out = 100)

grid <- expand.grid(grid_doy, grid_temp)
names(grid) <- c("day.of.year", "mean.temp")

dat_grid <- as_tibble(grid)

dat_grid <- dat_grid %>% 
  mutate(year = 0,
         weekend = FALSE)

dat_grid_fit_best <- predict(fit_gam_bayes_best, newdata = dat_grid)
dat_grid_fit_int <- predict(fit_gam_bayes_int, newdata = dat_grid)

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
                      dat_grid_fit_best,
                      NA))
dat_grid_2 <- dat_grid %>% 
  mutate(fit = ifelse(cloud,
                      dat_grid_fit_int,
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





# Model checking int ####


# Overlay plot
y_rep <- posterior_predict(fit_gam_bayes_int, draws = ITER/2)
y_rep
str(y_rep)
dim(y_rep)

ppc_dens_overlay(mort$tot_mort_prob, y_rep)


# Stats plot

# Mean
ppc_stat(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  # group = pest_data$building_id,
  stat = 'mean'
)

ppc_stat_grouped(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  group = mort$year,
  stat = 'mean'
)

# Standard deviation
ppc_stat(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  # group = pest_data$building_id,
  stat = 'sd'
)

ppc_stat_grouped(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  group = mort$year,
  stat = 'sd'
)

# !!! Qui c'è qualcosa di strano
# Non c'è omoschedasticit tra i diversi anni

ppc_intervals(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  x = mort$day.of.year
)

ppc_intervals_grouped(
  y = mort$tot_mort_prob,
  yrep = y_rep,
  x = mort$day.of.year,
  group = mort$year,
  facet_args = list(nrow = 2)
)


# Model comparison (with and without interaction) ####

loo_best <- loo(fit_gam_bayes_best)
loo_int <- loo(fit_gam_bayes_int)

compare(loo_best,
        loo_int)
# È meglio quello senza interazione


# waic_best <- waic(fit_gam_bayes_best)
# waic_int <- waic(fit_gam_bayes_int)
# 
# compare(waic_best,
#         waic_int)



