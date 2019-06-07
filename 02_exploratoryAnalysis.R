#####
# 2. Exploratory Descriptive Analysis
# 5/06/2019
#####

# Libraries ####

library(rstan)
library(rstanarm)
library(GGally)
library(arm)
library(gridExtra)
library(tidyverse)


# Death trend ####

theme_set(theme_bw())

data(milan.mort)

mort <- as_tibble(milan.mort)
rm(milan.mort)

summary(mort)

# plot(mort$day.num, mort$tot.mort)

p1 <- ggplot(data = mort,
       aes(x = day.num, y = tot.mort)) +
  geom_point(alpha = .5, size = .5) +
  geom_smooth(method = "loess", span = .1) +
  scale_x_continuous(breaks = seq(0, 3650, by = 365))


p2 <- ggplot(data = mort,
       aes(x = day.num, y = resp.mort)) +
  geom_point(alpha = .5, size = .5) +
  geom_smooth(method = "loess", span = .1) +
  scale_x_continuous(breaks = seq(0, 3650, by = 365)) +
  scale_y_continuous(breaks = seq(0, max(mort$resp.mort)+1, by = 2))

grid.arrange(p1, p2,
             ncol = 1)

hist(mort$tot.mort, breaks = 30)
barplot(table(mort$resp.mort))

plot(mort$tot.mort, mort$resp.mort)

ggplot(data = mort,
       aes(x = tot.mort, y = resp.mort)) +
  geom_point(alpha = .05) +
  scale_y_continuous(breaks = seq(0, max(mort$resp.mort)+1, by = 2)) +
  geom_smooth(method = "lm")


# Relationship between tot.mort and resp.mort ####

mort <- mort %>% 
  mutate(resp_rate = resp.mort / tot.mort)

ggplot(data = mort,
       aes(x = day.num, y = resp_rate)) +
  geom_point(alpha = .5, size = .5) +
  geom_smooth(method = "loess", span = .1) +
  scale_x_continuous(breaks = seq(0, 3650, by = 365))


gg <- mort %>% 
  dplyr::select(tot.mort, resp.mort, resp_rate) %>% 
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.1, size=1)))

gg[2,2] <- ggplot(data = mort,
                  aes(x = resp.mort)) +
  geom_bar(color = "black", fill = "white")
  
gg[2,1] <- gg[2,1] +
  geom_smooth(method = "lm")

gg[3,2] <- gg[3,2] +
  geom_smooth(method = "loess")

gg
         

# Popolazione 1981: 1.604.844	
# Popolazione 1991: 1.369.295
# Source
# https://www.tuttitalia.it/lombardia/18-milano/statistiche/censimenti-popolazione/

# La popolazione in quegli anni è diminuita
pop1 <- 1604844
pop2 <- 1369295

(pop1-pop2)/pop1



# Death weekly effect ####

mort <- mort %>% 
  mutate(day.of.week = factor(day.of.week))

ggplot(data = mort,
       aes(x = day.of.week, y = tot.mort)) +
  geom_boxplot()

ggplot(data = mort,
       aes(x = day.of.week, y = resp.mort)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, max(mort$resp.mort)+1, by = 2))


# Effect of air pollution ####

hist(mort$SO2, breaks = 50)
hist(log(mort$SO2 + 30), breaks = 50)

summary(mort$SO2)

hist(mort$TSP, breaks = 50)

hist(mort$mean.temp, breaks = 50)


mort <- mort %>% 
  mutate(SO2_log = log(mort$SO2 + 30))

gg <- mort %>% 
  dplyr::select(mean.temp, SO2_log, TSP, tot.mort, resp.mort) %>% 
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.1, size=1)))

gg[5,5] <- ggplot(data = mort,
                  aes(x = resp.mort)) +
  geom_bar(color = "black", fill = "white")

for(i in 2:5){
  for(j in 1:(i-1)){
    gg[i,j] <- gg[i,j] +
      geom_smooth(method = "loess")
  }
}

gg



# Add humidity
gg <- mort %>% 
  dplyr::select(mean.temp, rel.humid, SO2_log, TSP, tot.mort, resp.mort) %>% 
  ggpairs(lower = list(continuous = wrap("points", alpha = 0.1, size=1)))

gg[6,6] <- ggplot(data = mort,
                  aes(x = resp.mort)) +
  geom_bar(color = "black", fill = "white")

for(i in 2:6){
  for(j in 1:(i-1)){
    gg[i,j] <- gg[i,j] +
      geom_smooth(method = "loess")
  }
}

gg





