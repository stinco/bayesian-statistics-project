#####
# 3. Autoregressive effect
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

install.packages("plotly")

# Autoregressive effect ####

acf(mort$tot.mort,
    lag.max = 30)
pacf(mort$tot.mort,
    lag.max = 30)


acf(mort$resp.mort,
    lag.max = 30)
pacf(mort$resp.mort,
     lag.max = 30)


# Time effect ####

mort <- mort %>% 
  mutate(day.of.year = day.num %% 365,
         year = day.num %/% 365) %>% 
  select(day.num, day.of.year, year, day.of.week, holiday,
         mean.temp, rel.humid, SO2, SO2_log, TSP,
         everything())


ggplot(data = mort,
       aes(x = day.num, y = tot.mort)) +
  geom_point(aes(col = factor(year)),
             size = 1, alpha = .5,) +
  geom_smooth(method = "loess", span = .1) +
  scale_x_continuous(breaks = seq(0, 3650, by = 365))


p <- mort %>% 
  filter(year<10) %>% 
  ggplot(aes(x = day.of.year, y = tot.mort,
           col = factor(year))) +
  # geom_point(size = 1, alpha = .1,) +
  geom_smooth(method = "loess", se = F) +
  scale_x_continuous(breaks = seq(0, 365, by = 90)) +
  scale_color_brewer(palette = "RdBu")

ggplotly(p)


p <- mort %>% 
  filter(year<10) %>% 
  ggplot(aes(x = day.of.year, y = resp.mort,
             col = factor(year))) +
  # geom_point(size = 1, alpha = .1,) +
  geom_smooth(method = "loess", se = F) +
  scale_x_continuous(breaks = seq(0, 365, by = 90)) +
  scale_color_brewer(palette = "RdBu")

ggplotly(p)



# Add exposition ####

# Popolazione 1971: 
# Popolazione 1981: 1.604.844	
# Popolazione 1991: 1.369.295
# Source
# https://www.tuttitalia.it/lombardia/18-milano/statistiche/censimenti-popolazione/

# La popolazione in quegli anni è diminuita
pop1 <- 1604844
pop2 <- 1369295

(pop1-pop2)/pop1


census <- read_csv("milanoCensus.csv",
         col_names = c("year", "date", "pop"))



census <- census %>% 
  mutate(pop_m = pop/1e6,
         pop_k = pop/1e3,
         interest = (year %in% c(1971, 1981, 1991)))


ggplot(data = census,
       aes(x = year, y = pop_m)) +
  geom_point(size = 2) +
  # geom_line() +
  geom_area(alpha = .2, color = "black", fill = "blue") +
  # geom_ribbon(aes(ymin = 0, ymax = pop_m)) +
  scale_y_continuous(limits = c(0, max(census$pop_m) + .1)) +
  scale_x_continuous(breaks = census$year,
                     minor_breaks = census$year)


# Interpolazione giorno per giorno ####

census <- census %>% 
  separate(date, into = c("day", "mese")) %>% 
  mutate(month = as.integer(factor(mese, levels = c("gennaio",
                                         "febbraio",
                                         "marzo",
                                         "aprile",
                                         "maggio",
                                         "giugno",
                                         "luglio",
                                         "agosto",
                                         "settembre",
                                         "ottobre",
                                         "novembre",
                                         "dicembre")))) %>% 
  mutate(date = ISOdate(year, month, day))
  


day_num <- (365*10+2)


pop_cens1 <- census$pop_m[census$year == 1981]
pop_cens2 <- census$pop_m[census$year == 1991]

delta_pop1 <- (census$pop_m[census$year == 1981] - census$pop_m[census$year == 1971]) /
  as.integer(difftime(census$date[census$year == 1981], census$date[census$year == 1971], units = "days"))

delta_pop2 <- (census$pop_m[census$year == 1991] - census$pop_m[census$year == 1981]) /
  as.integer(difftime(census$date[census$year == 1991], census$date[census$year == 1981], units = "days"))


mort

day_cens1 <- as.integer(difftime(census$date[census$year == 1981], ISOdate(1979, 12, 31), units = "days"))
day_cens2 <- as.integer(difftime(census$date[census$year == 1991], ISOdate(1979, 12, 31), units = "days"))


mort <- mort %>% 
  mutate(interp = day.num < day_cens1) %>% 
  mutate(pop_m = ifelse(interp,
                        pop_cens1 - delta_pop1 * (day_cens1 - day.num),
                        pop_cens1 + delta_pop2 * (day.num - day_cens1)
                        ))




ggplot(data = mort,
       aes(x = day.num, y = pop_m)) +
  # geom_point(size = .5) +
  geom_line() +
  # geom_area(alpha = .2, color = "black", fill = "blue") +
  scale_y_continuous(limits = c(0, max(census$pop_m) + 100))



# mort$pop_m[2] - mort$pop_m[1]
# mort$pop_m[nrow(mort)] - mort$pop_m[nrow(mort)-1]
# 
# delta_pop1
# delta_pop2


mort <- mort %>% 
  mutate(tot_mort_prob = tot.mort / pop_m,
         resp_mort_prob = resp.mort / pop_m)





ggplot(data = mort,
       aes(x = day.num, y = tot_mort_prob)) +
  geom_point(aes(col = factor(year)),
             size = 1, alpha = .5,) +
  geom_smooth(method = "loess", span = .1) +
  scale_x_continuous(breaks = seq(0, 3650, by = 365)) +
  scale_color_brewer(palette = "RdBu")
  # scale_color_brewer(palette = "Spectral")



p <- mort %>% 
  filter(year<10) %>% 
  ggplot(aes(x = day.of.year, y = tot_mort_prob,
             col = factor(year))) +
  # geom_point(size = 1, alpha = .1,) +
  geom_smooth(method = "loess", se = F) +
  scale_x_continuous(breaks = seq(0, 365, by = 90)) +
  scale_color_brewer(palette = "RdBu")

ggplotly(p)


p <- mort %>% 
  filter(year<10) %>% 
  ggplot(aes(x = day.of.year, y = resp_mort_prob,
             col = factor(year))) +
  # geom_point(size = 1, alpha = .1,) +
  geom_smooth(method = "loess", se = F) +
  scale_x_continuous(breaks = seq(0, 365, by = 90)) +
  scale_color_brewer(palette = "RdBu")

ggplotly(p)




