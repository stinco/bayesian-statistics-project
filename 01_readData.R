#####
# 01. Read data
# 5/06/2019
#####

# Libraries ####

library(rstan)
library(rstanarm)
library(GGally)
library(arm)
library(gridExtra)
library(tidyverse)

# Problem B: Short-term effect of air pollution on mortality ####

# install.packages("SemiPar")
library(SemiPar)

data(milan.mort)

mort <- as_tibble(milan.mort)
rm(milan.mort)

summary(mort)

mort

ggpairs(mort,
        lower = list(continuous = wrap("points", alpha = 0.3, size=.1)),
        upper = list(combo = wrap("box", outlier.size = 0.1)),
        title = "")

pairs(mort, pch = ".")



# Problem C: Impact of UV radiation exposure on melanoma mortality ####

# install.packages("mlmRev")
library(mlmRev)

data(Mmmec)

mme <- as_tibble(Mmmec)


mme %>% 
  select(1:3) %>% 
  unique()

