library(ape)
library(tidyverse)
library(devtools) # load package
#devtools::install_github("r-lib/pkgbuild") # install updated version of pkgbuild from GitHub
library(pkgbuild) # load package
find_rtools() # should be TRUE, assuming you have Rtools 3.5

A <- ape::vcv.phylo(myphy)
?vcv.phylo

dados$Latitude <- as.numeric(dados$Latitude)
dados$Longitude <- as.numeric(dados$Longitude)

coords <- data.frame(dados$Latitude, dados$Longitude)
dist <- as.matrix(dist(coords))

library(brms)

variaveis <- bf(RiquezadeParasitos~abundance + RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants
                 + Temp + Prec, family = student())

prior <-  get_prior(variaveis, data = dados)
prior
make_stancode(variaveis, data = dados, 
              prior = prior)

model <- brm(
  RiquezadeParasitos~abundance + RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants
  + Temp + Prec, data = dados, family = student(),
  autocor = cor_car(dist),
  prior = c(
    prior(student_t(3, 14, 12), "Intercept"),                                                
    prior(gamma(2, 0.1),"nu"),                                                 
    prior(student_t(3, 0, 12),"sigma")))

str(dados)
summary(model)

plot(model, N = 4, ask = FALSE)

plot(conditional_effects(model), points = TRUE) 

hyp <- "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sigma^2) = 0"
(hyp <- hypothesis(model, hyp, class = NULL))

plot(hyp)