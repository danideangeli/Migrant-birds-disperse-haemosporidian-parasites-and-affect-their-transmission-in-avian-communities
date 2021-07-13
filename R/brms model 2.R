library(tidyverse)
library(ggplot2)
library(brms)
library(readxl)
library(ggpubr)
library(ape)

setwd("~/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas")

dados1 <- read_excel("PD7.xlsx", sheet = "Objetivo 2.txt", 
                    col_types = c("text", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "text", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", "numeric", 
                                  "numeric", 
                                  "numeric"))
dados1 <- filter(dados1, RiquezadeParasitos != "NA")

hist(dados1$RiquezadeParasitos)
hist(dados1$Prevalencia)
hist(dados1$RiquezadeHospedeiros)
hist(dados1$migrantes)
hist(dados1$Temp)
hist(dados1$Prec)
hist(dados1$n_migrants)

teste <- brm(RiquezadeParasitos~offset(TotaldeAves) + Prevalencia + (1|Bioma) + (1|Localidade),
             data = dados1, 
             family = negbinomial(),
             chain = 4, iter = 4000)
summary(teste)

teste2 <- brm(RiquezadeParasitos~offset(TotaldeAves) + log1p(Temp) + (1|Bioma) + (1|Localidade),
             data = dados1, 
             family = negbinomial(),
             chain = 4, iter = 4000)
summary(teste2)

teste3 <- brm(RiquezadeParasitos~offset(TotaldeAves) + Prec + (1|Bioma) + (1|Localidade),
             data = dados1, 
             family = negbinomial(),
             chain = 4, iter = 4000)

summary(teste3)

teste4 <- brm(RiquezadeParasitos~offset(TotaldeAves) + RiquezadeHospedeiros + (1|Bioma) + (1|Localidade),
              data = dados1, 
              family = negbinomial(),
              chain = 4, iter = 4000) 

summary(teste4)


variaveis <- bf(RiquezadeParasitos~offset(TotaldeAves) + abundance + migrantes +  
                  RiquezadeHospedeiros + (1|Bioma) + (1|Localidade), family = negbinomial())

prior <- get_prior(variaveis, data = dados1)
prior

dados1$abundance <- dados1$abundance * 100

model <- brm(RiquezadeParasitos~offset(TotaldeAves) + abundance + RiquezadeHospedeiros +
               (1|Bioma) + (1|Localidade),
             data = dados1, 
             family = negbinomial(),
             chain = 4, iter = 4000,
             prior = c(
               prior(student_t(3, 2.7, 2.5), "Intercept"),
               prior(student_t(3, 0, 2.5), "sd" ),
               prior(gamma(0.01, 0.01), "shape")
             )
)

summary(model)
plot(model, N = 4, ask = FALSE)

plot1 <- plot(conditional_effects(model), points = FALSE, theme = a)
a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot2 <- plot1$abundance + labs(x = "Proportion of migrant individuals", y = "Haemosporidian richness") #+ coord_cartesian(ylim = c(0, 15)) #caption = "Plasmodium")
+ geom_boxplot(aes(color = abundance)) 
plot2

plot2a <- plot1$migrantes + labs(x = "Proportion of migrant species", y = "Haemosporidian richness") #+ coord_cartesian(ylim = c(0, 15)) #caption = "Plasmodium")
+ geom_boxplot(aes(color = abundance)) 
plot2a

res1 <- residuals(model)
res1 <- as.data.frame(res1)
dados1$residuals <- res1$Estimate
dados1$residuals <- as.numeric(dados1$residuals)

dados1a <- as.matrix(dist(cbind(dados1$Longitude, dados1$Latitude)))
dados1b <- 1/dados1a
diag(dados1b) <- 0
dados1b <-ifelse(dados1b=="Inf",0,dados1b)

dados1b[1:5, 1:5]

Moran.I(dados1$residuals, dados1b, na.rm = TRUE)

dados1$PrevalenceP <- dados1$PrevalenceP * 100
dados1$PrevalenceH <- dados1$PrevalenceH * 100



modelP <- brm(RiquezaP~offset(TotaldeAves) + abundance + RiquezadeHospedeiros +
                (1|Bioma) + (1|Localidade),
              data = dados1, 
              family = negbinomial(),
              chain = 4, iter = 4000,
              prior = c(
                prior(student_t(3, 2.7, 2.5), "Intercept"),
                prior(student_t(3, 0, 2.5), "sd" ),
                prior(gamma(0.01, 0.01), "shape")
              )
)

summary(modelP)
parameters::p_value(modelP)
plot(modelP, N = 4, ask = FALSE)

plot3 <- plot(conditional_effects(modelP), points = FALSE, theme = a) 
a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot4 <- plot3$abundance + labs(x = "Proportion of migrant individuals", y = "Plasmodium richness") #caption = "Plasmodium")
+ geom_boxplot(aes(color = abundance))
plot4

plot4 <- plot3$migrantes + labs(x = "Proportion of migrant species", y = "Plasmodium richness") #caption = "Plasmodium")
+ geom_boxplot(aes(color = abundance))
plot4

res2 <- residuals(modelP)
res2 <- as.data.frame(res2)
dados1$residuals <- res2$Estimate
dados1$residuals <- as.numeric(dados1$residuals)

dados1a <- as.matrix(dist(cbind(dados1$Longitude, dados1$Latitude)))
dados1b <- 1/dados1a
diag(dados1b) <- 0
dados1b <-ifelse(dados1b=="Inf",0,dados1b )

dados1b[1:5, 1:5]

Moran.I(dados1$residuals, dados1b, na.rm = TRUE)


modelH <- brm(RiquezaH~offset(TotaldeAves) + abundance + RiquezadeHospedeiros +
                          (1|Bioma) + (1|Localidade),
                        data = dados1, 
                        family = negbinomial(),
                        chain = 4, iter = 4000,
                        prior = c(
                          prior(student_t(3, 2.7, 2.5), "Intercept"),
                          prior(student_t(3, 0, 2.5), "sd" ),
                          prior(gamma(0.01, 0.01), "shape")
                        )
)


summary(modelH)
parameters::p_value(modelH)
plot(modelH, N = 4, ask = FALSE)

plot5 <- plot(conditional_effects(modelH), points = FALSE, theme = a) 
a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot6 <- plot5$migrantes + labs(x = "Proportion of migrant individuals", y = "Haemoproteus richness")#, caption = "Haemoproteus")
+ geom_boxplot(aes(color = abundance))
plot6

res3 <- residuals(modelH)
res3 <- as.data.frame(res3)
dados1$residuals <- res3$Estimate
dados1$residuals <- as.numeric(dados1$residuals)

dados1a <- as.matrix(dist(cbind(dados1$Longitude, dados1$Latitude)))
dados1b <- 1/dados1a
diag(dados1b) <- 0
dados1b <-ifelse(dados1b=="Inf",0,dados1b )

dados1b[1:5, 1:5]

Moran.I(dados1$residuals, dados1b, na.rm = TRUE)

save.image("Obj2AOFFSETB.RData")
