library(readxl)
library(tidyverse)

dados <- read_excel("PD7.xlsx", sheet = "Objetivo 2.txt", 
                             col_types = c("text", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", "numeric", 
                                           "numeric", 
                                           "numeric"))
dados <- filter(dados, RiquezadeParasitos != "NA")

dados1 <- filter(dados1, RiquezadeParasitos != "NA")
data <- read_excel("PD7.xlsx", sheet = "Especie>10")

library(lme4)
library(nlme)

model <- lmer(RiquezadeParasitos~abundance + (1|RiquezadeHospedeiros) + (1|Prevalencia)
               + (1|TotaldeAves), dados)
summary(model)
parameters::p_value(model)
plot(model)

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_residuals(model, show.resid = TRUE,
               show.pred = TRUE)

model1 <- lmer(RiquezadeParasitos~abundance + RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants
               + Temp + Prec + (1|Bioma) , dados)
summary(model1)
(aov <- anova(model1))
parameters::p_value(model1)

plot(model1)
plot_residuals(model1, show.resid = TRUE,
               show.pred = TRUE)
plot(dados$RiquezadeParasitos ~ dados$abundance, col = c("red"), pch = 16, 
     xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1.1, bty= "l")
lines(dados$RiquezadeParasitos, fitted(model), col="blue")  

library(basicTrendline)
trendline(dados$abundance, dados$RiquezadeParasitos, col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1.1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

#Plasmodium

model2 <- lmer(RiquezaP~abundance + RiquezadeHospedeiros + PrevalenceP + migrantes + n_migrants
               + TotaldeAves + Temp + Prec + (1|Bioma) , dados)
summary(model2)
(aov <- anova(model2))
parameters::p_value(model2)

plot(model2)

plot(dados$RiquezaP ~ dados$abundance, col = c("blue"), pch = 16, 
     xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1.1, bty= "l")
lines(dados$RiquezadeParasitos, fitted(model), col="blue")  

library(basicTrendline)
trendline(dados$abundance, dados$RiquezaP, col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1.1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

#Haemoproteus

model3 <- lmer(RiquezaH~abundance + RiquezadeHospedeiros + PrevalenceH + migrantes + n_migrants
               + TotaldeAves + Temp + Prec + (1|Bioma) , dados)
summary(model3)
(aov <- anova(model3))
parameters::p_value(model3)
5.314403e-01

plot(model3)

plot(dados$RiquezaH ~ dados$abundance, col = c("green"), pch = 16, 
     xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1.1, bty= "l")
lines(dados$RiquezadeParasitos, fitted(model), col="blue")  

library(basicTrendline)
trendline(dados$abundance, dados$RiquezaH, col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1.1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")


