library(readxl)
library(tidyverse)

dados1 <- read_excel("PD7.xlsx", sheet = "Objetivo2b>=10", 
                     col_types = c("text", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric"))
dados1 <- filter(dados1, RiquezadeParasitos != "NA")
data <- read_excel("PD7.xlsx", sheet = "Especie>10")

dados2 <- dados1 %>%
  summarise(n_distinct(Especie))

#load myphy

library(lme4)
library(nlme)
library(phylolm)

model <- lmer(Prevalencia[as.factor(dados1$Especie)]~abundance + RiquezadeHospedeiros + migrantes + n_migrants + RiquezadeParasitos +
              + Totalsample + Temp + Prec + (1|Bioma), dados1)
summary(model)
(aov <- anova(model))
parameters::p_value(model)
4.460042e-02
plot(model)

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot(dados1$Prevalencia[as.factor(dados1$Especie)] ~ dados1$abundance, col = 1:150, pch = 1, 
     xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1.1, bty= "l")
lines(dados1$Prevalencia, fitted(model), col = "red")  

library(basicTrendline)
trendline(dados1$abundance, dados1$Prevalencia[as.factor(dados1$Especie)], col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1.1, bty= "l", ylim = c(0,0.6),
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

#5 individuos por especie

dados3 <- read_excel("PD7.xlsx", sheet = "Objetivo2b N>=5",
                     col_types = c("text", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric"))
dados3 <- filter(dados3, RiquezadeParasitos != "NA")

model1 <- lmer(Prevalencia[as.factor(dados3$Especie)]~abundance +  RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants + RiquezadeParasitos +
                + Totalsample + Temp + Prec + (1|Bioma), dados3)
summary(model1)
(aov <- anova(model1))
parameters::p_value(model1)
plot(model1)

plot(dados3$Prevalencia[as.factor(dados3$Especie)] ~ dados3$abundance, col = 1:150, pch = 1, 
     xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1.1, bty= "l")
lines(dados3$Prevalencia, fitted(model1), col = "red")  

library(basicTrendline)
trendline(dados3$abundance, dados3$Prevalencia[as.factor(dados3$Especie)], col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

#Todas as aves

dados4 <- read_excel("PD7.xlsx", sheet = "Objetivo 2b",
                     col_types = c("text", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric"))
dados4 <- filter(dados4, RiquezadeParasitos != "NA")


model2 <- lmer(Prevalencia[as.factor(dados4$Especie)]~abundance +  RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants + RiquezadeParasitos +
                 + (1|Totalsample) + (1|Temp) + (1|Prec) + (1|Bioma), dados4)
summary(model2)
(aov <- anova(model2))
parameters::p_value(model2)
plot(model2)

plot(dados4$Prevalencia[as.factor(dados4$Especie)] ~ dados4$abundance, col = 1:150, pch = 1, 
     xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1.1, bty= "l")
lines(dados4$Prevalencia, fitted(model2), col = "red")  

library(basicTrendline)
trendline(dados4$abundance, dados4$Prevalencia[as.factor(dados4$Especie)], col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

#Plasmodium

model3 <- lmer(PrevalenceP[as.factor(dados1$Especie)]~abundance + RiquezadeHospedeiros + migrantes + n_migrants + RiquezaP +
                + Totalsample + Temp + Prec + (1|Bioma), dados1)
summary(model3)
(aov <- anova(model3))
parameters::p_value(model3)
plot(model3)

plot(dados1$PrevalenceP[as.factor(dados1$Especie)] ~ dados1$abundance, col = 1:150, pch = 1, 
     xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1.1, bty= "l")
lines(dados1$Prevalencia, fitted(model), col = "red")  

trendline(dados1$abundance, dados1$PrevalenceP[as.factor(dados1$Especie)], col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1.1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

#Haemoproteus

model4 <- lmer(PrevalenceH[as.factor(dados1$Especie)]~abundance + RiquezadeHospedeiros + migrantes + n_migrants + RiquezaH +
                 + Totalsample + Temp + Prec + (1|Bioma), dados1)
summary(model4)
(aov <- anova(model4))
parameters::p_value(model4)
3.365182e-01
plot(model4)

plot(dados1$PrevalenceH[as.factor(dados1$Especie)] ~ dados1$abundance, col = 1:150, pch = 1, 
     xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1.1, bty= "l")
lines(dados1$Prevalencia, fitted(model), col = "red")  

trendline(dados1$abundance, dados1$PrevalenceH[as.factor(dados1$Especie)], col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1.1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

