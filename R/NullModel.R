#Null Models

library(lme4)
library(readxl)

str(dados$Bioma)
dados$Bioma <- as.factor(paste(dados$Bioma))

hist(dados$RiquezadeParasitos, breaks = 100)

hist(dados1$Prevalencia, breaks = 100)

#usar poisson distribution

#OBJETIVO 2A

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

model1 <- glmer(RiquezadeParasitos~abundance + (1|Bioma), data = dados, family = poisson())
model2 <- glmer(RiquezadeParasitos~abundance + RiquezadeHospedeiros + (1|Bioma), data = dados, family = poisson())
summary(model2)
model3 <- glmer(RiquezadeParasitos~abundance + RiquezadeHospedeiros + Prevalencia + (1|Bioma), data = dados, family = poisson())
summary(model3)
model4 <- glmer(RiquezadeParasitos~abundance + RiquezadeHospedeiros + Prevalencia + migrantes
                + (1|Bioma), data = dados, family = poisson())
summary(model4)
model5<- glmer(RiquezadeParasitos~abundance + RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants 
               + (1|Bioma), data = dados, family = poisson())
summary(model5)
model6 <- glmer(RiquezadeParasitos~abundance + RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants 
               + Temp + (1|Bioma), data = dados, family = poisson())
summary(model6)
model7 <- glmer(RiquezadeParasitos~abundance + RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants 
                + Prec + (1|Bioma), data = dados, family = poisson())
summary(model7)

AIC(model1, model2, model3, model4, model5)

summary(model5)
parameters::model_parameters(model5)

model5P<- glmer(RiquezaP~abundance + RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants 
               + (1|Bioma), data = dados, family = poisson())
summary(model5P)
parameters::parameters(model5P)

model5H<- glmer(RiquezaH~abundance + RiquezadeHospedeiros + Prevalencia + migrantes + n_migrants 
                + (1|Bioma), data = dados, family = poisson())
summary(model5H)
parameters::parameters(model5H)

plot(dados$RiquezadeParasitos ~ dados$abundance, col = "red", pch = 16, 
     xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1, bty= "l")
abline(dados$abundance, dados$RiquezadeParasitos, col = "blue") 

plot(dados$RiquezaP ~ dados$abundance, col = "blue", pch = 16, 
     xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1, bty= "l")

plot(dados$RiquezaH ~ dados$abundance, col = "green", pch = 16, 
     xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1, bty= "l")


#OBJETIVO 2B

dados1 <- read_excel("PD7.xlsx", sheet = "Objetivo2b>=10", 
                     col_types = c("text", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric"))
dados1 <- filter(dados1, RiquezadeParasitos != "NA")

test1 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance
               + (1|Localidade), data = dados1, family = binomial())
summary(test1)
parameters::p_value(test1)
test2 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance 
                + RiquezadeHospedeiros + (1|Localidade), data = dados1, family = binomial)
summary(test2)
parameters::p_value(test2)
test3 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance 
               + migrantes + (1|Localidade), data = dados1, family = binomial)
summary(test3)
parameters::p_value(test3)
test4 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance 
               + n_migrants + (1|Localidade), data = dados1, family = binomial)
summary(test4)
parameters::p_value(test4)
test5 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance 
               + RiquezadeParasitos + (1|Localidade), data = dados1, family = binomial)
summary(test5)
parameters::p_value(test5)
test6 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance 
               + Temp + (1|Localidade), data = dados1, family = binomial)
summary(test6)
parameters::p_value(test6)
test7 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance 
               + Prec + (1|Localidade), data = dados1, family = binomial)
summary(test7)
parameters::p_value(test7)
test8 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance 
               + Prec + Temp + (1|Localidade), data = dados1, family = binomial)
summary(test8)
parameters::p_value(test8)

AIC(test1, test2, test3, test4, test5, test6, test7, test8)

summary(test6)
parameters::p_value(test6)

test6P <- glmer(pbinom(PrevalenceP, Totalsample, PrevalenceP, log = FALSE)[as.factor(dados1$Especie)]~abundance 
               + Temp + (1|Localidade), data = dados1, family = binomial)
summary(test6P)
parameters::p_value(test6P)

test6H <- glmer(pbinom(PrevalenceH, Totalsample, PrevalenceH, log = FALSE)[as.factor(dados1$Especie)]~abundance 
                + Temp + (1|Localidade), data = dados1, family = binomial)
summary(test6H)
parameters::p_value(test6H)

library(basicTrendline)
trendline(dados1$abundance, pbinom(dados1$Prevalencia, dados1$Totalsample, dados1$Prevalencia[as.factor(dados1$Especie)], log = FALSE), col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l", ylim = c(0,0.6),
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

trendline(dados1$abundance, pbinom(dados1$PrevalenceP, dados1$Totalsample, dados1$PrevalenceP[as.factor(dados1$Especie)], log = FALSE), col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

trendline(dados1$abundance, pbinom(dados1$PrevalenceH, dados1$Totalsample, dados1$PrevalenceH[as.factor(dados1$Especie)], log = FALSE), col = "gray", linecolor = "red", CI.color = "pink",
          xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l",
          show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")
