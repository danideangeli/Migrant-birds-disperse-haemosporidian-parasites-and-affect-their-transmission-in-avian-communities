#Null Models

library(brms)
library(readxl)
library(tidyverse)
library(ggpubr)
setwd("~/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas")

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

str(dados$Bioma)
dados$Bioma <- as.factor(paste(dados$Bioma))

ggdensity(dados$RiquezadeParasitos, breaks = 100)
ggdensity(dados$Prevalencia, breaks = 100)
ggdensity(dados$RiquezadeHospedeiros, breaks = 100)
ggdensity(dados$migrantes, breaks = 100)
ggdensity(dados$Temp, breaks = 100)
ggdensity(dados$Prec, breaks = 100)
ggdensity(dados$n_migrants)

model1 <- glmer(RiquezadeParasitos~abundance + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
model2 <- glmer(RiquezadeParasitos~abundance + log1p(RiquezadeHospedeiros) + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model2)
model3 <- glmer(RiquezadeParasitos~abundance + log1p(Prevalencia) + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model3)
model4 <- glmer(RiquezadeParasitos~abundance + log1p(RiquezadeHospedeiros) + log1p(Prevalencia) + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model4)
model5 <- glmer(RiquezadeParasitos~abundance + log1p(migrantes) + (1|Localidade)
                + (1|Bioma), data = dados, family = poisson())
summary(model5)
model6<- glmer(RiquezadeParasitos~abundance + log1p(RiquezadeHospedeiros) + log1p(Prevalencia) + log1p(migrantes) 
               + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model6)
model7 <- glmer(RiquezadeParasitos~abundance + log1p(n_migrants)
                + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model7)
model8<- glmer(RiquezadeParasitos~abundance + log1p(RiquezadeHospedeiros) + log1p(Prevalencia) + log1p(migrantes) + log1p(n_migrants)
               + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model8)
model9 <- glmer(RiquezadeParasitos~abundance + log1p(Temp) 
                + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model9)
model10<- glmer(RiquezadeParasitos~abundance + log1p(RiquezadeHospedeiros) + log1p(Prevalencia) + log1p(migrantes) +
                log1p(n_migrants) + log1p(Temp) + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model10)
model11 <- glmer(RiquezadeParasitos~abundance + Prec 
                + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model11)
model12<- glmer(RiquezadeParasitos~abundance + log1p(RiquezadeHospedeiros) + log1p(Prevalencia) + log1p(migrantes) +
                        log1p(n_migrants) + log1p(Temp) + Prec + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model12)



AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12)

summary(model10)
parameters::model_parameters(model10)

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(devtools)


p <- plot_model(model10, type = "est", vline.color = "black", theme = a)


p1 <- p  + labs(title = "Parameters estimate",
        subtitle = "Parasite Richness")
        #caption = "Plasmodium")
        #x = "Dose (mg)", y = "Teeth length")
p1

a <- theme(
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"), 
        axis.line = element_line(colour = "black"))

p2 <- p1 + a
p2

p2 + scale_x_discrete(name = "", 
                         labels = c("Temperature", "Number of migrants", "Proportion of migrant species", 
                                    "Prevalence", "Host Richness", "Proportion of individual migrants"))      



plot_model(model10, type = "est")
?plot_model
plot_model(model10, type = "std")


model10P<- glmer(RiquezaP~abundance + log1p(RiquezadeHospedeiros) + log1p(Prevalencia) + log1p(migrantes) +
                        log1p(n_migrants) + log1p(Temp) + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model10P)
parameters::parameters(model10P)

plas <- plot_model(model10P, type = "est", vline.color = "black")

plas1 <- plas  + labs(title = "Parameters estimate",
                subtitle = "Parasite Richness",
                caption = "Plasmodium")
#x = "Dose (mg)", y = "Teeth length")
plas1

a <- theme(
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"), 
        axis.line = element_line(colour = "black"))

plas2 <- plas1 + a
plas2

plas2 + scale_x_discrete(name = "", 
                         labels = c("Temperature", "Number of migrants", "Proportion of migrant species", 
                                    "Prevalence", "Host Richness", "Proportion of individual migrants"))  

model10H<- glmer(RiquezaH~abundance + log1p(RiquezadeHospedeiros) + log1p(Prevalencia) + log1p(migrantes) +
                         log1p(n_migrants) + log1p(Temp) + (1|Bioma) + (1|Localidade), data = dados, family = poisson())
summary(model10H)
parameters::parameters(model10H)

H <- plot_model(model10H, type = "est", vline.color = "black")
?plot_model

H1 <- H  + labs(title = "Parameters estimate",
                      subtitle = "Parasite Richness",
                      caption = "Haemoproteus")
#x = "Dose (mg)", y = "Teeth length")
H1

a <- theme(
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"), 
        axis.line = element_line(colour = "black"))

H2 <- H1 + a
H2

H2 + scale_x_discrete(name = "", 
                       labels = c("Temperature", "Number of migrants", "Proportion of migrant species", 
                                  "Prevalence", "Host Richness", "Proportion of individual migrants"))

plot(dados$RiquezadeParasitos ~ dados$abundance, col = "red", pch = 16, 
     xlab = "Percentage of Migrants (%)", ylab = "Parasite Richness", cex.lab = 1, bty= "l")
abline(dados$abundance, dados$RiquezadeParasitos, col = "blue", lwd=3, lty=2)
?abline

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

ggdensity(dados1$RiquezadeParasitos, breaks = 100)
ggdensity(dados1$Prevalencia, breaks = 100)
ggdensity(dados1$RiquezadeHospedeiros, breaks = 100)
ggdensity(dados1$migrantes, breaks = 100)
ggdensity(dados1$Temp, breaks = 100)
ggdensity(dados1$Prec, breaks = 100)
ggdensity(dados1$n_migrants)


shapiro.test(dados1$RiquezadeParasitos)
shapiro.test(dados1$Prevalencia)
shapiro.test(dados1$RiquezadeHospedeiros)
shapiro.test(dados1$migrantes)
shapiro.test(dados1$Temp)
shapiro.test(dados1$Prec)
shapiro.test(dados1$n_migrants)

library(phylolm)
library(ape)
library(picante)

samp2 <- as.matrix(dados1)
samp2 <- dados1[,-2]
row.names(samp2) <- dados1[,2]
#.rowNamesDF(samp2, make.names=FALSE) <- dados1[,2]
#rownames(samp2) <- make.names(dados1[,2], unique = FALSE)
samp2 <- as.data.frame(samp2)
dados1 <- as.data.frame(dados1)

#library(tidyverse)
#samp <- dados1 %>% remove_rownames %>% column_to_rownames(var="Especie")

myphy <- readRDS("C:/Users/danid/Documents/Lab Poulin/PhD/PhD/Dados/PhD/myphy.rds")

length(which(myphy$tip.label%in%as.character(samp2[,2])))
todrop<-myphy$tip.label[which(myphy$tip.label%in%as.character(samp2[,2])==FALSE)]
mybirds_tree<-drop.tip(myphy,todrop)

#phy <- prune.missing(samp2$Especie, myphy)

summary.phylo(mybirds_tree)
print.phylo(mybirds_tree)
plot.phylo(mybirds_tree)

test<-match.phylo.data(mybirds_tree, samp2)
class(test)

inv.phylo <- MCMCglmm::inverseA(mybirds_tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)

?match.comm.dist
match.comm.dist(samp2, A)

#A <- ape::vcv.phylo(myphy)
class(A)
view(A)

test1 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~log1p(abundance) + 
         (1|Localidade) + (1|Bioma), data = dados1, family = binomial)

summary(test1)
parameters::p_value(test1)

test2 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~log1p(abundance) + 
                       RiquezadeHospedeiros + (1|Localidade) + (1|Bioma), data = dados1, family = binomial)
summary(test2)
parameters::p_value(test2)
parameters::parameters(test2)

test3 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~log1p(abundance) + 
               log1p(migrantes) + (1|Localidade) + (1|Bioma), data = dados1, family = binomial)
summary(test3)
parameters::p_value(test3)

test4 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~log1p(abundance) + 
        log1p(n_migrants) + (1|Localidade) + (1|Bioma), data = dados1, family = binomial)
summary(test4)
parameters::p_value(test4)

test5 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~log1p(abundance) 
               + log1p(RiquezadeParasitos) + (1|Localidade) + (1|Bioma), data = dados1, family = binomial)
summary(test5)
parameters::p_value(test5)

test6 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)~log1p(abundance) 
               + log1p(Temp) + (1|Localidade) + (1|Bioma) + (1|Especie), data = dados1, family = binomial)
summary(test6)
parameters::p_value(test6)

test7 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~log1p(abundance) 
               + log1p(Prec) + (1|Localidade) + (1|Bioma), data = dados1, family = binomial)
summary(test7)
parameters::p_value(test7)
test8 <- glmer(pbinom(Prevalencia, Totalsample, Prevalencia, log = FALSE)[as.factor(dados1$Especie)]~abundance 
               + Prec + Temp + (1|Localidade) + (1|Bioma), data = dados1, family = binomial)
summary(test8)
parameters::p_value(test8)

AIC(test1, test2, test3, test4, test5, test6, test7)

summary(test6)
parameters::parameters(test6)

test6P <- glmer(pbinom(PrevalenceP, Totalsample, PrevalenceP, log = FALSE)[as.factor(dados1$Especie)]~log1p(abundance) 
               + log1p(Temp) + (1|Localidade) + (1|Bioma), data = dados1, family = binomial)
summary(test6P)
parameters::parameters(test6P)

test6H <- glmer(pbinom(PrevalenceH, Totalsample, PrevalenceH, log = FALSE)[as.factor(dados1$Especie)]~log1p(abundance) 
                + log1p(Temp) + (1|Localidade) + (1|Bioma), data = dados1, family = binomial)
summary(test6H)
parameters::parameters(test6H)

#library(basicTrendline)
#trendline(dados1$abundance, pbinom(dados1$Prevalencia, dados1$Totalsample, dados1$Prevalencia[as.factor(dados1$Especie)], log = FALSE), col = "gray", linecolor = "red", CI.color = "pink",
          #xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l", ylim = c(0,0.6),
          #show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

#trendline(dados1$abundance, pbinom(dados1$PrevalenceP, dados1$Totalsample, dados1$PrevalenceP[as.factor(dados1$Especie)], log = FALSE), col = "gray", linecolor = "red", CI.color = "pink",
          #xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l",
          #show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

#trendline(dados1$abundance, pbinom(dados1$PrevalenceH, dados1$Totalsample, dados1$PrevalenceH[as.factor(dados1$Especie)], log = FALSE), col = "gray", linecolor = "red", CI.color = "pink",
          #xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l",
          #show.Rpvalue = FALSE, show.equation = FALSE, text.col = "white")

plot(dados1$Prevalencia[as.factor(dados1$Especie)] ~ dados1$abundance, col = "red", pch = 1, 
     xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l")
abline(dados1$abundance, dados1$Prevalencia[as.factor(dados1$Especie)], col = "blue") 

plot(dados1$PrevalenceP[as.factor(dados1$Especie)] ~ dados1$abundance, col = "blue", pch = 1, 
     xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l")

plot(dados$PrevalenceH ~ dados$abundance, col = "green", pch = 1, 
     xlab = "Percentage of Migrants (%)", ylab = "Prevalence (%)", cex.lab = 1, bty= "l")

