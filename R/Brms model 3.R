library(tidyverse)
library(ggplot2)
library(brms)
library(readxl)
library(ggpubr)
library(ape)
setwd("~/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas")

dados1 <- read_excel("PD7.xlsx", sheet = "Objetivo2b>=10", 
                     col_types = c("text", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric"))
dados1 <- filter(dados1, RiquezadeParasitos != "NA")

hist(dados1$RiquezadeParasitos)
hist(dados1$RiquezadeHospedeiros)
hist(dados1$abundance)
hist(dados1$Temp)
hist(dados1$Prec)
hist(dados1$n_migrants)


dados1$Especie = gsub(' ', '_', dados1$Especie)
dados1$Especie=as.factor(paste(dados1$Especie))
haemo_species <- dados1$Especie

myphy <- readRDS("C:/Users/danid/Documents/Lab Poulin/PhD/PhD/Dados/PhD/myphy.rds")

library(ape)

matches2 <-match(haemo_species, myphy$tip.label)
matches2 <-na.omit(matches2)
haemo_tree <-drop.tip(myphy, myphy$tip.label[-matches2])


#########################
sptest=as.factor(haemo_tree$tip.label)
sp=as.data.frame(sptest)

dados2=dados1 %>%
  filter(Especie %in% sp$sptest)

inv.phylo <- MCMCglmm::inverseA(haemo_tree, nodes = "TIPS", scale = TRUE)
A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)

dados2$phylo <- dados2$Especie
hist(dados2$Pos, breaks = 100)

teste <- brm(Pos~offset(Totalsample) + RiquezadeParasitos + (1|Bioma) + (1|Localidade) + (1|gr(phylo, cov = A)),
             data = dados2, 
             family = negbinomial(),
             chain = 4, iter = 4000, data2 = list(A = A))
summary(teste)

teste2 <- brm(Pos~offset(Totalsample) +  Prec + (1|Bioma) + (1|Localidade) + (1|gr(phylo, cov = A)),
              data = dados2, 
              family = negbinomial(),
              chain = 4, iter = 4000, data2 = list(A = A))
summary(teste2)

teste3 <- brm(Pos~offset(Totalsample) +  Temp + (1|Bioma) + (1|Localidade) + (1|gr(phylo, cov = A)),
              data = dados2, 
              family = negbinomial(),
              chain = 4, iter = 4000, data2 = list(A = A))

summary(teste3)

teste4 <- brm(Pos~offset(Totalsample) +  n_migrants + (1|Bioma) + (1|Localidade) + (1|gr(phylo, cov = A)),
              data = dados2, 
              family = negbinomial(),
              chain = 4, iter = 4000, data2 = list(A = A))

summary(teste4)

variaveis <- bf(Pos~offset(Totalsample)+abundance + migrantes + 
                  (1|Bioma) + (1|Localidade) + (1|gr(phylo, cov = A)), family = negbinomial())

prior <- get_prior(variaveis, data = dados2)
prior
dados2$abundance <- dados2$abundance*100

model <- brm(Pos~offset(Totalsample)+abundance + migrantes + (1|Bioma) + (1|Localidade) + (1|gr(phylo, cov = A)),
             data = dados2, 
             family = negbinomial(),
             chain = 4, iter = 4000, 
             data2 = list(A = A),
             prior = c(
               prior(student_t(3, 1.1, 2.5), "Intercept"),
               prior(student_t(3, 0, 2.5), "sd" ),
               prior(gamma(0.01, 0.01), "shape")
             )
)

summary(model)
plot(model, N = 4, ask = FALSE)

plot1 <- plot(conditional_effects(model), points = FALSE, theme = a)
a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot2 <- plot1$abundance + labs(x = "Proportion of migrant individuals", y = "Number of Infections") + coord_cartesian(ylim = c(0, 15)) #caption = "Plasmodium")
+ geom_boxplot(aes(color = abundance)) 
plot2

res1 <- residuals(model)
res1 <- as.data.frame(res1)
dados2$residuals <- res1$Estimate
dados2$residuals <- as.numeric(dados2$residuals)

dados2a <- as.matrix(dist(cbind(dados2$Longitude, dados2$Latitude)))
dados2b <- 1/dados2a
diag(dados2b) <- 0
dados2b <-ifelse(dados2b=="Inf",0,dados2b )

dados2b[1:5, 1:5]

Moran.I(dados2$residuals, dados2b, na.rm = TRUE)

prior <- get_prior(variaveis, data = dados3)
prior

save.image("Obct2BOFFSET.RData")
#save(dados3, file = "dados3.RData")
#save(dados4, file = "dados4.RData")
load("C:/Users/danid/Documents/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas/dados3.RData")
load("C:/Users/danid/Documents/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas/dados4.RData")

dados3$abundance <- dados3$abundance * 100
dados4$abundance <- dados4$abundance * 100

modelP <- brm(PosP~offset(Totalsample)+ abundance + (1|Bioma) + (1|Localidade) + (1|gr(phylo, cov = A)),
             data = dados3, 
             family = negbinomial(),
             data2 = list(A = A),
             prior = c(
               prior(student_t(3, 1.1, 2.5), "Intercept"),
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

plot4 <- plot3$abundance + labs(x = "Proportion of migrant individuals", y = "Number of Infections", caption = "Plasmodium")
+ geom_boxplot(aes(color = abundance))
plot4

res2 <- residuals(modelP)
res2 <- as.data.frame(res2)
dados5 <- filter(dados3, Localidade != "Parque_Nacional_de_Bras'lia")
dados5$residuals <- res2$Estimate
dados5$residuals <- as.numeric(dados5$residuals)

dados5a <- as.matrix(dist(cbind(dados5$Longitude, dados5$Latitude)))
dados5b <- 1/dados5a
diag(dados5b) <- 0
dados5b <-ifelse(dados5b=="Inf",0,dados5b )

dados5b[1:5, 1:5]

Moran.I(dados5$residuals, dados5b, na.rm = TRUE)

prior <- get_prior(variaveis, data = dados4)
prior

modelH <- brm(PosH~offset(Totalsample)+ abundance + (1|Bioma) + (1|Localidade) + (1|gr(phylo, cov = A)),
              data = dados4, 
              family = negbinomial(),
              data2 = list(A = A),
              prior = c(
                prior(student_t(3, 1.1, 2.5), "Intercept"),
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

plot6 <- plot5$abundance + labs(x = "Proportion of migrant individuals", y = "Number of Infections", caption = "Haemoproteus")
+ geom_boxplot(aes(color = abundance))
plot6

res3 <- residuals(modelH)
res3 <- as.data.frame(res3)
dados6 <- filter(dados4, Localidade != "Parque_Nacional_de_Bras'lia")
dados6$residuals <- res3$Estimate
dados6$residuals <- as.numeric(dados6$residuals)

dados6a <- as.matrix(dist(cbind(dados6$Longitude, dados6$Latitude)))
dados6b <- 1/dados6a
diag(dados6b) <- 0
dados6b <-ifelse(dados6b=="Inf",0,dados6b )

dados6b[1:5, 1:5]

Moran.I(dados6$residuals, dados6b, na.rm = TRUE)

save.image("Obj2B.RData")
