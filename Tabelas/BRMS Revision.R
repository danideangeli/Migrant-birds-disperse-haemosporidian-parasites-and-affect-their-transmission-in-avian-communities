library(ape)
library(tidyverse)
library(devtools) # load package
#devtools::install_github("r-lib/pkgbuild") # install updated version of pkgbuild from GitHub
library(pkgbuild) # load package
find_rtools() # should be TRUE, assuming you have Rtools 3.5
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(brms)
library(readxl)

setwd("~/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas")

mybirds_tree <- readRDS("C:/Users/danid/Documents/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas/mybirds_tree.rds")

A <- ape::vcv.phylo(mybirds_tree)
?vcv.phylo

dados <- read_excel("Linhagens5.xlsx")
head(dados)


library(brms)

variaveis1 <- bf(n_sites~Host_Status + n_bird_individuals + riqueza, family = poisson()) #check

prior1 <- get_prior(variaveis1, data = dados)
prior1

dados$Host_Status=as.factor(paste(dados$Host_Status))
dados$Host_Status<- relevel(dados$Host_Status, ref="R")

levels(dados$Host_Status)

dadosTaf <- filter(dados, Lineage_Name != "TARUF02")

model <- brm(
  n_sites~Host_Status + n_bird_individuals + riqueza, data = dados, 
  family = poisson(), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept")
  ))

?brmsfamily

summary(model)
parameters::p_value(model)
parameters::model_parameters(model)

plot(model, N = 4, ask = FALSE)

a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot <- plot(conditional_effects(model), points = FALSE, theme = a)

plot2 <- plot$Host_Status + labs(x = "Host Status", y = "Number of Biomes") #caption = "Plasmodium")
+ geom_boxplot(aes(color = Host_Status))
plot2

plot3 <- plot2 + geom_boxplot(aes(color = Host_Status))

save.image("Cap1.R.RData")

plot$Host_Status$data$estimate__
plot$Host_Status$data$lower__
plot$Host_Status$data$upper__

#dados1 <- with(dados, names(table(n_bird_individuals)[table(n_bird_individuals) >= 5]))
#dados1 <- dados[dados$n_bird_individuals%in% dados1, ] # selecionar esses dados da planilha
#dados1$n_bird_individuals <- factor(dados1$n_bird_individuals) # remover fatores vagos

dados1<- filter(dados, n_bird_individuals>=5)
dados1$n_bird_individuals <- as.numeric(dados1$n_bird_individuals)

dados1$Host_Status=as.factor(paste(dados1$Host_Status))
dados1$Host_Status<- relevel(dados1$Host_Status, ref="R")

levels(dados1$Host_Status)

prior2 = get_prior(variaveis1, data = dados1)
prior2


model2 <- brm(
  n_sites~Host_Status + n_bird_individuals + riqueza, data = dados1, 
  family = poisson(), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 1.4, 2.5), "Intercept")
  ))

summary(model2)
parameters::p_value(model2)
parameters::model_parameters(model2)

plot(model3, N = 4, ask = FALSE)

plo <- conditional_effects(model2)

plot(conditional_effects(model2), points = FALSE, theme = a)


dados2 <-  filter(dados, parasiteGenus == "Plasmodium")

dados2$Host_Status=as.factor(paste(dados2$Host_Status))
dados2$Host_Status<- relevel(dados2$Host_Status, ref="R")

levels(dados2$Host_Status)

prior3 = get_prior(variaveis1, data = dados2)
prior3

model3 <- brm(
  n_sites~Host_Status + n_bird_individuals + riqueza, data = dados2, 
  family = poisson(), chains = 4,
  iter = 4000, 
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept")                                              
  ))

summary(model3)
parameters::p_value(model3)

plot(model4, N = 4, ask = FALSE)

plot3 <- plot(conditional_effects(model3), points = FALSE, theme = a)

plot4 <- plot3$Host_Status + labs(x = "Host Status", y = "Number of locations", caption = "Plasmodium")
plot4


dados3 <-  filter(dados, parasiteGenus == "Haemoproteus")

prior4 = get_prior(variaveis1, data = dados3)
prior4

dados3$Host_Status=as.factor(paste(dados3$Host_Status))
dados3$Host_Status<- relevel(dados3$Host_Status, ref="R")

model4 <- brm(
  n_sites~Host_Status + n_bird_individuals + riqueza, data = dados3, 
  family = poisson(), chains = 4,
  iter = 4000, 
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept")
  ))

summary(model4)
parameters::p_value(model4)

plot(model4, N = 4, ask = FALSE)

plot5 <- plot(conditional_effects(model4), points = FALSE, theme = a)

plot6 <- plot5$Host_Status + labs(x = "Host Status", y = "Number of locations", caption = "Haemoproteus")
plot6

save.image("Models.RData")
