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

data <- read_excel("PD7.xlsx")

#saveRDS(mybirds_tree, "mybirds_tree.rds")

mybirds_tree <- readRDS("C:/Users/danid/Documents/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas/mybirds_tree.rds")

A <- ape::vcv.phylo(mybirds_tree)
?vcv.phylo

dados <- read_excel("Linhagens5.xlsx")
dados1 <- filter(dados, n_bird_individuals > 2)L

data1 <- read_excel("Banco Doutorado.xlsx", sheet = "Sequencias Brasil")

data2 <- read_excel("Linhagens.xlsx", sheet = "Sheet2")

head(dados)

#dados <- dados %>%
  #filter(species %in% mybirds_tree$tip.label)

#dados <- filter(dados, Status != "NA", Status != "SI")

#dados$Latitude <- as.numeric(dados$Latitude)
#dados$Longitude <- as.numeric(dados$Longitude)

#coords <- data.frame(dados$Latitude, dados$Longitude)
#dist <- as.matrix(dist(coords))

#dados$Host_Status=as.factor(paste(dados$Host_Status))

#dados$Host_Status<- relevel(dados$Host_Status, ref="R")

#levels(dados$Host_Status)

#dados$Lineage_Name <- as.factor(paste(dados$Lineage_Name))

data1 <- filter(data1, Localidade != "")

data4 <- data1 %>%
  dplyr::group_by(Linhagem) %>%
  dplyr::summarise(n_sites = n_distinct(Localidade), 
                   n_bird_individuals = n(), 
                   riqueza = n_distinct(Especie),
                   n_biomas = n_distinct(Bioma)) %>% 
  dplyr::arrange(-n_bird_individuals)

data4 <- filter(data4, Linhagem != "NA", Linhagem != "SI")
data4 <- as.data.frame(data4)

data3 <- data2 %>%
filter(Linhagem %in% dados$Lineage_Name)


data3 <- data2 %>%
  dplyr::group_by(Lineage_Name) %>%
  dplyr::group_by(site, add = TRUE) %>%
  dplyr::summarise(n_sites = n_distinct(site), 
                   n_bird_individuals = n(), 
                   n_status = n_distinct(Host_Status),
                   riqueza = n_distinct(species),
                   n_biomas = n_distinct(biomes),
                   n_regioes = n_distinct(regions)) %>%
  dplyr::arrange(-n_bird_individuals)
data3 <- as.data.frame(data3)

write.csv(data3, "linhagens8.csv")

data <- dados
data$n_sites <- NULL
data$riqueza <- NULL
data$n_biomas <- NULL
data$n_bird_individuals <- NULL
data$n_regioes <- NULL

data4 <- left_join(data3, data, by = "Lineage_Name")

write.csv(data4, "Linhagens9.csv")


dataR <- data2 %>%
  dplyr::group_by(Lineage_Name) %>%
  dplyr::group_by(Host_Status, add = TRUE) %>%
  dplyr::summarise(n_status = n_distinct(Host_Status))
  
dataR <- filter(dataR, Host_Status == "PM")

write.csv(data3, "Linhagens4.csv")

#dados$perlocation <- as.numeric(dados$perlocation)

#dados <- dados %>% 
  #mutate(Perlocation = perlocation*63/156)

#write.csv(dados, "Linhagens3.csv")

library(brms)

variaveis1 <- bf(n_sites~Host_Status, family = negbinomial()) #check

prior1 <- get_prior(variaveis1, data = dados)
prior1

dados$Host_Status=as.factor(paste(dados$Host_Status))
dados$Host_Status<- relevel(dados$Host_Status, ref="R")

levels(dados$Host_Status)

dados1 <- filter(dados,  n_bird_individuals >1 )


model2 <- brm(
  n_sites~Host_Status + n_bird_individuals + riqueza, data = dados1, 
  family = negbinomial(), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 0.7, 2.5), "Intercept"),                                             
    prior(gamma(0.01, 0.01),"shape")
  ))

summary(model2)
parameters::p_value(model2)
parameters::model_parameters(model2)

plot(model2, N = 4, ask = FALSE)

plot <- plot(conditional_effects(model2), points = FALSE, theme = a)

a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot2 <- plot$Host_Status + labs(x = "Host Status", y = "Number of Localities") #caption = "Plasmodium")
+ geom_boxplot(aes(color = Host_Status))
plot2
                                 
plot3 <- plot2 + geom_boxplot(aes(color = Host_Status))

plot$Host_Status$data$estimate__
plot$Host_Status$data$lower__
plot$Host_Status$data$upper__

?stat_summary
?conditional_effects()

dados5 <- with(dados4, names(table(n_bird_individuals)[table(n_bird_individuals) > 6]))
dados5 <- dados4[dados4$n_bird_individuals%in% dados5, ] # selecionar esses dados da planilha
dados5$n_bird_individuals <- factor(dados5$n_bird_individuals) # remover fatores vagos
dados5<- filter(dados4, n_bird_individuals>10)
dados5$n_bird_individuals <- as.numeric(dados5$n_bird_individuals)

dados5$Host_Status=as.factor(paste(dados5$Host_Status))
dados5$Host_Status<- relevel(dados5$Host_Status, ref="R")

levels(dados5$Host_Status)

prior2 = get_prior(variaveis1, data = dados5)
prior2

model3 <- brm(
  Perlocation~Host_Status + n_bird_individuals, data = dados5, 
  family = (Beta()), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 0, 10), "Intercept"),                                               
    prior(gamma(0.01, 0.01),"phi")
  ))

summary(model3)
parameters::p_value(model3)
parameters::model_parameters(model3)

plot(model3, N = 4, ask = FALSE)

plo <- conditional_effects(model3)

plot(conditional_effects(model3), points = FALSE, theme = a, 
     xlab = "Host Status", ylab = "Proportion of Locations")
a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

      
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

dados6 <-  filter(dados, parasiteGenus == "Plasmodium")

dados6$Host_Status=as.factor(paste(dados6$Host_Status))
dados6$Host_Status<- relevel(dados6$Host_Status, ref="R")

levels(dados6$Host_Status)

variaveis3 <- bf(n_sites~Host_Status, family = negbinomial ()) #check

prior3 = get_prior(variaveis3, data = dados6)
prior3

dados6$Perlocation <- as.numeric(dados6$Perlocation)
class(dados6$Perlocation)

model4 <- brm(
  n_sites~Host_Status + n_bird_individuals + riqueza, data = dados6, 
  family = negbinomial(), chains = 4,
  iter = 4000, 
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept"),                                               
    prior(gamma(0.01, 0.01),"shape")
  ))

summary(model4)
parameters::p_value(model4)

plot(model4, N = 4, ask = FALSE)

plot3 <- plot(conditional_effects(model4), points = FALSE, theme = a)
a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
     panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot4 <- plot3$Host_Status + labs(x = "Host Status", y = "Number of locatities", caption = "Plasmodium")
plot4


dados7 <-  filter(dados, parasiteGenus == "Haemoproteus")

variaveis3 <- bf(n_sites~Host_Status, family = negbinomial()) #check

prior4 = get_prior(variaveis3, data = dados7)
prior4

dados7$Host_Status=as.factor(paste(dados7$Host_Status))
dados7$Host_Status<- relevel(dados7$Host_Status, ref="R")

model5 <- brm(
  n_sites~Host_Status + n_bird_individuals + riqueza, data = dados7, 
  family = negbinomial(), #check family
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept"),                                               
    prior(gamma(0.01, 0.01),"shape")
  ))

summary(model5)
parameters::p_value(model5)

plot(model5, N = 4, ask = FALSE)

plot5 <- plot(conditional_effects(model5), points = FALSE, theme = a)
a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot6 <- plot5$Host_Status + labs(x = "Host Status", y = "Number of locatities", caption = "Haemoproteus")
plot6

plot3$Host_Status$data$estimate__
plot3$Host_Status$data$lower__
plot3$Host_Status$data$upper__

plot3$Host_Status$data$se__ #standard error


