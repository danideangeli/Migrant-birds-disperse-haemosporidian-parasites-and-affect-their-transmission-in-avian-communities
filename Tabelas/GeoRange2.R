library(ggplot2)
library(readxl)
library(GeoRange)
library(fossil)
library(data.table)
library(tidyverse)
library(readODS)

setwd("~/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas")
data1 <- read_excel("Linhagens9.xlsx")

data2 <- read_excel("Linhagens.xlsx", sheet = "Sheet2")
class(data2)
data2 <- as.data.frame(data2)
data2 <- data2[ , c("site", "Latitude", "Longitude")] 
data2 <- distinct(data2)

dados <- left_join(data1, data2, by = c("site" = "site"))
dados <- as.data.frame(dados)

data <- create.matrix(dados, tax.name ="Lineage_Name", locality ="site", abund = FALSE)
data3 <- t(data)
data3 <- as.data.frame(data3)
setDT(data3, keep.rownames = "site")
data4 <- left_join(data3, data2, by = c("site" = "site"))
setcolorder(data4, c("Longitude", "Latitude"))
data4$site <- NULL
data4[data1 == 0] <- NA

data4$Longitude=as.numeric(paste(data4$Longitude))
data4$Latitude=as.numeric(paste(data4$Latitude))
data4 <- data4 %>% drop_na(Longitude)
data4<- as.data.frame(data4)
class(data4)

Range <- GeoRange_MultiTaxa(OccMatrix=data4,TaxaStart=3)
?GeoRange_MultiTaxa

Range1 <- filter(Range, NLocs > 1)

save.image("Range.RData")

write.csv(Range1, "Range.csv")

setDT(Range1, keep.rownames = "Lineage_Name")
setDT(Range, keep.rownames = "Lineage_Name")

data6 <- read_excel("Linhagens5.xlsx")
data6 <- filter(data6, Host_Status != "M")
data6 <- filter(data6, Host_Status != "PM")

data5 <- left_join(Range, data6, by = c("Lineage_Name" = "Lineage_Name"))

write.csv(data5, "Linhagens10.csv")

data7 <- filter(data5, NLocs > 1)

library(brms)

dados1 <- read_ods("Linhagens10.ods")
class(dados1)
dados1$MST=as.numeric(paste(dados1$MST))
print(dados1$Host_Status)
dados1 <- filter(dados1, MST > 1)

variaveis1 <- bf(MST~Host_Status + n_bird_individuals + riqueza, family = Gamma(link = log)) #check

prior1 <- get_prior(variaveis1, data = dados1)
prior1

dados1$Host_Status=as.factor(paste(dados1$Host_Status))
dados1$Host_Status<- relevel(dados1$Host_Status, ref="R")

levels(dados1$Host_Status)

model2 <- brm(
  MST~Host_Status + n_bird_individuals + riqueza, data = dados1, 
  family = Gamma(link = "log"), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 7.3, 2.5), "Intercept"),                                               
    prior(gamma(0.01, 0.01),"shape")
  ))

summary(model2)
parameters::p_value(model2)

a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot <- plot(conditional_effects(model2), points = FALSE, theme = a)

plot2 <- plot$Host_Status + labs(x = "Host Status", y = "Geographical Range (km)") #caption = "Plasmodium")
+ geom_boxplot(aes(color = Host_Status))
plot2

#colnames(data3)[1] <- "Longitude"
#colnames(data3)[2] <- "Latitude"

dados2 <- filter(dados1, NLocs > 1)

summary(dados4$Host_Status)

model3 <- brm(
  MST~Host_Status + n_bird_individuals + riqueza, data = dados2, 
  family = skew_normal(), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept"),                                               
    prior(student_t(3, 0, 2.5),"sigma")
  ))

summary(model3)
parameters::p_value(model3)

plot3 <- plot(conditional_effects(model3), points = FALSE, theme = a)

plot4 <- plot3$Host_Status + labs(x = "Host Status", y = "Minimum Spanning Tree Distance") #caption = "Plasmodium")
+ geom_boxplot(aes(color = Host_Status))
plot4

save.image("GeoRangeModels.RData")

summary(dados2$Host_Status)

data(BivalvePBDB)
BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
Exemplo <- GeoRange_MultiTaxa(OccMatrix=BivalveMatrix,TaxaStart=3)

dados3 <- filter(dados1, parasiteGenus == "Plasmodium")
prior1

model4 <- brm(
  MST~Host_Status + n_bird_individuals + riqueza, data = dados3, 
  family = Gamma(link = log), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 7.3, 2.5), "Intercept"),                                               
    prior(gamma(0.01, 0.01),"shape")
  ))

summary(model4)
parameters::p_value(model4)

plot5 <- plot(conditional_effects(model4), points = FALSE, theme = a)

plot6 <- plot5$Host_Status + labs(x = "Host Status", y = "Geographical Range (km)", caption = "Plasmodium")
+ geom_boxplot(aes(color = Host_Status))
plot6

dados4 <- filter(dados1, parasiteGenus == "Haemoproteus")
dados4 <- filter(dados4, NLocs > 1)

model5 <- brm(
  MST~Host_Status + n_bird_individuals + riqueza, data = dados4, 
  family = Gamma(link = log), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 7.3, 2.5), "Intercept"),                                               
    prior(gamma(0.01, 0.01),"shape")
  ))

summary(model5)
parameters::p_value(model5)

plot7 <- plot(conditional_effects(model5), points = FALSE, theme = a)

plot8 <- plot7$Host_Status + labs(x = "Host Status", y = "Geographical Range (km)", caption = "Haemoproteus")
+ geom_boxplot(aes(color = Host_Status))
plot8

save.image("Geo2.RData")

hist(dados1$MST)
geom_density(dados1$MST)