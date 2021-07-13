library(ggplot2)
library(readxl)
library(GeoRange)
library(fossil)
library(data.table)
library(tidyverse)
library(readODS)

setwd("~/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas")
dados <- read_excel("Linhagens5.xlsx")

data2 <- read_excel("Linhagens.xlsx", sheet = "Sheet2")
class(data2)
data2 <- as.data.frame(data2)
?create.matrix
data <- create.matrix(data2, tax.name ="Lineage_Name", locality ="site", abund = FALSE)
data3 <- t(data)
data3 <- as.data.frame(data3)
setDT(data3, keep.rownames = "site")
data4 <- data2[ , c("site", "Latitude", "Longitude")] 
data5 <- distinct(data4)
data6 <- left_join(data3, data5, by = c("site" = "site"))
setcolorder(data6, c("Longitude", "Latitude"))
data6$site <- NULL
data6[data6 == 0] <- NA

data6$Longitude=as.numeric(paste(data6$Longitude))
data6$Latitude=as.numeric(paste(data6$Latitude))
data6 <- data6 %>% drop_na(Longitude)
data6<- as.data.frame(data6)
class(data6)

Range <- GeoRange_MultiTaxa(OccMatrix=data6,TaxaStart=3)
?GeoRange_MultiTaxa

Range1 <- filter(Range, NLocs > 1)

save.image("Range.RData")

write.csv(Range1, "Range.csv")

setDT(Range1, keep.rownames = "Lineage_Name")
setDT(Range, keep.rownames = "Lineage_Name")

data7 <- left_join(Range, dados, by = c("Lineage_Name" = "Lineage_Name"))

write.csv(data7, "Linhagens6.csv")

data8 <- filter(data7, NLocs > 1)

library(brms)

dados1 <- read_ods("Linhagens7.ods")
dados1$MST=as.numeric(paste(dados1$MST))

variaveis1 <- bf(MST~Host_Status + n_bird_individuals + riqueza, family = skew_normal()) #check

prior1 <- get_prior(variaveis1, data = dados1)
prior1

dados1$Host_Status=as.factor(paste(dados1$Host_Status))
dados1$Host_Status<- relevel(dados1$Host_Status, ref="R")

levels(dados1$Host_Status)

model2 <- brm(
  MST~Host_Status + n_bird_individuals + riqueza, data = dados1, 
  family = gaussian(), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept"),                                               
    prior(student_t(3, 0, 2.5),"sigma")
  ))

summary(model2)
parameters::p_value(model2)

a <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot <- plot(conditional_effects(model2), points = FALSE, theme = a)

plot2 <- plot$Host_Status + labs(x = "Host Status", y = "Minimum Spanning Tree Distance") #caption = "Plasmodium")
+ geom_boxplot(aes(color = Host_Status))
plot2

#colnames(data3)[1] <- "Longitude"
#colnames(data3)[2] <- "Latitude"

dados2 <- filter(dados1, NLocs > 1)

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

model4 <- brm(
  MST~Host_Status + n_bird_individuals + riqueza, data = dados3, 
  family = skew_normal(), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept"),                                               
    prior(student_t(3, 0, 2.5),"sigma")
  ))

summary(model4)
parameters::p_value(model4)

plot5 <- plot(conditional_effects(model4), points = FALSE, theme = a)

plot6 <- plot5$Host_Status + labs(x = "Host Status", y = "Minimum Spanning Tree Distance", caption = "Plasmodium")
+ geom_boxplot(aes(color = Host_Status))
plot6

dados4 <- filter(dados1, parasiteGenus == "Haemoproteus")

model5 <- brm(
  MST~Host_Status + n_bird_individuals + riqueza, data = dados4, 
  family = skew_normal(), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept"),                                               
    prior(student_t(3, 0, 2.5),"sigma")
  ))

summary(model5)
parameters::p_value(model5)

plot7 <- plot(conditional_effects(model5), points = FALSE, theme = a)

plot8 <- plot7$Host_Status + labs(x = "Host Status", y = "Minimum Spanning Tree Distance", caption = "Haemoproteus")
+ geom_boxplot(aes(color = Host_Status))
plot8

hist(dados1$MST)
geom_density(dados1$MST)
