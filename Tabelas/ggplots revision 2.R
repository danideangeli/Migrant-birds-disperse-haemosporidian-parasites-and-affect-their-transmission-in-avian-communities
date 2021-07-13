load("C:/Users/danid/Documents/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas/Geo2.RData")
setwd("~/Lab Poulin/PhD/PhD/Dados/Capítulo1/Tabelas")
library(tidyverse)
library(ggplot2)

dados1$Host_Status <- as.character(dados1$Host_Status)
dados1 <- dados1 %>%
  mutate(Host_Status = case_when(Host_Status == "R" ~ "Resident",
                                 Host_Status == "R_M" ~ "Resident and any migrant",
                                 TRUE ~ Host_Status))

ggplot(data=dados1, aes(x=Host_Status, y=MST)) + geom_boxplot() + 
  theme_classic() + xlab ("Host Status") + ylab ("Geographical Range (km)")


dados3$Host_Status <- as.character(dados3$Host_Status)
dados3 <- dados3 %>%
  mutate(Host_Status = case_when(Host_Status == "R" ~ "Resident",
                                 Host_Status == "R_M" ~ "Resident and any migrant",
                                 TRUE ~ Host_Status))

ggplot(data=dados3, aes(x=Host_Status, y=MST)) + geom_boxplot() + labs(caption = "Plasmodium") +
  theme_classic() + xlab ("Host Status") + ylab ("Geographical Range (km)") 

dados4$Host_Status <- as.character(dados4$Host_Status)
dados4 <- dados4 %>%
  mutate(Host_Status = case_when(Host_Status == "R" ~ "Resident",
                                 Host_Status == "R_M" ~ "Resident and any migrant",
                                 TRUE ~ Host_Status))

ggplot(data=dados4, aes(x=Host_Status, y=MST)) + geom_boxplot() + labs (caption = "Haemoproteus") +
  theme_classic() + xlab ("Host Status") + ylab ("Geographical Range (km)")

