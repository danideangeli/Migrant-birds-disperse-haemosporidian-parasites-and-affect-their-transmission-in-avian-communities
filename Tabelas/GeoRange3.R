library(brms)

dados1 <- read_ods("Linhagens11.ods")
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

res2 <- residuals(modelP)
res2 <- as.data.frame(res2)
dados3$residuals <- res2$Estimate
dados3$residuals <- as.numeric(dados3$residuals)

dados3a <- as.matrix(dist(cbind(dados3$Longitude, dados3$Latitude)))
dados3b <- 1/dados3a
diag(dados3b) <- 0
dados3b <-ifelse(dados3b=="Inf",0,dados3b )

dados3b[1:5, 1:5]

Moran.I(dados3$residuals, dados3b, na.rm = TRUE)

save.image("Geo3.Rdata")
