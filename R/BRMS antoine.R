dan<-readRDS("C:/Users/Antoine Filion/Desktop/Otago/Side project/Daniela/Analysis/brms_daniela.rds")
str(dan)
dan <- brms
dan$Host_Status=as.factor(paste(dan$Host_Status))
dan$n_sites=as.integer(paste(dan$n_sites))
#Let's set the reference level
dan$Host_Status=relevel(dan$Host_Status, ref="R")
#First, just a quick glance at the data
hist(dan$Perlocation, breaks=100)
library(brms)
test_form=bf(Perlocation ~ 1 , family=Beta())
#afterwards, we use the get_prior function
get_prior(test_form, data=dan)
mod2=brm(Perlocation~n_bird_individuals, data=dan, family=Beta,chains = 2, cores = 2, iter = 4000,
         prior = c(
           prior(student_t(3, 0, 10), "Intercept"),
           prior(gamma(0.01, 0.01), "phi")))
summary(mod2)
mod3=brm(Perlocation~Host_Status, data=dan, family=Beta,chains = 2, cores = 2, iter = 4000,
         prior = c(
           prior(student_t(3, 0, 10), "Intercept"),
           prior(gamma(0.01, 0.01), "phi")))
summary(mod3)
mod=brm(Perlocation~riqueza, data=dan, family=Beta,chains = 2, cores = 2, iter = 4000,
         prior = c(
           prior(student_t(3, 0, 10), "Intercept"),
           prior(gamma(0.01, 0.01), "phi")))
summary(mod)

loo2=loo(mod2)
loo3=loo(mod3)
loo4=loo(mod)
loo_dan=list(loo2,loo3, loo4)

loo_model_weights(loo_dan, method = "stacking")


loo_model_weights(loo_dan, method = "pseudobma")

?loo_model_weights
