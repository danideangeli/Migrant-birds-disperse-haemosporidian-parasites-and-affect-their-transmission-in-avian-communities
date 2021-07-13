library(brms)

variaveis1 <- bf(n_sites~Host_Status + n_bird_individuals + riqueza, family = poisson ()) #check

prior1 <- get_prior(variaveis1, data = dados)
prior1


modelP <- brm(
  n_sites~Host_Status + n_bird_individuals + riqueza, data = dados, 
  family = poisson(), chains = 4,
  iter = 4000,
  prior = c(
    prior(student_t(3, 0, 2.5), "Intercept")
  ))

loo1=loo(model2)
loo2=loo(modelP)

loo_list=list(loo1,loo2)
loo_model_weights(loo_list, method = "stacking")
loo_model_weights(loo_list, method = "pseudobma")
