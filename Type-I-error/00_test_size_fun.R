### Function to simulate a dataset:
Sim_once <- function(distr = "logit", maf, betaE, betaGE, betaG, beta0, n){
  G <- rbinom(n, 2, maf)
  E <- rnorm(n)
  GE <- (G*E)
  if(distr == "probit"){
    Ylat = beta0 + betaGE*GE + betaG*G + betaE*E + rnorm(n)
  }
  else if(distr == "logit"){
    Ylat = beta0 + betaGE*GE + betaG*G + betaE*E + rlogis(n)
  }
  
  Y = ifelse(Ylat >= 0, 1, 0)
  return(data.frame(Y = Y, G = G))
}

### Function to fit a model once
Fit_once <- function(distr = "logit", data, additive = FALSE){
  if(!additive){
    data$G <- as.factor(data$G)
  }
  mod <- glm(formula = Y~G, data = data, family = binomial(link = distr))
  summary(mod)
}

### Function to do inference once:
Inference_Once <- function(test = "dominance", result){
  if(test == "dominance"){
    test_result <- aod::wald.test(Sigma = result$cov.scaled,
                                  b = result$coefficients[,1],
                                  H0 = c(0),
                                  L = matrix(c(0,2,-1), nrow = 1))
  }
  else if(test == "joint"){
    test_result <- aod::wald.test(Sigma = result$cov.scaled,
                                  b = result$coefficients[,1],
                                  H0 = c(0,0),
                                  L = matrix(c(0,2,-1,0,1,0), nrow = 2, byrow = T))
  }
  else if(test == "additive"){
    test_result <- aod::wald.test(Sigma = result$cov.scaled,
                                  b = result$coefficients[,1],
                                  H0 = c(0),
                                  L = matrix(c(0,1), nrow = 1))
  }
  
  return(test_result$result$chi2)
}

### Function to run simulation for B times:
Simulation <- function(B = 100, original_distr = "logit", fit_distr = "logit", test = "joint",
                       maf = 0.5, betaE = 0, betaGE = 0, betaG = 0.5, beta0 = -1, n = 1000){
  all_result <- NULL
  for (i in 1:B) {
    data = Sim_once(distr = original_distr, maf = maf, betaE = betaE, betaGE = betaGE, betaG = betaG, beta0 = beta0, n = n)
    result = Fit_once(distr = fit_distr, data = data)
    add_result = Fit_once(distr = fit_distr, data = data, additive = TRUE)
    all_result <- rbind(all_result, Inference_Once(result = result, test = test))
    all_result <- rbind(all_result, Inference_Once(result = add_result, test = "additive"))
  }
  all_result <- as.data.frame(all_result)
  all_result$test <- rep(c(test, "additive"),B)
  all_result
}

### Function "Simulation for Power":
Simulation_for_Power <- function(B = 100, original_distr = "logit", fit_distr = "logit", test = "joint",
                                 maf = 0.5, betaE = 0, betaGE = 0, betaG = 0.5, beta0 = -1, n = 1000, alpha = 0.05){
  result <- Simulation(B = B, original_distr = original_distr, fit_distr = fit_distr, test = test,
                       maf = maf, betaE = betaE, betaGE = betaGE, betaG = betaG, beta0 = beta0, n = n)
  mean(result$P <= alpha)
}

### Function "Simulation for Error":
Simulation_for_Error <- function(B = 100, original_distr = "logit", fit_distr = "logit", test = "joint",
                                 maf = 0.5, betaE = 0, betaGE = 0, betaG = 0.5, beta0 = -1, n = 1000, alpha = 0.05){
  result <- Simulation(B = B, original_distr = original_distr, fit_distr = fit_distr, test = test,
                       maf = maf, betaE = betaE, betaGE = betaGE, betaG = betaG, beta0 = beta0, n = n)
  return(result %>% arrange(df))
}

 
# ### Simulation Experiment:
# ### For Type-I Error Rate:
# ### Case 1: when betaE = 0, both distribution being logistic (will work)
# set.seed(123)
# simulation_result1 <- Simulation(B = 1000, n = 2000)
# simulation_result1 <- as.data.frame(simulation_result1)
# hist(simulation_result1$chi2, breaks = 30)
# hist(simulation_result1$P, breaks = 20)
# mean(simulation_result1$P <= 0.05) #0.044
# ### Case 2: when betaE != 0, but both distributions being probit (will work)
# set.seed(123)
# simulation_result2 <- Simulation(B = 1000, betaE = 1, n = 2000, original_distr = "probit", fit_distr = "probit")
# simulation_result2 <- as.data.frame(simulation_result2)
# hist(simulation_result2$chi2, breaks = 30)
# hist(simulation_result2$P, breaks = 20)
# mean(simulation_result2$P <= 0.05) #0.048
# ### Case 3: when betaE != 0, but both distributions being logit (let's try)
# set.seed(123)
# simulation_result3 <- Simulation(B = 1000, betaE = 1, n = 2000)
# simulation_result3 <- as.data.frame(simulation_result3)
# hist(simulation_result3$chi2, breaks = 30)
# hist(simulation_result3$P, breaks = 20)
# mean(simulation_result3$P <= 0.05) #0.043
# 
# 
# ### For Power Computation:
# ### Case 1: when betaE = 0
# set.seed(123)
# simulation_result1 <- Simulation(B = 1000, n = 2000, betaGE = 1)
# simulation_result1 <- as.data.frame(simulation_result1)
# hist(simulation_result1$chi2, breaks = 30)
# hist(simulation_result1$P, breaks = 20)
# mean(simulation_result1$P <= 0.05) #0.158
# ### Case 2: when betaE != 0, but both distributions being probit
# set.seed(123)
# simulation_result2 <- Simulation(B = 1000, betaE = 1, n = 2000, betaGE = 1, original_distr = "probit", fit_distr = "probit")
# simulation_result2 <- as.data.frame(simulation_result2)
# hist(simulation_result2$chi2, breaks = 30)
# hist(simulation_result2$P, breaks = 20)
# mean(simulation_result2$P <= 0.05) #0.64
# ### Case 3: when betaE != 0, but both distributions being logit
# set.seed(123)
# simulation_result3 <- Simulation(B = 1000, betaE = 1, betaGE = 1, n = 2000)
# simulation_result3 <- as.data.frame(simulation_result3)
# hist(simulation_result3$chi2, breaks = 30)
# hist(simulation_result3$P, breaks = 20)
# mean(simulation_result3$P <= 0.05) #0.246




