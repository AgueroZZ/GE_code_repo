library(latex2exp)
library(tidyverse)
library(metR)

compute_adj_gamma <- function(beta0, betaG, betaE = 0, betaGE, muE = 0, sigmaE = 1, B = 100000, seed = 123){
  Prev_given_G <- function(G_val){
    G <- rep(G_val, B)
    E <- rnorm(B, mean = muE, sd = sigmaE)
    GE <- (G*E)
    Ylat = beta0 + betaGE*GE + betaG*G + betaE*E + rlogis(B)
    Y = ifelse(Ylat >= 0, 1, 0)
    return(mean(Y))
  }
  set.seed(seed)
  return(qlogis(c(Prev_given_G(0), Prev_given_G(1), Prev_given_G(2))))
}

Compute_Gamma <- function(beta_GE, beta_0 = -2, beta_G = 1, sigma_E = 1, beta_E = 0, model = "probit"){
  if(model == "probit"){
    G <- c(0,1,2)
    sigma_eps <- 1
    eta_G = (beta_0 + beta_G * G)
    deno <- sqrt(((beta_E + beta_GE*G)*sigma_E)^2 + sigma_eps^2)
    gammaVEC <- eta_G/deno
    gammaVEC
  }
  else if(model == "logit"){
    compute_adj_gamma(beta0 = beta_0, betaG = beta_G, betaE = beta_E, betaGE = beta_GE, muE = 0, sigmaE = sigma_E, B = 100000, seed = 123)
  }
}

Compute_Preva <- function(beta_GE, beta_0 = -2, beta_G = 1, sigma_E = 1, beta_E = 0, model = "probit", p = 0.5){
  gammaVec <- Compute_Gamma(beta_GE = beta_GE, beta_0 = beta_0, beta_G = beta_G, sigma_E = sigma_E, beta_E = beta_E, model = model)
  q <- 1 - p
  if(model == "logit"){
    p^2 * plogis(gammaVec[3]) + (2*q*p) * plogis(gammaVec[2]) + (q^2) * plogis(gammaVec[1])
  }
  else{
    p^2 * pnorm(gammaVec[3]) + (2*q*p) * pnorm(gammaVec[2]) + (q^2) * pnorm(gammaVec[1])
  }
}

Compute_beta0 <- function(preva, beta_GE = 0, beta_G = 0, sigma_E = 1, beta_E = 0, model = "probit", p = 0.5){
  the_func <- function(x){
    Compute_Preva(beta_0 = x, beta_GE = beta_GE, beta_G = beta_G, sigma_E = sigma_E, beta_E = beta_E, model = model, p = p) - preva
  }
  root_solution <- uniroot(the_func, interval = c(-10, 10))
  root_solution$root
}

compute_ncp <- function(n, beta_0 = -2, p = 0.5, sigma_E = 1, beta_E = 0, beta_G, beta_GE, model = "probit"){
  q <- 1 - p
  I <- matrix(data = 0, nrow = 3, ncol = 3)
  gammaVec <- Compute_Gamma(beta_0 = beta_0, beta_G = beta_G, beta_E = beta_E, beta_GE = beta_GE, sigma_E = sigma_E, model = model)
  if(model == "probit"){
    I <- matrix(c((q^2)*(dnorm(gammaVec[1])),0,0,0,(2*p*q)*(dnorm(gammaVec[2])),0,0,0,(p^2)*(dnorm(gammaVec[3]))), byrow = T, nrow = 3)
  }
  else{
    I <- matrix(c((q^2)*(dlogis(gammaVec[1])),0,0,0,(2*p*q)*(dlogis(gammaVec[2])),0,0,0,(p^2)*(dlogis(gammaVec[3]))), byrow = T, nrow = 3)
  }
  COVmat <- solve(I)/n
  L1 <- matrix(cbind(c(p*q,-2*p*q,p*q,-q,(q-p),p)), nrow = 2, byrow = T)
  ncp_joint <- t(L1%*%c(gammaVec)) %*% solve(L1%*% COVmat %*%t(L1)) %*% (L1%*%c(gammaVec))
  L2 <- matrix(cbind(c(p*q,-2*p*q,p*q)), nrow = 1, byrow = T)
  ncp_dom <- t(L2%*%c(gammaVec)) %*% solve(L2%*% COVmat %*%t(L2)) %*% (L2%*%c(gammaVec))
  L3 <- matrix(cbind(c(-q,(q-p),p)), nrow = 1, byrow = T)
  ncp_add <- t(L3%*%c(gammaVec)) %*% solve(L3%*% COVmat %*%t(L3)) %*% (L3%*%c(gammaVec))
  return(list(gammaVec = gammaVec, COVmat = COVmat, ncp_joint = ncp_joint, ncp_dom = ncp_dom, ncp_add = ncp_add))
}


### Step 4: Convert ncp to power:
compute_power_ncp <- function(ncp, df = 1, a = 5e-8){
  1 - pchisq(q = qchisq(p = (1-a), df = df), ncp = ncp, df = df)
}


sim_once <- function(n, p, preva, beta_0 = NULL, betaE_vec, betaG_vec, betaGE_vec, model = "probit", a = 5e-3, sigma_E = 1){
  all_sim_result <- expand.grid(betaE = betaE_vec, betaG = betaG_vec, betaGE = betaGE_vec)
  joint_power_vec <- numeric(length = nrow(all_sim_result))
  add_power_vec <- numeric(length = nrow(all_sim_result))
  dom_power_vec <- numeric(length = nrow(all_sim_result))
  
  pb <- progress_bar$new(total = nrow(all_sim_result), format = "[:bar] :percent :elapsed")
  joint_ncp_vec <- c()
  add_ncp_vec <- c()
  dom_ncp_vec <- c()
  
  for (i in 1:nrow(all_sim_result)) {
    if(is.null(beta_0)){
      beta_0 <- Compute_beta0(beta_GE = all_sim_result$betaGE[i], preva = preva, beta_G = all_sim_result$betaG[i], sigma_E = sigma_E, beta_E = all_sim_result$betaE[i], model = model, p = p)
    }
    ncp_result <- compute_ncp(n = n, beta_E = all_sim_result$betaE[i], beta_G = all_sim_result$betaG[i],
                              beta_GE = all_sim_result$betaGE[i], p = p, beta_0 = beta_0, model = model)
    joint_ncp_vec[i] <- ncp_result$ncp_joint
    joint_power_vec[i] <- compute_power_ncp(ncp = joint_ncp_vec[i], df = 2, a = a)
    add_ncp_vec[i] <- ncp_result$ncp_add
    add_power_vec[i] <- compute_power_ncp(ncp = add_ncp_vec[i], df = 1, a = a)
    dom_ncp_vec[i] <- ncp_result$ncp_dom
    dom_power_vec[i] <- compute_power_ncp(ncp = dom_ncp_vec[i], df = 1, a = a)
    pb$tick()
  }
  all_sim_result$joint_power <- joint_power_vec
  all_sim_result$add_power <- add_power_vec
  all_sim_result$dom_power <- dom_power_vec
  all_sim_result$joint_ncp <- joint_ncp_vec
  all_sim_result$add_ncp <- add_ncp_vec
  all_sim_result$dom_ncp <- dom_ncp_vec
  all_sim_result
}
