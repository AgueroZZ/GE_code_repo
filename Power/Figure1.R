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
gammaD_logistic_adj <- function(beta_GE, beta_0 = -2, beta_G = 1, sigma_E = 1, beta_E = 0){
  gamma_vec <- compute_adj_gamma(beta0 = beta_0, betaG = beta_G, betaE = beta_E, betaGE = beta_GE, muE = 0, sigmaE = sigma_E)
  gamma_vec[3] - 2* gamma_vec[2] + gamma_vec[1]
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
  root_solution <- uniroot(the_func, interval = c(-30, 30))
  root_solution$root
}

preva_choice <- 0.05; sigmaE_choice <- 1
obj <- function(betaG, betaGE, the_model = "probit", betaE = 0, preva = 0.1, sigmaE = 1) {
  beta0 <- Compute_beta0(preva = preva, beta_GE = betaGE, beta_G = betaG, beta_E = betaE, sigma_E = sigmaE)
  vec <- Compute_Gamma(beta_0 = beta0, beta_G = betaG, beta_GE = betaGE, beta_E = betaE, model = the_model, sigma_E = sigmaE)
  round(vec[3] - 2*vec[2] + vec[1], digits = 10)
}
obj2 <- function(betaG, betaGE, the_model = "probit", p = 0.3, betaE = 0, preva = 0.1, sigmaE = 1) {
  beta0 <- Compute_beta0(preva = preva, beta_GE = betaGE, beta_G = betaG, beta_E = betaE, sigma_E = sigmaE)
  vec <- Compute_Gamma(beta_0 = beta0, beta_G = betaG, beta_GE = betaGE, beta_E = betaE, model = the_model, sigma_E = sigmaE)
  gammaD <- vec[3] - 2*vec[2] + vec[1]
  q <- 1 - p
  R2A <- (2*(p*gammaD + (vec[2] - vec[1]))^2)/((2*(p*gammaD + (vec[2] - vec[1]))^2) + p*q*gammaD^2)
  round(R2A, digits = 10)
}
obj_vec <- Vectorize(obj); obj2_vec <- Vectorize(obj2)


betaGE_vec <- seq(-1, 1, length.out = 500)
betaE_vec <- c(-0.5, 0, 0.5)
betaG_vec <- c(-0.5, 0, 0.5)

# Define color and linetype palettes
colors <- c("#E69F00", # orange
            "#56B4E9", # sky blue
            "#009E73") # bluish green
linetypes <- c(1,2,4)

# Loop over betaE values
for (betaEval in betaE_vec) {
  pdf(paste0("figures/gammaD_", "_betaE_", betaEval, ".pdf"), width = 5, height = 5)
  par(mgp = c(2.2, 1, 0))  # 更近
  
  plot(NULL, xlim = range(betaGE_vec), ylim = c(-1, 1), 
       cex.lab = 1.2, cex.axis = 1.2, lwd = 2.5,
       xlab = expression(beta[GE]), ylab = expression(gamma[D]))
  
  for (i in seq_along(betaG_vec)) {
    betaGval <- betaG_vec[i]
    
    # Compute obj for each betaGE value
    y_vals <- obj_vec(betaG = betaGval, betaGE = betaGE_vec, betaE = betaEval, preva = preva_choice, sigmaE = sigmaE_choice)
    
    lines(betaGE_vec, y_vals, col = colors[i], lty = linetypes[i], lwd = 2.5)
  }
  
  # Add legend for the first value of betaEval
  if(betaEval == betaE_vec[1]) {
    legend("topleft", legend = betaG_vec, bty = "n",
           col = colors, lty = linetypes, title = expression(beta[G]), lwd = 2.5)
  }
  dev.off()
}
for (betaEval in betaE_vec) {
  pdf(paste0("figures/RD_", "_betaE_", betaEval, ".pdf"), width = 5, height = 5)
  par(mgp = c(2.2, 1, 0))  # 更近
  plot(NULL, xlim = range(betaGE_vec), ylim = c(0, 1), 
       cex.lab = 1.2, cex.axis = 1.2, lwd = 2.5,
       xlab = expression(beta[GE]), ylab = expression(R[D]^2))
  
  for (i in seq_along(betaG_vec)) {
    betaGval <- betaG_vec[i]
    
    # Compute obj for each betaGE value
    y_vals <- 1 - obj2_vec(betaG = betaGval, betaGE = betaGE_vec, betaE = betaEval, preva = preva_choice, sigmaE = sigmaE_choice)
    
    lines(betaGE_vec, y_vals, col = colors[i], lty = linetypes[i], lwd = 2.5)
  }
  
  # Add legend for the first value of betaEval
  if(betaEval == betaE_vec[1]) {
    legend("topleft", legend = betaG_vec, bty = "n",
           col = colors, lty = linetypes, title = expression(beta[G]), lwd = 2.5)
  }

  dev.off()
}




