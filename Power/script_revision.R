.libPaths(c("~/lib", .libPaths()))
library(progress)
library(tidyverse)
library(lattice)
library(gridExtra)
source(file = "functions.R")

n <- 300000; p <- 0.3; a <- 5e-8
betaE_vec <- c(-0.5, 0, 0.5)
betaG_vec <- c(-0.5, 0, 0.5)
betaGE_vec <- seq(-1, 1, length.out = 100)

all_sim_result <- sim_once(n = n, p = p, preva = 0.1, betaE_vec = betaE_vec, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, model = "probit", a = a)

# Define color and linetype palettes
colors <- rainbow(length(betaG_vec))
linetypes <- 1:length(betaG_vec)

for (betaEval in betaE_vec) {
  pdf(paste0("figures/dom_power_n_", n, "_MAF_", p, "_betaE_", betaEval, ".pdf"), width = 5, height = 5)
  # Initialize an empty plot using the first betaGval
  first_betaGval <- betaG_vec[1]
  selected_sim_result <- all_sim_result %>%
    filter(betaE == betaEval & betaG == first_betaGval)

  plot(dom_power ~ betaGE, data = selected_sim_result,
       type = "l", xlab = expression(beta[GE]), ylab = "Power",
       ylim = c(0, 1), col = colors[1], lty = linetypes[1])

  # Add lines for all betaG values
  for (i in seq_along(betaG_vec)) {
    betaGval <- betaG_vec[i]
    selected_sim_result <- all_sim_result %>%
      filter(betaE == betaEval & betaG == betaGval)

    lines(dom_power ~ betaGE, data = selected_sim_result,
          col = colors[i], lty = linetypes[i])
  }

  # Add legend
  legend("bottomright", legend = betaG_vec,
         col = colors, lty = linetypes, title = expression(beta[G]))
  dev.off()
}
for (betaEval in betaE_vec) {
  pdf(paste0("figures/joint_power_n_", n, "_MAF_", p, "_betaE_", betaEval, ".pdf"), width = 5, height = 5)
  # Initialize an empty plot using the first betaGval
  first_betaGval <- betaG_vec[1]
  selected_sim_result <- all_sim_result %>%
    filter(betaE == betaEval & betaG == first_betaGval)

  plot(joint_power ~ betaGE, data = selected_sim_result,
       type = "l", xlab = expression(beta[GE]), ylab = "Power",
       ylim = c(0, 1), col = colors[1], lty = linetypes[1])

  # Add lines for all betaG values
  for (i in seq_along(betaG_vec)) {
    betaGval <- betaG_vec[i]
    selected_sim_result <- all_sim_result %>%
      filter(betaE == betaEval & betaG == betaGval)

    lines(joint_power ~ betaGE, data = selected_sim_result,
          col = colors[i], lty = linetypes[i])
  }

  # Add legend
  legend("bottomright", legend = betaG_vec,
         col = colors, lty = linetypes, title = expression(beta[G]))
  dev.off()
}






n <- 30000; p <- 0.3; a <- 5e-8
betaE_vec <- c(-0.5, 0, 0.5)
betaG_vec <- c(-0.5, 0, 0.5)
betaGE_vec <- seq(-1, 1, length.out = 100)

all_sim_result <- sim_once(n = n, p = p, preva = 0.1, betaE_vec = betaE_vec, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, model = "probit", a = a)

# Define color and linetype palettes
colors <- rainbow(length(betaG_vec))
linetypes <- 1:length(betaG_vec)

for (betaEval in betaE_vec) {
  pdf(paste0("figures/dom_power_n_", n, "_MAF_", p, "_betaE_", betaEval, ".pdf"), width = 5, height = 5)
  # Initialize an empty plot using the first betaGval
  first_betaGval <- betaG_vec[1]
  selected_sim_result <- all_sim_result %>%
    filter(betaE == betaEval & betaG == first_betaGval)

  plot(dom_power ~ betaGE, data = selected_sim_result,
       type = "l", xlab = expression(beta[GE]), ylab = "Power",
       ylim = c(0, 1), col = colors[1], lty = linetypes[1])

  # Add lines for all betaG values
  for (i in seq_along(betaG_vec)) {
    betaGval <- betaG_vec[i]
    selected_sim_result <- all_sim_result %>%
      filter(betaE == betaEval & betaG == betaGval)

    lines(dom_power ~ betaGE, data = selected_sim_result,
          col = colors[i], lty = linetypes[i])
  }

  # Add legend
  legend("bottomright", legend = betaG_vec,
         col = colors, lty = linetypes, title = expression(beta[G]))
  dev.off()
}
for (betaEval in betaE_vec) {
  pdf(paste0("figures/joint_power_n_", n, "_MAF_", p, "_betaE_", betaEval, ".pdf"), width = 5, height = 5)
  # Initialize an empty plot using the first betaGval
  first_betaGval <- betaG_vec[1]
  selected_sim_result <- all_sim_result %>%
    filter(betaE == betaEval & betaG == first_betaGval)

  plot(joint_power ~ betaGE, data = selected_sim_result,
       type = "l", xlab = expression(beta[GE]), ylab = "Power",
       ylim = c(0, 1), col = colors[1], lty = linetypes[1])

  # Add lines for all betaG values
  for (i in seq_along(betaG_vec)) {
    betaGval <- betaG_vec[i]
    selected_sim_result <- all_sim_result %>%
      filter(betaE == betaEval & betaG == betaGval)

    lines(joint_power ~ betaGE, data = selected_sim_result,
          col = colors[i], lty = linetypes[i])
  }

  # Add legend
  legend("bottomright", legend = betaG_vec,
         col = colors, lty = linetypes, title = expression(beta[G]))
  dev.off()
}
