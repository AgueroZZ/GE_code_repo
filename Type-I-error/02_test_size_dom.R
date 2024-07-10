.libPaths(c("~/lib", .libPaths()))
library(progress)
library(dplyr)
source(file = "00_test_size_fun.R")

### More complete experiment:
betaE_vec = c(0,1)
maf_vec = c(0.5,0.3,0.1)
all_sim_result <- expand.grid(betaE = betaE_vec, maf = maf_vec)
B  = 100000
for (i in 1:nrow(all_sim_result)) {
  result <- Simulation_for_Error(B = B, n = 100000, betaE = all_sim_result$betaE[i], betaG = 0.5,
                                       betaGE = 0, maf = all_sim_result$maf[i], test = "dominance")
  
  save(result, file = paste0("betaE_", all_sim_result$betaE[i], "_maf", all_sim_result$maf[i], "_test_size_n100000.rda"))
}
