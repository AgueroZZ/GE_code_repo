### Simulate a genotype dataframe
### n: number of individuals
### p: Minor allele frequency
simulate_genotype <- function(n, p) {
  # Simulate a genotype matrix with n individuals and p minor allele frequency
  # Each individual has two alleles (0, 1, or 2)

  # Output a dataframe, with additive and dominant codings
  df <- data.frame(
    id = 1:n
  )

  genotype_matrix <- matrix(0, nrow = n, ncol = 2)

  for (i in 1:n) {
    genotype_matrix[i, ] <- rbinom(2, 1, p)
  }

  df$G0 <- ifelse(rowSums(genotype_matrix) == 0, 1, 0)
  df$G1 <- ifelse(rowSums(genotype_matrix) == 1, 1, 0)
  df$G2 <- ifelse(rowSums(genotype_matrix) == 2, 1, 0)
  df$GA <- rowSums(genotype_matrix)
  df$GD <- ifelse(rowSums(genotype_matrix) == 1, 1, 0)
  return(df)
}

# simulate phenotype
# gamma: effect at each value of genotype
# binary: if TRUE, convert the phenotype to binary (0, 1) based on whether Y >= 0
simulate_phenotype <- function(df, gamma = c(0.3, 0.45, 0.45), binary = TRUE) {
  # Simulate a phenotype based on the genotype dataframe
  n <- nrow(df)

  # Simulate the error term
  e <- rlogis(n, 1)

  # Calculate the phenotype for both models
  df$y <- gamma[1]*df$G0 + gamma[2]*df$G1 + gamma[3]*df$G2 + e

  # If binary is TRUE, convert the phenotype to binary
  if (binary) {
    # Convert to binary based on whether y >= 0
    df$y <- ifelse(df$y >= 0, 1, 0)
  }

  return(df)
}

# set seed for reproducibility
replication_once <- function(seed, maf_vec){
  joint_pval <- c()
  add_pval <- c()
  dom_pval <- c()
  for (maf in maf_vec) {
    # Simulate a genotype dataframe
    df_try <- simulate_genotype(300000, maf)
    df_all <- simulate_phenotype(df_try, binary = TRUE)

    ### Test additive and dominant effects each, as well as jointly
    model_full <- glm(y ~ GA + GD, data = df_all, family = binomial)

    # joint test
    model_reduced <- glm(y ~ 1, data = df_all, family = binomial)

    # compare p-values
    add_pval <- c(add_pval, summary(model_full)$coefficients[2,4])
    dom_pval <- c(dom_pval, summary(model_full)$coefficients[3,4])
    # joint_pval <- c(joint_pval, anova(model_reduced, model_full, test = "LRT")$`Pr(>Chi)`[2])

    # Do Wald test for joint test rather than likelihood ratio test
    # model_reduced <- glm(y ~ GA, data = df_all, family = binomial)
    MLE_vec <- summary(model_full)$coefficients[2:3, 1]
    COV_mat <- summary(model_full)$cov.scaled[2:3, 2:3]
    joint_test_stat <- t(MLE_vec) %*% solve(COV_mat) %*% MLE_vec
    joint_pval <- c(joint_pval, pchisq(joint_test_stat, df = 2, lower.tail = FALSE))
  }
  df <- data.frame(
    maf = maf_vec,
    joint_pval = joint_pval,
    add_pval = add_pval,
    dom_pval = dom_pval
  )
  return(df)
}

# result_once <- data.frame(
#   maf = numeric(),
#   joint_pval = numeric(),
#   add_pval = numeric(),
#   dom_pval = numeric()
# )
# # obtain 50 replications
# B <- 50
# for (i in 1:B) {
#   result_once <- rbind(result_once, replication_once(i, seq(0.05, 0.5, by = 0.05)))
# }
# 
# # save the result
# save(result_once, file = "result_once.RData")

# summary of the result
library(tidyverse)
# load the result
load("result_once.RData")


# Take out rows with Inf
result_once <- result_once %>%
  filter(!is.infinite(joint_pval) & !is.infinite(add_pval) & !is.infinite(dom_pval))

# summarize the mean, upper and lower bounds of the p-values
summary_joint <- result_once %>%
  group_by(maf) %>%
  summarise(
    mean = mean(-log10(joint_pval)),
    upper = quantile(-log10(joint_pval), 0.975),
    lower = quantile(-log10(joint_pval), 0.025)
  )

summary_add <- result_once %>%
  group_by(maf) %>%
  summarise(
    mean = mean(-log10(add_pval)),
    upper = quantile(-log10(add_pval), 0.975),
    lower = quantile(-log10(add_pval), 0.025)
  )

summary_dom <- result_once %>%
  group_by(maf) %>%
  summarise(
    mean = mean(-log10(dom_pval)),
    upper = quantile(-log10(dom_pval), 0.975),
    lower = quantile(-log10(dom_pval), 0.025)
  )


# plot
png("joint_test_vs_additive_and_dominant.png", width = 450, height = 450)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
plot(summary_joint$maf, (summary_joint$mean), type = "o", col = "red", lwd = 2, ylim = c(0, 100), xlab = "Minor Allele Frequency", ylab = "-log10(p-value)", main = "Joint Test vs. Additive and Dominant Tests")
polygon(c(summary_joint$maf, rev(summary_joint$maf)), c((summary_joint$upper), rev((summary_joint$lower))), col = rgb(1, 0, 0, 0.2), border = NA)
lines(summary_add$maf, (summary_add$mean), col = "blue", lwd = 2, type = "o")
polygon(c(summary_add$maf, rev(summary_add$maf)), c((summary_add$upper), rev((summary_add$lower))), col = rgb(0, 0, 1, 0.2), border = NA)
lines(summary_dom$maf, (summary_dom$mean), col = "green", lwd = 2, type = "o")
polygon(c(summary_dom$maf, rev(summary_dom$maf)), c((summary_dom$upper), rev((summary_dom$lower))), col = rgb(0, 1, 0, 0.2), border = NA)
legend("topright", legend = c("Joint Test", "Additive Test", "Dominance Test"), col = c("red", "blue", "green"), lty = 1, lwd = 2)
dev.off()


summary_joint$cor <- (1 - 2*summary_joint$maf)/sqrt(1- 2*summary_joint$maf*(1 - summary_joint$maf))
summary_add$cor <- (1 - 2*summary_add$maf)/sqrt(1- 2*summary_add$maf*(1 - summary_add$maf))
summary_dom$cor <- (1 - 2*summary_dom$maf)/sqrt(1- 2*summary_dom$maf*(1 - summary_dom$maf))

png("joint_test_vs_additive_and_dominant_correlation.png", width = 450, height = 450)
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
plot(summary_joint$cor, (summary_joint$mean), type = "o", col = "red", lwd = 2, ylim = c(0, 100), xlab = "Correlation between GA and GD", ylab = "-log10(p-value)", main = "Joint Test vs. Additive and Dominant Tests")
polygon(c(summary_joint$cor, rev(summary_joint$cor)), c((summary_joint$upper), rev((summary_joint$lower))), col = rgb(1, 0, 0, 0.2), border = NA)
lines(summary_add$cor, (summary_add$mean), col = "blue", lwd = 2, type = "o")
polygon(c(summary_add$cor, rev(summary_add$cor)), c((summary_add$upper), rev((summary_add$lower))), col = rgb(0, 0, 1, 0.2), border = NA)
lines(summary_dom$cor, (summary_dom$mean), col = "green", lwd = 2, type = "o")
polygon(c(summary_dom$cor, rev(summary_dom$cor)), c((summary_dom$upper), rev((summary_dom$lower))), col = rgb(0, 1, 0, 0.2), border = NA)
legend("topright", legend = c("Joint Test", "Additive Test", "Dominance Test"), col = c("red", "blue", "green"), lty = 1, lwd = 2)
dev.off()



