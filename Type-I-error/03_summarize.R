library(dplyr)
library(showtext)
showtext_auto()

load(file = "results/betaE_0_maf0.1_test_size_n100000.rda")
B  = 100000
pdf(file = "nullbetaE0_maf0.1.pdf", width = 5, height = 5)
hist(result$P, breaks = 30, freq = FALSE, xlab = "P-val", 
     main = bquote(paste("β"[E], " = 0, MAF = 0.1")))
abline(h = 1, col = "red")
dev.off()
alpha = 5e-2
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-3
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-4
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
load(file = "results/betaE_1_maf0.1_test_size_n100000.rda")
pdf(file = "nullbetaE1_maf0.1.pdf", width = 5, height = 5)
hist(result$P, breaks = 30, freq = FALSE, xlab = "P-val", 
     main = bquote(paste("β"[E], " = 1, MAF = 0.1")))
abline(h = 1, col = "red")
dev.off()
B  = 100000
alpha = 5e-2
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-3
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-4
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)



load(file = "results/betaE_0_maf0.3_test_size_n100000.rda")
B  = 100000
pdf(file = "nullbetaE0_maf0.3.pdf", width = 5, height = 5)
hist(result$P, breaks = 30, freq = FALSE, xlab = "P-val", 
     main = bquote(paste("β"[E], " = 0, MAF = 0.3")))
abline(h = 1, col = "red")
dev.off()
alpha = 5e-2
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-3
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-4
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
load(file = "results/betaE_1_maf0.3_test_size_n100000.rda")
pdf(file = "nullbetaE1_maf0.3.pdf", width = 5, height = 5)
hist(result$P, breaks = 30, freq = FALSE, xlab = "P-val", 
     main = bquote(paste("β"[E], " = 1, MAF = 0.3")))
abline(h = 1, col = "red")
dev.off()
B  = 100000
alpha = 5e-2
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-3
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-4
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)







load(file = "results/betaE_0_maf0.5_test_size_n100000.rda")
pdf(file = "nullbetaE0_maf0.5.pdf", width = 5, height = 5)
hist(result$P, breaks = 30, freq = FALSE, xlab = "P-val", 
     main = bquote(paste("β"[E], " = 0, MAF = 0.5")))
abline(h = 1, col = "red")
dev.off()
B  = 100000
alpha = 5e-2
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-3
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-4
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
load(file = "results/betaE_1_maf0.5_test_size_n100000.rda")
pdf(file = "nullbetaE1_maf0.5.pdf", width = 5, height = 5)
hist(result$P, breaks = 30, freq = FALSE, xlab = "P-val", 
     main = bquote(paste("β"[E], " = 1, MAF = 0.5")))
abline(h = 1, col = "red")
dev.off()
B  = 100000
alpha = 5e-2
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-3
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)
alpha = 5e-4
result %>% group_by(df) %>% summarise(error = mean(P <= alpha),
                                      lower = max(error - 2* sqrt((error*(1-error))/B), 0),
                                      upper = min(error + 2* sqrt((error*(1-error))/B), 1)
)











