.libPaths(c("~/lib", .libPaths()))
library(progress)
library(tidyverse)
library(lattice)
library(gridExtra)
source(file = "functions.R")

level_num = 100
coul1 <- cm.colors(level_num)
coul2 <- rev(terrain.colors(level_num))

plot_single_case <- function(n, p, preva, betaG_vec, betaGE_vec, betaE_vec, a = 5e-8, location_repo = ""){
  all_sim_result <- sim_once(n = n, p = p, preva = 0.1, betaE_vec = betaE_vec, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, model = "probit", a = a)
  all_sim_result$power_gain <- all_sim_result$joint_power - all_sim_result$add_power
  for (betaE_interested in betaE_vec) {
    selected_data <- all_sim_result %>% filter(betaE == betaE_interested)
    
    my_color_scale <- scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                                           midpoint = 0, limit = c(-1, 1), 
                                           space = "Lab", name= "Power Increase")
    
    ggplot(selected_data, aes(x = betaG, y = betaGE, fill = power_gain)) +
      geom_raster() +  # Or geom_raster() for large datasets
      my_color_scale +
      labs(x = expression(beta[G]), y = expression(beta[GE]), title = "Power Gain from the Joint Test") +
      theme_minimal() +
      theme(axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 20, face = "bold"),
            legend.key.size = unit(2, "lines"),  # Larger legend keys
            plot.title = element_text(size = 20, face = "bold"))
    ggsave(filename = paste0("figures/", location_repo, "powergain_n_", n, "_MAF_", p, "_betaE_", betaE_interested, ".pdf"), width = 5, height = 5)
    
    my_color_scale2 <- scale_fill_gradient2(low = "white", high = "red", 
                                           midpoint = 0, limit = c(0, 1), 
                                           space = "Lab", name= "Power")
    
    ggplot(selected_data, aes(x = betaG, y = betaGE, fill = dom_power)) +
      geom_raster() +  # Or geom_raster() for large datasets
      my_color_scale2 +
      labs(x = expression(beta[G]), y = expression(beta[GE]), title = "") +
      theme_minimal() +
      theme(axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 16, face = "bold"),             
            legend.title = element_text(size = 20, face = "bold"),           
            legend.key.size = unit(2, "lines"),  # Larger legend keys
            plot.title = element_text(size = 20, face = "bold")) +
      geom_point(aes(x = 0, y = 0), shape = 3, size = 5, color = "black")  # Small cross at the origin
    
    
    ggsave(filename = paste0("figures/", location_repo, "dom_power_n_", n, "_MAF_", p, "_betaE_", betaE_interested, ".pdf"), width = 5, height = 5)
    

    ggplot(selected_data, aes(x = betaG, y = betaGE, fill = joint_power)) +
      geom_raster() +  # Or geom_raster() for large datasets
      my_color_scale2 +
      labs(x = expression(beta[G]), y = expression(beta[GE]), title = "") +
      theme_minimal() +
      theme(axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 16, face = "bold"),
            legend.title = element_text(size = 20, face = "bold"),
            legend.key.size = unit(2, "lines"),  # Larger legend keys
            plot.title = element_text(size = 20, face = "bold")) +
      geom_point(aes(x = 0, y = 0), shape = 3, size = 5, color = "black")  # Small cross at the origin
    
    ggsave(filename = paste0("figures/", location_repo, "joint_power_n_", n, "_MAF_", p, "_betaE_", betaE_interested, ".pdf"), width = 5, height = 5)
    
    ggplot(selected_data, aes(x = betaG, y = betaGE, fill = add_power)) +
      geom_raster() +  # Or geom_raster() for large datasets
      my_color_scale2 +
      labs(x = expression(beta[G]), y = expression(beta[GE]), title = "") +
      theme_minimal() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            legend.text = element_text(size = 16, face = "bold"),             
            legend.title = element_text(size = 20, face = "bold"),
            legend.key.size = unit(2, "lines"),  # Larger legend keys
            plot.title = element_text(size = 20, face = "bold")) + 
      geom_point(aes(x = 0, y = 0), shape = 3, size = 5, color = "black")  # Small cross at the origin
    
    ggsave(filename = paste0("figures/", location_repo, "add_power_n_", n, "_MAF_", p, "_betaE_", betaE_interested, ".pdf"), width = 5, height = 5)
    
  }
  all_sim_result
}


n = 3000; p = 0.3; preva = 0.1
betaE_vec <- seq(-0.5,0.5, by = 0.5)
betaG_vec = seq(-1,1, by = 0.01)
betaGE_vec = seq(-1,1, by = 0.01)
plot_single_case(n = n, p = p, preva = preva, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, betaE_vec = betaE_vec, location_repo = "n = 3000/", a = 5e-8)

n = 4000;
plot_single_case(n = n, p = p, preva = preva, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, betaE_vec = betaE_vec, location_repo = "n = 4000/", a = 5e-8)

n = 8000;
plot_single_case(n = n, p = p, preva = preva, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, betaE_vec = betaE_vec, location_repo = "n = 8000/", a = 5e-8)

n = 30000;
plot_single_case(n = n, p = p, preva = preva, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, betaE_vec = betaE_vec, location_repo = "n = 30,000/", a = 5e-8)

n = 300000; 
plot_single_case(n = n, p = p, preva = preva, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, betaE_vec = betaE_vec, location_repo = "n = 300,000/", a = 5e-8)

n = 800000;
plot_single_case(n = n, p = p, preva = preva, betaG_vec = betaG_vec, betaGE_vec = betaGE_vec, betaE_vec = betaE_vec, location_repo = "n = 800,000/", a = 5e-8)



