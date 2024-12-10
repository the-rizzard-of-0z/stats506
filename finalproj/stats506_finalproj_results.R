# stats 506 final project sims

setwd("/Users/leonlin/Documents/stats506local/stats506/finalproj")
source("stats506_finalproj_functions.R")
set.seed(100)

# Load required libraries
library(geepack)
library(MASS)

# parameters
n_sim <- 20  # Number of simulations
sample_sizes <- c(10, 20, 40, 80, 160, 320)  # For consistency
num_clusters <- c(2, 3, 4, 5, 6, 10)  # For efficiency
link_functions <- c("log", "identity")
mean_var_structures <- c("Poisson", "Gamma")
working_corr_structures <- c("independence", "exchangeable", "ar1")
beta <- c(runif(1, min = 9, max = 11), runif(3, min = -2, max = 2))  # Intercept and covariates
beta <- c(10, c(2,-1,3)) 
corr_param <- 0.8

consistency_results <- data.frame()
efficiency_results <- data.frame()

# Loop consisitcy 
for (n_obs in sample_sizes) {
  for (link_function in link_functions) {
    for (mean_var_structure in mean_var_structures) {
      print(paste("n:", n_obs))
      print(paste("link:", link_function))
      print(paste("meanvar:", mean_var_structure))
      for (real_corr in working_corr_structures) { 
        for (specified_corr in working_corr_structures) { 
          res <- assess_consistency_efficiency_rmse(
            n_sim = n_sim,
            n_obs = n_obs,
            beta = beta,
            link_function = link_function,
            mean_var_structure = mean_var_structure,
            corr_structure = real_corr, 
            mis_working_corr = specified_corr, 
            group_size = n_obs / 2,
            corr_param = corr_param
          )
          consistency_results <- rbind(consistency_results, data.frame(
            SampleSize = n_obs,
            LinkFunction = link_function,
            MeanVarStructure = mean_var_structure,
            RealWorkingCorr = real_corr,  
            SpecifiedWorkingCorr = specified_corr, 
            BetaHatRMSE = res$mis_beta_hat_rmse
          ))
        }
      }
    }
  }
}


# Loop efficeny
for (n_clusters in num_clusters) {
  for (link_function in link_functions) {
    for (mean_var_structure in mean_var_structures) {
      print(paste("n_clusters:", n_clusters))
      print(paste("link:", link_function))
      print(paste("meanvar:", mean_var_structure))
      for (real_corr in working_corr_structures) { 
        for (specified_corr in working_corr_structures) {
          n_obs <- 180  
          res <- assess_consistency_efficiency_rmse(
            n_sim = n_sim,
            n_obs = n_obs,
            beta = beta,
            link_function = link_function,
            mean_var_structure = mean_var_structure,
            corr_structure = real_corr,  
            mis_working_corr = specified_corr,  
            group_size = n_obs / n_clusters,
            corr_param = corr_param
          )
          efficiency_results <- rbind(efficiency_results, data.frame(
            NumClusters = n_clusters,
            LinkFunction = link_function,
            MeanVarStructure = mean_var_structure,
            RealWorkingCorr = real_corr, 
            SpecifiedWorkingCorr = specified_corr, 
            SERMSE = res$mis_se_rmse
          ))
        }
      }
    }
  }
}


head(consistency_results)
head(efficiency_results)

write.csv(consistency_results, "consistency_results.csv", row.names = FALSE)
write.csv(efficiency_results, "efficiency_results.csv", row.names = FALSE)


