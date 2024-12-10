# STATS 506 FINAL PROJ FUNCTIONS

setwd("/Users/leonlin/Documents/stats506local/stats506/finalproj")
library(MASS)
library(geepack)
library(Matrix)

# Simulates Data 

simulate_X <- function(n_obs, beta, corr_structure, group_size, corr_param, std_devs = rep(2, group_size)) {
  p <- length(beta) - 1  
  if (corr_structure == "independence") {
    corr_mat <- diag(group_size)  
  } else if (corr_structure == "exchangeable") {
    corr_mat <- matrix(corr_param, nrow = group_size, ncol = group_size) 
    diag(corr_mat) <- 1  
  } else if (corr_structure == "ar1") {
    corr_mat <- matrix(0, nrow = group_size, ncol = group_size)
    for (i in 1:group_size) {
      for (j in 1:group_size) {
        corr_mat[i, j] <- corr_param^abs(i - j) 
      }
    }
  } else {
    stop("Unsupported correlation structure")
  }
  
 
  D <- diag(std_devs)  
  cov_mat <- D %*% corr_mat %*% D 
  
  num_groups <- n_obs / group_size
  if (n_obs %% group_size != 0) {
    stop("n_obs must be divisible by group_size.")
  }
  
  X <- do.call(rbind, lapply(1:num_groups, function(i) {
    predictors <- mvrnorm(n = p, mu = rep(20, group_size), Sigma = cov_mat)
    return(t(predictors))
  }))
  
  X <- cbind(Intercept = 1, X)
  return(X)
}

simulate_X_Y <- function(n_obs, beta, link_function, mean_var_structure, corr_structure, group_size, corr_param) {
  # Generate predictors
  X <- simulate_X(n_obs = n_obs, beta = beta, corr_structure = corr_structure, 
                           group_size = group_size, corr_param = corr_param)
  # Compute linear predictor
  eta <- X %*% beta
  # Apply link function
  if (link_function == "log") {
    mu <- exp(eta)
  } else if (link_function == "identity") {
    mu <- log(1 + exp(eta))
  } else {
    stop("Unsupported link function")
  }
  # Apply mean-var structure 
  if (mean_var_structure == "Poisson") {
    Y <- rpois(n_obs, lambda = mu*3) #scaled by 3
  } else if (mean_var_structure == "Gamma") {
    shape <- 3
    Y <- rgamma(n_obs, shape = shape, scale = mu / shape)
  } else {
    stop("Unsupported mean-variance structure")
  }
  return(list(X = X, Y = Y))
}

# working_corr is the "mis-specified correlation structure" used in the GEE

fit_gee <- function(X, Y, group_size, link_function, mean_var_structure, mis_working_corr) {
  library(geepack)
  num_groups <- nrow(X) / group_size
  data <- data.frame(
    Y = Y,
    group = rep(1:num_groups, each = group_size), 
    X
  )
  colnames(data)[3:ncol(data)] <- paste0("X", 1:(ncol(data) - 2))
  formula <- as.formula(paste("Y ~", paste(colnames(data)[3:ncol(data)], collapse = " + ")))
  family <- switch(mean_var_structure,
                   "Poisson" = poisson(link = link_function),
                   "Gamma" = Gamma(link = link_function),
                   stop("unsupported mean-variance structure"))
  gee_fit <- geeglm(
    formula = formula,
    data = data,
    family = family,
    id = group,
    corstr = mis_working_corr
  )
  return(gee_fit)
}

assess_consistency_efficiency_rmse <- function(n_sim, n_obs, beta, link_function, mean_var_structure, 
                                           corr_structure, mis_working_corr, group_size, corr_param) {
  proper_metrics <- list(beta_hat_rmse = c(), se_rmse = c())
  mis_metrics <- list(beta_hat_rmse = c(), se_rmse = c())
  
  for (i in 1:n_sim) {
    print(i)
    data <- simulate_X_Y(n_obs, beta, link_function, mean_var_structure, corr_structure, group_size, corr_param)
    
    fitted_glm_by_gee_proper <- tryCatch(
      fit_gee(data$X[,-1], data$Y, group_size, link_function, mean_var_structure, corr_structure),
      error = function(e) { message("error in prpoper GEE: ", e$message); return(NULL) }
    )
    if (is.null(fitted_glm_by_gee_proper)) next 
    se_proper <- sqrt(diag(fitted_glm_by_gee_proper$geese$vbeta.naiv))  
    if (any(is.na(se_proper))) next 
    beta_hat_proper <- coef(fitted_glm_by_gee_proper)
    
    # Mis-specified working correlation structure
    fitted_glm_by_gee_mis <- tryCatch(
      fit_gee(data$X[,-1], data$Y, group_size, link_function, mean_var_structure, mis_working_corr),
      error = function(e) { message("error in misspecified GEE: ", e$message); return(NULL) }
    )
    if (is.null(fitted_glm_by_gee_mis)) next  
    se_mis <- sqrt(diag(fitted_glm_by_gee_mis$geese$vbeta.naiv))  
    if (any(is.na(se_mis))) next  
    beta_hat_mis <- coef(fitted_glm_by_gee_mis)
    
    proper_metrics$beta_hat_rmse <- c(proper_metrics$beta_hat_rmse, sqrt(mean((beta_hat_proper - beta)^2)))
    proper_metrics$se_rmse <- c(proper_metrics$se_rmse, sqrt(mean(se_proper^2)))
    
    mis_metrics$beta_hat_rmse <- c(mis_metrics$beta_hat_rmse, sqrt(mean((beta_hat_mis - beta)^2)))
    mis_metrics$se_rmse <- c(mis_metrics$se_rmse, sqrt(mean(se_mis^2)))
  }
  
  result <- list(
    proper_beta_hat_rmse = mean(proper_metrics$beta_hat_rmse, na.rm = TRUE),
    proper_se_rmse = mean(proper_metrics$se_rmse, na.rm = TRUE),
    mis_beta_hat_rmse = mean(mis_metrics$beta_hat_rmse, na.rm = TRUE),
    mis_se_rmse = mean(mis_metrics$se_rmse, na.rm = TRUE)
  )
  return(result)
}

assess_consistency_efficiency_rmse <- function(n_sim, n_obs, beta, link_function, mean_var_structure, 
                                               corr_structure, mis_working_corr, group_size, corr_param) {
  proper_metrics <- list(beta_hat_rmse = c(), se_rmse = c())
  mis_metrics <- list(beta_hat_rmse = c(), se_rmse = c())
  
  for (i in 1:n_sim) {
    print(i)
    data <- simulate_X_Y(n_obs, beta, link_function, mean_var_structure, corr_structure, group_size, corr_param)
    
    fitted_glm_by_gee_proper <- tryCatch(
      fit_gee(data$X[,-1], data$Y, group_size, link_function, mean_var_structure, corr_structure),
      error = function(e) { message("error in prpoper GEE: ", e$message); return(NULL) }
    )
    if (is.null(fitted_glm_by_gee_proper)) next  
    beta_hat_proper <- coef(fitted_glm_by_gee_proper)
    
    fitted_glm_by_gee_mis <- tryCatch(
      fit_gee(data$X[,-1], data$Y, group_size, link_function, mean_var_structure, mis_working_corr),
      error = function(e) { message("Error in mis-specified GEE: ", e$message); return(NULL) }
    )
    if (is.null(fitted_glm_by_gee_mis)) next 
    beta_hat_mis <- coef(fitted_glm_by_gee_mis)
    
    proper_metrics$beta_hat_rmse <- c(proper_metrics$beta_hat_rmse, sqrt(mean((beta_hat_proper - beta)^2)))
    proper_metrics$se_rmse <- cbind(proper_metrics$se_rmse, beta_hat_proper)
    
    mis_metrics$beta_hat_rmse <- c(mis_metrics$beta_hat_rmse, sqrt(mean((beta_hat_mis - beta)^2)))
    mis_metrics$se_rmse <- cbind(proper_metrics$se_rmse, beta_hat_proper)
  }
  
  proper_metrics$se_rmse <- apply(proper_metrics$se_rmse, 1, sd)
  mis_metrics$se_rmse <- apply(mis_metrics$se_rmse, 1, sd)
  
  result <- list(
    proper_beta_hat_rmse = mean(proper_metrics$beta_hat_rmse, na.rm = TRUE),
    proper_se_rmse = sqrt(mean(proper_metrics$se_rmse, na.rm = TRUE)^2),
    mis_beta_hat_rmse = mean(mis_metrics$beta_hat_rmse, na.rm = TRUE),
    mis_se_rmse = sqrt(mean(mis_metrics$se_rmse, na.rm = TRUE)^2)
  )
  return(result)
}

 
