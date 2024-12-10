# Load ggplot2 for plotting
library(ggplot2)
library(tidyverse)
setwd("/Users/leonlin/Documents/stats506local/stats506/finalproj")
source("stats506_finalproj_functions.R")

consistency_results <- read.csv("consistency_results.csv")
efficiency_results <- read.csv("efficiency_results.csv")

plot_efficiency <- function(data, real_corr) {
  # Extract unique interaction labels from the data for validation
  unique_labels <- unique(interaction(data$MeanVarStructure, data$LinkFunction))
  print(unique_labels) # Debugging step to verify interaction levels

  ggplot(data[data$RealWorkingCorr == real_corr, ], aes(x = NumClusters, y = log(1+SERMSE,10))) +
    geom_point(aes(
      color = interaction(MeanVarStructure, LinkFunction), 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = interaction(MeanVarStructure, LinkFunction), 
      group = interaction(SpecifiedWorkingCorr, MeanVarStructure, LinkFunction)
    )) +
    scale_color_manual(
      values = c(
        "Poisson.log" = "blue", 
        "Poisson.identity" = "red", 
        "Gamma.log" = "green", 
        "Gamma.identity" = "purple"
      ),
      labels = c(
        "Poisson, Log", 
        "Poisson, Identity", 
        "Gamma, Log", 
        "Gamma, Identity"
      )
    ) +
    scale_shape_manual(values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18)) +
    labs(
      title = paste("Efficiency Plot for Real Working Correlation:", real_corr),
      x = "Number of Clusters",
      y = "Beta Hat RMSE",
      color = "GLM Type",
      shape = "Specified Working Corr"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}


real_corrs <- unique(efficiency_results$RealWorkingCorr)
plots <- lapply(real_corrs, function(corr) plot_efficiency(efficiency_results, corr))

pdf("efficiency_plots.pdf", width = 10, height = 7)
for (plot in plots) print(plot)
dev.off()

consistency_results$InteractionFactor <- factor(
  interaction(consistency_results$MeanVarStructure, consistency_results$LinkFunction),
  levels = c("Poisson.log", "Poisson.identity", "Gamma.log", "Gamma.identity")
)

plot_consistency <- function(data, real_corr) {
  ggplot(data[data$RealWorkingCorr == real_corr, ], 
         aes(x = SampleSize, y = BetaHatRMSE)) +
    geom_point(aes(
      color = InteractionFactor, 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = InteractionFactor, 
      group = interaction(SpecifiedWorkingCorr, InteractionFactor)
    )) +
    scale_color_manual(
      values = c("Poisson.log" = "blue", "Poisson.identity" = "red", 
                 "Gamma.log" = "green", "Gamma.identity" = "purple"),
      labels = c("Poisson, Log Link", "Poisson, Identity Link", 
                 "Gamma, Log Link", "Gamma, Identity Link")
    ) +
    scale_shape_manual(
      values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18),
      labels = c("Independence", "Exchangeable", "AR(1)")
    ) +
    labs(
      title = paste("Consistency of Beta-hat under", real_corr, "Working Correlation Structure"),
      x = "Sample Size",
      y = "RMSE of Beta-hat",
      color = "GLM Type (Mean-Variance, Link Function)",
      shape = "Specified Working Correlation Structure"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

real_corrs <- unique(consistency_results$RealWorkingCorr)
plots <- lapply(real_corrs, function(corr) plot_consistency(consistency_results, corr))

# Save or display the plots
pdf("consistency_plots.pdf", width = 10, height = 7)
for (plot in plots) print(plot)
dev.off()


# DELETE LATER 

plot_efficiency_gamma <- function(data, real_corr) {
  gamma_data <- data[data$RealWorkingCorr == real_corr & data$MeanVarStructure == "Gamma", ]
  ggplot(gamma_data, aes(x = NumClusters, y = SERMSE)) +
    geom_point(aes(
      color = LinkFunction, 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = LinkFunction, 
      group = interaction(SpecifiedWorkingCorr, LinkFunction)
    )) +
    scale_color_manual(values = c("log" = "green", "identity" = "purple"),
                       labels = c("Gamma, Log", "Gamma, Identity")) +
    scale_shape_manual(values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18)) +
    labs(
      title = paste("Efficiency Plot for Gamma - Real Working Correlation:", real_corr),
      x = "Number of Clusters",
      y = "Beta Hat RMSE",
      color = "Link Function",
      shape = "Specified Working Corr"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

plot_consistency_gamma <- function(data, real_corr) {
  gamma_data <- data[data$RealWorkingCorr == real_corr & data$MeanVarStructure == "Gamma", ]
  ggplot(gamma_data, aes(x = SampleSize, y = BetaHatRMSE)) +
    geom_point(aes(
      color = LinkFunction, 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = LinkFunction, 
      group = interaction(SpecifiedWorkingCorr, LinkFunction)
    )) +
    scale_color_manual(values = c("log" = "green", "identity" = "purple"),
                       labels = c("Gamma, Log", "Gamma, Identity")) +
    scale_shape_manual(values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18)) +
    labs(
      title = paste("Consistency Plot for Gamma - Real Working Correlation:", real_corr),
      x = "Sample Size",
      y = "SE Beta Hat RMSE",
      color = "Link Function",
      shape = "Specified Working Corr"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

real_corrs <- unique(efficiency_results$RealWorkingCorr)
efficiency_plots_gamma <- lapply(real_corrs, function(corr) plot_efficiency_gamma(efficiency_results, corr))

pdf("efficiency_plots_gamma.pdf", width = 10, height = 7)
for (plot in efficiency_plots_gamma) print(plot)
dev.off()

real_corrs <- unique(consistency_results$RealWorkingCorr)
consistency_plots_gamma <- lapply(real_corrs, function(corr) plot_consistency_gamma(consistency_results, corr))

pdf("consistency_plots_gamma.pdf", width = 10, height = 7)
for (plot in consistency_plots_gamma) print(plot)
dev.off()

# DOUBLE DELETE LATER 

efficiency_results <- efficiency_results %>%
  group_by(RealWorkingCorr, NumClusters, LinkFunction, MeanVarStructure) %>%
  mutate(
    CorrectRMSE = SERMSE[SpecifiedWorkingCorr == RealWorkingCorr],
    RMSE_Ratio = SERMSE / CorrectRMSE
  ) %>%
  filter(SpecifiedWorkingCorr != RealWorkingCorr)

consistency_results <- consistency_results %>%
  group_by(RealWorkingCorr, SampleSize, LinkFunction, MeanVarStructure) %>%
  mutate(
    CorrectRMSE = BetaHatRMSE[SpecifiedWorkingCorr == RealWorkingCorr],
    RMSE_Ratio = BetaHatRMSE / CorrectRMSE
  ) %>%
  filter(SpecifiedWorkingCorr != RealWorkingCorr)

efficiency_results$InteractionFactor <- factor(
  interaction(efficiency_results$MeanVarStructure, efficiency_results$LinkFunction),
  levels = c("Poisson.log", "Poisson.identity", "Gamma.log", "Gamma.identity")
)

plot_efficiency_ratio <- function(data, real_corr) {
  ggplot(data[data$RealWorkingCorr == real_corr, ], aes(x = NumClusters, y = RMSE_Ratio)) +
    geom_point(aes(
      color = InteractionFactor, 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = InteractionFactor, 
      group = interaction(SpecifiedWorkingCorr, InteractionFactor)
    )) +
    scale_color_manual(
      values = c("Poisson.log" = "blue", "Poisson.identity" = "red", 
                 "Gamma.log" = "green", "Gamma.identity" = "purple"),
      labels = c("Poisson, Log", "Poisson, Identity", 
                 "Gamma, Log", "Gamma, Identity")
    ) +
    scale_shape_manual(
      values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18),
      labels = c("Independence", "AR(1)", "AR(1)")
    ) +
    labs(
      title = paste("Efficiency of Beta-hat under", real_corr, "Working Correlation Structure in Relation to Cluster Size"),
      x = "Number of Clusters",
      y = "Ratio of SE(Beta-hat) RMSE",
      color = "GLM Type (Mean-Variance, Link Function)",
      shape = "Specified Working Correlation Structure"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

plot_consistency_ratio <- function(data, real_corr) {
  ggplot(data[data$RealWorkingCorr == real_corr, ], aes(x = SampleSize, y = RMSE_Ratio)) +
    geom_point(aes(
      color = interaction(MeanVarStructure, LinkFunction), 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = interaction(MeanVarStructure, LinkFunction), 
      group = interaction(SpecifiedWorkingCorr, MeanVarStructure, LinkFunction)
    )) +
    scale_color_manual(values = c("Poisson.log" = "blue", "Poisson.identity" = "red", 
                                  "Gamma.log" = "green", "Gamma.identity" = "purple"),
                       labels = c("Poisson, Log", "Poisson, Identity", 
                                  "Gamma, Log", "Gamma, Identity")) +
    scale_shape_manual(values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18)) +
    labs(
      title = paste("Consistency Ratios - Real Working Corr:", real_corr),
      x = "Sample Size",
      y = "RMSE Ratio (Incorrect / Correct)",
      color = "GLM Type",
      shape = "Specified Working Corr"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

real_corrs <- unique(efficiency_results$RealWorkingCorr)
efficiency_plots_ratio <- lapply(real_corrs, function(corr) plot_efficiency_ratio(efficiency_results, corr))

pdf("efficiency_ratio_plots.pdf", width = 10, height = 7)
for (plot in efficiency_plots_ratio) print(plot)
dev.off()

real_corrs <- unique(consistency_results$RealWorkingCorr)
consistency_plots_ratio <- lapply(real_corrs, function(corr) plot_consistency_ratio(consistency_results, corr))

pdf("consistency_ratio_plots.pdf", width = 10, height = 7)
for (plot in consistency_plots_ratio) print(plot)
dev.off()




plot_efficiency_identity <- function(data, real_corr) {
  identity_data <- data[data$RealWorkingCorr == real_corr & data$LinkFunction == "identity", ]
  ggplot(identity_data, aes(x = NumClusters, y = SERMSE)) +
    geom_point(aes(
      color = MeanVarStructure, 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = MeanVarStructure, 
      group = interaction(SpecifiedWorkingCorr, MeanVarStructure)
    )) +
    scale_color_manual(values = c("Poisson" = "red", "Gamma" = "purple")) +
    scale_shape_manual(values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18)) +
    labs(
      title = paste("Efficiency Plot for Identity Link - Real Working Corr:", real_corr),
      x = "Number of Clusters",
      y = "Beta Hat RMSE",
      color = "Mean-Variance Structure",
      shape = "Specified Working Corr"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

real_corrs <- unique(efficiency_results$RealWorkingCorr)
efficiency_plots_identity <- lapply(real_corrs, function(corr) plot_efficiency_identity(efficiency_results, corr))

pdf("efficiency_identity_plots.pdf", width = 10, height = 7)
for (plot in efficiency_plots_identity) print(plot)
dev.off()


plot_consistency_identity <- function(data, real_corr) {
  identity_data <- data[data$RealWorkingCorr == real_corr & data$LinkFunction == "identity", ]
  ggplot(identity_data, aes(x = SampleSize, y = BetaHatRMSE)) +
    geom_point(aes(
      color = MeanVarStructure, 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = MeanVarStructure, 
      group = interaction(SpecifiedWorkingCorr, MeanVarStructure)
    )) +
    scale_color_manual(values = c("Poisson" = "red", "Gamma" = "purple")) +
    scale_shape_manual(values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18)) +
    labs(
      title = paste("Consistency Plot for Identity Link - Real Working Corr:", real_corr),
      x = "Sample Size",
      y = "SE Beta Hat RMSE",
      color = "Mean-Variance Structure",
      shape = "Specified Working Corr"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

real_corrs <- unique(consistency_results$RealWorkingCorr)
consistency_plots_identity <- lapply(real_corrs, function(corr) plot_consistency_identity(consistency_results, corr))

pdf("consistency_identity_plots.pdf", width = 10, height = 7)
for (plot in consistency_plots_identity) print(plot)
dev.off()


plot_efficiency_log <- function(data, real_corr) {
  log_data <- data[data$RealWorkingCorr == real_corr & data$LinkFunction == "log", ]
  ggplot(log_data, aes(x = NumClusters, y = SERMSE)) +
    geom_point(aes(
      color = MeanVarStructure, 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = MeanVarStructure, 
      group = interaction(SpecifiedWorkingCorr, MeanVarStructure)
    )) +
    scale_color_manual(values = c("Poisson" = "blue", "Gamma" = "green")) +
    scale_shape_manual(values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18)) +
    labs(
      title = paste("Efficiency Plot for Log Link - Real Working Corr:", real_corr),
      x = "Number of Clusters",
      y = "Beta Hat RMSE",
      color = "Mean-Variance Structure",
      shape = "Specified Working Corr"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

real_corrs <- unique(efficiency_results$RealWorkingCorr)
efficiency_plots_log <- lapply(real_corrs, function(corr) plot_efficiency_log(efficiency_results, corr))

pdf("efficiency_log_plots.pdf", width = 10, height = 7)
for (plot in efficiency_plots_log) print(plot)
dev.off()

plot_consistency_log <- function(data, real_corr) {
  log_data <- data[data$RealWorkingCorr == real_corr & data$LinkFunction == "log", ]
  ggplot(log_data, aes(x = SampleSize, y = BetaHatRMSE)) +
    geom_point(aes(
      color = MeanVarStructure, 
      shape = SpecifiedWorkingCorr
    ), size = 3) +
    geom_line(aes(
      color = MeanVarStructure, 
      group = interaction(SpecifiedWorkingCorr, MeanVarStructure)
    )) +
    scale_color_manual(values = c("Poisson" = "blue", "Gamma" = "green")) +
    scale_shape_manual(values = c("independence" = 16, "exchangeable" = 17, "ar1" = 18)) +
    labs(
      title = paste("Consistency Plot for Log Link - Real Working Corr:", real_corr),
      x = "Sample Size",
      y = "SE Beta Hat RMSE",
      color = "Mean-Variance Structure",
      shape = "Specified Working Corr"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

real_corrs <- unique(consistency_results$RealWorkingCorr)
consistency_plots_log <- lapply(real_corrs, function(corr) plot_consistency_log(consistency_results, corr))

pdf("consistency_log_plots.pdf", width = 10, height = 7)
for (plot in consistency_plots_log) print(plot)
dev.off()


