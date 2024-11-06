# Working analysis script (1) for Seminar thesis: 
# Interest Rate Cuts and Household Investment in Risky Assets: An empirical examination of the ‘Reaching for Yield’ Hypothesis
# by Aaron Feldman and Chirag Khanna
# University of Tuebingen, WS 2024-2025
# Contact: 
# aaron.feldman@student.uni-tuebingen.de
# chirag.khanna@student.uni-tuebingen.de

# Install packages and libraries before running the main script to ensure bvar() runs smoothly
install.packages("bvartools", dependencies = TRUE)  
install.packages("BVAR", dependencies = TRUE)

library(bvartools)
library(BVAR)
library(ggplot2)

# Define custom settings for lambda, alpha, and psi with minimal shrinkage, to approximate Normal-Wishart behavior
lambda_custom <- bv_lambda(mode = 0.2, sd = 0.3, min = 0.1, max = 2)  
alpha_custom <- bv_alpha(mode = 1.1, sd = 0.3, min = 0.5, max = 2)     
psi_custom <- bv_psi(scale = 0.5, shape = 1)                            

# Define the Minnesota prior with the custom settings to allow more flexibility in covariance
mn_prior <- bv_minnesota(lambda = lambda_custom, alpha = alpha_custom, psi = psi_custom)

# Define an uninformative initial conditions dummy prior to allow model-driven initialization
init_dummy <- bv_dummy(mode = 1, sd = 1, min = 0.1, max = 20, fun = function(Y, lags, par) {
  Y_init <- Y[1, ] / par
  X_init <- c(1 / par, rep(Y_init, lags))
  return(list("Y" = Y_init, "X" = X_init))
})

# Set up priors with hierarchical adjustment for lambda and alpha
priors <- bv_priors(
  hyper = c("lambda", "alpha"),  # Set hierarchical flexibility for key parameters
  mn = mn_prior,
  init = init_dummy
)

# Define and fit the BVAR model with selected lag length
x <- bvar(data_adj_subset, lags = 2, n_draw = 50000L, n_burn = 5000L, priors = priors, verbose = TRUE)

# Calculate impulse responses with a horizon of 20
irf_result <- irf(x, horizon = 20, fevd = FALSE)

# Rename dimensions of the IRF result array to match variable names
dimnames(irf_result$irf)[[2]] <- colnames(data_adj_subset)  # Response variables
dimnames(irf_result$irf)[[4]] <- colnames(data_adj_subset)  # Shock variables

# Extract the position of the "r" shock
r_position <- which(colnames(data_adj_subset) == "r")

# Prepare IRF data for plot and table
irf_data <- lapply(colnames(data_adj_subset), function(variable) {
  # Invert responses for the "r" shock to simulate a negative shock
  response_values <- -irf_result$irf[, variable, , r_position]
  
  # Calculate mean, lower bound (2.5th percentile), and upper bound (97.5th percentile) for a 95% credible interval
  response_mean <- apply(response_values, 2, mean)
  response_lower <- apply(response_values, 2, quantile, probs = 0.025)
  response_upper <- apply(response_values, 2, quantile, probs = 0.975)
  
  # Create a data frame for ggplot
  data.frame(
    Horizon = 0:(length(response_mean) - 1),
    Response = response_mean,
    Lower = response_lower,
    Upper = response_upper,
    Variable = variable
  )
})

# Combine all response data into a single data frame for plotting
irf_df <- do.call(rbind, irf_data)
irf_df$Variable <- factor(irf_df$Variable, levels = c("y", "u", "r", "p", "p_exp", "disp_inc", "sentiment", "market_diff", "cci_diff", "ra"))

# Plot the impulse responses with confidence intervals
ggplot(irf_df, aes(x = Horizon, y = Response)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.3) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3, nrow = 4) +
  labs(
    title = "Impulse Responses to a Negative Shock in 3-month Treasury rate (r)",
    x = "Horizon (Quarters)", 
    y = "Response"
  ) +
  theme_minimal()

# Calculate statistics for Horizon 0 and Horizon 1 and split the results into separate tables
posterior_mean_0 <- list()
posterior_sd_0 <- list()
lower_95_0 <- list()
upper_95_0 <- list()

posterior_mean_1 <- list()
posterior_sd_1 <- list()
lower_95_1 <- list()
upper_95_1 <- list()

for (variable in colnames(data_adj_subset)) {
  # Extract response draws for each horizon for the specific variable
  response_draws <- irf_result$irf[, variable, , r_position]
  
  # Horizon 0 (Contemporaneous impact)
  mean_0 <- mean(response_draws[, 1])
  sd_0 <- sd(response_draws[, 1])
  lower_95_0_h <- quantile(response_draws[, 1], probs = 0.025)
  upper_95_0_h <- quantile(response_draws[, 1], probs = 0.975)
  
  # Store Horizon 0 results
  posterior_mean_0[[length(posterior_mean_0) + 1]] <- mean_0
  posterior_sd_0[[length(posterior_sd_0) + 1]] <- sd_0
  lower_95_0[[length(lower_95_0) + 1]] <- lower_95_0_h
  upper_95_0[[length(upper_95_0) + 1]] <- upper_95_0_h
  
  # Horizon 1 (First quarter impact)
  mean_1 <- mean(response_draws[, 2])
  sd_1 <- sd(response_draws[, 2])
  lower_95_1_h <- quantile(response_draws[, 2], probs = 0.025)
  upper_95_1_h <- quantile(response_draws[, 2], probs = 0.975)
  
  # Store Horizon 1 results
  posterior_mean_1[[length(posterior_mean_1) + 1]] <- mean_1
  posterior_sd_1[[length(posterior_sd_1) + 1]] <- sd_1
  lower_95_1[[length(lower_95_1) + 1]] <- lower_95_1_h
  upper_95_1[[length(upper_95_1) + 1]] <- upper_95_1_h
}

# Create separate data frames for Horizon 0 and Horizon 1 results
summary_df_0 <- data.frame(
  Variable = colnames(data_adj_subset),
  Horizon = 0,
  Posterior_Mean = unlist(posterior_mean_0),
  Lower_95 = unlist(lower_95_0),
  Upper_95 = unlist(upper_95_0)
)

summary_df_1 <- data.frame(
  Variable = colnames(data_adj_subset),
  Horizon = 1,
  Posterior_Mean = unlist(posterior_mean_1),
  Lower_95 = unlist(lower_95_1),
  Upper_95 = unlist(upper_95_1)
)

# Display the summary tables for Horizon 0 and Horizon 1
print("Horizon 0 Summary")
print(summary_df_0)

print("Horizon 1 Summary")
print(summary_df_1)




#### Specification Analysis #####




# Load necessary packages
install.packages("progress")
library(progress)

# Define Cholesky orderings with `ra` placed last and `disp_inc` incorporated
orderings <- list(
  c("r", "y", "u", "p", "p_exp", "disp_inc", "sentiment", "market_diff", "cci_diff", "ra"),
  c("y", "u", "r", "p", "p_exp", "disp_inc", "sentiment", "market_diff", "cci_diff", "ra"),
  c("p", "p_exp", "r", "y", "u", "disp_inc", "sentiment", "market_diff", "cci_diff", "ra"),
  c("market_diff", "sentiment", "r", "y", "u", "p", "p_exp", "disp_inc", "cci_diff", "ra"),
  c("sentiment", "cci_diff", "r", "y", "u", "p", "p_exp", "disp_inc", "market_diff", "ra")
)

# Define helper functions
compute_forecast_error <- function(forecast, test_data) {
  forecast_values <- as.numeric(forecast)
  test_values <- as.numeric(test_data)
  
  rmse <- sqrt(mean((forecast_values - test_values)^2, na.rm = TRUE))
  mae <- mean(abs(forecast_values - test_values), na.rm = TRUE)
  return(c(RMSE = rmse, MAE = mae))
}

extract_irf_summary <- function(irf_result) {
  irf_means <- apply(irf_result$irf, c(2, 3), mean)
  ci_widths <- apply(irf_result$irf, c(2, 3), function(x) diff(quantile(x, c(0.025, 0.975), na.rm = TRUE)))
  
  return(list(mean_irf = irf_means, ci_width = ci_widths))
}

# Define Hyperparameter grids with itterative refinement
lambda_values <- seq(0.2, 1.0, by = 0.1)
alpha_values <- seq(0.5, 1.5, by = 0.2)
psi_values <- seq(0.5, 1.5, by = 0.2)
lag_values <- 1:4

# Initialize data frames to store results
forecast_results <- data.frame()
irf_results <- data.frame()

# Define Train-Test split
train_data <- as.matrix(data_adj_subset[1:round(0.8 * nrow(data_adj_subset)), ])
test_data <- as.matrix(data_adj_subset[(round(0.8 * nrow(data_adj_subset)) + 1):nrow(data_adj_subset), ])

# Start progress bar
total_iterations <- length(orderings) * length(lag_values) * length(lambda_values) * length(alpha_values) * length(psi_values)
pb <- progress_bar$new(total = total_iterations, format = "[:bar] :current/:total (:percent) ETA: :eta")

# Loop through specifications with multiple orderings
for (ordering in orderings) {
  # Reorder the data according to the current ordering
  train_ordered <- train_data[, ordering]
  test_ordered <- test_data[, ordering]
  
  for (lag in lag_values) {
    for (lambda in lambda_values) {
      for (alpha in alpha_values) {
        for (psi in psi_values) {
          
          # Set Up priors with refined hyperparameters
          lambda_custom <- bv_lambda(mode = lambda, sd = 0.3, min = 0.1, max = 2)
          alpha_custom <- bv_alpha(mode = alpha, sd = 0.3, min = 0.5, max = 2)
          psi_custom <- bv_psi(scale = psi, shape = 1)
          mn_prior <- bv_minnesota(lambda = lambda_custom, alpha = alpha_custom, psi = psi_custom)
          priors <- bv_priors(hyper = c("lambda", "alpha"), mn = mn_prior)
          
          # Train BVAR model
          model <- bvar(train_ordered, lags = lag, n_draw = 500, n_burn = 100, priors = priors, verbose = FALSE)
          
          # Forecast accuracy
          forecast_output <- predict(model, newdata = test_ordered)
          
          # Extract the forecast as a mean of simulations over the forecast horizon (first 12 periods)
          forecast_mean <- apply(forecast_output$fcast, c(2, 3), mean)
          forecast <- as.numeric(forecast_mean[1:12, ])
          test_subset <- test_ordered[1:12, ]
          
          # Compute and store forecast error metrics
          errors <- compute_forecast_error(forecast, test_subset)
          forecast_results <- rbind(forecast_results, data.frame(Ordering = paste(ordering, collapse = " > "), Lag = lag, Lambda = lambda, Alpha = alpha, Psi = psi, RMSE = errors[1], MAE = errors[2]))
          
          # Impulse response analysis
          irf_result <- irf(model, horizon = 20, fevd = FALSE)
          irf_summary <- extract_irf_summary(irf_result)
          
          # Store mean IRF and CI widths (for stability assessment)
          for (variable in ordering) {
            var_index <- match(variable, ordering)
            irf_mean <- mean(irf_summary$mean_irf[var_index, ])
            irf_ci_width <- mean(irf_summary$ci_width[var_index, ])
            irf_results <- rbind(irf_results, data.frame(Ordering = paste(ordering, collapse = " > "), Lag = lag, Lambda = lambda, Alpha = alpha, Psi = psi, Variable = variable, Mean_IRF = irf_mean, CI_Width = irf_ci_width))
          }
          
          # Update progress bar
          pb$tick()
        }
      }
    }
  }
}

# Summarize results and find the best performing configuration
best_forecast <- forecast_results[which.min(forecast_results$RMSE), ]
print("Best Forecast Configuration:")
print(best_forecast)
