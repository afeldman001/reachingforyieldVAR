# SVAR Model: Seminar Thesis
# Interest Rate Cuts and Household Investment in Risky Assets: 
# An empirical examination of the 'Reaching for Yield' Hypothesis
# by Aaron Feldman, Chirag Khanna, and Mohammad Rahbari
# University of Tuebingen, WS 2024-2025
# Contact: 
# aaron.feldman@student.uni-tuebingen.de
# chirag.khanna@student.uni-tuebingen.de
# mohammad.rahbari@student.uni-tuebingen.de

# Clear workspace and console output
rm(list = ls()) # clears the environment
cat("\014") # clears the console

# Load required libraries
library(dplyr)
library(tidyr)
library(vars)
library(ggplot2)
library(gridExtra)

# Load detrended data
data <- monthly_data %>% filter(date < as.Date("2024-04-01"))

# Define lag order and prepare data
p <- 4  # Number of lags
nvars <- ncol(data) - 1  # Exclude the date column
T <- nrow(data)

# Exclude the date column and prepare data matrix
Y <- as.matrix(data[, -1])
variables <- colnames(data)[-1]  # Extract variable names
colnames(Y) <- variables

# Prepare lagged regressors matrix
X <- embed(Y, p + 1)
Y_dep <- X[, 1:nvars]  # Dependent variables (adjusted for lags)
X <- X[, -(1:nvars)]  # Lagged independent variables
X <- cbind(1, X)  # Add constant term

# Estimate coefficients using OLS
B_hat <- solve(t(X) %*% X) %*% t(X) %*% Y_dep
u_hat <- Y_dep - X %*% B_hat  # Residuals

# Compute residual covariance matrix
nparams <- nvars * p + 1  # Number of parameters
Sigma_u <- crossprod(u_hat) / (T - p - nparams)

# Perform Cholesky decomposition
B_0_inv <- t(chol(Sigma_u))

# Display Cholesky decomposition
cat("Cholesky decomposition of the residual covariance matrix (B_0_inv):\n")
print(B_0_inv)

# Identify the index for the FFR variable
ffr_shock_index <- which(variables == "FFR")

# Scale the FFR shock to reflect a 1% change
scaling_factor_ffr <- 1 / abs(B_0_inv[ffr_shock_index, ffr_shock_index])
B_0_inv_scaled <- B_0_inv
B_0_inv_scaled[, ffr_shock_index] <- B_0_inv[, ffr_shock_index] * scaling_factor_ffr

# Scale the Volatility shock to reflect a 1 standard deviation change
volatility_shock_index <- which(variables == "Volatility_Shock")
scaling_factor_vol <- 1 / abs(B_0_inv[volatility_shock_index, volatility_shock_index])
B_0_inv_scaled[, volatility_shock_index] <- B_0_inv[, volatility_shock_index] * scaling_factor_vol

# Scale the Uncertainty Cycle (volatility component) to reflect a 1 standard deviation change
uncertainty_shock_index <- which(variables == "Monthly_Uncertainty")
scaling_factor_uncertainty <- 1 / abs(B_0_inv[uncertainty_shock_index, uncertainty_shock_index])
B_0_inv_scaled[, uncertainty_shock_index] <- B_0_inv[, uncertainty_shock_index] * scaling_factor_uncertainty

# Scale the Risk Aversion (volatility component) to reflect a 1 standard deviation change
risk_aversion_shock_index <- which(variables == "Monthly_Risk_Aversion")
scaling_factor_risk_aversion <- 1 / abs(B_0_inv[risk_aversion_shock_index, risk_aversion_shock_index])
B_0_inv_scaled[, risk_aversion_shock_index] <- B_0_inv[, risk_aversion_shock_index] * scaling_factor_risk_aversion

# Verify scaling factors and indices
cat("Uncertainty Shock Index:", uncertainty_shock_index, "\n")
cat("Risk Aversion Shock Index:", risk_aversion_shock_index, "\n")
cat("Scaling Factor (Uncertainty):", scaling_factor_uncertainty, "\n")
cat("Scaling Factor (Risk Aversion):", scaling_factor_risk_aversion, "\n")

# Construct companion matrix
if (p == 1) {
  # Simplified companion matrix for p = 1
  A_comp <- t(B_hat[-1, ])  # Only the first block
} else {
  # Full companion matrix for p > 1
  A_comp <- matrix(0, nvars * p, nvars * p)
  A_comp[1:nvars, 1:(nvars * p)] <- t(B_hat[-1, ])
  A_comp[(nvars + 1):(nvars * p), 1:(nvars * (p - 1))] <- diag(nvars * (p - 1))
}

# Helper function to normalize and reverse a shock
normalize_shock <- function(B_0_inv, shock_index) {
  scaling_factor <- B_0_inv[shock_index, shock_index]
  B_0_inv_scaled <- B_0_inv
  B_0_inv_scaled[, shock_index] <- -B_0_inv[, shock_index] / scaling_factor
  return(B_0_inv_scaled)
}

# Step 1: Normalize and reverse the FFR shock
ffr_shock_index <- which(variables == "FFR")
B_0_inv_scaled <- normalize_shock(B_0_inv, ffr_shock_index)

cat("Baseline Variables:", variables, "\n")

# Step 2: Recompute IRFs with scaled shocks
nsteps <- 36  # Horizon for IRFs
irfMat_scaled <- array(0, c(nvars, nvars, nsteps + 1))
irfMat_scaled[, , 1] <- B_0_inv_scaled

Ah <- diag(nvars * p)
for (h in 1:nsteps) {
  Ah <- Ah %*% A_comp
  irfMat_scaled[, , h + 1] <- Ah[1:nvars, 1:nvars] %*% B_0_inv_scaled
}

# Step 3: Verify the impact response of the FFR
impact_response_ffr <- irfMat_scaled[ffr_shock_index, ffr_shock_index, 1]
cat("Impact response of FFR to its own shock (should be -1):", impact_response_ffr, "\n")

# Display the impact matrix after normalizing and reversing the FFR shock
cat("\nImpact matrix (B_0_inv_scaled) after normalization and sign reversal of the FFR shock:\n")
print(B_0_inv_scaled)

# Step 4: Extract and plot responses of Risk Assets and Risk Aversion to FFR Shock
risk_assets_index <- which(variables == "log_Household_Equity")
risk_aversion_index <- which(variables == "Monthly_Risk_Aversion")

response_risk_assets_ffr <- irfMat_scaled[risk_assets_index, ffr_shock_index, ] * 100
response_risk_aversion_ffr <- irfMat_scaled[risk_aversion_index, ffr_shock_index, ] * 100

# Bootstrap confidence intervals for Household_Equity_Cycle response to FFR shock
n_boot <- 1000
irf_boot <- matrix(0, n_boot, nsteps + 1)  # Store bootstrapped IRFs

set.seed(123)  # For reproducibility
for (b in 1:n_boot) {
  # Resample residuals with replacement
  u_boot <- u_hat[sample(1:nrow(u_hat), nrow(u_hat), replace = TRUE), ]
  
  # Generate bootstrapped Y matrix
  Y_boot <- matrix(0, nrow = T, ncol = nvars)
  Y_boot[1:p, ] <- Y[1:p, ]  # Initialize with original lags
  for (t in (p + 1):T) {
    lagged_vars <- c(1, as.vector(t(Y_boot[(t - p):(t - 1), ])))  # Constant + lagged variables
    lagged_vars <- matrix(lagged_vars, ncol = 1)
    Y_boot[t, ] <- (t(B_hat) %*% lagged_vars)[, 1] + u_boot[t - p, ]
  }
  
  # Recompute residual covariance matrix Sigma_u_boot
  Sigma_u_boot <- crossprod(u_boot) / (T - p - nparams)
  
  # Perform Cholesky decomposition for the bootstrap sample
  B_0_inv_boot <- t(chol(Sigma_u_boot))
  
  # Scale the bootstrap impact matrix for shocks
  B_0_inv_boot_scaled <- B_0_inv_boot
  B_0_inv_boot_scaled[, ffr_shock_index] <- -B_0_inv_boot[, ffr_shock_index] * scaling_factor_ffr
  # Add scaling for other shocks if needed
  
  # Recompute IRFs for bootstrapped sample
  irfMat_boot <- array(0, c(nvars, nvars, nsteps + 1))
  irfMat_boot[, , 1] <- B_0_inv_boot_scaled  # Initial impact matrix
  Ah_boot <- diag(nvars * p)  # Identity matrix
  for (h in 1:nsteps) {
    Ah_boot <- Ah_boot %*% A_comp
    irfMat_boot[, , h + 1] <- Ah_boot[1:nvars, 1:nvars] %*% B_0_inv_boot_scaled
  }
  
  # Store the bootstrapped IRFs for the response of log_Household_Equity to FFR shock
  irf_boot[b, ] <- irfMat_boot[risk_assets_index, ffr_shock_index, ] * 100
}


# Compute confidence intervals (68% and 95%) from bootstrapped IRFs
ci_95 <- apply(irf_boot, 2, quantile, probs = c(0.025, 0.975))
ci_68 <- apply(irf_boot, 2, quantile, probs = c(0.16, 0.84))

# Add confidence intervals to the IRF data frame
irf_data <- data.frame(
  Horizon = 0:nsteps,
  Response = response_risk_assets_ffr,
  CI_95_Lower = ci_95[1, ],
  CI_95_Upper = ci_95[2, ],
  CI_68_Lower = ci_68[1, ],
  CI_68_Upper = ci_68[2, ]
)


# Plot response of log_Household_Equity to FFR Shock with dashed confidence intervals
ggplot(irf_data, aes(x = Horizon, y = Response)) +
  geom_line(color = "blue", size = 1.2) +
  geom_line(aes(y = CI_95_Lower), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_95_Upper), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_68_Lower), linetype = "dashed", color = "black") +
  geom_line(aes(y = CI_68_Upper), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Response of Household Risky Asset Allocations to a 1% Expansionary FFR Shock",
    x = "Horizon (Months)",
    y = "Response (% Change)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 35),
    axis.title.y = element_text(size = 35),
    axis.text = element_text(size = 30),
    panel.grid = element_blank()  # Removes all gridlines
  )

# Compute confidence intervals (68% and 95%) for Monthly_Risk_Aversion response to FFR shock
irf_boot_risk_aversion <- matrix(0, nrow = n_boot, ncol = nsteps + 1) # Store bootstrapped IRFs for risk aversion

set.seed(123)  # For reproducibility
for (b in 1:n_boot) {
  # Resample residuals with replacement
  u_boot <- u_hat[sample(1:nrow(u_hat), nrow(u_hat), replace = TRUE), ]
  
  # Generate bootstrapped Y matrix
  Y_boot <- matrix(0, nrow = T, ncol = nvars)
  colnames(Y_boot) <- colnames(Y)  # Assign column names from Y
  Y_boot[1:p, ] <- Y[1:p, ]  # Initialize with original lags
  for (t in (p + 1):T) {
    lagged_vars <- c(1, as.vector(t(Y_boot[(t - p):(t - 1), ])))  # Constant + lagged variables
    lagged_vars <- matrix(lagged_vars, ncol = 1)
    Y_boot[t, ] <- (t(B_hat) %*% lagged_vars)[, 1] + u_boot[t - p, ]
  }
  
  # Recompute residual covariance matrix Sigma_u_boot
  Sigma_u_boot <- crossprod(u_boot) / (T - p - nparams)
  
  # Perform Cholesky decomposition for the bootstrap sample
  B_0_inv_boot <- t(chol(Sigma_u_boot))
  
  # Scale the bootstrap impact matrix for shocks
  B_0_inv_boot_scaled <- B_0_inv_boot
  B_0_inv_boot_scaled[, ffr_shock_index] <- -B_0_inv_boot[, ffr_shock_index] * scaling_factor_ffr
  
  # Recompute IRFs for bootstrapped sample
  irfMat_boot <- array(0, c(nvars, nvars, nsteps + 1))
  irfMat_boot[, , 1] <- B_0_inv_boot_scaled  # Initial impact matrix
  Ah_boot <- diag(nvars * p)  # Identity matrix
  for (h in 1:nsteps) {
    Ah_boot <- Ah_boot %*% A_comp
    irfMat_boot[, , h + 1] <- Ah_boot[1:nvars, 1:nvars] %*% B_0_inv_boot_scaled
  }
  
  # Store the bootstrapped IRFs for the response of Risk_Aversion_Cycle to FFR shock
  irf_boot_risk_aversion[b, ] <- irfMat_boot[risk_aversion_index, ffr_shock_index, ] * 100
}

# Compute confidence intervals (68% and 95%) from bootstrapped IRFs for Risk_Aversion_Cycle
ci_95_risk_aversion <- apply(irf_boot_risk_aversion, 2, quantile, probs = c(0.025, 0.975))
ci_68_risk_aversion <- apply(irf_boot_risk_aversion, 2, quantile, probs = c(0.16, 0.84))

# Add confidence intervals to the IRF data frame for Risk_Aversion_Cycle
irf_data_risk_aversion <- data.frame(
  Horizon = 0:nsteps,
  Response = response_risk_aversion_ffr,
  CI_95_Lower = ci_95_risk_aversion[1, ],
  CI_95_Upper = ci_95_risk_aversion[2, ],
  CI_68_Lower = ci_68_risk_aversion[1, ],
  CI_68_Upper = ci_68_risk_aversion[2, ]
)

# Plot response of Risk_Aversion_Cycle to FFR Shock with dashed confidence intervals
ggplot(irf_data_risk_aversion, aes(x = Horizon, y = Response)) +
  geom_line(color = "orange", size = 1.2) +
  geom_line(aes(y = CI_95_Lower), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_95_Upper), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_68_Lower), linetype = "dashed", color = "black") +
  geom_line(aes(y = CI_68_Upper), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Response of Risk Aversion to a 1% Expansionary FFR Shock",
    x = "Horizon (Months)",
    y = "Response (% Change)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 35),
    axis.title.y = element_text(size = 35),
    axis.text = element_text(size = 30),
    panel.grid = element_blank()  # Removes all gridlines
  )

# Step 1: Identify the index for the Uncertainty_Cycle variable
uncertainty_index <- which(variables == "Monthly_Uncertainty")

# Step 2: Extract the response of Uncertainty_Cycle to FFR shock
response_uncertainty_ffr <- irfMat_scaled[uncertainty_index, ffr_shock_index, ] * 100

# Step 3: Bootstrap confidence intervals for Monthly_Uncertainty response to FFR shock
irf_boot_uncertainty <- matrix(0, n_boot, nsteps + 1)  # Store bootstrapped IRFs for uncertainty

set.seed(123)  # For reproducibility
for (b in 1:n_boot) {
  # Resample residuals with replacement
  u_boot <- u_hat[sample(1:nrow(u_hat), nrow(u_hat), replace = TRUE), ]
  
  # Generate bootstrapped Y matrix
  Y_boot <- matrix(0, nrow = T, ncol = nvars)
  Y_boot[1:p, ] <- Y[1:p, ]  # Initialize with original lags
  for (t in (p + 1):T) {
    lagged_vars <- c(1, as.vector(t(Y_boot[(t - p):(t - 1), ])))  # Constant + lagged variables
    lagged_vars <- matrix(lagged_vars, ncol = 1)
    Y_boot[t, ] <- (t(B_hat) %*% lagged_vars)[, 1] + u_boot[t - p, ]
  }
  
  # Recompute residual covariance matrix Sigma_u_boot
  Sigma_u_boot <- crossprod(u_boot) / (T - p - nparams)
  
  # Perform Cholesky decomposition for the bootstrap sample
  B_0_inv_boot <- t(chol(Sigma_u_boot))
  
  # Scale the bootstrap impact matrix for shocks
  B_0_inv_boot_scaled <- B_0_inv_boot
  B_0_inv_boot_scaled[, ffr_shock_index] <- -B_0_inv_boot[, ffr_shock_index] * scaling_factor_ffr
  
  # Recompute IRFs for bootstrapped sample
  irfMat_boot <- array(0, c(nvars, nvars, nsteps + 1))
  irfMat_boot[, , 1] <- B_0_inv_boot_scaled  # Initial impact matrix
  Ah_boot <- diag(nvars * p)  # Identity matrix
  for (h in 1:nsteps) {
    Ah_boot <- Ah_boot %*% A_comp
    irfMat_boot[, , h + 1] <- Ah_boot[1:nvars, 1:nvars] %*% B_0_inv_boot_scaled
  }
  
  # Store the bootstrapped IRFs for the response of Uncertainty_Cycle to FFR shock
  irf_boot_uncertainty[b, ] <- irfMat_boot[uncertainty_index, ffr_shock_index, ] * 100
}

# Step 4: Compute confidence intervals (68% and 95%) from bootstrapped IRFs for Uncertainty_Cycle
ci_95_uncertainty <- apply(irf_boot_uncertainty, 2, quantile, probs = c(0.025, 0.975))
ci_68_uncertainty <- apply(irf_boot_uncertainty, 2, quantile, probs = c(0.16, 0.84))

# Step 5: Add confidence intervals to the IRF data frame for Uncertainty_Cycle
irf_data_uncertainty <- data.frame(
  Horizon = 0:nsteps,
  Response = response_uncertainty_ffr,
  CI_95_Lower = ci_95_uncertainty[1, ],
  CI_95_Upper = ci_95_uncertainty[2, ],
  CI_68_Lower = ci_68_uncertainty[1, ],
  CI_68_Upper = ci_68_uncertainty[2, ]
)

# Step 6: Plot the response of Uncertainty_Cycle to FFR Shock
ggplot(irf_data_uncertainty, aes(x = Horizon, y = Response)) +
  geom_line(color = "brown", size = 1.2) +
  geom_line(aes(y = CI_95_Lower), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_95_Upper), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_68_Lower), linetype = "dashed", color = "black") +
  geom_line(aes(y = CI_68_Upper), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Response of Uncertainty to a 1% Expansionary FFR Shock",
    x = "Horizon (Months)",
    y = "Response (% Change)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 35),
    axis.title.y = element_text(size = 35),
    axis.text = element_text(size = 30),
    panel.grid = element_blank()  # Removes all gridlines
  )

# Step 1: Identify the index for the SP500 variable
sp500_index <- which(variables == "log_price_adjusted")

# Step 2: Extract the response of SP500 to FFR shock
response_sp500_ffr <- irfMat_scaled[sp500_index, ffr_shock_index, ] * 100

# Step 3: Bootstrap confidence intervals for SP500 response to FFR shock
irf_boot_sp500 <- matrix(0, n_boot, nsteps + 1)  # Store bootstrapped IRFs for SP500

set.seed(123)  # For reproducibility
for (b in 1:n_boot) {
  # Resample residuals with replacement
  u_boot <- u_hat[sample(1:nrow(u_hat), nrow(u_hat), replace = TRUE), ]
  
  # Generate bootstrapped Y matrix
  Y_boot <- matrix(0, nrow = T, ncol = nvars)
  Y_boot[1:p, ] <- Y[1:p, ]  # Initialize with original lags
  for (t in (p + 1):T) {
    lagged_vars <- c(1, as.vector(t(Y_boot[(t - p):(t - 1), ])))  # Constant + lagged variables
    lagged_vars <- matrix(lagged_vars, ncol = 1)
    Y_boot[t, ] <- (t(B_hat) %*% lagged_vars)[, 1] + u_boot[t - p, ]
  }
  
  # Recompute residual covariance matrix Sigma_u_boot
  Sigma_u_boot <- crossprod(u_boot) / (T - p - nparams)
  
  # Perform Cholesky decomposition for the bootstrap sample
  B_0_inv_boot <- t(chol(Sigma_u_boot))
  
  # Scale the bootstrap impact matrix for shocks
  B_0_inv_boot_scaled <- B_0_inv_boot
  B_0_inv_boot_scaled[, ffr_shock_index] <- -B_0_inv_boot[, ffr_shock_index] * scaling_factor_ffr
  
  # Recompute IRFs for bootstrapped sample
  irfMat_boot <- array(0, c(nvars, nvars, nsteps + 1))
  irfMat_boot[, , 1] <- B_0_inv_boot_scaled  # Initial impact matrix
  Ah_boot <- diag(nvars * p)  # Identity matrix
  for (h in 1:nsteps) {
    Ah_boot <- Ah_boot %*% A_comp
    irfMat_boot[, , h + 1] <- Ah_boot[1:nvars, 1:nvars] %*% B_0_inv_boot_scaled
  }
  
  # Store the bootstrapped IRFs for the response of SP500 to FFR shock
  irf_boot_sp500[b, ] <- irfMat_boot[sp500_index, ffr_shock_index, ] * 100
}

# Step 4: Compute confidence intervals (68% and 95%) from bootstrapped IRFs for SP500
ci_95_sp500 <- apply(irf_boot_sp500, 2, quantile, probs = c(0.025, 0.975))
ci_68_sp500 <- apply(irf_boot_sp500, 2, quantile, probs = c(0.16, 0.84))

# Step 5: Add confidence intervals to the IRF data frame for SP500
irf_data_sp500 <- data.frame(
  Horizon = 0:nsteps,
  Response = response_sp500_ffr,
  CI_95_Lower = ci_95_sp500[1, ],
  CI_95_Upper = ci_95_sp500[2, ],
  CI_68_Lower = ci_68_sp500[1, ],
  CI_68_Upper = ci_68_sp500[2, ]
)

# Step 6: Plot the response of SP500 to FFR Shock
ggplot(irf_data_sp500, aes(x = Horizon, y = Response)) +
  geom_line(color = "green", size = 1.2) +
  geom_line(aes(y = CI_95_Lower), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_95_Upper), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_68_Lower), linetype = "dashed", color = "black") +
  geom_line(aes(y = CI_68_Upper), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Response of SP500 to a 1% Expansionary FFR Shock",
    x = "Horizon (Months)",
    y = "Response (% Change)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 16),
    panel.grid = element_blank()  # Removes all gridlines
  )


# Step 1: Identify the index for the Volatility_Shock variable
volatility_shock_index <- which(variables == "Volatility_Shock")

# Step 2: Extract the response of log_Household_Equity to Volatility_Shock
response_risk_assets_vol <- irfMat_scaled[risk_assets_index, volatility_shock_index, ] * 100

# Step 3: Bootstrap confidence intervals for log_Household_Equity response to Volatility_Shock
irf_boot_vol <- matrix(0, n_boot, nsteps + 1)  # Store bootstrapped IRFs for Volatility_Shock

set.seed(123)  # For reproducibility
for (b in 1:n_boot) {
  # Resample residuals with replacement
  u_boot <- u_hat[sample(1:nrow(u_hat), nrow(u_hat), replace = TRUE), ]
  
  # Generate bootstrapped Y matrix
  Y_boot <- matrix(0, nrow = T, ncol = nvars)
  Y_boot[1:p, ] <- Y[1:p, ]  # Initialize with original lags
  for (t in (p + 1):T) {
    lagged_vars <- c(1, as.vector(t(Y_boot[(t - p):(t - 1), ])))  # Constant + lagged variables
    lagged_vars <- matrix(lagged_vars, ncol = 1)
    Y_boot[t, ] <- (t(B_hat) %*% lagged_vars)[, 1] + u_boot[t - p, ]
  }
  
  # Recompute residual covariance matrix Sigma_u_boot
  Sigma_u_boot <- crossprod(u_boot) / (T - p - nparams)
  
  # Perform Cholesky decomposition for the bootstrap sample
  B_0_inv_boot <- t(chol(Sigma_u_boot))
  
  # Scale the bootstrap impact matrix for shocks
  B_0_inv_boot_scaled <- B_0_inv_boot
  B_0_inv_boot_scaled[, volatility_shock_index] <- B_0_inv_boot[, volatility_shock_index] # * scaling_factor_vol
  
  # Recompute IRFs for bootstrapped sample
  irfMat_boot <- array(0, c(nvars, nvars, nsteps + 1))
  irfMat_boot[, , 1] <- B_0_inv_boot_scaled  # Initial impact matrix
  Ah_boot <- diag(nvars * p)  # Identity matrix
  for (h in 1:nsteps) {
    Ah_boot <- Ah_boot %*% A_comp
    irfMat_boot[, , h + 1] <- Ah_boot[1:nvars, 1:nvars] %*% B_0_inv_boot_scaled
  }
  
  # Store the bootstrapped IRFs for the response of log_Household_Equity to Volatility_Shock
  irf_boot_vol[b, ] <- irfMat_boot[risk_assets_index, volatility_shock_index, ] * 100
}

# Step 4: Compute confidence intervals (68% and 95%) from bootstrapped IRFs for Volatility_Shock
ci_95_vol <- apply(irf_boot_vol, 2, quantile, probs = c(0.025, 0.975))
ci_68_vol <- apply(irf_boot_vol, 2, quantile, probs = c(0.16, 0.84))

# Step 5: Add confidence intervals to the IRF data frame for Volatility_Shock
irf_data_vol <- data.frame(
  Horizon = 0:nsteps,
  Response = response_risk_assets_vol,
  CI_95_Lower = ci_95_vol[1, ],
  CI_95_Upper = ci_95_vol[2, ],
  CI_68_Lower = ci_68_vol[1, ],
  CI_68_Upper = ci_68_vol[2, ]
)

# Step 6: Plot the response of log_Household_Equity to Volatility_Shock
ggplot(irf_data_vol, aes(x = Horizon, y = Response)) +
  geom_line(color = "purple", size = 1.2) +
  geom_line(aes(y = CI_95_Lower), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_95_Upper), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_68_Lower), linetype = "dashed", color = "black") +
  geom_line(aes(y = CI_68_Upper), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Response of Risky Asset Allocations to a Volatility Shock",
    x = "Horizon (Months)",
    y = "Response (% Change)"
  ) +
  theme_minimal() +
   theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 35),
    axis.title.y = element_text(size = 35),
    axis.text = element_text(size = 30),
    panel.grid = element_blank()  # Removes all gridlines
  )


# Step 5: Save IRF data for further analysis
irf_data <- data.frame(
  Variable = rep(variables, each = nvars * (nsteps + 1)),
  Shock = rep(rep(variables, each = (nsteps + 1)), nvars),
  Horizon = rep(0:nsteps, nvars * nvars),
  Response = as.vector(irfMat_scaled)
)





# identify the index for the log_Consumer_Confidence variable
consumer_confidence_index <- which(variables == "log_Consumer_Confidence")

# extract the IRF for log_Consumer_Confidence to FFR Shock
response_consumer_confidence_ffr <- irfMat_scaled[consumer_confidence_index, ffr_shock_index, ] * 100

# bootstrap confidence intervals for Consumer Confidence response to FFR shock
irf_boot_consumer <- matrix(0, n_boot, nsteps + 1)  # store bootstrapped IRFs

for (b in 1:n_boot) {
  # resample residuals with replacement
  u_boot <- u_hat[sample(1:nrow(u_hat), nrow(u_hat), replace = TRUE), ]
  
  # generate bootstrapped Y matrix
  Y_boot <- matrix(0, nrow = T, ncol = nvars)
  Y_boot[1:p, ] <- Y[1:p, ]  # initialize with original lags
  for (t in (p + 1):T) {
    lagged_vars <- c(1, as.vector(t(Y_boot[(t - p):(t - 1), ])))  # constant + lagged variables
    lagged_vars <- matrix(lagged_vars, ncol = 1)
    Y_boot[t, ] <- (t(B_hat) %*% lagged_vars)[, 1] + u_boot[t - p, ]
  }
  
  # recompute residual covariance matrix Sigma_u_boot
  Sigma_u_boot <- crossprod(u_boot) / (T - p - nparams)
  
  # perform Cholesky decomposition for the bootstrap sample
  B_0_inv_boot <- t(chol(Sigma_u_boot))
  
  # scale the bootstrap impact matrix for FFR shocks
  B_0_inv_boot_scaled <- B_0_inv_boot
  B_0_inv_boot_scaled[, ffr_shock_index] <- -B_0_inv_boot[, ffr_shock_index] * scaling_factor_ffr
  
  # recompute IRFs for bootstrapped sample
  irfMat_boot <- array(0, c(nvars, nvars, nsteps + 1))
  irfMat_boot[, , 1] <- B_0_inv_boot_scaled  # initial impact matrix
  Ah_boot <- diag(nvars * p)  # identity matrix
  for (h in 1:nsteps) {
    Ah_boot <- Ah_boot %*% A_comp
    irfMat_boot[, , h + 1] <- Ah_boot[1:nvars, 1:nvars] %*% B_0_inv_boot_scaled
  }
  
  # store the bootstrapped IRFs for the response of log_Consumer_Confidence to FFR shock
  irf_boot_consumer[b, ] <- irfMat_boot[consumer_confidence_index, ffr_shock_index, ] * 100
}

# compute confidence intervals (68% and 95%) from bootstrapped IRFs
ci_95_consumer <- apply(irf_boot_consumer, 2, quantile, probs = c(0.025, 0.975))
ci_68_consumer <- apply(irf_boot_consumer, 2, quantile, probs = c(0.16, 0.84))

# add confidence intervals to the IRF data frame for Consumer Confidence
irf_data_consumer <- data.frame(
  Horizon = 0:nsteps,
  Response = response_consumer_confidence_ffr,
  CI_95_Lower = ci_95_consumer[1, ],
  CI_95_Upper = ci_95_consumer[2, ],
  CI_68_Lower = ci_68_consumer[1, ],
  CI_68_Upper = ci_68_consumer[2, ]
)

# plot response of log_Consumer_Confidence to FFR Shock with dashed confidence intervals
ggplot(irf_data_consumer, aes(x = Horizon, y = Response)) +
  geom_line(color = "blue", size = 1.2) +
  geom_line(aes(y = CI_95_Lower), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_95_Upper), linetype = "dashed", color = "red") +
  geom_line(aes(y = CI_68_Lower), linetype = "dashed", color = "black") +
  geom_line(aes(y = CI_68_Upper), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Response of Consumer Confidence to a 1% Expansionary FFR Shock",
    x = "Horizon (Months)",
    y = "Response (% Change)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 35),
    axis.title.y = element_text(size = 35),
    axis.text = element_text(size = 30),
    panel.grid = element_blank()  # removes all gridlines
  )



# SVAR Model Extensions: Reversed Order and Simplified Model

# Reversed Order SVAR Model
cat("\n--- Reversed Order SVAR Model ---\n")

# Reverse the variable order
reversed_variables <- rev(variables)
Y_reversed <- Y[, reversed_variables]
colnames(Y_reversed) <- reversed_variables

# Prepare lagged regressors for reversed order
X_reversed <- embed(Y_reversed, p + 1)
Y_dep_reversed <- X_reversed[, 1:nvars]
X_reversed <- X_reversed[, -(1:nvars)]
X_reversed <- cbind(1, X_reversed)

# Estimate coefficients using OLS for reversed order
B_hat_reversed <- solve(t(X_reversed) %*% X_reversed) %*% t(X_reversed) %*% Y_dep_reversed
u_hat_reversed <- Y_dep_reversed - X_reversed %*% B_hat_reversed
Sigma_u_reversed <- crossprod(u_hat_reversed) / (T - p - nparams)

# Perform Cholesky decomposition for reversed order
B_0_inv_reversed <- t(chol(Sigma_u_reversed))
cat("Cholesky decomposition for reversed order:\n")
print(B_0_inv_reversed)

# Identify the index for the FFR variable in the reversed model
ffr_shock_index_reversed <- which(reversed_variables == "FFR")

# Scale FFR shock in reversed model
scaling_factor_ffr_reversed <- 1 / abs(B_0_inv_reversed[ffr_shock_index_reversed, ffr_shock_index_reversed])
B_0_inv_reversed_scaled <- B_0_inv_reversed
B_0_inv_reversed_scaled[, ffr_shock_index_reversed] <- B_0_inv_reversed[, ffr_shock_index_reversed] * scaling_factor_ffr_reversed

# Normalize and reverse the FFR shock for the reversed model
ffr_shock_index_reversed <- which(reversed_variables == "FFR")
B_0_inv_reversed_scaled <- normalize_shock(B_0_inv_reversed, ffr_shock_index_reversed)

cat("Reversed Variables:", reversed_variables, "\n")

# IRF computation for reversed order with scaled shocks
irfMat_reversed <- array(0, c(nvars, nvars, nsteps + 1))
irfMat_reversed[, , 1] <- B_0_inv_reversed_scaled

# Construct companion matrix for reversed order
if (p == 1) {
  # Simplified companion matrix for p = 1
  A_comp_reversed <- t(B_hat_reversed[-1, ])  # Only the first block of coefficients
} else {
  # Full companion matrix for p > 1
  A_comp_reversed <- matrix(0, nvars * p, nvars * p)
  A_comp_reversed[1:nvars, 1:(nvars * p)] <- t(B_hat_reversed[-1, ])
  A_comp_reversed[(nvars + 1):(nvars * p), 1:(nvars * (p - 1))] <- diag(nvars * (p - 1))
}


Ah_reversed <- diag(nvars * p)
for (h in 1:nsteps) {
  Ah_reversed <- Ah_reversed %*% A_comp_reversed
  irfMat_reversed[, , h + 1] <- Ah_reversed[1:nvars, 1:nvars] %*% B_0_inv_reversed_scaled
}

# Display the scaled impact matrix for verification
cat("\nImpact matrix (B_0_inv_reversed_scaled) after normalization and sign reversal of the FFR shock (reversed model):\n")
print(B_0_inv_reversed_scaled)


# Simplified 4-Variable SVAR Model
cat("\n--- Simplified 4-Variable SVAR Model ---\n")

# Select the desired variables for the simplified model
simplified_variables <- c("Monthly_Risk_Aversion", "Monthly_Uncertainty",  "FFR", "log_Household_Equity")
Y_simplified <- Y[, simplified_variables]
colnames(Y_simplified) <- simplified_variables
nvars_simplified <- length(simplified_variables)

# Prepare lagged regressors for the simplified model
X_simplified <- embed(Y_simplified, p + 1)
Y_dep_simplified <- X_simplified[, 1:nvars_simplified]  # Dependent variables
X_simplified <- X_simplified[, -(1:nvars_simplified)]  # Lagged independent variables
X_simplified <- cbind(1, X_simplified)  # Add constant term

# Estimate coefficients using OLS for simplified model
B_hat_simplified <- solve(t(X_simplified) %*% X_simplified) %*% t(X_simplified) %*% Y_dep_simplified
u_hat_simplified <- Y_dep_simplified - X_simplified %*% B_hat_simplified
Sigma_u_simplified <- crossprod(u_hat_simplified) / (T - p - nvars_simplified)

# Perform Cholesky decomposition for simplified model
B_0_inv_simplified <- t(chol(Sigma_u_simplified))
cat("Cholesky decomposition for simplified model:\n")
print(B_0_inv_simplified)

# Identify the index for the FFR variable in the simplified model
ffr_shock_index_simplified <- which(simplified_variables == "FFR")

# Scale FFR shock in simplified model
scaling_factor_ffr_simplified <- 1 / abs(B_0_inv_simplified[ffr_shock_index_simplified, ffr_shock_index_simplified])
B_0_inv_simplified_scaled <- B_0_inv_simplified
B_0_inv_simplified_scaled[, ffr_shock_index_simplified] <- B_0_inv_simplified[, ffr_shock_index_simplified] * scaling_factor_ffr_simplified

# Normalize and reverse the FFR shock for the simplified model
ffr_shock_index_simplified <- which(simplified_variables == "FFR")
B_0_inv_simplified_scaled <- normalize_shock(B_0_inv_simplified, ffr_shock_index_simplified)

cat("Simplified Variables:", simplified_variables, "\n")

# IRF computation for simplified model with scaled shocks
irfMat_simplified <- array(0, c(nvars_simplified, nvars_simplified, nsteps + 1))
irfMat_simplified[, , 1] <- B_0_inv_simplified_scaled

# Construct companion matrix for simplified model
if (p == 1) {
  # Simplified companion matrix for p = 1
  A_comp_simplified <- t(B_hat_simplified[-1, ])  # Only the first block of coefficients
} else {
  # Full companion matrix for p > 1
  A_comp_simplified <- matrix(0, nvars_simplified * p, nvars_simplified * p)
  A_comp_simplified[1:nvars_simplified, 1:(nvars_simplified * p)] <- t(B_hat_simplified[-1, ])
  A_comp_simplified[(nvars_simplified + 1):(nvars_simplified * p), 
                    1:(nvars_simplified * (p - 1))] <- diag(nvars_simplified * (p - 1))
}

Ah_simplified <- diag(nvars_simplified * p)
for (h in 1:nsteps) {
  Ah_simplified <- Ah_simplified %*% A_comp_simplified
  irfMat_simplified[, , h + 1] <- Ah_simplified[1:nvars_simplified, 1:nvars_simplified] %*% B_0_inv_simplified_scaled
}

# Display the scaled impact matrix for verification
cat("\nImpact matrix (B_0_inv_simplified_scaled) after normalization and sign reversal of the FFR shock (simplified model):\n")
print(B_0_inv_simplified_scaled)

# Combine IRFs for the response of risky assets to an FFR shock for all models

# Extract IRFs for baseline model
irf_baseline <- data.frame(
  Horizon = 0:nsteps,
  Response = irfMat_scaled[risk_assets_index, ffr_shock_index, ] * 100,
  Model = "Baseline"
)

# Extract IRFs for reversed order model
risk_assets_index_reversed <- which(reversed_variables == "log_Household_Equity")
ffr_shock_index_reversed <- which(reversed_variables == "FFR")
irf_reversed <- data.frame(
  Horizon = 0:nsteps,
  Response = irfMat_reversed[risk_assets_index_reversed, ffr_shock_index_reversed, ] * 100,
  Model = "Reversed Order"
)

# Extract IRFs for simplified model
risk_assets_index_simplified <- which(simplified_variables == "log_Household_Equity")
ffr_shock_index_simplified <- which(simplified_variables == "FFR")
irf_simplified <- data.frame(
  Horizon = 0:nsteps,
  Response = irfMat_simplified[risk_assets_index_simplified, ffr_shock_index_simplified, ] * 100,
  Model = "Simplified"
)

cat("Baseline: FFR Index:", ffr_shock_index, "Risk Assets Index:", risk_assets_index, "\n")
cat("Reversed: FFR Index:", ffr_shock_index_reversed, "Risk Assets Index:", risk_assets_index_reversed, "\n")
cat("Simplified: FFR Index:", ffr_shock_index_simplified, "Risk Assets Index:", risk_assets_index_simplified, "\n")

print("Baseline IRF for Risk Assets to FFR Shock:")
print(head(irfMat_scaled[risk_assets_index, ffr_shock_index, ]))

print("Reversed Order IRF for Risk Assets to FFR Shock:")
print(head(irfMat_reversed[risk_assets_index_reversed, ffr_shock_index_reversed, ]))

print("Simplified IRF for Risk Assets to FFR Shock:")
print(head(irfMat_simplified[risk_assets_index_simplified, ffr_shock_index_simplified, ]))

# Combine all IRFs into a single dataframe
combined_irfs <- rbind(irf_baseline, irf_reversed, irf_simplified)

# Plot the combined IRFs
ggplot(combined_irfs, aes(x = Horizon, y = Response, color = Model)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Response of Risk Assets to a 1% Expansionary FFR Shock",
    x = "Horizon (Months)",
    y = "Response (% Change)",
    color = "Model Specification"
  ) +
  scale_color_manual(values = c("Baseline" = "blue", "Reversed Order" = "red", "Simplified" = "green")) +
  theme_minimal() +
  theme(
    text = element_text(size = 16), # General text size
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5), # Title size and alignment
    axis.title.x = element_text(size = 35), # X-axis title size
    axis.title.y = element_text(size = 35), # Y-axis title size
    axis.text = element_text(size = 30), # Axis tick label size
    legend.title = element_text(size = 30), # Legend title size
    legend.text = element_text(size = 25), # Legend text size
    panel.grid = element_blank() # Removes all gridlines
  )

# IRF Comparison for Different Lags in the Baseline Model
cat("\n--- IRF Comparison for Different Lags ---\n")

# Initialize a list to store IRFs for each lag order
irf_list <- list()

# Loop over different lag orders (1, 2, 3, 4)
for (lag_order in 1:4) {
  cat("Computing IRFs for lag order:", lag_order, "\n")
  
  # Prepare lagged regressors for the current lag order
  X_lag <- embed(Y, lag_order + 1)
  Y_dep_lag <- X_lag[, 1:nvars]  # Dependent variables
  X_lag <- X_lag[, -(1:nvars)]  # Lagged independent variables
  X_lag <- cbind(1, X_lag)  # Add constant term
  
  # Estimate coefficients using OLS for the current lag order
  B_hat_lag <- solve(t(X_lag) %*% X_lag) %*% t(X_lag) %*% Y_dep_lag
  u_hat_lag <- Y_dep_lag - X_lag %*% B_hat_lag
  Sigma_u_lag <- crossprod(u_hat_lag) / (T - lag_order - nvars)
  
  # Perform Cholesky decomposition for the current lag order
  B_0_inv_lag <- t(chol(Sigma_u_lag))
  
  # Scale the FFR shock in the current lag order
  scaling_factor_ffr_lag <- -1 / abs(B_0_inv_lag[ffr_shock_index, ffr_shock_index])  # Negative for expansionary shock
  B_0_inv_lag_scaled <- B_0_inv_lag
  B_0_inv_lag_scaled[, ffr_shock_index] <- B_0_inv_lag[, ffr_shock_index] * scaling_factor_ffr_lag
  
  
  # Compute IRFs for the current lag order
  irfMat_lag <- array(0, c(nvars, nvars, nsteps + 1))
  irfMat_lag[, , 1] <- B_0_inv_lag_scaled
  
  # Construct the companion matrix for the current lag order
  A_comp_lag <- matrix(0, nvars * lag_order, nvars * lag_order)
  A_comp_lag[1:nvars, 1:(nvars * lag_order)] <- t(B_hat_lag[-1, ])
  if (lag_order > 1) {
    A_comp_lag[(nvars + 1):(nvars * lag_order), 1:(nvars * (lag_order - 1))] <- diag(nvars * (lag_order - 1))
  }
  
  Ah_lag <- diag(nvars * lag_order)
  for (h in 1:nsteps) {
    Ah_lag <- Ah_lag %*% A_comp_lag
    irfMat_lag[, , h + 1] <- Ah_lag[1:nvars, 1:nvars] %*% B_0_inv_lag_scaled
  }
  
  # Store the IRF for the response of risk assets to FFR shock
  irf_list[[paste0("Lag_", lag_order)]] <- data.frame(
    Horizon = 0:nsteps,
    Response = irfMat_lag[risk_assets_index, ffr_shock_index, ] * 100,
    Lag = paste0("Lag ", lag_order)
  )
}

# Combine IRFs for all lag orders
combined_irfs_lags <- do.call(rbind, irf_list)

# Plot the combined IRFs for all lag orders
ggplot(combined_irfs_lags, aes(x = Horizon, y = Response, color = Lag)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Response of Risky Assets to a 1% Expansionary FFR Shock for Different Lag Orders",
    x = "Horizon (Months)",
    y = "Response (% Change)",
    color = "Lag Order"
  ) +
  scale_color_manual(values = c("Lag 1" = "blue", "Lag 2" = "red", "Lag 3" = "green", "Lag 4" = "purple")) +
  theme_minimal() +
  theme(
    text = element_text(size = 16), # General text size
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5), # Title size and alignment
    axis.title.x = element_text(size = 35), # X-axis title size
    axis.title.y = element_text(size = 35), # Y-axis title size
    axis.text = element_text(size = 30), # Axis tick label size
    legend.title = element_text(size = 30), # Legend title size
    legend.text = element_text(size = 25), # Legend text size
    panel.grid = element_blank() # Removes all gridlines
  )
