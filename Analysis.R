# Working analysis script for Seminar thesis: 
# Interest Rate Cuts and Household Investment in Risky Assets: An empirical examination of the ‘Reaching for Yield’ Hypothesis
# by Aaron Feldman and Chirag Khanna
# University of Tuebingen, WS 2024-2025
# Contact: 
# aaron.feldman@student.uni-tuebingen.de
# chirag.khanna@student.uni-tuebingen.de

# Install packages and libraries before running the main script
# to ensure bvar() runs smoothly


install.packages("bvartools", dependencies = TRUE)  
install.packages("BVAR", dependencies = TRUE)

library(bvartools)
library(BVAR)


# Prepare data with named columns
colnames(data_adj_subset) <- c("ffr_diff", "p_exp", "cci_diff", "sentiment", "market_diff", "ra_diff")

# Define and fit the BVAR model with selected lag length
# If bvar() throws unused arguments error:
# Paste into console in following order: (1) install.packages("BVAR") 
# -> restart session 
# (yes); (2) library(BVAR) -> run line 29

x <- bvar(data_adj_subset, lags = 1, n_draw = 10000L, n_burn = 2000L, verbose = TRUE)

# Calculate impulse responses with a horizon of 10
irf_result <- irf(x, horizon = 20, fevd = FALSE)

# Rename dimensions of the IRF result array to match variable names
dimnames(irf_result$irf)[[2]] <- colnames(data_adj_subset)  # Response variables
dimnames(irf_result$irf)[[4]] <- colnames(data_adj_subset)  # Shock variables

# Extract the position of the "ffr_diff" shock
ffr_position <- which(colnames(data_adj_subset) == "ffr_diff")

# Apply a negative shock by inverting the IRF responses for the "ffr_diff" shock
irf_data <- lapply(colnames(data_adj_subset), function(variable) {
  # Invert the responses for the "ffr_diff" shock to simulate a negative shock
  response_values <- -irf_result$irf[, variable, , ffr_position]
  
  # Calculate mean, lower bound (16th percentile), and upper bound (84th percentile) across draws
  response_mean <- apply(response_values, 2, mean)
  response_lower <- apply(response_values, 2, quantile, probs = 0.16)
  response_upper <- apply(response_values, 2, quantile, probs = 0.84)
  
  # Create a data frame with Horizon, Response, Lower Bound, Upper Bound, and Variable columns
  data.frame(
    Horizon = 0:(length(response_mean) - 1),
    Response = response_mean,
    Lower = response_lower,
    Upper = response_upper,
    Variable = variable
  )
})

# Combine all response data into a single data frame
irf_df <- do.call(rbind, irf_data)

# Adjust the ordering of the variable factor levels
irf_df$Variable <- factor(irf_df$Variable, levels = c("ffr_diff", "p_exp", "cci_diff", "sentiment", "market_diff", "ra_diff"))

# Plot the impulse responses with confidence intervals using ggplot2
library(ggplot2)

ggplot(irf_df, aes(x = Horizon, y = Response)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.3) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3, nrow = 2) +  # Set ncol or nrow as desired for layout
  labs(
    title = "Impulse Responses to a Negative Shock in Federal Funds Rate (ffr_diff)",
    x = "Horizon (Quarters)", 
    y = "Response"
  ) +
  theme_minimal()




