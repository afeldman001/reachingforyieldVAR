# Data setup for Seminar thesis: 
# Interest Rate Cuts and Household Investment in Risky Assets: An empirical examination of the ‘Reaching for Yield’ Hypothesis
# by Aaron Feldman and Chirag Khanna
# University of Tuebingen, WS 2024-2025
# Contact: 
# aaron.feldman@student.uni-tuebingen.de
# chirag.khanna@student.uni-tuebingen.de

# clear workspace and console output
rm(list = ls())
cat("\014")

# install and load required packages
install.packages(c("yfR", "dplyr", "lubridate", "zoo", "ggplot2", "stats", "mFilter", "tidyr", "purr", "vars"))

library(yfR)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(stats)
library(mFilter)
library(tidyr)
library(purrr)
library(vars)  # for VAR models

###############################

### Add Additional Variables for SVAR ###

# define GitHub raw URLs for additional datasets
cpi_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/CPIAUCSL%20(1).csv"
ppi_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/WPSID61.csv"
ip_index_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/INDPRO.csv"
ffr_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/FEDFUNDS.csv"
household_equity_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/Risk_Assets.csv"

# fetch and process CPI data
cpi_data <- read.csv(cpi_url) %>%
  mutate(
    date = as.Date(observation_date), # convert observation_date to Date
    CPI = CPIAUCSL                   # rename the CPI column
  ) %>%
  dplyr::select(date, CPI) %>%       # use dplyr's select function explicitly
  filter(!is.na(CPI))                # ensure no missing values

# fetch and process PPI data
ppi_data <- read.csv(ppi_url) %>%
  mutate(
    date = as.Date(observation_date), # convert observation_date to Date
    PPI = WPSID61                     # rename the PPI column
  ) %>%
  dplyr::select(date, PPI) %>%               # keep only the relevant columns
  filter(!is.na(PPI))                 # ensure no missing values

# fetch and process Industrial Production Index data
ip_index_data <- read.csv(ip_index_url) %>%
  mutate(
    date = as.Date(observation_date), # convert observation_date to Date
    IP_Index = INDPRO                 # rename the Industrial Production Index column
  ) %>%
  dplyr::select(date, IP_Index) %>%          # keep only the relevant columns
  filter(!is.na(IP_Index))            # ensure no missing values

# fetch and process Federal Funds Rate data
ffr_data <- read.csv(ffr_url) %>%
  mutate(
    date = as.Date(observation_date), # convert observation_date to Date
    FFR = FEDFUNDS                    # rename the Federal Funds Rate column
  ) %>%
  dplyr::select(date, FFR) %>%               # keep only the relevant columns
  filter(!is.na(FFR))                 # ensure no missing values

# fetch and process Household Equity Ownership data
household_equity_data <- read.csv(household_equity_url) %>%
  mutate(
    date = as.Date(observation_date), # convert observation_date to Date
    Household_Equity = BOGZ1FL153064486Q # rename the Household Equity column
  ) %>%
  dplyr::select(date, Household_Equity) %>%  # keep only the relevant columns
  filter(!is.na(Household_Equity))    # ensure no missing values

# interpolate quarterly household equity data to monthly frequency
household_equity_monthly <- household_equity_data %>%
  complete(date = seq.Date(as.Date("1990-01-01"), as.Date("2024-11-01"), by = "month")) %>%
  arrange(date) %>%
  mutate(Household_Equity = zoo::na.approx(Household_Equity, na.rm = FALSE))

# merge all datasets into one dataframe
merged_data <- reduce(
  list(cpi_data, ppi_data, ip_index_data, ffr_data, household_equity_monthly),
  full_join,
  by = "date"
) %>%
  filter(date >= as.Date("1990-01-01") & date <= as.Date("2024-11-01")) %>%
  arrange(date)

# Define event dates for major expansionary monetary events
expansionary_events <- as.Date(c("2008-10-01", "2009-03-01", "2020-04-01"))

# Add the dummy variable to merged_data
merged_data <- merged_data %>%
  mutate(Economic_Shock = ifelse(date %in% expansionary_events, 1, 0))

# ensure log-transformed variables are interpolated for missing values
merged_data <- merged_data %>%
  mutate(
    log_CPI = log(CPI),
    log_PPI = ifelse(PPI > 0, log(PPI), NA),
    log_IP_Index = ifelse(IP_Index > 0, log(IP_Index), NA),
    log_Household_Equity = ifelse(Household_Equity > 0, log(Household_Equity), NA)
  ) %>%
  # interpolate missing values for log-transformed series
  mutate(
    log_PPI = zoo::na.approx(log_PPI, na.rm = FALSE),
    log_IP_Index = zoo::na.approx(log_IP_Index, na.rm = FALSE),
    log_Household_Equity = zoo::na.approx(log_Household_Equity, na.rm = FALSE)
  ) %>%
  # drop rows where interpolation wasn't possible
  filter(!is.na(log_CPI) & !is.na(log_PPI) & !is.na(log_IP_Index) & !is.na(log_Household_Equity))

# hp detrend all variables
hp_log_cpi <- hpfilter(merged_data$log_CPI, freq = 129600)
hp_log_ppi <- hpfilter(merged_data$log_PPI, freq = 129600)
hp_log_ip_index <- hpfilter(merged_data$log_IP_Index, freq = 129600)
hp_log_ffr <- hpfilter(merged_data$FFR, freq = 129600) # FFR is not log-transformed
hp_log_household_equity <- hpfilter(merged_data$log_Household_Equity, freq = 129600)

# create a new dataframe for HP detrended cycles
hp_data <- data.frame(
  date = merged_data$date,
  CPI_Cycle = hp_log_cpi$cycle,
  IP_Index_Cycle = hp_log_ip_index$cycle,
  FFR_Cycle = hp_log_ffr$cycle,
  PPI_Cycle = hp_log_ppi$cycle,
  Economic_Shock = merged_data$Economic_Shock,
  Household_Equity_Cycle = hp_log_household_equity$cycle
)


# debugging: check for missing values and data alignment
#print("Summary of Merged Data:")
#print(summary(merged_data))

# convert the dataset to long format for ggplot faceting
long_data <- hp_data %>%
  dplyr::select(date, CPI_Cycle, IP_Index_Cycle, FFR_Cycle, PPI_Cycle, Household_Equity_Cycle) %>%
  pivot_longer(
    cols = c(CPI_Cycle, IP_Index_Cycle, FFR_Cycle, PPI_Cycle, Household_Equity_Cycle),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(long_data, aes(x = date, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(
    title = "Detrended Cycles for SVAR Variables",
    x = "Date",
    y = "Detrended Value"
  ) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


################################

# define start and end dates
start_date <- "1990-01-01"
end_date <- "2024-04-01"

# fetch S&P 500 data for realized variance
sp500_data <- yf_get(tickers = "^GSPC", first_date = start_date, last_date = end_date) %>%
  arrange(ref_date) %>%
  mutate(
    daily_return = log(price_adjusted / lag(price_adjusted)), # calculate daily log returns
    realized_variance = zoo::rollapply(
      daily_return,
      width = 21, # 21-day rolling window
      FUN = function(x) mean(x^2, na.rm = TRUE), # calculate rolling variance
      fill = NA, # fill with NA for incomplete windows
      align = "right" # align the window to the right
    )
  ) %>%
  filter(realized_variance > 0) # ensure only positive realized variance

# log transform adjusted prices
sp500_data <- sp500_data %>%
  mutate(
    log_price_adjusted = log(price_adjusted) # compute the natural log
  )

# aggregate S&P 500 data to monthly frequency
sp500_monthly <- sp500_data %>%
  mutate(month = floor_date(ref_date, "month")) %>% # group by month
  group_by(month) %>%
  summarise(
    log_price_adjusted = mean(log_price_adjusted, na.rm = TRUE) # monthly average
  ) %>%
  ungroup()

# apply HP filter to the monthly aggregated data
hp_log_sp500 <- hpfilter(sp500_monthly$log_price_adjusted, freq = 129600) # apply HP filter with lambda for monthly data

# add the detrended cycle to the monthly data
sp500_monthly <- sp500_monthly %>%
  mutate(sp500_cycle = hp_log_sp500$cycle)

# rename 'month' to 'date' for merging consistency
sp500_cycle_data <- sp500_monthly %>%
  rename(date = month)

# add the detrended stock market variable to merged_data
merged_data <- merged_data %>%
  left_join(sp500_cycle_data, by = "date") # Merge by date


# debugging: check daily returns and realized variance
#summary(sp500_data$daily_return)
#summary(sp500_data$realized_variance)

# fetch VIX data for implied variance
vix_data <- yf_get(tickers = "^VIX", first_date = start_date, last_date = end_date) %>%
  arrange(ref_date) %>%
  mutate(VIX_squared = price_close^2) # calculate VIX squared

# debugging: check VIX squared
#summary(vix_data$VIX_squared)

# align dates between VIX and S&P 500 data
vix_data <- vix_data %>%
  filter(ref_date %in% sp500_data$ref_date)

# merge datasets
financial_data <- merge(vix_data, sp500_data, by = "ref_date", suffixes = c("_vix", "_sp500"))

### Using actual realized variance (RVAR) ###

# calculate risk aversion (RA) and uncertainty (UC)
financial_data <- financial_data %>%
  mutate(
    Uncertainty = realized_variance, # use realized variance as Uncertainty
    Risk_Aversion = VIX_squared - Uncertainty # compute Risk Aversion
  )

# calculate quantile thresholds for winsorization
uncertainty_quantiles <- quantile(financial_data$Uncertainty, probs = c(0.01, 0.99), na.rm = TRUE)
risk_aversion_quantiles <- quantile(financial_data$Risk_Aversion, probs = c(0.01, 0.99), na.rm = TRUE)

# winsorize RA and UC using calculated quantiles
financial_data <- financial_data %>%
  mutate(
    Winsorized_Uncertainty = pmin(pmax(Uncertainty, uncertainty_quantiles[1]), uncertainty_quantiles[2]),
    Winsorized_Risk_Aversion = pmin(pmax(Risk_Aversion, risk_aversion_quantiles[1]), risk_aversion_quantiles[2])
  )

# normalize winsorized components for visualization
financial_data <- financial_data %>%
  mutate(
    Norm_Winsorized_Uncertainty = Winsorized_Uncertainty / max(Winsorized_Uncertainty, na.rm = TRUE),
    Norm_Winsorized_Risk_Aversion = Winsorized_Risk_Aversion / max(Winsorized_Risk_Aversion, na.rm = TRUE)
  )


# define NBER recession periods
recessions <- data.frame(
  start = as.Date(c("1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01")),
  end = as.Date(c("1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01")),
  label = c("1990 Recession", "2001 Dot-Com Recession", "2008 Financial Crisis", "COVID-19 Recession")
)

# define max volatility events between January 1, 1990, and November 1, 2024
volatility_events <- data.frame(
  start = as.Date(c(
    "1990-10-01", "1997-11-01", "1998-09-01", "2001-09-01", "2002-09-01",
    "2003-02-01", "2008-10-01", "2009-03-01", "2011-08-01", "2020-04-01"
  )),
  end = as.Date(c(
    "1990-11-01", "1997-12-01", "1998-10-01", "2001-10-01", "2002-10-01",
    "2003-03-01", "2008-11-01", "2009-03-31", "2011-08-31", "2020-04-30"
  )),
  label = c(
    "Gulf War I", "Asian Crisis", "Russian/LTCM", "9/11", "Worldcom/Enron",
    "Gulf War II", "Credit Crunch", "global financial crisis market bottom", 
    "european sovereign debt crisis", "covid-19 pandemic economic shutdown"
  )
)

# simplified plot with filtered events
ggplot(financial_data, aes(x = ref_date)) + 
  # add normalized uncertainty and risk aversion lines
  geom_line(aes(y = Norm_Winsorized_Uncertainty, color = "Normalized Winsorized Uncertainty (UC)")) +
  geom_line(aes(y = Norm_Winsorized_Risk_Aversion, color = "Normalized Winsorized Risk Aversion (RA)")) +
  # add shaded regions for recessions
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
            fill = "gray", alpha = 0.6, inherit.aes = FALSE) +
  # add shaded regions for filtered volatility events
  geom_rect(data = volatility_events, aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
            fill = "blue", alpha = 0.3, inherit.aes = FALSE) +
  # add labels and customize the theme
  labs(
    title = "Simplified VIX Decomposition with Recessions and Post-1990 Volatility Events",
    x = "Date",
    y = "Normalized Value",
    color = "Component"
  ) +
  theme_minimal()

# save results for further analysis (optional)
#write.csv(financial_data, "vix_decomposition_winsorized.csv", row.names = FALSE)


### Using fitted RVAR from a two variable regression ###

# step 1: two-variable regression to predict realized variance (Uncertainty)
forecast_model <- lm(realized_variance ~ lag(VIX_squared, 1) + lag(realized_variance, 1), data = financial_data) # lm() creates a linear regression model 

# step 2: display regression summary (coefficients with p-values)
regression_summary <- summary(forecast_model)
print(regression_summary)

# extract coefficients and p-values
coefficients_table <- regression_summary$coefficients
print("Coefficients with p-values:")
print(coefficients_table)

# step 2: fitted values as Uncertainty (UC)
financial_data <- financial_data %>%
  mutate(
    Fitted_Uncertainty = predict(forecast_model, newdata = financial_data), # fitted values for UC
    Risk_Aversion = VIX_squared - Fitted_Uncertainty # compute RA
  )

# step 3: Winsorization to handle outliers
uncertainty_quantiles <- quantile(financial_data$Fitted_Uncertainty, probs = c(0.01, 0.99), na.rm = TRUE)
risk_aversion_quantiles <- quantile(financial_data$Risk_Aversion, probs = c(0.01, 0.99), na.rm = TRUE)

financial_data <- financial_data %>%
  mutate(
    Winsorized_Uncertainty = pmin(pmax(Fitted_Uncertainty, uncertainty_quantiles[1]), uncertainty_quantiles[2]),
    Winsorized_Risk_Aversion = pmin(pmax(Risk_Aversion, risk_aversion_quantiles[1]), risk_aversion_quantiles[2])
  )

# step 4: normalize for visualization
financial_data <- financial_data %>%
  mutate(
    Norm_Winsorized_Uncertainty = Winsorized_Uncertainty / max(Winsorized_Uncertainty, na.rm = TRUE),
    Norm_Winsorized_Risk_Aversion = Winsorized_Risk_Aversion / max(Winsorized_Risk_Aversion, na.rm = TRUE)
  )

# plot results with recession shading
ggplot(financial_data, aes(x = ref_date)) + 
  geom_line(aes(y = Norm_Winsorized_Uncertainty, color = "Normalized Winsorized Uncertainty (UC)")) +
  geom_line(aes(y = Norm_Winsorized_Risk_Aversion, color = "Normalized Winsorized Risk Aversion (RA)")) +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
            fill = "gray", alpha = 0.6, inherit.aes = FALSE) +
  # add shaded regions for filtered volatility events
  geom_rect(data = volatility_events, aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
            fill = "blue", alpha = 0.3, inherit.aes = FALSE) +
  labs(
    title = "Simplified VIX Decomposition (Regression-Based UC)",
    x = "Date",
    y = "Normalized Value",
    color = "Component"
  ) +
  theme_minimal()

# save results for further analysis (optional)
#write.csv(financial_data, "vix_decomposition_regression_based.csv", row.names = FALSE)

### Aggregate to Monthly and Apply HP Detrending ###

# isolate the variables of interest
vol_data <- financial_data %>%
  dplyr::select(ref_date, Norm_Winsorized_Uncertainty, Norm_Winsorized_Risk_Aversion)

# aggregate to monthly frequency
monthly_data <- vol_data %>%
  mutate(month = floor_date(ref_date, "month")) %>%
  group_by(month) %>%
  summarise(
    Monthly_Uncertainty = mean(Norm_Winsorized_Uncertainty, na.rm = TRUE),
    Monthly_Risk_Aversion = mean(Norm_Winsorized_Risk_Aversion, na.rm = TRUE)
  ) %>%
  ungroup()


# plot results with recession shading
ggplot(monthly_data, aes(x = month)) + 
  geom_line(aes(y = Monthly_Uncertainty, color = "Monthly Uncertainty (UC)")) +
  geom_line(aes(y = Monthly_Risk_Aversion, color = "Monthly Risk Aversion (RA)")) +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
            fill = "gray", alpha = 0.3, inherit.aes = FALSE) +
  # add shaded regions for filtered volatility events
  geom_rect(data = volatility_events, aes(xmin = start, xmax = end, ymin = 0, ymax = 1),
            fill = "blue", alpha = 0.3, inherit.aes = FALSE) +
  labs(
    title = "Simplified VIX Decomposition (Regression-Based UC)",
    x = "Date",
    y = "Normalized Value",
    color = "Component"
  ) +
  theme_minimal()

# HP detrend the series
hp_uncertainty <- hpfilter(monthly_data$Monthly_Uncertainty, freq = 129600) # Lambda for monthly data
hp_risk_aversion <- hpfilter(monthly_data$Monthly_Risk_Aversion, freq = 129600)

# create a new dataframe with date, trend, and cycle components
detrended_vol_data <- data.frame(
  date = monthly_data$month, # use the existing date column
  Uncertainty_Cycle = hp_uncertainty$cycle,
  Risk_Aversion_Cycle = hp_risk_aversion$cycle
) %>%
  filter(date >= as.Date("1990-01-01") & date <= as.Date("2024-04-01"))

# align the date range of detrended_vol_data with hp_data
detrended_vol_data <- detrended_vol_data %>%
  filter(date %in% hp_data$date)

# merge the detrended series into hp_data and add Economic_Shock
hp_data <- hp_data %>%
  left_join(detrended_vol_data, by = "date") %>%
  mutate(
    SP500_Cycle = merged_data$sp500_cycle, # add SP500_Cycle from merged_data
    Economic_Shock = merged_data$Economic_Shock # add Economic_Shock from merged_data
  ) %>%
  dplyr::select(
    date, 
    CPI_Cycle,
    IP_Index_Cycle,
    PPI_Cycle,
    FFR_Cycle,
    SP500_Cycle, # use SP500_Cycle directly now
    Economic_Shock, # add Economic_Shock as the last variable
    Uncertainty_Cycle,
    Risk_Aversion_Cycle,
    Household_Equity_Cycle # ensure Household_Equity_Cycle is last
  ) %>%
  arrange(date) # ensure the dataframe remains ordered by date

# reshape hp_data to long format for ggplot
hp_data_long <- hp_data %>%
  pivot_longer(
    cols = -date, # select all columns except "date"
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "CPI_Cycle",
        "IP_Index_Cycle",
        "PPI_Cycle",
        "FFR_Cycle",
        "SP500_Cycle",
        "Economic_Shock",
        "Uncertainty_Cycle",
        "Risk_Aversion_Cycle",
        "Household_Equity_Cycle" # ensure proper ordering for ggplot
      )
    ),
    # rename variables for better clarity in the plot
    Variable = recode(
      Variable,
      "CPI_Cycle" = "Log-transformed CPI Cycle",
      "IP_Index_Cycle" = "Log-transformed IP Index Cycle",
      "PPI_Cylcel" = "Log-transformed PPI Cycle",
      "FFR_Cycle" = "FFR Cycle",
      "SP500_Cycle" = "Log-transformed S&P 500 Cycle",
      "Economic_Shock" = "Economic Shock Dummy-Variable",
      "Uncertainty_Cycle" = "Uncertainty Cycle",
      "Risk_Aversion_Cycle" = "Risk Aversion Cycle",
      "Household_Equity_Cycle" = "Log-transformed Household Equity Cycle"
    )
  )

# create the ggplot figure
ggplot(hp_data_long, aes(x = date, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) + # create subplots with free y-scales
  theme_minimal() +
  labs(
    title = "HP Detrended Variables",
    x = "Date",
    y = "Detrended Value"
  ) +
  scale_x_date(
    limits = as.Date(c("1990-01-01", "2024-04-01")), # explicitly set the date range
    expand = c(0, 0),                                # remove padding around date range
    date_breaks = "5 years",                         # adjust intervals for x-axis ticks
    date_labels = "%Y"                               # format date labels
  ) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # customize subplot titles
    axis.text.x = element_text(angle = 45, hjust = 1)    # rotate x-axis labels for clarity
  )

#### define a function to calculate AIC and SIC for a range of lags ###

calculate_aic_sic <- function(data, max_lag) {
  # ensure data is a matrix
  data_matrix <- as.matrix(data)
  T <- nrow(data_matrix) # number of observations
  K <- ncol(data_matrix) # number of variables
  
  # initialize storage for AIC and SIC values
  aic_values <- numeric(max_lag)
  sic_values <- numeric(max_lag)
  
  for (lag in 1:max_lag) {
    # create lagged data matrices
    lagged_data <- embed(data_matrix, lag + 1)
    Y <- lagged_data[, 1:K] # dependent variable
    X <- lagged_data[, -c(1:K)] # lagged independent variables
    
    # estimate VAR coefficients using OLS
    B_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
    residuals <- Y - X %*% B_hat
    Sigma_u_hat <- (t(residuals) %*% residuals) / (T - lag * K)
    
    # calculate AIC and SIC
    log_likelihood <- -0.5 * T * (K * log(2 * pi) + log(det(Sigma_u_hat)) + K)
    num_parameters <- K * K * lag # number of parameters in the VAR
    aic_values[lag] <- -2 * log_likelihood + 2 * num_parameters
    sic_values[lag] <- -2 * log_likelihood + log(T) * num_parameters
  }
  
  # determine the optimal lag based on AIC and SIC
  optimal_aic_lag <- which.min(aic_values)
  optimal_sic_lag <- which.min(sic_values)
  
  return(list(aic_values = aic_values, sic_values = sic_values, 
              optimal_aic_lag = optimal_aic_lag, optimal_sic_lag = optimal_sic_lag))
}

# apply the function to your detrended data
detrended_vars <- hp_data %>%
  dplyr::select(-date) %>% # exclude the date column
  drop_na() # ensure there are no missing values

# set the maximum number of lags to consider
max_lag <- 8

# calculate AIC and SIC
aic_sic_results <- calculate_aic_sic(detrended_vars, max_lag)

# print the results
print("AIC values for each lag:")
print(aic_sic_results$aic_values)

print("SIC values for each lag:")
print(aic_sic_results$sic_values)

print(paste("Optimal lag based on AIC:", aic_sic_results$optimal_aic_lag))
print(paste("Optimal lag based on SIC:", aic_sic_results$optimal_sic_lag))

