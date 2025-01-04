# Data setup for Seminar thesis: 
# Interest Rate Cuts and Household Investment in Risky Assets: An empirical examination of the ‘Reaching for Yield’ Hypothesis
# by Aaron Feldman and Chirag Khanna
# University of Tuebingen, WS 2024-2025
# Contact: 
# aaron.feldman@student.uni-tuebingen.de
# chirag.khanna@student.uni-tuebingen.de

# install and load required packages
install.packages(c("yfR", "dplyr", "lubridate", "zoo", "ggplot2", "stats", "mFilter", "tidyr", "purr", "vars", "tseries", "forecast"))

# clear workspace and console output
rm(list = ls())
cat("\014")


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
library(tseries)  # For ADF test
library(forecast)

###############################

### Add Additional Variables for SVAR ###

# define GitHub raw URLs for additional datasets
cpi_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/CPIAUCSL%20(1).csv"
ppi_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/WPSID61.csv"
ip_index_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/INDPRO.csv"
ffr_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/FEDFUNDS.csv"
cc_data_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/UMCSENT.csv"
unrate_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/UNRATE%20(1).csv"
dspi_url <- "https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/DSPI.csv"
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

# fetch and process consumer sentiment data
cc_data <- read.csv(cc_data_url) %>%
  mutate(
    date = as.Date(observation_date),  # Convert observation_date to Date
    Consumer_Confidence = UMCSENT     # Rename the column
  ) %>%
  dplyr::select(date, Consumer_Confidence) %>%  # Select relevant columns
  filter(!is.na(Consumer_Confidence))           # Remove missing values

# fetch and process unemployment rate data
unrate_data <- read.csv(unrate_url) %>%
  mutate(
    date = as.Date(observation_date),  # Convert observation_date to Date
    Unemployment_Rate = UNRATE         # Rename the column
  ) %>%
  dplyr::select(date, Unemployment_Rate) %>%  # Select relevant columns
  filter(!is.na(Unemployment_Rate))           # Remove missing values

# fetch and process disposible income data
dspi_data <- read.csv(dspi_url) %>%
  mutate(
    date = as.Date(observation_date),  # Convert observation_date to Date
    Disposable_Income = DSPI           # Rename the column
  ) %>%
  dplyr::select(date, Disposable_Income) %>%  # Select relevant columns
  filter(!is.na(Disposable_Income))           # Remove missing values

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
  list(
    cpi_data,
    ppi_data,
    ip_index_data,
    ffr_data,
    household_equity_monthly,
    cc_data %>% dplyr::select(date, Consumer_Confidence),    # Consumer Confidence
    unrate_data %>% dplyr::select(date, Unemployment_Rate),  # Unemployment Rate
    dspi_data %>% dplyr::select(date, Disposable_Income)     # Disposable Income
  ),
  full_join,
  by = "date"
) %>%
  # Filter by date range and sort by date
  filter(date >= as.Date("1990-01-01") & date <= as.Date("2024-11-01")) %>%
  arrange(date)

# Define dates for major volatility  events
volatility_events <- as.Date(c("1990-10-01", "1997-11-01", "1998-09-01", "2001-09-01", "2002-09-01",
                                 "2003-02-01", "2008-10-01", "2009-03-01", "2011-08-01", "2020-04-01"))

# Add the dummy variable to merged_data
merged_data <- merged_data %>%
  mutate(Volatility_Shock = ifelse(date %in% volatility_events, 1, 0))

# ensure log-transformed variables are interpolated for missing values
merged_data <- merged_data %>%
  mutate(
    log_CPI = log(CPI),
    log_PPI = ifelse(PPI > 0, log(PPI), NA),
    log_IP_Index = ifelse(IP_Index > 0, log(IP_Index), NA),
    log_Household_Equity = ifelse(Household_Equity > 0, log(Household_Equity), NA),
    log_Disposable_Income = ifelse(Disposable_Income > 0, log(Disposable_Income), NA),  # Log Disposable Income
    log_Consumer_Confidence = ifelse(Consumer_Confidence > 0, log(Consumer_Confidence), NA)  # Log Consumer Sentiment
  ) %>%
  # interpolate missing values for log-transformed series
  mutate(
    log_PPI = zoo::na.approx(log_PPI, na.rm = FALSE),
    log_IP_Index = zoo::na.approx(log_IP_Index, na.rm = FALSE),
    log_Household_Equity = zoo::na.approx(log_Household_Equity, na.rm = FALSE),
    log_Disposable_Income = zoo::na.approx(log_Disposable_Income, na.rm = FALSE),
    log_Consumer_Confidence = zoo::na.approx(log_Consumer_Confidence, na.rm = FALSE)
  ) %>%
  # Drop rows where interpolation wasn't possible
  filter(!is.na(log_CPI) & !is.na(log_PPI) & !is.na(log_IP_Index) & 
           !is.na(log_Household_Equity) & !is.na(log_Disposable_Income) & 
           !is.na(log_Consumer_Confidence))

# ensure log-transformed variables exist and then compute differences
merged_data <- merged_data %>%
  mutate(
    log_CPI = c(NA, diff(log_CPI)),                           # log-difference CPI
    log_PPI = c(NA, diff(log_PPI)),                           # log-difference PPI
    log_IP_Index = c(NA, diff(log_IP_Index)),                 # log-difference IP Index
    FFR = c(NA, diff(FFR)),                                   # difference FFR (already raw scale)
    log_Household_Equity = c(NA, diff(log_Household_Equity)), # log-difference Household Equity
    log_Disposable_Income = c(NA, diff(log_Disposable_Income)), # log-difference Disposable Income
    Unemployment_Rate = c(NA, diff(Unemployment_Rate))        # difference Unemployment Rate
  ) %>%
  # remove rows with NA values resulting from differencing
  filter(!is.na(log_CPI) & !is.na(log_PPI) & !is.na(log_IP_Index) & 
           !is.na(FFR) & !is.na(log_Household_Equity) & 
           !is.na(log_Disposable_Income) & !is.na(Unemployment_Rate))

# Verify that new variables are added correctly
print("Log-Differenced Variables Added:")
print(head(merged_data))


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

# Log transform adjusted prices and apply differencing
sp500_data <- sp500_data %>%
  mutate(
    log_price_adjusted = log(price_adjusted),          # Compute the natural log
    log_price_adjusted = c(NA, diff(log_price_adjusted))   # Log-difference SP500
  ) %>%
  # Remove rows with NA values introduced by differencing
  filter(!is.na(log_price_adjusted))

# aggregate S&P 500 data to monthly frequency
sp500_monthly <- sp500_data %>%
  mutate(month = floor_date(ref_date, "month")) %>% # group by month
  group_by(month) %>%
  summarise(
    log_price_adjusted = mean(log_price_adjusted, na.rm = TRUE) # monthly average
  ) %>%
  ungroup() %>%
  rename(date = month) # Rename 'month' to 'date'


# add the stock market variable to merged_data
merged_data <- merged_data %>%
  left_join(sp500_monthly, by = "date") # Merge by date


# debugging: check daily returns and realized variance
#summary(sp500_data$daily_return)
#summary(sp500_data$realized_variance)

# fetch VIX data for implied variance
vix_data <- yf_get(tickers = "^VIX", first_date = start_date, last_date = end_date) %>%
  arrange(ref_date) %>%
  mutate(VIX_squared = price_close^2) # calculate VIX_squared

# debugging: check VIX squared
#summary(vix_data$VIX_squared)

# align dates between VIX and S&P 500 data
vix_data <- vix_data %>%
  filter(ref_date %in% sp500_data$ref_date)

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



### Using fitted RVAR from a two variable regression ###

# merge datasets
financial_data <- merge(vix_data, sp500_data, by = "ref_date", suffixes = c("_vix", "_sp500"))

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
    Risk_Aversion = VIX_squared - Fitted_Uncertainty # compute risk aversion as residuals of regression
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

# plot normalized uncertainty with recession and volatility events overlayed
ggplot(financial_data, aes(x = ref_date, y = Norm_Winsorized_Uncertainty)) +
  geom_line(color = "blue", size = 1) + # plot normalized uncertainty
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.3, inherit.aes = FALSE) + # overlay recession periods
  geom_rect(data = volatility_events, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.6, inherit.aes = FALSE) + # overlay volatility events
  labs(
    title = "Normalized Winsorized Uncertainty",
    x = "Date",
    y = "Normalized Uncertainty"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# plot normalized risk aversion with recession and volatility events overlayed
ggplot(financial_data, aes(x = ref_date, y = Norm_Winsorized_Risk_Aversion)) +
  geom_line(color = "orange", size = 1) + # plot normalized risk aversion
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.3, inherit.aes = FALSE) + # overlay recession periods
  geom_rect(data = volatility_events, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.6, inherit.aes = FALSE) + # overlay volatility events
  labs(
    title = "Normalized Winsorized Risk Aversion",
    x = "Date",
    y = "Normalized Risk Aversion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# save results for further analysis (optional)
#write.csv(financial_data, "vix_decomposition_regression_based.csv", row.names = FALSE)

### Aggregate to Monthly ###

# isolate the variables of interest and remove rows with NAs
vol_data <- financial_data %>%
  dplyr::select(ref_date, Norm_Winsorized_Uncertainty, Norm_Winsorized_Risk_Aversion) %>%
  filter(!is.na(Norm_Winsorized_Uncertainty) & !is.na(Norm_Winsorized_Risk_Aversion))

# aggregate to monthly frequency
monthly_data <- vol_data %>%
  mutate(month = floor_date(ref_date, "month")) %>%
  group_by(month) %>%
  summarise(
    Monthly_Uncertainty = mean(Norm_Winsorized_Uncertainty, na.rm = TRUE),
    Monthly_Risk_Aversion = mean(Norm_Winsorized_Risk_Aversion, na.rm = TRUE)
  ) %>%
  ungroup()
  


# plot monthly uncertainty with recession and volatility events overlayed
ggplot(monthly_data, aes(x = month, y = Monthly_Uncertainty)) +
  geom_line(color = "blue", size = 1.2) + # plot monthly uncertainty
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.3, inherit.aes = FALSE) + # overlay recession periods
  geom_rect(data = volatility_events, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.6, inherit.aes = FALSE) + # overlay volatility events
  labs(
    title = "Monthly Uncertainty (UC) with Recession and Volatility Events",
    x = "Date",
    y = "Monthly Uncertainty"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5), # increase title size and center it
    axis.title.x = element_text(size = 18, face = "bold"),            # increase x-axis title size
    axis.title.y = element_text(size = 18, face = "bold"),            # increase y-axis title size
    axis.text = element_text(size = 14),                              # increase axis tick label size
    panel.grid = element_blank()                                      # remove gridlines for a cleaner look
  )

# plot monthly risk aversion with recession and volatility events overlayed
ggplot(monthly_data, aes(x = month, y = Monthly_Risk_Aversion)) +
  geom_line(color = "orange", size = 1.2) + # plot monthly risk aversion
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.3, inherit.aes = FALSE) + # overlay recession periods
  geom_rect(data = volatility_events, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.6, inherit.aes = FALSE) + # overlay volatility events
  labs(
    title = "Monthly Risk Aversion (RA) with Recession and Volatility Events",
    x = "Date",
    y = "Monthly Risk Aversion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5), # increase title size and center it
    axis.title.x = element_text(size = 18, face = "bold"),            # increase x-axis title size
    axis.title.y = element_text(size = 18, face = "bold"),            # increase y-axis title size
    axis.text = element_text(size = 14),                              # increase axis tick label size
    panel.grid = element_blank()                                      # remove gridlines for a cleaner look
  )

# Rename `month` to `date` in monthly_data if necessary
monthly_data <- monthly_data %>%
  rename(date = month)

# Add specified variables from merged_data to monthly_data
monthly_data <- monthly_data %>%
  left_join(
    merged_data %>% dplyr::select(
      date, 
      Volatility_Shock,
      log_IP_Index, 
      log_CPI, 
      log_PPI, 
      log_price_adjusted,
      FFR,
      log_Household_Equity,
      log_Disposable_Income,   
      log_Consumer_Confidence, 
      Unemployment_Rate        
    ), 
    by = "date"
  )

# Reorder columns in monthly_data
monthly_data <- monthly_data %>%
  dplyr::select(
    date,
    Volatility_Shock,
    log_IP_Index,
    Unemployment_Rate,
    log_Disposable_Income,
    log_Consumer_Confidence,
    log_CPI,
    log_PPI,
    Monthly_Uncertainty,      
    Monthly_Risk_Aversion,   
    log_price_adjusted,
    FFR,
    log_Household_Equity
  )

# truncate the 'monthly_data' data-frame to start from December 1, 1990
monthly_data <- monthly_data %>%
  filter(date >= as.Date("1990-12-01"))

# Stationarity Check Using Augmented Dickey-Fuller Test
check_stationarity <- function(data_frame) {
  stationarity_results <- data.frame(Variable = character(), P_Value = numeric(), Stationary = character())
  
  for (col in colnames(data_frame)) {
    if (col != "date") {  # Exclude the date column
      adf_result <- adf.test(data_frame[[col]], k = 0)  # ADF test with no lags
      stationary <- ifelse(adf_result$p.value <= 0.05, "Yes", "No")
      
      stationarity_results <- rbind(stationarity_results, data.frame(
        Variable = col,
        P_Value = round(adf_result$p.value, 4),
        Stationary = stationary
      ))
    }
  }
  
  return(stationarity_results)
}

# Run stationarity check on monthly_data
stationarity_results <- check_stationarity(monthly_data)

# Print stationarity results
print("Stationarity Check Results (ADF Test):")
print(stationarity_results)

# Optional: Identify non-stationary variables
non_stationary <- stationarity_results %>% filter(Stationary == "No")
if (nrow(non_stationary) > 0) {
  cat("Non-Stationary Variables Detected:\n")
  print(non_stationary$Variable)
} else {
  cat("All variables are stationary.\n")
}

# Reshape monthly_data to long format for ggplot
monthly_data_long <- monthly_data %>%
  pivot_longer(
    cols = -date, # select all columns except "date"
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    # rename variables for better clarity in the plot
    Variable = recode(
      Variable,
      "Volatility_Shock" = "Volatility Shock",
      "log_IP_Index" = "Log-transformed IP Index",
      "Unemployment_Rate" = "Unemployment Rate",
      "log_Disposable_Income" = "log-transformed Disposable Income",
      "log_Consumer_Confidence" = "log-transformed Consumer Confidence",
      "log_CPI" = "Log-transformed CPI",
      "log_PPI" = "Log-transformed PPI",
      "Monthly_Uncertainty" = "Monthly Uncertainty",
      "Monthly_Risk_Aversion" = "Monthly Risk Aversion",
      "log_price_adjusted" = "Log-transformed Adjusted Price",
      "FFR" = "Federal Funds Rate (FFR)",
      "log_Household_Equity" = "Log-transformed Household Equity"
    )
  )

# Create the ggplot figure
ggplot(monthly_data_long, aes(x = date, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) + # create subplots with free y-scales
  theme_minimal() +
  labs(
    title = "Log-Differenced Variables",
    x = "Date",
    y = "Value"
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


# Filter the data for FFR and Log-transformed Household Equity
filtered_data <- monthly_data_long %>%
  filter(Variable %in% c("Federal Funds Rate (FFR)", "Log-transformed Household Equity"))

# Create the plot
ggplot(filtered_data, aes(x = date, y = Value, color = Variable)) +
  geom_line(size = 1.2) +
  labs(
    title = "Federal Funds Rate (FFR) and Log-transformed Household Equity Over Time",
    x = "Date",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = c(0, 0)
  )



# Perform lag order selection for the VAR system using monthly_data
lag_selection <- VARselect(as.matrix(monthly_data[, -1]), lag.max = 12, type = "const")

# Display the optimal lags for each criterion
cat("Optimal lag order based on AIC:", lag_selection$selection["AIC(n)"], "\n")
cat("Optimal lag order based on SIC:", lag_selection$selection["SC(n)"], "\n")
cat("Optimal lag order based on HQC:", lag_selection$selection["HQ(n)"], "\n")



