# Data setup for Seminar thesis: 
# Interest Rate Cuts and Household Investment in Risky Assets: An empirical examination of the ‘Reaching for Yield’ Hypothesis
# by Aaron Feldman and Chirag Khanna
# University of Tuebingen, WS 2024-2025
# Contact: 
# aaron.feldman@student.uni-tuebingen.de
# chirag.khanna@student.uni-tuebingen.de

# Clear variables from workspace and console output
rm(list=ls())
cat("\014")

# Install each package individually from CRAN (except BVAR)
install.packages("dplyr", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("lubridate", dependencies = TRUE)
install.packages("urca", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("vars", dependencies = TRUE)
install.packages("purrr", dependencies = TRUE)

# Load main libraries
library(urca)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)
library(vars)
library(dplyr)

# Load GDP data
y <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/GDP%20(1).csv")

# Load unemployment rate
u <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/UNRATE.csv")

# Load inflation rate
p <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/CPIAUCSL.csv")

# Load household balance sheet data and isolate key variables 
householdRA <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/Household_equity_ownership.csv")

# Load and update AAII Investor sentiment data
sentiment_data <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/sentiment_clean.csv",
header = FALSE,
col.names = c("Reported_Date", "Bullish", "Neutral", "Bearish", "Total", "Bullish_8_week_MA", "Bull_Bear_Spread"),
skip=3
)

# Load Consumer Confidence Index data
cci <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/CCI%20(1).csv", 
header=FALSE, 
col.names=c("Date", "CCI"),
skip=3
)
cci$Date <- as.Date(cci$Date, format="%Y-%m-%d")

# Load inflation expectations data
p_exp <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/MICH%20(1).csv")

# Load stock market data
market <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/sp500.csv",header=FALSE, 
col.names=c("Date","Weekly_High","Weekly_Low","Weekly_Close"), skip=3)
market$Date <- as.Date(market$Date, format="%m-%d-%y")
market$Weekly_Close <- gsub("[^0-9.]", "", market$Weekly_Close)
market$Weekly_Close <- as.numeric(market$Weekly_Close)
market <- market %>%
  select(Date, Weekly_Close)

# Load the 3-month rate Rate data
r <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/TB3MS.csv")

# Isolate risky assets 
ra <- householdRA %>%
  mutate(Quarter = as.Date(DATE, format="%Y-%m-%d"), 
  risk_assets = BOGZ1FL153064486Q_CHG
  ) %>%
  select(Quarter, risk_assets) %>%
  na.omit()

# Isolate 'Bull-Bear Spread' as 'sent' variable from sentiment_clean
sentiment_data$Reported_Date <- as.Date(sentiment_data$Reported_Date, format = "%m-%d-%y")     # convert Reported_Date to Date format
sentiment_data$Bull_Bear_Spread <- as.numeric(gsub("%", "", sentiment_data$Bull_Bear_Spread))  # convert Bull_Bear_Spread to numeric format
sentiment <- sentiment_data %>%
  select(Reported_Date, Bull_Bear_Spread)

# Aggregate sentiment to quarters from weekly
sentiment_q <- sentiment %>%              
  mutate(Quarter = floor_date(Reported_Date, "quarter")) %>%
  group_by(Quarter) %>%
  summarize(Sentiment_Quarterly = mean(Bull_Bear_Spread, na.rm = TRUE))

# Aggregate CCI data from monthly to quarterly
cci <- cci %>%
  mutate(Quarter=floor_date(Date, "quarter")) %>%
  group_by(Quarter) %>%
  summarize(cci_q=mean(CCI, na.rm=TRUE)) # quarterly cci index values are the mean of monthly index for each quarter

# Aggregate market data into quarterly from weekly
market <- market %>%
  mutate(Quarter=floor_date(Date, "quarter")) %>%
  group_by(Quarter) %>%
  summarize(market_q=mean(Weekly_Close, na.rm=TRUE))

# Rename columns
colnames(y) <- c("Quarter", "GDP")
colnames(u) <- c("Quarter", "u")
colnames(p) <- c("Quarter", "p")
colnames(ra) <- c("Quarter", "risk_assets")
colnames(sentiment_q) <- c("Quarter", "sentiment")
colnames(p_exp) <- c("Quarter", "p_exp")
colnames(market) <- c("Quarter", "market")
colnames(cci) <- c("Quarter", "cci")
colnames(r) <- c("Quarter", "r")

# Convert `Quarter` to Date in each data frame
y$Quarter <- as.Date(y$Quarter)
u$Quarter <- as.Date(u$Quarter)
p$Quarter <- as.Date(p$Quarter)
ra$Quarter <- as.Date(ra$Quarter)
sentiment_q$Quarter <- as.Date(sentiment_q$Quarter)
p_exp$Quarter <- as.Date(p_exp$Quarter)
market$Quarter <- as.Date(market$Quarter)
cci$Quarter <- as.Date(cci$Quarter)
r$Quarter <- as.Date(r$Quarter)

# Filter each data frame using the common time period
quarters <- data.frame(Quarter = seq.Date(from = as.Date("1987-07-01"), to = as.Date("2024-04-01"), by = "quarter"))

# Left join each data frame with the consistent quarter sequence to align all dates
y <- left_join(quarters, y, by = "Quarter")
u <- left_join(quarters, u, by = "Quarter")
p <- left_join(quarters, p, by = "Quarter")
ra <- left_join(quarters, ra, by = "Quarter")
sentiment_q <- left_join(quarters, sentiment_q, by = "Quarter")
p_exp <- left_join(quarters, p_exp, by = "Quarter")
market <- left_join(quarters, market, by = "Quarter")
cci <- left_join(quarters, cci, by = "Quarter")
r <- left_join(quarters, r, by = "Quarter")

# Combine data into data frame, joining on "Quarter"
data <- reduce(
  list(y, u, p, ra, sentiment_q, p_exp, market, cci, r),
  full_join,
  by="Quarter"
)

# Convert character columns to numeric, handling any NA values that might be present
data <- data %>%
  mutate(
    across(c(GDP, u, p, risk_assets, r, sentiment, p_exp, cci, market), ~ as.numeric(.))
  )

# Check correlation matrix to identify highly correlated variables
cor_matrix <- cor(data %>% select(-Quarter), use = "complete.obs")
print("Correlation Matrix")
print(cor_matrix)

# Visualize data
# Convert data to long format for plotting multiple variables in one plot
# Plot all variables in subplots
data_long <- data %>%
  pivot_longer(
    cols = c(GDP, u, p, risk_assets, r, sentiment, p_exp, cci, market),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(data_long, aes(x = Quarter, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Time Series of Key Variables",
       x = "Quarter",
       y = "Value") +
  theme_minimal()

# Transform data for stationarity
data_adj <- data %>%
  transmute(
    Quarter, # keep the date column for time alignment
    # log-difference s&p 500
    market_diff=log(market)-lag(log(market),1),
    cci_diff=cci-lag(cci,1),
    ra=data$risk_assets,
    y=data$GDP,
    u=data$u,
    p=data$p,
    r,
    sentiment,
    p_exp
    )

data_adj <- data_adj %>%
  slice(-1)

# Visulaize data again
# Convert data to long format for plotting multiple variables in one plot
# Plot all variables in subplots
data_long <- data_adj %>%
  pivot_longer(
    cols = c(y, u, p, ra, r, sentiment, p_exp, cci_diff, market_diff),
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(data_long, aes(x = Quarter, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Time Series of Key Variables",
       x = "Quarter",
       y = "Value") +
  theme_minimal()

# Test for stationarity using ADF
adf_y <- ur.df(data_adj$y, type="drift", selectlags="AIC")
adf_u <- ur.df(data_adj$u, type="drift", selectlags="AIC")
adf_p <- ur.df(data_adj$p, type="drift", selectlags="AIC")
adf_ra <- ur.df(data_adj$ra, type="drift", selectlags="AIC")
adf_market <- ur.df(data_adj$market_diff, type="drift", selectlags="AIC")
adf_r <- ur.df(data_adj$r, type = "drift", selectlags = "AIC")
adf_cci <- ur.df(data_adj$cci_diff, type = "drift", selectlags = "AIC")
adf_sentiment <- ur.df(data_adj$sentiment, type = "drift", selectlags = "AIC")
adf_p_exp <- ur.df(data_adj$p_exp, type = "drift", selectlags = "AIC")
  
# Display ADF test results
summary(adf_y)             # result: p-value < 0.001 (2.2e-16)   -> stationary
summary(adf_u)             # result: p-value < 0.001 (2.2e-16)   -> stationary
summary(adf_p)             # result: p-value < 0.001 (3.347e-11) -> stationary
summary(adf_ra)            # result: p-value < 0.001 (2.2e-16)   -> stationary
summary(adf_market)        # result: p-value < 0.001 (2.444e-12) -> stationary
summary(adf_r)             # result: p-value < 0.001 (4.603e-06) -> stationary
summary(adf_cci)           # result: p-value < 0.001 (1.164e-12) -> stationary
summary(adf_sentiment)     # result: p-value < 0.001 (1.22e-08)  -> stationary
summary(adf_p_exp)         # result: p-value < 0.001 (2.2e-16)   -> stationary

# Perform lag selection using BIC
data_adj_subset <- data_adj %>% select(-Quarter)
lag_selection <- VARselect(data_adj_subset, lag.max = 10, type = "const")
bic_lag <- lag_selection$selection["SC(n)"] # BIC (Schwarz Criterion)
print(bic_lag)

# Reorder variables for BVAR analysis. The ordering will have important theoretical implications
# if Cholesky is the chosen identification startegy.
data_adj_subset <- data_adj_subset %>%
  select(y, u, p, r, market_diff, sentiment, cci_diff, p_exp, ra)

# Prepare data with named columns
colnames(data_adj_subset) <- c("y", "u", "p", "r", "market_diff", "sentiment", "cci_diff", "p_exp", "ra")

# Ensure data_adj_subset has all predictor columns and excludes "ra" if it's the response
predictors <- setdiff(names(data_adj_subset), "ra")

# Calculate VIF manually for each predictor in data_adj_subset
vif_values <- sapply(predictors, function(predictor) {
  # Create a formula to predict the current predictor using the remaining predictors
  formula <- as.formula(paste(predictor, "~ ."))
  
  # Subset data to include the current predictor and all other predictors
  model_data <- data_adj_subset[, c(predictor, setdiff(predictors, predictor))]
  
  # Fit the linear model
  model <- lm(formula, data = model_data)
  
  # Extract R-squared
  r_squared <- summary(model)$r.squared
  
  # Calculate VIF
  vif_value <- 1 / (1 - r_squared)
  return(vif_value)
})

# Display VIF values
vif_values
