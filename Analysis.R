# Working script for Seminar thesis: 
# Interest Rate Cuts and Household Investment in Risky Assets: An empirical examination of the ‘Reaching for Yield’ Hypothesis
# by Aaron Feldman and Chirag Khanna
# University of Tuebingen, WS 2024-2025
# Contact: 
# aaron.feldman@student.uni-tuebingen.de
# chirag.khanna@student.uni-tuebingen.de

# Define and install/load all required packages
packages <- c("urca", "tseries", "vars", "lubridate", "dplyr", 
              "readxl", "ggplot2", "tidyr", "purrr", "devtools", 
              "BVAR", "bvartools")

# Install any packages that aren't already installed
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})
# Clear variables from workspace and console output
rm(list=ls())
cat("\014")


# Load household balance sheet data and isolate key variables 
householdBS <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/Houshold_Balance_Sheet.csv")
colnames(householdBS)
householdBS_data <- householdBS %>% # 
  select(
    date,
    FL152090005.Q,  # Total Net Worth
    FL154090005.Q,  # Total Financial Assets
    LM153064105.Q,  # Directly Held Corporate Equities
    LM153064175.Q,  # Indirectly Held Corporate Equities
    LM154022005.Q,  # Directly Held Debt Securities
    LM154022075.Q,  # Indirectly Held Debt Securities
    LM155035015.Q,  # Real Estate Holdings
    FL154000025.Q,  # Total Deposits
    FA156012005.Q   # Disposable Personal Income
  )
householdBS_data <- householdBS_data %>%
  mutate(
    date = as.Date(ifelse(grepl(":Q1", date), paste0(sub(":Q1", "-01-01", date)),
                   ifelse(grepl(":Q2", date), paste0(sub(":Q2", "-04-01", date)),
                   ifelse(grepl(":Q3", date), paste0(sub(":Q3", "-07-01", date)),
                   paste0(sub(":Q4", "-10-01", date))))))
  )

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

# Load the Federal Funds Effective Rate data
ffr <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/DFF%20(2).csv")

# Isolate risky assets minus total deposits as variable 'ra' from the household balance sheet data
householdBS_data <- householdBS_data %>%
  mutate(ra = FL154090005.Q - FL154000025.Q) # ra = risky assets (total financial assets - total deposits)
ra <- householdBS_data %>%
  select(date, ra) %>%
  arrange(date)

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
colnames(ra) <- c("Quarter", "risk_assets")
colnames(sentiment_q) <- c("Quarter", "sentiment")
colnames(p_exp) <- c("Quarter", "p_exp")
colnames(market) <- c("Quarter", "market")
colnames(cci) <- c("Quarter", "cci")
colnames(ffr) <- c("Quarter", "ffr")

# Convert `Quarter` to Date in each data frame
ra$Quarter <- as.Date(ra$Quarter)
sentiment_q$Quarter <- as.Date(sentiment_q$Quarter)
p_exp$Quarter <- as.Date(p_exp$Quarter)
market$Quarter <- as.Date(market$Quarter)
cci$Quarter <- as.Date(cci$Quarter)
ffr$Quarter <- as.Date(ffr$Quarter)

# Combine data into data frame, joining on "Quarter"
data <- reduce(
  list(ra, sentiment_q, p_exp, market, cci, ffr),
  full_join,
  by="Quarter"
)

# Filter data for matching date range (Q2 1987 - Q3 2023)
data <- data[143:288, ]
data$ffr <- as.numeric(unlist(data$ffr))
data$market <- as.numeric(unlist(data$market))
data$risk_assets <- as.numeric(unlist(data$risk_assets))
data$cci <- as.numeric(unlist(data$cci))
data$sentiment <- as.numeric(unlist(data$sentiment))
data$p_exp <- as.numeric(unlist(data$p_exp))

# Visualize data
ggplot(data, aes(x=Quarter)) +
  geom_line(aes(y=risk_assets), color="blue") +
  labs(title="Risky Assets", x="Quarter", y="Risky Assets (millions $)")

ggplot(data, aes(x=Quarter)) +
  geom_line(aes(y=ffr), color="black") +
  labs(title="Federal Funds Effective Rate", x="Quarter", y="DFF (%)")

ggplot(data, aes(x=Quarter)) +
  geom_line(aes(y=sentiment), color="green") +
  labs(title="Investor Sentiment", x="Quarter", y="Bull-Bear Spread (%)")

ggplot(data, aes(x=Quarter)) +
  geom_line(aes(y=p_exp), color="red") +
  labs(title="Inflation Expectations", x="Quarter", y="Expectations (%)")

ggplot(data, aes(x=Quarter)) +
  geom_line(aes(y=cci), color="orange") +
  labs(title="Consumer Confidence Index", x="Quarter", y="CCI")

ggplot(data, aes(x=Quarter)) +
  geom_line(aes(y=market), color="purple") +
  labs(title="S&P 500 Index", x="Quarter", y="Index ($)")

# Transform data for stationarity
data_adj <- data %>%
  transmute(
    Quarter, # keep the date column for time alignment
  # log-difference the risky assets and s&p 500 series
    ra_diff=log(risk_assets)-lag(log(risk_assets),1),
    market_diff=log(market)-lag(log(market),1),
         # first order difference federal funds effective rate and consumer confidence
    ffr_diff=ffr-lag(ffr,1),
    cci_diff=cci-lag(cci,1),
    sentiment,
    p_exp
    )

data_adj <- data_adj %>%
  slice(-1)

# Visulaize data again
ggplot(data_adj, aes(x=Quarter)) +
  geom_line(aes(y=ra_diff), color="blue") +
  labs(title="Risky Assets", x="Quarter", y="Risky Assets (millions $)")

ggplot(data_adj, aes(x=Quarter)) +
  geom_line(aes(y=ffr_diff), color="black") +
  labs(title="Federal Funds Effective Rate", x="Quarter", y="DFF (%)")

ggplot(data_adj, aes(x=Quarter)) +
  geom_line(aes(y=sentiment), color="green") +
  labs(title="Investor Sentiment", x="Quarter", y="Bull-Bear Spread (%)")

ggplot(data_adj, aes(x=Quarter)) +
  geom_line(aes(y=p_exp), color="red") +
  labs(title="Inflation Expectations", x="Quarter", y="Expectations (%)")

ggplot(data_adj, aes(x=Quarter)) +
  geom_line(aes(y=cci_diff), color="orange") +
  labs(title="Consumer Confidence Index", x="Quarter", y="CCI")

ggplot(data_adj, aes(x=Quarter)) +
  geom_line(aes(y=market_diff), color="purple") +
  labs(title="S&P 500 Index", x="Quarter", y="Index ($)")

# Test for stationarity using ADF
adf_ra <- ur.df(data_adj$ra_diff, type="drift", selectlags="AIC")
adf_market <- ur.df(data_adj$market_diff, type="drift", selectlags="AIC")
adf_ffr <- ur.df(data_adj$ffr_diff, type = "drift", selectlags = "AIC")
adf_cci <- ur.df(data_adj$cci_diff, type = "drift", selectlags = "AIC")
adf_sentiment <- ur.df(data_adj$sentiment, type = "drift", selectlags = "AIC")
adf_p_exp <- ur.df(data_adj$p_exp, type = "drift", selectlags = "AIC")
  
# Display ADF test results
summary(adf_ra)            # result: p-value < 0.001 (2.2e-16)   -> stationary
summary(adf_market)        # result: p-value < 0.001 (3.668e-12) -> stationary
summary(adf_ffr)           # result: p-value < 0.001 (7.355e-06) -> stationary
summary(adf_cci)           # result: p-value < 0.001 (1.851e-12) -> stationary
summary(adf_sentiment)     # result: p-value < 0.001 (1.405e-08) -> stationary
summary(adf_p_exp)         # result: p-value < 0.001 (2.2e-16)   -> stationary

# Perform lag selection using BIC
data_adj_subset <- data_adj %>% select(-Quarter)
lag_selection <- VARselect(data_adj_subset, lag.max = 10, type = "const")
bic_lag <- lag_selection$selection["SC(n)"] # BIC (Schwarz Criterion)
print(bic_lag)

# Reorder variables for BVAR analysis. The ordering will have important theoretical implications
# if Cholesky is the chosen identification startegy.
data_adj_subset <- data_adj_subset %>%
  select(ffr_diff, p_exp , cci_diff, sentiment, market_diff, ra_diff)

# Define and fit the BVAR model with selected lag length
x <- bvar(data_adj_subset, lags = 1, n_draw = 10000L, n_burn = 2000L, verbose = TRUE)

# Calculate impulse responses
irf(x) <- irf(x, horizon = 10, fevd = FALSE)

# Extract the results from the BVAR model object
irf_results <- irf(x)$irf

irf_ffr <- irf_results[, , , 1] # Extract the responses to an 'ffr_diff' shock

# Extract the number of draws, variables, and horizons for indexing
num_draws <- dim(irf_ffr)[1]
num_vars <- dim(irf_ffr)[2]
num_horizons <- dim(irf_ffr)[3]

# Create a list to store each variable's IRF data
irf_ffr_list <- lapply(1:num_vars, function(i) {
  data.frame(
    Horizon = 0:(num_horizons - 1),
    Variable = paste("Variable", i),  
    Response = -apply(irf_ffr[, i, ], 2, mean),  # Average over draws and invert for negative shock
    Lower = -apply(irf_ffr[, i, ], 2, quantile, 0.16),  # Adjust lower bound
    Upper = -apply(irf_ffr[, i, ], 2, quantile, 0.84)   # Adjust upper bound
  )
})

# Bind all variable responses into a single data frame for plotting
irf_ffr_long <- do.call("rbind", irf_ffr_list)

# Plot using ggplot2
ggplot(irf_ffr_long, aes(x = Horizon, y = Response)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.3) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Impulse Response to a Negative Shock in Federal Funds Rate (ffr_diff)", 
       x = "Horizon (Quarters)", 
       y = "Response") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold")
  )

