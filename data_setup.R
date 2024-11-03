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
# Convert data to long format for plotting multiple variables in one plot
# Plot all variables in subplots
data_long <- data %>%
  pivot_longer(
    cols = c(risk_assets, ffr, sentiment, p_exp, cci, market),
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
# Convert data to long format for plotting multiple variables in one plot
# Plot all variables in subplots
data_long <- data_adj %>%
  pivot_longer(
    cols = c(ra_diff, ffr_diff, sentiment, p_exp, cci_diff, market_diff),
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
