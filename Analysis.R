# Working script for Seminar thesis: 
# Interest Rate Cuts and Household Investment in Risky Assets: An empirical examination of the ‘Reaching for Yield’ Hypothesis
# by Aaron Feldman and Chirag Khanna
# University of Tuebingen, WS 2024-2025
# Contact: 
# aaron.feldman@student.uni-tuebingen.de
# chirag.khanna@student.uni-tuebingen.de


# Clear variables from workspace
rm(list=ls())

# Clear console output
cat("\014")

# Install required packages
install.packages("lubridate")
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("purrr")
library(devtools)
library(BVAR)
library(bvartools)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)



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
data <- data %>%
  mutate(ffr=as.numeric(ffr))

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

