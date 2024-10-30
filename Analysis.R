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
library(devtools)
library(BVAR)
library(bvartools)
library(readxl)
library(dplyr)
library(lubridate)

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

# Load the Federal Funds Effective Rate data
ffr <- read.csv("https://raw.githubusercontent.com/afeldman001/reachingforyieldVAR/refs/heads/main/DFF%20(2).csv")

# Isolate risky assets minus total deposits as variable 'ra' from the household balance sheet data
householdBS_data <- householdBS_data %>%
  mutate(ra = FL154090005.Q - FL154000025.Q) # ra = risky assets (total financial assets - total deposits)
ra <- householdBS_data %>%
  select(date, ra)

# Isolate 'Bull-Bear Spread' as 'sent' variable from sentiment_clean
sentiment_data$Reported_Date <- as.Date(sentiment_data$Reported_Date, format = "%m-%d-%y")     # convert Reported_Date to Date format
sentiment_data$Bull_Bear_Spread <- as.numeric(gsub("%", "", sentiment_data$Bull_Bear_Spread))  # convert Bull_Bear_Spread to numeric format
sentiment <- sentiment_data %>%
  select(Reported_Date, Bull_Bear_Spread)
sentiment_q <- sentiment %>%              # aggregate to quarterly frequency from weekly                                                         
  mutate(Quarter = floor_date(Reported_Date, "quarter")) %>%
  group_by(Quarter) %>%
  summarize(Sentiment_Quarterly = mean(Bull_Bear_Spread, na.rm = TRUE))

# Aggregate CCI data from monthly to quarterly
cci <- cci %>%
  mutate(Quarter=floor_date(Date, "quarter")) %>%
  group_by(Quarter) %>%
  summarize(cci_q=mean(CCI, na.rm=TRUE)) # quarterly cci index values are the mean of monthly index for each quarter

