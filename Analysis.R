# Clear variables from workspace
rm(list=ls())

# Clear console output
cat("\014")

# Set working directory
setwd("/Users/aaronfeldman/Desktop/Thesis WD")

# Install required packages
install.packages("dplyr")
install.packages("readxl")
library(devtools)
library(BVAR)
library(bvartools)
library(readxl)
library(dplyr)

# Load the household balance sheet data and isolate key variables 
householdBS <- read.csv("/Users/aaronfeldman/Desktop/Thesis WD/Houshold_Balance_Sheet.csv")
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

# Load the AAII Investor sentiment data
sentiment_data <- read_excel("sentiment_clean.xlsx", sheet="Sentiment_clean", skip=2)
sentiment_data <- sentiment_data %>%
  rename(
    Reported_Date = `...1`,
    Bullish = `...2`,
    Neutral = `...3`,
    Bearish = `...4`,
    Total = `...5`,
    Bullish_8_week_MA = `Bullish`,
    Bull_Bear_Spread = `...7`
  )

