# reachingforyieldVAR

This project aims to examine the 'Reaching for Yield' hypothesis using time series econometric methods. 

The specific question we intend to examine follows: Does the ‘reaching for yield’ hypothesis hold in observed data, such that decreases in interest rates, as measured by the 3-Month Treasury Yield, lead households and nonprofits serving households to increase investments in risky assets, with individual investor sentiment and consumer confidence serving as an indicator of this risk-taking behavior?

User Guide:

1. Run 'data_setup.R': loads data, isolates key variables, plots variables, tests for stationarity, transforms series, plots transformed series, selects lag length, and applies order for      Cholesky decomposition.
  
2. Run 'Analysis.R' to fit the BVAR model using the BVAR package in R (Kuschnig, N., & Vashold, L., 2024), calculate IRF, plot the responses of variables to a negative shock in the 3-month treasury yield, report the IRF results for horizons 0 -5 in the console, calculates Forecast Error Variance decomposition and produces summary tables for Horizons 0 - 5.

3. There is a separate section of 'Analysis.R' that performs an iterative model configuration and hyperparameter tuning that assists in specifying the model. 

Data sources:

1. Board of Governors of the Federal Reserve System (US), Federal Funds Effective Rate [DFF], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DFF, October 29, 2024.

2. University of Michigan, University of Michigan: Inflation Expectation [MICH], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/MICH, October 29, 2024.

3. Consumer Confidence Index: https://www.oecd.org/en/data/indicators/consumer-confidence-index-cci.html

4. Household Balance Sheet data: https://www.federalreserve.gov/releases/z1/dataviz/z1/balance_sheet/chart/

5. Investor sentiment: Investors, A. A. of I. (n.d.). Sentiment survey historical data. AAII. https://www.aaii.com/sentimentsurvey/sent_results 

6. S&P 500 Index: Investors, A. A. of I. (n.d.). Sentiment survey historical data. AAII. https://www.aaii.com/sentimentsurvey/sent_results

7. Board of Governors of the Federal Reserve System (US), Households and Nonprofit Organizations; Directly and Indirectly Held Corporate Equities as a Percentage of Financial Assets; Assets, Level [BOGZ1FL153064486Q], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/BOGZ1FL153064486Q, November 9, 2024.

8. Board of Governors of the Federal Reserve System (US), 3-Month Treasury Bill Secondary Market Rate, Discount Basis [TB3MS], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/TB3MS, November 9, 2024.

9. U.S. Bureau of Economic Analysis, Real Disposable Personal Income [DSPIC96], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DSPIC96, November 9, 2024.
