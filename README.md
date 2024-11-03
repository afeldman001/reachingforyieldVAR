# reachingforyieldVAR

This project aims to examine the 'Reaching for Yield' hypothesis using time series econometric methods. 

The specific question we intend to examine follows: Does the ‘reaching for yield’ hypothesis hold in observed data, such that prolonged periods of low interest rates, as measured by the Federal Funds Effective Rate, lead households and nonprofits serving households to increase investments in risky assets, with individual investor sentiment and consumer confidence serving as an indicator of this risk-taking behavior?

User Guide^*:

1. Run 'data_setup.R': loads data, isolates key variables, plots variables, tests for stationarity, transforms series, plots transformed series, selects lag length, and applies order for      Cholesky decomposition.
  
2. Run 'Analysis.R' to fit the BVAR model, calculate IRF, and plot the responses of variables to a negative shock in the federal funds rate.

* Note: the scripts should be run in this order to avoid conflicts when installing packages. 

Data sources:

1. Board of Governors of the Federal Reserve System (US), Federal Funds Effective Rate [DFF], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DFF, October 29, 2024.

2. University of Michigan, University of Michigan: Inflation Expectation [MICH], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/MICH, October 29, 2024.

3. Consumer Confidence Index: https://www.oecd.org/en/data/indicators/consumer-confidence-index-cci.html

4. Household Balance Sheet data: https://www.federalreserve.gov/releases/z1/dataviz/z1/balance_sheet/chart/

5. Investor sentiment: Investors, A. A. of I. (n.d.). Sentiment survey historical data. AAII. https://www.aaii.com/sentimentsurvey/sent_results 

6. S&P 500 Index: Investors, A. A. of I. (n.d.). Sentiment survey historical data. AAII. https://www.aaii.com/sentimentsurvey/sent_results 
