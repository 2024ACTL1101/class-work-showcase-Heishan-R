
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
#Create a new column to represent the daily return for AMD
df$AMD_Daily_Return <- (df$AMD - lag(df$AMD)) / lag(df$AMD)

#Create a new column to represent the daily return for S&P 500
df$GSPC_Daily_Return <- (df$GSPC - lag(df$GSPC)) / lag(df$GSPC)
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
#Create a new column to represent the daily risk free rate by using the formula
df$Daily_RF <- (1 + df$RF / 100)^(1/360) - 1
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
#Create a new column to represent the excess return for AMD, by subtracting the daily risk-free rate from their respective returns.
df$AMD_Excess_Return <- df$AMD_Daily_Return - df$Daily_RF

#Create a new column to represent the excess return for S&P 500, by subtracting the daily risk-free rate from their respective returns.
df$GSPC_Excess_Return <- df$GSPC_Daily_Return - df$Daily_RF
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
#Performed linear regression using AMD's excess return and S&P 500's excess return
capm_model <- lm(AMD_Excess_Return ~ GSPC_Excess_Return, data = df)

#Summary of the linear regression 
summary(capm_model)

#Getting the beta value from the linear regression, as the GSPC_Excess_Return coefficient represents beta
beta <- coef(capm_model) ["GSPC_Excess_Return"]

#Printing the beta value
cat("Beta value:", beta, "\n")
```
#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The Beta value is given by 1.57 (2dp). Since beta measures the sensitivity of the stock's returns to fluctuations in the market, we can determine that AMD is more volatile than the market. The beta value is higher than 1 and implies that AMD's stock price will move more significantly than the market in response to market changes. Specifically in relation to the 1.57 beta value, if the market goes up or down by 1%, AMD's stock price would go up or down by 1.57%, which shows that AMD is more volatile than the market. 

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
#Plotting the scatter plot and CAPM regression line
ggplot(df, aes(x = GSPC_Excess_Return, y = AMD_Excess_Return)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("CAPM Analysis: AMD vs S&P 500 Excess Returns") +
  xlab("S&P 500 Excess Return") +
  ylab("AMD Excess Return")
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
#Defining the given risk-free rate and expected return for the S&P 500
risk_free_rate <- 0.05
expected_market_return <- 0.133

#Calculate the daily standard error from the CAPM model (Residual standard error)
daily_standard_error <- summary(capm_model)$sigma

#Calculate the annual standard error for prediction
annual_standard_error <- daily_standard_error * sqrt(252)

#The expected AMD return with regards to the CAPM model
expected_amd_return <- risk_free_rate + beta * (expected_market_return - risk_free_rate)

#Define the prediction interval and use t-distribution to calculate the margin of error which falls between the 90% prediction interval
prediction <- 0.10
t_value <- qt(1 - prediction / 2, df = nrow(df) - 2)
margin_error <- t_value * annual_standard_error

#Calculate the lower and upper bounds for the 90% prediction interval
lower_bound <- expected_amd_return - margin_error
upper_bound <- expected_amd_return + margin_error

#Print out the Expected AMD return and the Lower and Upper bound for the 90% prediction interval
cat("Expected AMD Return:", expected_amd_return, "\n")
cat("Lower bound for prediction interval:", lower_bound, "\n")
cat("Upper bound for prediction interval:", upper_bound, "\n")
```
#### Interpretation
The expected return of 0.18 suggests that AMD is expected to perform well in the market. However, the prediction interval, ranging from -0.49 to 0.85, reflects AMD's high volatility, as seen by the beta value of 1.57. This value indicates that AMD's returns are highly sensitive to market movements and as a result, there is a broad range of potential outcomes for returns. For investors, purchasing AMD stock comes with a high level of risk, making it more suitable for risk loving investors.
