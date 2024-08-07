
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```

##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  #Setting the current share price from the data in the variable current_share_price
  current_share_price <- amd_df$close[i]
  
  if (previous_price == 0) {
    #Check if it is the first day and then buy 100 shares
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_share_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] = accumulated_shares
  } else if (i == nrow(amd_df)) {
    #Check if it is the last row, and if so, sell all the shares
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- current_share_price * accumulated_shares
    accumulated_shares <- 0
    amd_df$accumulated_shares[i] = accumulated_shares
  } else if (current_share_price < previous_price) {
    #Check if the current share price is less than the previous share price and if so, buy 100 shares
    amd_df$trade_type[i] <- "buy"
    amd_df$costs_proceeds[i] <- -current_share_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] = accumulated_shares
  } 
  
  #Set the previous share price as the current share price for the next increment of the loop
  previous_price <- current_share_price
}
```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
#Defining the trading period
start_date <- as.Date('2021-11-22')
end_date <- as.Date('2022-11-21')

# Initialise the trading period using the start and end dates.
amd_altered_df <- subset(amd_df, date >= start_date & date <= end_date)

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_altered_df$trade_type <- NA
amd_altered_df$costs_proceeds <- NA  # Corrected column name
amd_altered_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_altered_df)) {
  #Setting the current share price from the data in the variable current_share_price
  current_share_price <- amd_altered_df$close[i]
  
  if (previous_price == 0) {
    #Check if it is the first day and then buy 100 shares
    amd_altered_df$trade_type[i] <- "buy"
    amd_altered_df$costs_proceeds[i] <- -current_share_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_altered_df$accumulated_shares[i] = accumulated_shares
  } else if (i == nrow(amd_altered_df)) {
    #Check if it is the last row, and if so, sell all the shares
    amd_altered_df$trade_type[i] <- "sell"
    amd_altered_df$costs_proceeds[i] <- current_share_price * accumulated_shares
    accumulated_shares <- 0
    amd_altered_df$accumulated_shares[i] = accumulated_shares
  } else if (current_share_price < previous_price) {
    #Check if the current share price is less than the previous share price and if so, buy 100 shares
    amd_altered_df$trade_type[i] <- "buy"
    amd_altered_df$costs_proceeds[i] <- -current_share_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_altered_df$accumulated_shares[i] = accumulated_shares
  } 
  
  #Set the previous share price as the current share price for the next increment of the loop
  previous_price <- current_share_price
}
```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
#Define the profit_or_loss variable and invested_capital variable to 0
profit_or_loss <- 0
invested_capital <- 0

#Loop through the amd_df from the first row to the last row.
for (i in 1:nrow(amd_altered_df)) {
  #Define the current_cost variable to the respected cost proceeds of the row
  current_costs <- amd_altered_df$costs_proceeds[i]
  
  #Check whether the current_cost is not NA
  if (is.na(current_costs) == FALSE) {
    #Accumulating the sum of the profit_or_loss by adding the current_cost
    profit_or_loss <- profit_or_loss + current_costs
  }
  
  #Define the current_trade_type variable to the respected trade_type of the row
  current_trade_type <- amd_altered_df$trade_type[i]
  
  #Check whether the current_trade_type is not NA and the current_trade_type is "buy"
  if (is.na(current_trade_type) == FALSE && current_trade_type == "buy") {
    invested_capital <- invested_capital - amd_altered_df$costs_proceeds[i]
  }
}

#Calculate the Return on Investment (ROI) by dividing profit_or_loss by invested_capital, then multiplying that value by 100
ROI <- (profit_or_loss / invested_capital) * 100
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option 2}
#Defining the trading period
start_date <- as.Date('2021-11-22')
end_date <- as.Date('2022-11-21')

# Initialise the trading period using the start and end dates.
amd_strategy_df <- subset(amd_df, date >= start_date & date <= end_date)

# Initialise columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_strategy_df$trade_type <- NA
amd_strategy_df$costs_proceeds <- NA  # Corrected column name
amd_strategy_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialise variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
average_purchase_price <- 0

for (i in 1:nrow(amd_strategy_df)) {
  #Setting the current share price from the data in the variable current_share_price
  current_share_price <- amd_strategy_df$close[i]
  
  if (previous_price == 0) {
    #Check if it is the first day and then buy 100 shares
    amd_strategy_df$trade_type[i] <- "buy"
    amd_strategy_df$costs_proceeds[i] <- -current_share_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_strategy_df$accumulated_shares[i] = accumulated_shares
    
    #Set the average purchase price to the current share price
    average_purchase_price <- current_share_price
  } else if (i == nrow(amd_strategy_df)) {
    #Check if it is the last row, and if so, sell all the shares
    amd_strategy_df$trade_type[i] <- "sell"
    amd_strategy_df$costs_proceeds[i] <- current_share_price * accumulated_shares
    accumulated_shares <- 0
    amd_strategy_df$accumulated_shares[i] = accumulated_shares
  } else if (current_share_price < (0.8 * average_purchase_price)) {
    #Check if the current share price is 20% less than the average purchase price
    amd_strategy_df$trade_type[i] <- "sell"
    amd_strategy_df$costs_proceeds[i] <- current_share_price * (accumulated_shares / 2)
    accumulated_shares <- accumulated_shares / 2
    amd_strategy_df$accumulated_shares[i] <- accumulated_shares
  } else if (current_share_price < previous_price) {
    #Check if the current share price is less than the previous share price and if so, buy 100 shares
    amd_strategy_df$trade_type[i] <- "buy"
    amd_strategy_df$costs_proceeds[i] <- -current_share_price * share_size
    accumulated_shares <- accumulated_shares + share_size
    amd_strategy_df$accumulated_shares[i] <- accumulated_shares
    
    #Calculate the average purchase price by using the previous purchase and current share price, and the number of shares purchased
    average_purchase_price <- (average_purchase_price * (accumulated_shares - share_size) + (current_share_price * share_size)) / accumulated_shares
  }
  
  #Set the previous share price as the current share price for the next increment of the loop
  previous_price <- current_share_price
}

#Define the profit_or_loss variable and invested_capital variable to 0
profit_or_loss_2 <- 0
invested_capital_2 <- 0

#Loop through the amd_df from the first row to the last row.
for (i in 1:nrow(amd_strategy_df)) {
  #Define the current_cost variable to the respected cost proceeds of the row
  current_costs <- amd_strategy_df$costs_proceeds[i]
  
  #Check whether the current_cost is not NA
  if (is.na(current_costs) == FALSE) {
    #Accumulating the sum of the profit_or_loss by adding the current_cost
    profit_or_loss_2 <- profit_or_loss_2 + current_costs
  }
  
  #Define the current_trade_type variable to the respected trade_type of the row
  current_trade_type <- amd_strategy_df$trade_type[i]
  
  #Check whether the current_trade_type is not NA and the current_trade_type is "buy"
  if (is.na(current_trade_type) == FALSE && current_trade_type == "buy") {
    invested_capital_2 <- invested_capital_2 - amd_strategy_df$costs_proceeds[i]
  }
}

#Calculate the Return on Investment (ROI) by dividing profit_or_loss by invested_capital, then multiplying that value by 100
ROI_2 <- (profit_or_loss_2 / invested_capital_2) * 100
```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
print(paste("Profit/Loss before the strategy is $", profit_or_loss))
print(paste("The Return on Investment before the strategy is", ROI, "%"))

print(paste("Profit/Loss aftern the strategy is $", profit_or_loss_2))
print(paste("The Return on Investment after the strategy is", ROI_2, "%"))
```

The initial Profit/Loss over the chosen period, before the strategy was applied, was -$357,845.01 (nearest cent). This hence produced a ROI of -26.639% (3dp) which can be reflected through the performance of the AMD stock during the chosen trading period. Since shares were bought based off slight dips in the share price, with no consideration of significant price drops, there was an aggressive accumulation on shares during downward trends of the AMD stock. Once the stop-loss mechanism was implemented in the trading strategy the Profit/Loss over the chosen period was -$256,269.08 (nearest cent) and the ROI was -22.054% (3dp). The stop-loss mechanism had slightly improved the Profit/Loss by $101,575.93 and ROI by 4.585%. This suggests that although the adapted strategy aimed to mitigate potential losses by selling half of the holdings when the price dropped significantly, it was not effective enough to completely alleviate the losses. 

```{r}
plot(amd_strategy_df$date, amd_strategy_df$close,'l')

```
In general, the chosen trading period shows a large drop in the stock price and this can be seen throughout the whole industry with companies such as NVIDIA and Intel also experienced a large drop in their stock prices. Covid restriction were mostly lifted at the end of 2021 for most countries, the demand for PC parts for home use declined and with high prices of AMD's CPU and GPU parts sales went down and the stock price with it. Furthermore, another major driver for AMD's downward stock price was their acquisition of Xilinx on 14th February 2022, which had cost almost $50 billion. This acquisition was good for AMD, however, the extra shares provided for the Xilinx deal slumped the share price and further explains the declining trend of the trading period. 
