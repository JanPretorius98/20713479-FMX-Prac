# Financial Econometrics 871 Practical Test Repository

This repository contains the materials for the Financial Econometrics 871 practical test, including datasets, utility scripts for analysis, and documentation templates for questions and presentations.

##  Internal Note/To Do:
- Fix presentation notes of Question 1 (specifically the boxplots)

##  Note on question documentation:

Documentation for questions will follow this general format:
- Questions guiding analysis
- Code
  - Data Operations
  - Plotting Code
  - Any additional code
- Pitfalls and problems faced throughout analysis

---

## Table of Contents
1. [Installation](#installation)
2. [Project Organization](#project-organization)
3. [Question 1](#question-1)
4. [Question 2](#question-2)
5. [Question 3](#question-3)
6. [Question 4](#question-4)
7. [Question 5](#question-5)
8. [Question 6](#question-6)
9. [Utilities Functions](#utilities-functions)
10. [Documentation](#documentation)
11. [Authors](#authors)


---
##  Installation

The project makes use of the following packages, which can be loaded with the external script `libraries.R` (see UTILITIES below):

- `dplyr`
- `ggplot2`
- `tidyverse`
- `tidyr`
- `stringr`
- `tidytext`
- `glue`
- `readxl`
- `xtable`
- `PerformanceAnalytics`
- `RcppRoll`
- `tbl2xts`
- `lubridate`
- `gt`
- `rmsfuns`
- `RiskPortfolios`
- `fitHeavyTail`
- `rportfolios`
- `ggExtra`
- `reshape2`
- `GGally`
- `rmgarch`

These libraries are loaded using the pacman package, which offers a more convenient way to load R packages, installing them if necessary. Ensure that pacman is installed on your machine by running the following code in R:

```r
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

```

---

## Project Organization

### `DATA`:
Contains all data files for the practical.

### `UTILITIES`:
This folder houses external scripts that enhance functionality.
  - `aesthetics.R`: Provides a standardised plotting theme.
  - `libraries.R`: Loads all necessary libraries and packages for the project via pacman.
  - `capping.R`: Provides the `Proportional_Cap_Foo` function
  - `missing-values.R`: Provides the `impute_missing_returns` function
  - `fee-converter.R`: Provides the `fee_converter` function

### `WRITE-UPS`:
Stores all write-ups and documentation for questions.
  - `Basic-Question-Template`: A folder housing the template for basic questions.
  - `Presentation-Question-Template`: A folder housing template for presentations.
  - `Question-#`: Write-up and code for Questions
  
### `LICENSE`:
This project is licensed under the MIT License, which is a highly permissive and flexible open-source license.

---

##  Question 1

This question is stored in `WRITE-UPS`->`Question-1`. The presentation was drawn up using rmarkdown and beamer, in the file `Question-1-Presentation.Rmd`; while the knitted presentation is the file `Question-1-Presentation.pdf`. All code used to create the plots, and wrangle the data has been stored in the `code` folder, but also shown below.

The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:

1. **Static Analysis:** How does the AI Implementer fund's performance compare with the Capped SWIX benchmark and ASISA active managers over the entire period?
2. **Fees and Performance:** Does the AI Implementer fund consistently outperform the benchmark after accounting for fees?
3. **Rolling Returns Performance:** How significantly do management and performance fees impact the net returns of the AI Implementer fund compared to the benchmark and peers?
4. **Correlations:** What is the correlation between benchmark and the performance of the AI Implementer fund?

### Code:


#### Data operations:

- Uncertain whether all of the funds in ASISA are actively managed. My line of thinking is that if the fund is not an index (`Index` == "No"), it is actively managed. None of the funds are fund of funds (FoF), so this column is unneeded. Here I filter for actively managed funds and drop the Index and FoF columns (for cleaner data structure).
- Comparing all of the ASISA funds might be too broad. I then opted to randomly select 4 funds, as well as the average returns, to compare with the benchmark and AI fund.
- I then also adjusted returns based on fees, using the `fee-converter` function I created, that adjusts returns based on annual fees; which I specified as 100bps
- I prefer working with aggregate data sets (easier for plotting and analysis). The main merged data frames have the prefix Rets_ ("Returns").
  -  `Rets_long`: long format of merged data
  -  `Rets_wide`: wide format of merged data
  -  `Rets_long_cum`: filtered version of Rets_long that only starts with date >= 2014-02-28 (the date from which all funds have full data availability); contains cumulative returns for each fund and benchmark.
  -  `Rets_rolling`: contains 3-year rolling returns for funds 
- I later realised that I wanted to produce a scatter plot, which required additional data operations and allows me to compare the actively managed and AI funds with the benchmark. This new data set was saved as `Funds_all` (that is, all the AI and actively managed funds, compared to the Benchmark)
  -  The reason this was done: I needed both an x- and y-variable (and not the benchmark, AI, and actively managed funds in one variable (column)
  -  I then created `Funds_all` where the Benchmark can be used as an x-variable and AI and Actively Managed funds can be used as y-variables

```r

set.seed(123321)
sample_size = 4

# Filter ASISA for actively managed funds
ASISA_filtered <- ASISA %>%
  filter(Index == "No") %>%
  select(-Index, -FoF)

# Get average returns for active funds
ASISA_mean <- ASISA_filtered %>%
  group_by(date) %>%
  summarise(Returns = mean(Returns, na.rm = TRUE)) %>% 
  mutate(Fund = "Active Avg") %>%
  mutate(Returns = fee_converter(Returns, 0.01))

# Sample 4 random active funds from ASISA
ASISA_prepared <- ASISA_filtered %>%
  distinct(Fund) %>%
  sample_n(sample_size) %>%
  inner_join(ASISA_filtered, by = "Fund") %>%
  mutate(Returns = fee_converter(Returns, 0.01))

# Preparing AI dataframe
AI_prepared <- AI %>%
  rename(Returns = AI_Fund) %>%
  mutate(Fund = "AI Implementer")

# Preparing BM dataframe
BM_prepared <- BM %>%
  select(-Tickers) %>%
  mutate(Fund = "Benchmark")

# Merging all dataframes
Rets_long <- bind_rows(AI_prepared, BM_prepared, ASISA_prepared, ASISA_mean)


# Converting to wide format
Rets_wide <- Rets_long %>%
  spread(key = Fund, value = Returns)

#   Calculate cumulative returns
Rets_long_cum <- Rets_long %>%
  filter(date >= as.Date("2014-02-28")) %>%
  group_by(Fund) %>%
  mutate(Cumulative_Returns = cumprod(1 + Returns)) %>%
  ungroup()

#   Comparing Funds against the Benchmark

# Prepare BM_all
BM_all <- BM_prepared %>%
  select(-Fund) %>%
  rename(Benchmark = Returns)

# Prepare ASISA_all
ASISA_all <- ASISA_prepared %>%
  mutate(Fund = "Actively Managed")

# Merge dataframes
merge_AI <- left_join(AI_prepared, BM_all, by = 'date')
merge_ASISA <- left_join(ASISA_all, BM_all, by = 'date')

Funds_all <- bind_rows(merge_AI, merge_ASISA)

```

#### Data Inspection:

```r

# Basic data inspection
tablestats <-
  Rets_wide %>% tbl_xts() %>% 
  table.Stats(., ci = 0.95, digits = 3)
print(tablestats[,1:7])

```

#### Plotting:

```r

# Calculate the median for the AI fund

ai_median <- median(Rets_wide$`AI Implementer`, na.rm = TRUE)

# Create the boxplot

boxplot <- Rets_long %>% 
    ggplot(aes(x = Fund, y = Returns, fill = Fund)) +
      geom_boxplot(color = "black") +
      geom_hline(yintercept = ai_median, linetype = "dashed", color = "black", size = 1) +
      scale_fill_manual(values = palette) +
      labs(title = "Comparison of Returns* Across Funds",
           subtitle = "Horizontal dashed line represents median of AI Implementer returns,\nwhich is 0.02.",
           caption = "*After fees (Management fee is 100bps)",
           x = "Fund",
           y = "Returns",
           fill = "Fund") +
      th +
      theme(legend.position = "none",
            axis.text.x = element_text(angle=90, hjust = 1))
ggsave("Figures/boxplot.png", plot = boxplot)

```

```r

# Create the scatter plot

scatplot <- Funds_all %>% 
    ggplot(aes(x = Benchmark, y = Returns, color = Fund)) +
        geom_point(size = 1) +  # Add points
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +  # 45-degree line
        geom_smooth(method = "lm", formula = y ~ x, aes(group = Fund), se = FALSE) +  # Ab-lines
        scale_color_manual(values = palette) +
        labs(title = "Comparison of Fund Returns* vs. Benchmark",
             subtitle = "Dashed line represents a 45º-line, indicating a 1-to-1 correlation between\nfund returns and benchmark.",
             caption = "*After fees (Management fee is 100bps)",
             x = "Benchmark Returns",
             y = "Fund Returns",
             color = "Fund Type") + th

ggsave("Figures/scatplot.png", plot = scatplot)

```


```r

# Create the cumulative returns plot

cumplot <- Rets_long_cum %>% 
    #filter(Fund == "Active Avg" | Fund == "AI Implementer" | Fund == "Benchmark") %>% 
    ggplot(aes(x = date, y = Cumulative_Returns, color = Fund)) +
      geom_line(size = 1) +
      scale_color_manual(values = palette) +
      labs(title = "Growth of $1 Invested* Over Time",
           caption = "*After fees (Management fee is 100bps)",
           x = "Date",
           y = "Cumulative Returns",
           color = "Fund") +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "")) +
      th

ggsave("Figures/cumplot.png", plot = cumplot)

```

### Some pitfalls along the way:

- Comparing the plethora of ASISA funds with the benchmark and AI fund proved too wide. I had to narrow down the funds to compare. I first tried average returns, but this means that we lose a lot of the particular nuance in many of the funds. I then opted to compare quartiles, but in doing so, you bias the comparison by comparing low performing, average performing, and high performing funds by "selecting them". You also lose very important risk metrics. The final call was to randomly select 4 funds to compare as well as the average returns of the active funds.
- Many of the actively managed funds have data only starting from much later. For the cumulative returns analysis, I had to filter the data from specific periods (in this case from 2014-02-28) in order to accurately compare cumulative returns.
- I decided calculating rolling returns was the best option to meaningfully compare the funds with each other.

---

##  Question 2

This question is stored in `WRITE-UPS`->`Question-2`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:

1. **How do Hedged and Unhedged Portfolios Compare in Terms of Cumulative Returns?**
   - This question aims to understand the overall performance of hedged versus unhedged portfolios over time. The cumulative returns plot helps in visualizing which strategy outperforms the other over the study period.

2. **What is the Impact of Currency Hedging on Rolling Returns?**
   - By examining 3-year rolling returns, the analysis seeks to determine how currency hedging influences the stability and consistency of returns over different market conditions.

3. **How Does Currency Movement Affect Portfolio Returns in USD Terms?**
   - This aspect of the analysis focuses on the relationship between USD-ZAR currency movements and the performance of portfolios when converted to USD. It aims to understand the risk and return dynamics from a currency perspective.

4. **What is the Downside Risk Associated with Hedged and Unhedged Portfolios?**
   - The analysis here is geared towards understanding the risks involved in both strategies, especially focusing on the downside risks and how they differ between the hedged and unhedged portfolios.

5. **How do Hedged and Unhedged Portfolios Compare in Terms of Rolling Volatility?**
   - This question delves into the volatility aspect of the portfolios. It examines whether hedging reduces the variability of returns over time and how significant this reduction is.

6. **What are the Distributions of Rolling Volatility for Hedged and Unhedged Portfolios?**
   - By creating histograms and violin plots, the analysis aims to explore the distribution and range of volatilities for both portfolio structures, giving insights into their risk profiles.


### Approach:

Let us first work on an approach to simulate currency hedging. Here is my line of thinking to do so:

I attempt to simulate the effect of currency hedging on an investment portfolio by adjusting the returns of South African indices (J433, ALBI) and US$ denominated indices in response to the ZAR/USD exchange rate fluctuations. The methodology involves recalculating the returns of these local indices by inflating/deflating them with exchange rate growth, derived from the ZAR/USD exchange rate data. This adjustment aims to nullify the impact of currency movements, mimicking a hedging strategy that protects against exchange rate risks. The performance of this 'fully hedged' portfolio is then compared with an 'unhedged' portfolio (without such adjustments) to evaluate the effectiveness of the simulated currency hedging strategy in stabilizing portfolio returns against foreign exchange volatility. **Note:** Both the 'hedged' and 'unhedged' portfolios were rebalanced every quarter.

For more clarification, consider the simplified schematic below:
![](MISC/Schematic_Hedging.jpeg)

To replicate the study and compare a hedged and unhedged portfolio, we'll need to follow these steps:

1.  Create the Portfolio Composition:

- The portfolio is a 60/40 Equity/Bond split with a 70/30 Local/Global split.
- In the Indexes dataframe, we have MSCI_ACWI and Bbg_Agg for global equity and bond indices, and J433 and ALBI for local equity and bond indices.

2.  Calculate Portfolio Returns:

- Combine returns according to the specified portfolio weights.

3. Hedging and Unhedging:

- For the unhedged portfolio, we'll use the returns as they are.
- For the hedged portfolio, we'll need to adjust the returns of the indices using the exchange rate data in ZAR. This simulates the effect of hedging against currency fluctuations.

4.  Compare the Portfolios:

- Calculate the rolling realized volatility for both portfolios.
- Compare these volatilities over time to analyze the impact of hedging.
- Calculate return metrics.

### Code:

1. **Data Loading**: Imported data from `Cncy_Hedge_Assets.rds` and `Monthly_zar.rds`, standardizing date formats for consistency.

2. **Currency Conversion**: Adjusted portfolio returns based on USD-ZAR exchange rates to analyze both hedged and unhedged portfolios in USD terms.

3. **Portfolio Returns Calculation**: Used custom functions to calculate returns for both portfolio types across different rebalancing periods.

4. **Performance Analysis**: Computed and plotted cumulative and rolling returns to compare portfolio performance over time.

5. **Exchange Rate Impact Assessment**: Analyzed the relationship between USD-ZAR exchange rate movements and USD portfolio returns.

6. **Risk Metrics Calculation**: Evaluated downside risks of both portfolio types to understand their risk profiles.

7. **Volatility Analysis**: Calculated and visualized rolling volatility, comparing volatility distributions of hedged and unhedged portfolios.

#### Data operations:

```r

RebMonths <- c(3, 6, 9, 12) # Rebalancing periods

# Convert the date format to Year-Month for both dataframes
Indexes <- Indexes %>% 
  mutate(date = floor_date(as.Date(date), "month"))

ZAR <- ZAR %>% 
  mutate(date = floor_date(as.Date(date), "month"))

# Note on calculatePortfolioReturns():
    # Need a wide format dataframe with a date column and returns only!
    # It is a very situational specific function and not very general

# Calculate returns of unhedged portfolio
Unhedged_portf <- calculatePortfolioReturns(Indexes, RebMonths) %>% 
    mutate(Structure = "Unhedged")

# Calculate returns of hedged portfolio

# Align dates and compute growth rate of ZAR to USD exchange rate
ZAR <- ZAR %>% mutate(ExchangeRateGrowth = (value / lag(value)) - 1)

# Adjust returns in Indexes by exchange rate appreciation/depreciation
Indexes_adjusted <- Indexes %>%
  mutate(date = as.Date(date)) %>%
  left_join(ZAR %>% select(date, ExchangeRateGrowth), by = "date") %>%
  mutate_at(vars(J433, ALBI), ~ . * (1 + ExchangeRateGrowth)) %>% # Inflate Rand-denominated indexes
  mutate_at(vars(MSCI_ACWI, Bbg_Agg), ~ . / (1 + ExchangeRateGrowth)) %>% # Deflate Dollar-denominated indexes
  select(-c(ExchangeRateGrowth))

Hedged_portf <- calculatePortfolioReturns(Indexes_adjusted, RebMonths) %>% 
    mutate(Structure = "Hedged")

# Merge the unhedged and hedged portfolio returns
combined_portfolio <- bind_rows(Unhedged_portf, Hedged_portf)

# Calculate cumulative and rolling returns
combined_portfolio <- combined_portfolio %>%
  group_by(Structure) %>%
  arrange(date) %>%
  mutate(CumulativeReturn = cumprod(1 + PortfolioReturn)) %>%
  mutate(RollRets = RcppRoll::roll_prod(1 + PortfolioReturn, 36, fill = NA, 
  align = "right")^(12/36) - 1) %>% 
  group_by(date) %>% filter(any(!is.na(RollRets))) %>% 
  ungroup()

```

#### Plotting:

```r

# Plot the cumulative returns
combined_portfolio %>% 
ggplot(aes(x = date, y = CumulativeReturn, color = Structure)) +
  geom_line() +
  th +
  labs(title = "Cumulative Portfolio Returns: Hedged vs Unhedged",
       x = "Date",
       y = "Cumulative Return",
       color = "Structure") +
  scale_color_manual(values = palette)

```



```r

# Plot the rolling returns
combined_portfolio %>% 
ggplot(aes(x = date, y = RollRets, color = Structure)) +
  geom_line() +
  th +
  labs(title = "3-Year Rolling Returns: Hedged vs Unhedged",
       x = "Date",
       y = "Rolling 3 year Returns (Ann.)",
       color = "Structure") +
  scale_color_manual(values = palette)

```

```r

# Convert returns to USD-returns
Indexes_USD <- Indexes %>%
  mutate(date = as.Date(date)) %>%
  left_join(ZAR %>% select(date, ExchangeRateGrowth), by = "date") %>%
  mutate_at(vars(J433, ALBI), ~ . / (1 + ExchangeRateGrowth)) %>% 
  select(-c(ExchangeRateGrowth))

USD_portf <- calculatePortfolioReturns(Indexes_adjusted, RebMonths) %>% 
    mutate(Hedged = "Yes")

scatter_data <- USD_portf %>%
  left_join(ZAR %>% select(date, ExchangeRateGrowth), by = "date") %>%
  rename(USD_ZAR_Returns = ExchangeRateGrowth,
         Portfolio_Returns_USD = PortfolioReturn) %>%
  select(date, USD_ZAR_Returns, Portfolio_Returns_USD)

# Create the scatter plot
p <- scatter_data %>% 
        ggplot(aes(x = USD_ZAR_Returns, y = Portfolio_Returns_USD)) +
            geom_point(color = "#D98515", alpha=0.6) +
            geom_smooth(method = "lm", color = "darkgrey", se=FALSE) +
            scale_x_continuous(labels = scales::percent_format(scale = 100), limits = c(-0.15, 0.15)) +
            scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(-0.15, 0.15)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
            geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
            th +
            labs(x = "USD-ZAR Returns", 
                 y = "Portfolio Returns (USD)", 
                 title = "Scatter Plot with Marginal Distributions",
                 caption = "Calculations from 28 February 2002 - 31 August 2023")

# Add marginal histograms
p <- ggExtra::ggMarginal(p, type = "density", fill = "#1E3364", alpha = 0.6)

print(p)
```


# Next create the downside risk table from Prac 2

```r

# Separate hedged and unhedged data
df_hedged <- combined_portfolio %>% filter(Structure == "Hedged") %>% select(date, PortfolioReturn)
df_unhedged <- combined_portfolio %>% filter(Structure == "Unhedged") %>% select(date, PortfolioReturn)

# Rename columns to match the example structure
df_hedged <- df_hedged %>% rename(Hedged = PortfolioReturn)
df_unhedged <- df_unhedged %>% rename(Unhedged = PortfolioReturn)

# Calculate Downside Risk
tabdownside <- table.DownsideRisk(left_join(df_unhedged, df_hedged, by = "date") %>% tbl_xts(.), 
                                  ci = 0.95, Rf = 0, MAR = 0)

# Select specific rows
tabdownside <- tabdownside[c(1,5,7,8:11),]

# Format and display the table
tabdownside %>% 
  data.frame() %>% 
  tibble::rownames_to_column() %>% 
  gt() %>% 
  tab_header(title = glue("Downside Risk Estimates")) %>% 
  fmt_percent(columns = 2:3, decimals = 2)



```

```r

# Calculate 30-day rolling volatility
combined_portfolio <- combined_portfolio %>%
    group_by(Structure) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(RollingVolatility = rollapply(PortfolioReturn, width = 36, FUN = sd, na.rm = TRUE, fill = NA, align = "right"))

# Plotting
ggplot(combined_portfolio, aes(x = date, y = RollingVolatility, color = Structure)) +
    geom_line() +
    scale_color_manual(values = palette) +
    labs(title = "36-month Rolling Volatility",
         x = "Date",
         y = "Volatility (Standard Deviation)",
         color = "Structure") +
    th


```

```r

volatility_data <- combined_portfolio %>%
  filter(!is.na(RollingVolatility))

# Create a histogram of the rolling volatility
ggplot(volatility_data, aes(x = RollingVolatility, fill = Structure)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30, colour = "black", size = 0.3) +
  labs(title = "Histogram of 36-Month Rolling Volatility",
       x = "Rolling Volatility (Standard Deviation)",
       y = "Frequency") +
  scale_fill_manual(values = palette) +
  th

```


```r

# Create a box plot of the rolling volatility
ggplot(volatility_data, aes(x = Structure, y = RollingVolatility, fill = Structure, color = Structure)) +
  geom_violin(alpha = 0.6) +
  geom_jitter() +
  labs(title = "Violin Plot of 36-Month Rolling Volatility",
       x = "Portfolio Structure",
       y = "Rolling Volatility (Standard Deviation)") +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  th

```


### Some pitfalls along the way:

- Because of the weighting of the portfolio, we would need to readjust the weights periodically.
- The last day of the month for ZAR does not align with Indexes. However, I opted to calculate the month on month exchange rate appreciation/depreciation and then overlapped that with the dates for Indexes

---

##  Question 3

This question is stored in `WRITE-UPS`->`Question-3`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:

1. **Size and Sector Impact on ALSI and SWIX Performance**: How do size categories (large, mid, small caps) and sector distributions influence the performance of the ALSI and SWIX indexes over time?

2. **Rebalancing Effects and Market Dynamics**: What is the impact of rebalancing days on the ALSI and SWIX indexes? Are there correlations with market volatility and currency performance?

3. **Capping Levels and Index Performance**: What are the effects of different capping levels (5%, 10%, uncapped) on the diversification and performance of the ALSI and SWIX indexes?

### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

- The first issue I encountered is missing values in the Index_Name column of ALSI. I first identified Tickers that resulted in these NA values. I initially replaced the NA values in the Index_Name column with "Unclassified" as a straightforward and transparent way to handle missing data in this context. This approach allows one to keep all the data points in the analysis while acknowledging that certain tickers couldn't be classified into the existing size categories (Large, Mid, Small Caps).
- However, after looking at their movement, which closely resembled small caps movement, I imputed these missing values as "Small Caps"
- Rolling returns and daily data: A common approach for daily data is to use a 252-day window, as there are typically 252 trading days in a year. This is the approach that was followed.
- More missing weight values for J403. I just replaced the NA values with 0,000046 and 0,0023 for HPB and ROC respectively, reflecting numbers close to the weights at the time.
- When I attempted to cap by industry, I ran into the problem of being unable to calculate the unweighted realized industry returns, because I needed the data on an industry-wide level. The capping process turned out much more complex then I could have imagined. I then decided to opt for the easiest (albeit not the best) route and got the average unweighted returns per industry in order to reweight based on the capping.
- In the end, I decided to abandon the project as I was consuming way too much time on this question and opted to use what I already had for the report. The unfinished code has, however, been included.

---

##  Question 4

This question is stored in `WRITE-UPS`->`Question-4`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

---

##  Question 5

This question is stored in `WRITE-UPS`->`Question-5`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

---

##  Question 6

This question is stored in `WRITE-UPS`->`Question-6`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:

1. **How can a global balanced index fund portfolio be optimized considering various constraints and historical data?**
   - This question is addressed through the construction of the portfolio, considering constraints like maximum exposure to equities and bonds, and the use of historical return data for optimization.

2. **What is the impact of time-span and rebalancing strategies on portfolio performance?**
   - The analysis involves filtering assets based on their historical data availability (minimum 3 years) and exploring different look-back periods for rolling optimization, reflecting on how these factors influence portfolio construction and rebalancing.

3. **How do correlations between different assets in the portfolio evolve and influence portfolio diversification?**
   - The focus here is on understanding asset correlations through hierarchical clustering and potentially GARCH models (though the latter was left out), which helps in identifying asset classes or groups with similar return characteristics for better diversification strategies.

### Code:
In the provided code, the main operations include:

1. **Filtering Assets**: Assets with less than three years of data are removed to ensure a stable analysis base.
2. **Calculating Returns**: Monthly returns for each asset are computed, focusing on the last trading day of each month.
3. **Data Reshaping**: The dataset is transformed into a wide format, with separate columns for each asset's returns.
4. **Covariance and Mean Estimation**: The covariance matrix and mean returns for the assets are calculated, essential for portfolio optimization.
5. **Portfolio Optimization Setup**: Constraints are applied for portfolio optimization, including limits on asset weights and specific allocations for equities and bonds.
6. **Rolling Optimization**: The portfolio is optimized over different rolling periods to analyze performance dynamics.
7. **Visualization**: Key visualizations are prepared, including weight distribution over time and a correlation matrix plot.

#### Data operations:

```r

# Function that checks for elements with less than a specific lifespan and returns a filtered dataframe if necessary
check_and_filter_time_span <- function(data, date_column, name_column) {
  # Calculate the time span for each asset
  time_span_data <- data %>%
    group_by({{name_column}}) %>%
    mutate(
      time_span = as.numeric(difftime(max({{date_column}}), min({{date_column}}), units = "days"))
    ) %>%
    ungroup()

  # Check if any asset has less than 3 years of data
  if(any(time_span_data$time_span < (3 * 365))) {
    # Filter out assets with less than 3 years of data
    filtered_data <- time_span_data %>%
      filter(time_span >= (3 * 365))

    return(filtered_data)
  } else {
    print("All elements meet the criteria")  
    return(data)
  }
}


Portf <- bind_rows(msci, MAA) %>% 
  select(-Name)

# Now apply the check_and_filter_time_span function
Portf <- check_and_filter_time_span(Portf, date, Ticker)

# Calculate returns
Portf <- Portf %>%
  arrange(Ticker, date) %>%
  group_by(Ticker, YM = format(date, "%Y-%m")) %>%
  filter(date == max(date)) %>%  # Keep only the last day of each month
  mutate(
    Monthly_Price = dplyr::last(Price)
  ) %>%
  ungroup() %>%
  group_by(Ticker) %>%
  mutate(
    Returns = (Monthly_Price/lag(Monthly_Price) - 1)
  ) %>%
  ungroup() %>%
  select(-Monthly_Price, -YM)  # Optionally remove the helper columns


# Prepare the wide format data
Portf_wide <- Portf %>%
  select(-Price) %>%
  pivot_wider(
    names_from = Ticker,
    values_from = Returns
  )

# Filter data from 2010
Portf <- Portf %>%
  filter(date >= as.Date("2011-01-01"))

# Make a wide df
Portf_wide <- Portf_wide %>%
  filter(date >= as.Date("2011-01-01"))

```

```r

# Sample covariance and mean:
#First, we need to ensure that the date column is removed to calc sigma and mu (and ensure it is a matrix)
Portf_ret_mat <- data.matrix(Portf_wide[,-1])

Sigma <- RiskPortfolios::covEstimation(Portf_ret_mat)

Mu <- Portf_wide %>% 
    summarise(across(-date, ~prod(1+.)^(1/n())-1)) %>% 
    purrr::as_vector()

NStox <- ncol(Portf_ret_mat)
meq = 1 
LB = 0.01
UB = 0.25
eq_UB = 0.6 #equity constraint
bond_UB = 0.4 #bond constraint

#   Let's figure out where the equities and bonds are located in my data:
    #  1. Equity; 2. Equity; 3. Equity; 4. Equity; 5. Currency; 6. Bond; 7. Bond; 8. Bond; 9. Bond; 10. Bond; 11. Bond; 12. Commodity; 13. Currency

eq_const_mat <- rbind(
  -diag(4), # Constraints for equities
  matrix(0, nrow = 9, ncol = 4) # No equity constraints for other asset types
)

bond_const_mat <- rbind(
  matrix(0, nrow = 5, ncol = 6), # No bond constraints for first 5 assets (Equity, Currency)
  -diag(6), # Constraints for bonds
  matrix(0, nrow = 2, ncol = 6) # No bond constraints for last 2 assets (Commodity, Currency)
)

bvec <- c( 1, rep(LB, NStox), -rep(UB, NStox), -rep(bond_UB, 6), -rep(eq_UB, 4))
Amat <- cbind(1, diag(NStox), -diag(NStox), bond_const_mat, eq_const_mat )

Portf_Weights <- 
  left_join(
  optim_foo(Type = "mv", mu, Sigma, LB = 0, UB = 0.4, printmsg = F),
  optim_foo(Type = "minvol", mu, Sigma, LB= 0, UB =0.4, printmsg = F),
  by = c("Tickers")) %>% 
    left_join(.,optim_foo(Type = "erc", mu, Sigma, LB= 0, UB =0.4, printmsg = F),by = c("Tickers")) %>% 
      left_join(.,optim_foo(Type = "riskeff", mu, Sigma, LB= 0, UB =0.4, printmsg = F),by = c("Tickers"))

```

#### Plotting:
```r

EOQ_datevec <- Portf_wide %>%
  select(date) %>%
  unique() %>%
  mutate(YM = format(date, "%Y%m")) %>%  # Format to include both year and month
  group_by(YM) %>%
  filter(lubridate::month(date) %in% c(1, 4, 7, 10)) %>%  # Filter for end of quarters
  summarise(date = dplyr::last(date)) %>%
  pull(date)


Lookback_24 <- EOQ_datevec %>% 
    map_df(~Roll_optimizer(Portf_wide, EOQ_datevec = ., LookBackSel = 24))


head(Lookback_24, 13) %>%
  gt() %>%
  tab_header(
    title = "Result Lookback Table (2-Years)"
  )


```
```r

ggplot(Lookback_24, aes(x = date, y = weight, fill = stocks)) +
  geom_bar(stat = "identity", color = "black") +
  th +
  labs(
    title = "Weight Distribution Over Time",
    x = "Date",
    y = "Weight",
    fill = "Asset"
  ) +
    theme(legend.position = "right")



```

```r

corr_plot <- Portf_wide %>% 
    select(-date)

# Nice visualization of correlations
ggcorr(corr_plot, method = c("everything", "pearson")) 
```

```r

# Calculate the correlation matrix (use complete data or a specific period as required)
cor_matrix <- cor(Portf_wide[,-1], use = "complete.obs")

# Convert the correlation matrix to a distance matrix
dist_matrix <- as.dist(1 - cor_matrix)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, hang = -1, cex = 0.6, main = "Hierarchical Clustering of Assets")

```
####  Misc

```r

#returns_data <- Portf_wide[,-1]

# Specify a multivariate GARCH model - DCC GARCH
#uspec <- ugarchspec(mean.model = list(armaOrder = c(1, 1)), variance.model = list(garchOrder = c(1, 1), model = "sGARCH"))
#spec <- dccspec(uspec = multispec(replicate(ncol(returns_data), uspec)), dccOrder = c(1, 1), distribution = "mvnorm")

# Fit the DCC GARCH model
#fit <- dccfit(spec, data = returns_data)

# Extract conditional correlations
#conditional_correlations <- rcov(fit)

# Example: accessing the correlation matrix at the first time point
#conditional_correlations[,,1]


```

### Some pitfalls along the way:
- In general not many issues with this question.
- I did attempt to create a simple GARCH model, but in the end decided that this would be overkill. I decided that what I already had was enough.
---


##  Utilities Functions:

The following section shows the code used in the UTILITIES folder (in alphabetical order):

### `aesthetics.R`

```r
#   Goal: Creating themes and aesthetics for plots.

path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/01-Masters-2023/02 Financial Econometrics/20713479-FMX-Prac/"
source(paste0(path, "UTILITIES/libraries.R")) #   Load libraries

# Define colour palette
palette <- c("#1F6F70", "#D98515", "#1E3364", "#75A21B", "#C93D44", "#38B0FF", "#8E22AC")

# Define plot theme
th <- theme(
    # Background and grid
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "#565857", size = 0.1),
    panel.grid.minor = element_line(color = "#565857", size = 0.1),
    axis.line = element_line(size = 0.3, color = "black"),

    # Axis titles and labels
    axis.title.x = element_text(size = 12,family = "Palatino", vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 12,family = "Palatino",vjust = 0.5, hjust = 0.5,face = "bold"),
    axis.text.y = element_text(size = 10,family = "Palatino"),
    axis.text.x = element_text(size = 10,family = "Palatino"),

    # Title, subtitle, and caption
    plot.title = element_text(size = 14,family = "Palatino",face = "bold"),
    plot.subtitle = element_text(size = 12,family = "Palatino"),
    plot.caption = element_text(size = 10,family = "Palatino", hjust = 0),

    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 12,family = "Palatino"),
    legend.title = element_text(size = 12,family = "Palatino",face = "bold"),
    legend.key = element_blank(),

    # Other
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12,family = "Palatino",vjust = 1,hjust = 0.5, face="bold"),
    strip.background = element_blank(),
    text = element_text(family = "Palatino")
)

```

### `capping.R`

```r
#   Goal: Creating themes and aesthetics for plots.

path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/01-Masters-2023/02 Financial Econometrics/20713479-FMX-Prac/"
source(paste0(path, "UTILITIES/libraries.R")) #   Load libraries

# Define colour palette
palette <- c("#1F6F70", "#D98515", "#1E3364", "#75A21B", "#C93D44", "#38B0FF", "#8E22AC")

# Define plot theme
th <- theme(
    # Background and grid
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_line(color = "#565857", size = 0.1),
    panel.grid.minor = element_line(color = "#565857", size = 0.1),
    axis.line = element_line(size = 0.3, color = "black"),

    # Axis titles and labels
    axis.title.x = element_text(size = 12,family = "Palatino", vjust = 0.5, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 12,family = "Palatino",vjust = 0.5, hjust = 0.5,face = "bold"),
    axis.text.y = element_text(size = 10,family = "Palatino"),
    axis.text.x = element_text(size = 10,family = "Palatino"),

    # Title, subtitle, and caption
    plot.title = element_text(size = 14,family = "Palatino",face = "bold"),
    plot.subtitle = element_text(size = 12,family = "Palatino"),
    plot.caption = element_text(size = 10,family = "Palatino", hjust = 0),

    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 12,family = "Palatino"),
    legend.title = element_text(size = 12,family = "Palatino",face = "bold"),
    legend.key = element_blank(),

    # Other
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12,family = "Palatino",vjust = 1,hjust = 0.5, face="bold"),
    strip.background = element_blank(),
    text = element_text(family = "Palatino")
)
```

### `fee_converter.R`

```r

# Goal: calculate returns after fees

fee_converter <- function(stock_returns, annual_fee) {
  # Function to convert annual fee to monthly compounded fee
  monthly_fee_converter <- function(annual_fee) {
    (1 + annual_fee)^(1/12) - 1
  }
  
  # Calculate the monthly compounded fee
  monthly_fee = monthly_fee_converter(annual_fee)
  
  # Adjusting returns for the monthly compounded fee
  adjusted_returns = stock_returns - monthly_fee
  
  return(adjusted_returns)
}

```

### `libraries.R`

```r

pacman::p_load(dplyr,
               ggplot2,
               tidyverse,
               tidyr,
               stringr,
               tidytext,
               readxl,
               xtable,
               PerformanceAnalytics,
               tbl2xts,
               lubridate,
               gt,
               rmsfuns,
               RiskPortfolios,
               fitHeavyTail)

```

### `missing-values.R`

```r

#   Goal: Handling missing values in data

path <- "/Users/janhendrikpretorius/Library/CloudStorage/OneDrive-StellenboschUniversity/01-Masters-2023/02 Financial Econometrics/20713479-FMX-Prac/"
source(paste0(path, "UTILITIES/libraries.R")) #   Load libraries

impute_missing_returns <- function(return_mat, impute_returns_method = "NONE"){
  # Make sure we have a date column called date:
  if( !"date" %in% colnames(return_mat) ) stop("No 'date' column provided in return_mat. Try again please.")
  
  # Note my use of 'any' below
  # Also note that I 'return' return_mat - which stops the function and returns return_mat.
  if( impute_returns_method %in% c("NONE", "None", "none") ) {
    if( any(is.na(return_mat)) ) warning("There are missing values in the return matrix.. Consider maybe using impute_returns_method = 'Drawn_Distribution_Own' / 'Drawn_Distribution_Collective'")
    return(return_mat)
  }
  
  
  if( impute_returns_method  == "Average") {
    
    return_mat <-
      return_mat %>% gather(Stocks, Returns, -date) %>%
      group_by(date) %>%
      mutate(Avg = mean(Returns, na.rm=T)) %>%
      mutate(Avg = coalesce(Avg, 0)) %>% # date with no returns - set avg to zero
      ungroup() %>%
      mutate(Returns = coalesce(Returns, Avg)) %>% select(-Avg) %>% spread(Stocks, Returns)
    
    # That is just so much easier when tidy right? See how I gathered and spread again to give back a wide df?
    return(return_mat)
  } else
    
    if( impute_returns_method  == "Drawn_Distribution_Own") {
      
      N <- nrow(return_mat)
      return_mat <-
        # DIY: see what density function does
        left_join(return_mat %>% gather(Stocks, Returns, -date),
                  return_mat %>% gather(Stocks, Returns, -date) %>% group_by(Stocks) %>%
                    mutate(Dens = list(density(Returns, na.rm=T))) %>%
                    summarise(set.seed(as.numeric(format( Sys.time(), format = "%s"))/1e3*sample(1:100)[1]), Random_Draws = list(sample(Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y))),
                  by = "Stocks"
        ) %>%  group_by(Stocks) %>%
        # Random draw from sample:
        mutate(Returns = coalesce(Returns, Random_Draws[[1]][row_number()])) %>%
        select(-Random_Draws) %>% ungroup() %>% spread(Stocks, Returns)
      return(return_mat)
    } else
      
      if( impute_returns_method  == "Drawn_Distribution_Collective") {
        NAll <- nrow(return_mat %>% gather(Stocks, Returns, -date))
        # DIY: see what density function does
        return_mat <-
          bind_cols(
            return_mat %>% gather(Stocks, Returns, -date),
            return_mat %>% gather(Stocks, Returns, -date) %>%
              mutate(Dens = list(density(Returns, na.rm=T))) %>%
              summarise(set.seed(as.numeric(format( Sys.time(), format = "%s"))/1e3*sample(1:100)[1]), Random_Draws = list(sample(Dens[[1]]$x, NAll, replace = TRUE, prob=.$Dens[[1]]$y))) %>%
              unnest(Random_Draws)
          ) %>%
          mutate(Returns = coalesce(Returns, Random_Draws)) %>% select(-Random_Draws) %>% spread(Stocks, Returns)
        return(return_mat)
      } else
        
        if( impute_returns_method  == "Zero") {
          warning("This is probably not the best idea but who am I to judge....")
          return_mat[is.na(return_mat)] <- 0
          return(return_mat)
        } else
          stop("Please provide a valid impute_returns_method method. Options include:\n'Average', 'Drawn_Distribution_Own', 'Drawn_Distribution_Collective' and 'Zero'.")
  
  return_mat
  
}


```


---

## Documentation

Links to further documentation, if available.

---

## Authors

- Jan-Hendrik Pretorius, Stellenbosch University
  - Project and analysis

- Nico Katzke
  - `Proportional_Cap_Foo` function
  - `impute_missing_returns` function
