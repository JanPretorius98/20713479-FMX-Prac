# Financial Econometrics 871 Practical Test Repository

This repository contains the materials for the Financial Econometrics 871 practical test, including datasets, utility scripts for analysis, and documentation templates for questions and presentations.

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

The project makes use of the following packages, that can be loaded with the external script `libraries.R` (see `UTILITIES` below):
- dplyr
- ggplot2
- tidyverse
- stringr
- tidytext
- readxl
- xtable

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
3. **Cumulative Returns Performance:** How significantly do management and performance fees impact the net returns of the AI Implementer fund compared to the benchmark and peers?
4. **Correlations:** What is the correlation between benchmark and the performance of the AI Implementer fund?

### Code:


#### Data operations:

- Uncertain whether all of the funds in ASISA are actively managed. My line of thinking is that if the fund is not and index (`Index` == "No"), it is actively managed. None of the funds are fund of funds (FoF), so this column is unneeded. Here I filter for actively managed funds and drop the Index and FoF columns (for cleaner data structure).
- Comparing all of the ASISA funds might be too broad. I then opted to randomly select 4 funds, as well as the average returns, to compare with the benchmark and AI fund.
- I then also adjusted returns based on fees, using the `fee-converter` function I created, that adjusts returns based on annual fees; which I specified as 100bps
- I prefer working with one aggregate data set (easier for plotting and analysis). The main merged data frame is referred to as Rets_ ("Returns"). I also merged all the data both in long (Rets_long) and wide (Rets_merged) format, depending on the need.
- I later realised that I wanted to produce a scatter plot, which required additional data operations and allows me to compare all the actively managed funds with the AI and benchmark. This new data set was saved as Funds_all (that is, all the AI and actively managed funds, compared to the Benchmark)

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

---

##  Question 2

This question is stored in `WRITE-UPS`->`Question-2`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:

1. **Impact on Returns:** What is the impact of currency hedging on the long-term returns of the portfolio? Does hedging enhance or diminish overall returns?
2. **Volatility Comparison:** How does the rolling realized volatility of a fully hedged portfolio compare to an unhedged portfolio over time? 
3. **Correlation Analysis:** How does the correlation between the South African Rand (ZAR) and global assets (like MSCI ACWI and Global Bond Aggregate) influence the portfolio's performance?
4. **Risk-Adjusted Returns:** How do risk-adjusted returns (e.g., Sharpe Ratio) differ between the hedged and unhedged portfolios?
5. **Diversification Benefits:** Does currency hedging enhance the diversification benefits of the portfolio? How does it affect the portfolio's risk profile?
6. **Tactical vs. Strategic Hedging:** What insights can be gained about the effectiveness of tactical (short-term) versus strategic (long-term) currency hedging?
7. **Currency Fluctuations:** How do fluctuations in the ZAR/USD exchange rate impact the performance of the local and global components of the portfolio?

### Approach:

To replicate the study and compare a hedged and unhedged portfolio, we'll need to follow these steps:

1.  Create the Portfolio Composition:

- The portfolio is a 60/40 Equity/Bond split with a 70/30 Local/Global split.
- In the Indexes dataframe, we have MSCI_ACWI and Bbg_Agg for global equity and bond indices, and J433 and ALBI for local equity and bond indices.

2.  Calculate Portfolio Returns:

- Combine returns according to the specified portfolio weights.

3. Hedging and Unhedging:

- For the unhedged portfolio, we'll use the returns as they are.
- For the hedged portfolio, we'll need to adjust the returns of the global components (MSCI_ACWI and Bbg_Agg) using the exchange rate data in ZAR. This simulates the effect of hedging against currency fluctuations.

4.  Compare the Portfolios:

- Calculate the rolling realized volatility for both portfolios.
- Compare these volatilities over time to analyze the impact of hedging.

### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

- Because of the weighting of the portfolio, we would need to readjust the weights periodically.
- The last day of the month for ZAR does not align with Indexes. However, I opted to calculate the month on month exchange rate appreciation/depreciation and then overlapped that with the dates for Indexes

---

##  Question 3

This question is stored in `WRITE-UPS`->`Question-3`. The following section explains step by step how the problem in question 1 was approached. This section also contains code chunks used in the creation of the write-up document.

### The folllowing questions guide the analysis:


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

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


### Code:


#### Data operations:


#### Plotting:



### Some pitfalls along the way:

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
