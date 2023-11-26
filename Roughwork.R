library(PerformanceAnalytics)
library(dplyr)
library(xts)

# Create Mock Data
set.seed(123)
data_frame <- tibble(
  Date = as.Date('2020-01-01') + 0:29, 
  Asset1 = rnorm(30, 0.05, 0.02),
  Asset2 = rnorm(30, 0.04, 0.03)
)


# Calculate cumulative returns
  # PA
xts_data <- xts(data_frame[,-1], order.by = data_frame$Date)
chart.CumReturns(xts_data, main = "Cumulative Returns")

  # ggplot
data_frame <- data_frame %>%
  mutate(Asset1.cum = cumprod(1 + Asset1),
         Asset2.cum = cumprod(1 + Asset2))

# Reshape data to long format
long_data <- data_frame %>%
  gather(key = "Asset", value = "CumulativeReturn", -Date)

ggplot(long_data, aes(x = Date, y = CumulativeReturn, color = Asset)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cumulative Returns of Asset1 and Asset2",
       x = "Date",
       y = "Cumulative Return")

pacman::p_load(tidyverse);pacman::p_load(tbl2xts)
DAX <- fmxdat::DAX

# Let's first determine when we need to rebalance (go through my code here at least 5 times to understand all the nuances, especially the grouping logic):
Rebalance_Days <- 
  
  DAX %>% 
  
  mutate(Year = format(date, "%Y"), Month = format(date, "%b"), Day = format(date, "%a")) %>% 
  
  filter(Month %in% c("Jan", "Apr", "Jul", "Oct")) %>% 
  
  select(date, Year,  Month, Day ) %>% unique() %>% 
  
  group_by(Month) %>% 
  
  filter(Day == "Wed") %>% 
  
  group_by(Year, Month) %>% 
  
  filter( date == first(date)) %>% 
  
  pull(date)

rebalance_col <-
  
  DAX %>% 
  
  filter(date %in% Rebalance_Days) %>% 
  
  # Now we have to distinguish rebalances - to create something to group by:
  mutate(RebalanceTime = format(date, "%Y%B")) %>% 
  
  # Now we can group...
  group_by(RebalanceTime) %>% 
  
  # Now trim down to 30 stocks and reweight so sum(w)=1
  arrange(desc(weight)) %>% 
  
  top_n(30, weight) %>% 
  
  mutate(weight = weight/sum(weight)) %>%
  
  ungroup() %>% 
  
  arrange(date) %>% 
  
  select(-Sector, -return)


# Question 1

library(rmsfuns)
pacman::p_load("tidyr", "tbl2xts","devtools","lubridate", "readr", "PerformanceAnalytics", "ggplot2", "dplyr")
dailydata <- fmxdat::findata
pacman::p_load("TTR")
dailydata <- 
  dailydata %>% arrange(Date) %>% 
  mutate(across(.cols = -Date, .fns = ~TTR::ROC(., type = c("continuous", "discrete")[2]))) %>% 
  # Equivalent to:   # mutate_at(.vars = vars(-Date), ~./lag(.)-1) %>% 
  # continuous equivalent to:   # mutate_at(.vars = vars(-Date), ~(log(.)-log(lag(.)))) 
  mutate_at(.vars = vars(-Date), ~na.locf(., na.rm = F, maxgap = 5)) %>% filter(Date > first(Date))
# Pad NA's back max 5 days:
# Let's not waste our time - remove spaces in column names!
colnames(dailydata) <- 
  gsub("JSE\\.","",colnames(dailydata))
colnames(dailydata) <- 
  gsub("\\.Close","",colnames(dailydata))

tablestats <-
  dailydata %>% tbl_xts() %>% 
  table.Stats(., ci = 0.95, digits = 3)
print(tablestats[,1:5])


# Question 2

library(rportfolios)
dailydata <- fmxdat::findata

dailydata.subset <- 
  
  dailydata %>% 
  
  gather(Stocks, Px, -Date) %>% 
  
  arrange(Date) %>% 
  
  group_by(Stocks) %>% 
  
  mutate(Returns = Px/lag(Px)-1) %>% ungroup() %>% filter(Date > first(Date)) %>% 
  
  select(-Px)

# Let's assume the portfolio rebalances each January and July.

# First, let's save the exact rebalance dates and save the random weight and date information to be used later:
# Below is a very nice way to save months and years: let's rebalance at month 1 and 7... 

RebMonths <- c(3, 6, 9, 12) # Make a parameter that can easily be changed later.

RandomWeights <- 
  
  dailydata.subset %>% 
  
  mutate(Months = as.numeric(format(Date, format = "%m")), 
         
         YearMonths = as.numeric(format(Date, format = "%Y%m"))) %>% 
  
  filter(Months %in% RebMonths) %>% 
  
  group_by(YearMonths, Months, Stocks) %>% filter(Date == last(Date)) %>% ungroup()

# Now let's create a column with the random weights assigned to each stock conforming to the following parameters:
# Let's also create a random weighting vector for our selected stocks, with the following parameters:
# They have to sum to 1...
# Let's add constraints too - you can only have a maximum exposure to a single stock up to 20% of the equal weight.
N_Stocks <- length(unique(RandomWeights$Stocks))

Max_Exposure <-(1/N_Stocks)*1.20

# Minimum exposure is, say, 2%:
Min_Exposure <- 0.02

# Now to append the weight vector, let's use the random.bounded function from rportfolios.

RandomWeights_adj <-  
  bind_cols(RandomWeights %>% arrange(Date),
            RandomWeights %>% group_by(Date) %>% 
              
              do( Randweights = random.bounded(n = nrow(.), 
                                               x.t = 1, # Full investment... 
                                               x.l = rep( Min_Exposure, nrow(.)), # Lower Bound 
                                               x.u = rep( Max_Exposure, nrow(.)), 
                                               max.iter = 1000) ) %>% ungroup() %>% unnest(Randweights) %>% select(-Date)
  )

# Sanity check: Create a stop function if it doesn't hold...
if( RandomWeights_adj %>% group_by(Date) %>% 
    
    summarise(Fully_Invested = sum(Randweights)) %>% filter(Fully_Invested > 1.000001 | Fully_Invested < 0.9999999 ) %>% nrow() > 0 ) stop("\n=============\n Ooops! \nWeights do not sum to 1... Please check!\n===========\n")

# Create equal weight portfolios as well:
RandomWeights_adj <- 
  
  RandomWeights_adj %>% 
  
  group_by(Date) %>% 
  
  mutate(EqualWeights = 1/n()) %>% 
  
  ungroup() %>% select(-Months, -YearMonths)

# Right, so now we have equal and random-weights that we can use at rebalancing dates: January and July.

pacman::p_load("PerformanceAnalytics")

# Now we use the Safe_Return.portfolio function from PerformanceAnalytics
# Note, as with most PA functions, the inputs are xts and wide...
# Also, let's assume you are investing R1000 at the start:
Fund_Size_at_Start <- 1000

Rand_weights <- 
  RandomWeights_adj %>% select(Date, Stocks, Randweights) %>% spread(Stocks, Randweights) %>% tbl_xts()

EW_weights <- 
  RandomWeights_adj %>% select(Date, Stocks, EqualWeights) %>% spread(Stocks, EqualWeights) %>% tbl_xts()

df_Returns <- 
  dailydata.subset %>% spread(Stocks, Returns)

df_Returns[is.na(df_Returns)] <- 0
xts_df_Returns <- df_Returns %>% tbl_xts()

Rand_RetPort <- 
  rmsfuns::Safe_Return.portfolio(xts_df_Returns, 
                                 
                                 weights = Rand_weights, lag_weights = TRUE,
                                 
                                 verbose = TRUE, contribution = TRUE, 
                                 
                                 value = Fund_Size_at_Start, geometric = TRUE) 

EW_RetPort <- 
  rmsfuns::Safe_Return.portfolio(xts_df_Returns, 
                                 
                                 weights = EW_weights, lag_weights = TRUE,
                                 
                                 verbose = TRUE, contribution = TRUE, 
                                 
                                 value = Fund_Size_at_Start, geometric = TRUE) 

# Clean and save portfolio returns and weights:
Rand_Contribution <- 
  Rand_RetPort$"contribution" %>% xts_tbl() 

Rand_BPWeight <- 
  
  Rand_RetPort$"BOP.Weight" %>% xts_tbl() 

Rand_BPValue <- 
  
  Rand_RetPort$"BOP.Value" %>% xts_tbl()  

# Clean and save portfolio returns and weights:
EW_Contribution <- 
  EW_RetPort$"contribution" %>% xts_tbl() 

EW_BPWeight <- 
  EW_RetPort$"BOP.Weight" %>% xts_tbl()  

EW_BPValue <- 
  EW_RetPort$"BOP.Value" %>% xts_tbl()


names(Rand_Contribution) <- c("date", names(Rand_RetPort$"contribution"))
names(Rand_BPWeight) <- c("date", names(Rand_RetPort$"BOP.Weight"))
names(Rand_BPValue) <- c("date", names(Rand_RetPort$"BOP.Value"))

names(EW_Contribution) <- c("date", names(Rand_RetPort$"contribution"))
names(EW_BPWeight) <- c("date", names(Rand_RetPort$"BOP.Weight"))
names(EW_BPValue) <- c("date", names(Rand_RetPort$"BOP.Value"))

# Look at what these data.frames each convey - incredible right?

# Let's bind all of these together now:

df_port_return_Random <- 
  left_join(dailydata.subset %>% rename("date" = Date),
            Rand_BPWeight %>% gather(Stocks, weight, -date),
            by = c("date", "Stocks") ) %>% 
  
  left_join(.,
            Rand_BPValue %>% gather(Stocks, value_held, -date),
            by = c("date", "Stocks") ) %>% 
  
  left_join(.,
            Rand_Contribution %>% gather(Stocks, Contribution, -date),
            by = c("date", "Stocks"))

df_port_return_EW <- 
  left_join(dailydata.subset %>% rename("date" = Date),
            EW_BPWeight %>% gather(Stocks, weight, -date),
            by = c("date", "Stocks") ) %>% 
  
  left_join(.,
            EW_BPValue %>% gather(Stocks, value_held, -date),
            by = c("date", "Stocks") ) %>% 
  
  left_join(.,
            EW_Contribution %>% gather(Stocks, Contribution, -date),
            by = c("date", "Stocks"))

# Calculate Portfolio Returns:
df_Portf_Random <- 
  df_port_return_Random %>% group_by(date) %>% summarise(PortfolioReturn = sum(Returns*weight, na.rm =TRUE)) %>% 
  filter(PortfolioReturn != 0)

# Calculate Portfolio Returns:
df_Portf_EW <- 
  df_port_return_EW %>% group_by(date) %>% summarise(PortfolioReturn = sum(Returns*weight, na.rm =TRUE)) %>% 
  filter(PortfolioReturn != 0)




####  Volatility

dailydata <- fmxdat::DailyTRIs

colnames(dailydata) <- gsub(" SJ", "", colnames(dailydata) )

xts.data.dailyreturns <- 
  
  dailydata %>% 
  
  gather(Stock, TRI, -Date) %>% 
  
  group_by(Stock) %>% 
  
  mutate(Return = TRI / lag(TRI)-1) %>%  
  
  ungroup() %>% 
  
  tbl_xts(tblData = ., cols_to_xts = Return, spread_by = Stock)


### Question 3

dailydata <- fmxdat::DailyTRIs

colnames(dailydata) <- gsub(" SJ", "", colnames(dailydata) )

xts.data.dailyreturns <- 
  
  dailydata %>% 
  
  gather(Stock, TRI, -Date) %>% 
  
  group_by(Stock) %>% 
  
  mutate(Return = TRI / lag(TRI)-1) %>%  
  
  ungroup() %>% 
  
  tbl_xts(tblData = ., cols_to_xts = Return, spread_by = Stock)



## Question 6

# Create data 
data <- data.frame( var1 = 1:100 + rnorm(100,sd=20), v2 = 1:100 + rnorm(100,sd=27), v3 = rep(1, 100) + rnorm(100, sd = 1)) 
data$v4 = data$var1 ** 2 
data$v5 = -(data$var1 ** 2) 


# Nice visualization of correlations
ggcorr(data, method = c("everything", "pearson")) 
