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


