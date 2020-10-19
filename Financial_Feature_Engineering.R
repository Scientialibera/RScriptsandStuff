### Author: EA
### Description:
# The purpose of this script is to highlight the usefulness of combining forecasting methods with machine learning 
# algorithms. In this case we only use a handful of tickers, but future implementations of the script will
# download the totality of tickers in and Index, select the top performing stock and forecast its predicted value
# over a desired span of time.

library(BatchGetSymbols)
library(ggplot2)
library(zoo)
library(TTR)
library(quantmod)
library(xts)
library(sweep)
library(rvest)
library(tidyverse)
library(stringr)
library(forecast)
library(timetk)
library(lubridate)
library(plotly)
library(dplyr)
library(tidyquant)
library(PerformanceAnalytics)

# Data and Feature Engineering

# In this first part we download the ticker information we are interested in (or a whole index) and
# build a data frame with various financial indicators that we must compute. These features will help us 
# select the top performing stock, we then forecast these stock to give us an idea of investment growth
# through time.

# Custom Functions

  # We want to center and scale our features and will use this function to achieve that goal.

scale_this <- function(x) as.vector(scale(x))

# Since we will be merging tables SQL style, we need a row i.d vector.

index <- c(1:400)

# Load data frame with tickers and other information. Choose columns of interest and combine in a single DF.
stockDf <- readRDS("Path to your stock data frame")

dateTck <- stockDf[601:1000, 2:1] # Remember to change your indexes as they are 
price <- stockDf[601:1000, 8:9]   # different than mine
dateTck <- cbind(index,dateTck, price)

# Since we will be working with a group of tickers and would like to apply our functions by group, we must 
# group by ticker name.

grpdStock <- group_by(dateTck, ticker)

# Initial time series graph of selected tickers.
ggplot(grpdStock, aes(x = ref.date, y = price.adjusted, color = ticker)) + geom_line() + scale_x_date()

# In order to feed information into our classification machine learning algorithms, we must create a table of features which
# will allow us to predict some useful characteristic. We compute several popular financial indicators.
Features <- mutate(grpdStock, Avg10 = runMean(price.adjusted, 10)) %>% 
  mutate(Avg20 = runMean(price.adjusted, 20)) %>% 
  mutate(Std10 = runSD(price.adjusted, 10)) %>% 
  mutate(Std20 = runSD(price.adjusted, 20)) %>% 
  mutate(Rsi5 = RSI(price.adjusted, 5, "SMA")) %>% 
  mutate(Rsi14 = RSI(price.adjusted, 14, "SMA")) %>% 
  mutate(Avg20Ret = runMean(ret.adjusted.prices, 20))

macd12269 <- grpdStock %>% do(macd = MACD(.$price.adjusted))
macd12269 <- do.call(rbind, macd12269$macd)

macd7205 <- grpdStock %>% do(macd = MACD(.$price.adjusted, 7, 20, 5, "SMA"))
macd7205 <- do.call(rbind, macd7205$macd)

bbands <- grpdStock %>% do(macd = TTR::BBands(.$price.adjusted, 20, "SMA",2))
bbands <- do.call(rbind, bbands$macd)

direction <- ifelse(Features$Avg20Ret > 0, 1, 0)

# In order to merge our data frames we must select an index, which we first created at the beginning of the script.
FinFeatures <- cbind(index, macd12269,macd7205, bbands, direction)
TotFeatures <- merge(Features, FinFeatures, by = "index")
ctrScl <- TotFeatures %>% 
  group_by(ticker) %>% 
  mutate(Avg10 = scale_this(Avg10),
         Avg20 = scale_this(Avg20),
         Std10 = scale_this(Std10),
         Std20 = scale_this(Std20),
         Rsi5 = scale_this(Rsi5),
         Rsi14 = scale_this(Rsi14),
         macd = scale_this(macd),
         signal = scale_this(signal),
         macd.1 = scale_this(macd.1),
         signal.1 = scale_this(signal.1),
         dn = scale_this(dn),
         mavg = scale_this(mavg),
         up = scale_this(up),
         pctB = scale_this(pctB))
         
remove(bbands, macd12269, macd7205, Features, FinFeatures, stockDf, dateTck, price, index)

# Selection of top performing stock

# Forecasting

# After selecting out top performing stock we can now proceed to forecast their at a certain point in time. 
# Since we are working with several ts at a time, we must nest each ticker in a table and only then can we
# apply our forecasting method.

tsTcks <- select(ctrScl, ref.date, ticker, price.adjusted)

tsNested <- tsTcks %>%
  group_by(ticker) %>% 
  nest()

dailyPricests <- tsNested %>% 
  mutate(data.ts = map(.x = data,
                       .f = tk_ts,
                       select = -ref.date,
                       start = 2020,
                       freq = 365))

# We can finally fi our model, in this case we opt for an ARIMA Model. Note that it will be added in the form of a 
# table, which mean we must unnest our table

tsfit <-  dailyPricests %>% 
  mutate(fit.arima = map(data.ts, forecast::auto.arima))

tsTidy <- tsfit %>% 
  mutate(tidy = map(fit.arima, sw_tidy)) %>% 
  unnest(tidy) %>% 
  spread(key = ticker, value = estimate)
  
# To see how well our model fits the data, we can plot the model residuals. 
augment_fit_arima <- tsfit %>%
  mutate(augment = map(fit.arima, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment)

augment_fit_arima %>% 
  
ggplot(aes(x = date, y = .resid, group = ticker)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Closing price by  Ticker",
       subtitle = "Arima Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~ ticker, scale = "free_y", ncol = 1) +
  scale_x_date(date_labels = "%Y")

# We are now ready to use our fitted models and forecast 30 days ahead

tsFcast <- tsfit %>% 
  mutate(fcast.arima = map(fit.arima, forecast, h = 30))
         
tsFcastClean <- tsFcast %>% 
  mutate(sweep = map(fcast.arima, sw_sweep, fitted = F, timetk_idx = T)) %>% 
  unnest(sweep)

# Let's plot our forecast and see how well our stocks will do next month (possibly!).

tsFcastClean %>% 
ggplot(aes(x = index, y = price.adjusted, color = key, group = ticker)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Ticker Price by firm",
       subtitle = "ETS Model Forecasts",
       x = "", y = "Units") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ ticker, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

