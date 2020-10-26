### Author: EA
### Description:
# The purpose of this script is to show how one can download a set of tickers, or if need be all of the 
# tickers in an Index, perform some preliminary analysis to select the best options with regards to risk 
# and forecast price 30 days into the future. The secondary purpose of this script is to get comfortable
# using tibbles when working with lots of data since it makes everything tidy -er! (Haha). As always,
# this script is for educational proposes and shouldn't replace a financial adviser. 

library(BatchGetSymbols)
library(ggplot2)
library(zoo)
library(TTR)
library(quantmod)
library(xts)
library(sweep)
library(rvest)
library(tidyverse)
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

# Updated list of stock in the S&P500 Index
dfTickers <- GetSP500Stocks()
tickers <- dfTickers$Tickers

sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_node("table.wikitable") %>%
  html_table() %>% 
  select(ticker = Symbol , Sector = `GICS Sector`, Subsector = `GICS Sub-Industry`) %>%
  as_tibble() %>% 
  group_by(ticker) %>% 
  nest()

# Get symbols from yahoo finance
batchSymbols <- BatchGetSymbols::BatchGetSymbols(tickers = tickers, 
                                                 first.date = "2020-01-01",
                                                 last.date = "2020-10-16",
                                                 thresh.bad.data = 1, 
                                                 freq.data = "daily",
                                                 do.complete.data = T,
                                                 do.fill.missing.prices = T,
                                                 cache.folder = file.path(tempdir(), 
                                                                          'BGS_Cache') )
remove(tickers, dfTickers)

# Make a table with the columns we need, in hour case, date, ticker, adjusted price and adjusted geometric return.
tickerTable <- batchSymbols$df.tickers %>%  select(ref.date, ticker, price.adjusted, ret.adjusted.prices) %>% 
  group_by(ticker)

# To apply our log returns functions per group, we must first group by ticker, then use the do function to
# get our values per group.
logreturn <- tickerTable %>% 
  group_by(ticker) %>% 
  do(logret = c(diff(log(.$price.adjusted)), 0)) %>% 
  unnest(cols = c(logret))
logreturn <- as.matrix(logreturn$logret)

# Now we can work on our data as a tibble, which has tables as single entries in some cases. However,
# the mean and std function will result in a single value.

tickerTable <- as_tibble(cbind(tickerTable, logreturn = logreturn))

nestedTable <- tickerTable %>%
  group_by(ticker) %>% 
  nest() %>% 
  mutate(lreturns = map(.x = data,
                         .f = tk_ts,
                         select = logreturn,
                         start = 2020,
                         freq = 365))

nestedTable <- merge(nestedTable, sp_500, by = "ticker")

# Here we apply our two functions to all of our groups.
MeanStd <-  nestedTable %>% 
  mutate(MeanLogReturns = map_dbl(lreturns, mean),
         StdLogReturns = map_dbl(lreturns, sd))

# For our third indicator we compute the coefficient of variation, or Risk/Earnings ratio.
MeanStd <- MeanStd %>% 
  mutate(CV = map2_dbl(StdLogReturns, MeanLogReturns, ~ .x / .y ))

# Top quartile highest earning stocks
topReturn <- MeanStd %>% filter(MeanLogReturns > quantile(MeanStd$MeanLogReturns, .75))

# Top quartile lowest risk stocks
topLRisk <- MeanStd %>% filter(abs(StdLogReturns) < quantile(MeanStd$StdLogReturns, .25))

# Top quintile lowest risk/earning ratio
topREratio <- MeanStd %>% filter(MeanLogReturns > 0)
topREratio1 <- filter(topREratio, CV < quantile(topREratio$CV, .3))

# Information about the sector of our tickers
grpsectr <- topREratio1 %>% unnest(cols = data.y) %>% 
  group_by(Sector) %>% 
  summarise(count = n())

  ggplot(grpsectr, aes(x = Sector,
             y = count
  )) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), size = 3, nudge_y = 4, nudge_x = .1) + 
  scale_y_continuous(limits = c(0,100)) +
  ggtitle(label = "Sector Frequency Among SP500 Stocks") +
  xlab(label = "GICS Sector") +
  theme(plot.title = element_text(size = 16)) + 
  coord_flip() 
  

# Forecasting

# After selecting out top performing stock we can now proceed to forecast their at a certain point in time. 
# Since we are working with several ts at a time, we must nest each ticker in a table and only then can we
# apply our forecasting method. We will forecast our selection of top lowest risk/reward.

tibbleDta <- topREratio1 %>% unnest(cols = c(data.x, lreturns, data.y)) %>% select(ref.date, ticker, price.adjusted) %>% 
  group_by(ticker) %>% 
  nest()

fcastDta <- tibbleDta %>% 
  mutate(data.ts = map(.x = data,
                       .f = tk_ts,
                       select = -ref.date,
                       start = 2020,
                       freq = 365))

# We can finally fit our model, in this case we opt for an ARIMA Model. Note that it will be added in the form of a 
# table, which means we must unnest our table

tsfit <-  fcastDta %>% 
  mutate(fit.arima = map(data.ts, forecast::auto.arima))

tsTidy <- tsfit %>% 
  mutate(tidy = map(fit.arima, sw_tidy)) %>% 
  unnest(tidy) %>% 
  spread(key = ticker, value = estimate)

########################################################################## 

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
  facet_wrap(~ ticker, scale = "free_y", ncol = 10) +
  scale_x_date(date_labels = "%Y")

# We are now ready to use our fitted models and forecast 30 days ahead

fcastSeries <- tsfit %>% 
  mutate(fcast.arima = map(fit.arima, forecast, h = 30))
         
tsFcastClean <- fcastSeries %>% 
  mutate(sweep = map(fcast.arima, sw_sweep, fitted = F, timetk_idx = T)) %>% 
  unnest(sweep)

# Let's plot our forecast and see how well our stocks will do next month (possibly!).
w <- tsFcastClean %>%  select(ticker, key, price.adjusted, index, lo.80, hi.80, lo.95, hi.95) %>% 
  group_by(ticker) %>% 
  filter()


w %>% 
ggplot(aes(x = index, y = price.adjusted, color = key, group = ticker)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Ticker Price by firm",
       subtitle = "ETS Model Forecasts",
       x = "", y = "Units",
       size = 3) +
  scale_x_date(limit=c(as.Date("2020-09-11"),as.Date("2020-11-14"))) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We can also plot a risk/earnings graph to see how groups are clustered

plotTable <- topREratio1 %>%  unnest(cols = c(data.x, lreturns, data.y))

plotly::plot_ly(data   = plotTable,
                type   = "scatter",
                mode   = "markers",
                x      = ~ StdLogReturns ,
                y      = ~ MeanLogReturns,
                colors = "Blues",
                text   = ~ str_c(
                  "Ticker: ", ticker, "<br>",
                  "Sector: ", Sector, "<br>",
                  "Sub Sector: ", Subsector, "<br>") ,
                marker = list(opacity = 0.8,
                              symbol = 'circle',
                              sizemode = 'diameter',
                              sizeref = 4.0,
                              line = list(width = 2, color = '#FFFFFF'))
) %>%
  layout(title   = 'S&amp;P500 Analysis: Stock Risk vs Reward',
         xaxis   = list(title = 'Risk/Variability (StDev Log Returns)',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwidth = 2),
         yaxis   = list(title = 'Reward/Growth (Mean Log Returns)',
                        gridcolor = 'rgb(255, 255, 255)',
                        zerolinewidth = 1,
                        ticklen = 5,
                        gridwith = 2),
         margin = list(l = 100,
                       t = 100,
                       b = 100),
         font   = list(color = '#FFFFFF'),
         paper_bgcolor = 'rgb(0, 0, 0)',
         plot_bgcolor = 'rgb(0, 0, 0)')

# The final step id to select the tickers which are least correlated to each other as to avoid fatal group risk.

topSpread <- plotTable %>% select(ticker, ref.date, lreturns) %>% 
  spread(key = ticker, value = lreturns) %>% 
  na.omit()

corrPlot <- topSpread %>% 
  select(-ref.date) %>% 
  cor()

corrPlot %>% corrplot::corrplot( order = "hclust",
                                 addrect = 11)

col<- colorRampPalette(c("blue", "white", "red"))(50)
heatmap(x = corrPlot, col = col, symm = TRUE)

corrTable <- data.table::setDT(reshape2::melt(corrPlot))[Var1 != Var2][order(value)]

head(corrTable)

