### Author: EA
### Description:
# This is a continuation of our quantitative finance series. Our first examples centered around downloading stock 
# from the American market. In this example we show love to our friends up north in Canada. At first one would think 
# that directly downloading .TO tickers would be as easy as parsing the list to our getSymbols function. However, 
# since the function uses the yahoo finance API we must spell the tickers as Yahoo has them in their database. 
# We exchange [:punct:] for "-" and add ".TO" at the end of all ticker. From there, the program should be familiar. We 
# pre-select some stock based off of its Information Ratio and we then create three portfolios, each with different 
# constraints.

library(tidyverse)
library(tidyquant)

set.seed(301)  

# Download and format tickers for Yahoo Finance API

doc <-htmltab::htmltab("https://en.wikipedia.org/w/index.php?title=S%26P/TSX_Composite_Index", 2)

doc <- as.vector(doc %>% select(Symbol))

rep_1 <-  gsub("[[:punct:]]", "-", doc$Symbol)
rep_2 <- as.vector(stringi::stri_paste(rep_1, ".TO"))

# Download tickers and get log returns
stock_returns_monthly_TSX <-  rep_2 %>%
tq_get(get  = "stock.prices",
from = "2020-04-01",
to   = "2020-10-30") %>%
group_by(symbol) %>%
tq_transmute(select     = adjusted, 
mutate_fun = periodReturn, 
period     = "daily",
type = "log", 
col_rename = "Ra")

# Baseline returns 

baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2020-04-01",
         to   = "2020-10-30") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               type = "log",
               period     = "daily", 
               col_rename = "Rb")
  
# We join our two data frames by date and then group them by symbol so we can analyze and pre-select.

RaRb_preselection <- left_join(stock_returns_monthly_TSX, 
                               baseline_returns_monthly,
                               by = "date") %>% group_by(symbol)

CAPM_stock <- RaRb_preselection %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM) 

# You can change this threshold or select another feature if you'd like 

stock_top_percentile <- CAPM_stock %>% filter(AnnualizedAlpha > quantile(CAPM_stock$AnnualizedAlpha , .80))

tickers <- stock_top_percentile$symbol

stock_returns_monthly_multi <- stock_returns_monthly_TSX %>%  filter(symbol %in% tickers) 

# The fastest way to analyze portfolios is to randomly create a bunch and see which ones seem like a good fit. We can of course
# then run performance checks and graph wealth growth.

hundredths <- c(1:100)
x <- sample(hundredths, size=length(tickers), replace=TRUE)
ind <- which(x %in% sample(x, length(tickers)*.72))
x[ind]<-0

refIndex <- cbind(tickers, x)

wts_map <- tibble(
  symbols = tickers,
  weights = x
) %>% 
  mutate(weights = weights / sum(weights))

# After randomly creating a portfolio with our top performing stock, we can check how it did against our reference Index.

portfolio_returns_monthly <- stock_returns_monthly_TSX %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra,  
               weights     = wts_map, 
               col_rename  = "Ra")


RaRb_single_portfolio <- left_join(portfolio_returns_monthly, 
                                   baseline_returns_monthly,
                                   by = "date")

RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = InformationRatio)
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = VaR)

# We can use a bar-plot and draw a linear regression to see returns trend

portfolio_returns_monthly %>%
  ggplot(aes(x = date, y = Ra)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Portfolio Returns",
       x = "", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

# It's be useful to see how a 10, 000 dollar investment would have grown through time.

portfolio_growth_monthly <- stock_returns_monthly_TSX %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = wts_map , 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

portfolio_growth_monthly %>%
  ggplot(aes(x = date, y = investment.growth)) +
  geom_line(size = 2, color = palette_light()[[1]]) +
  labs(title = "Portfolio Growth",
       x = "", y = "Portfolio Value") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

# Let's now repeat the same processes but this time with 100 randomly selected portfolios. 
# Here we perform our random vector creation and repeat the same steps as before.

set.seed(12)  
repetitions <- 100                ################### Filter by CAPM

stock_returns_monthly_multi <- stock_returns_monthly_multi %>%
  tq_repeat_df(n = repetitions)

xm <- sample(hundredths, length(tickers)*repetitions, replace=TRUE)
indm <- which(xm %in% sample(xm, length(tickers)*2.5))
xm[indm]<-0                       ############## MAKE WEIGHTS SUM TO ONE

refIndexM <- cbind(tickers, xm)

# The weights vector is now created for our defined number of portfolios.

weights_table <-  tibble(tickers) %>%
  tq_repeat_df(n = repetitions) %>%
  bind_cols(tibble(xm)) %>%
  group_by(portfolio) %>% 
  mutate(xm = xm / sum(xm))


portfolio_returns_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")

RaRb_multiple_portfolio <- left_join(portfolio_returns_monthly_multi, 
                                     baseline_returns_monthly,
                                     by = "date")

# Here we compute some useful financial indicators.

z <- RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)


RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio)

RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Stats)

Ar <- RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns)

RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.Correlation)

RaRb_multiple_portfolio %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.DownsideRisk)

# We can now plot the growth of all 100 portfolios

portfolio_growth_monthly_multi <- stock_returns_monthly_multi %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = weights_table, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

#############################################################

portfolio_growth_monthly_multi %>%
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio), legend.position = "none")) +
  geom_line(size = 2, legend.position = "none") +
  labs(title = "Quantitatively Optimized Portfolios",
       subtitle = "Comparing Multiple Portfolios",
       x = "", y = "Portfolio Value",
       color = "Portfolio",
       legend.position = "none") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "none")
scale_y_continuous(labels = scales::dollar)

# With help of the CAPM table (Variable z), choose the portfolio of interest and look it up on the key matrix table. Sort it by weights
# and you will see which stock has which weight. With this information in hand you are now quite well-informed and 
# ready to make some investments.

options(digits=2)
keymatrix <- weights_table %>% spread(key = portfolio, value = xm)