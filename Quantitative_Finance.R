### Author: EA
### Description:
# General financial and time series analysis for investment optimization.
# ***NOTE this script is intended for educational purposes only and is not an investment tool.

library(BatchGetSymbols)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
dfTickers <- GetSP500Stocks()
tickers <- dfTickers$Tickers

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = "2020-01-01",
                         last.date = "2020-10-16", 
                         freq.data = "daily",
                         thresh.bad.data = 1,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )
tck <- l.out$df.tickers[,7:8]
priceAdjusted <- l.out$df.tickers$price.adjusted
tsAdjusted <- cbind(tck, priceAdjusted)

tsPlot <- melt(tsAdjusted, "ref.date")

ggplot(tsAdjusted, aes(x = ref.date, y = priceAdjusted, color = ticker, show.)) + geom_line() + scale_x_date() + theme(legend.position="none")



