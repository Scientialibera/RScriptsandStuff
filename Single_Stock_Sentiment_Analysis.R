### Author: EA
### Description:
# This is a small program I built for proof of concept. While you do need to feed a pre-downloaded html file into the algorithm, this will be solved by using RSelenium instead of rvest. 
# Moreover, using tibbles means scalability shouldn't be too much of a problem. Fingers crossed.
library(rvest)
library(tidytext)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyverse)

myURL2 <-  "" #Insert here the pre-downloaded yahoo finance html news page
df2 <- myURL2 %>% 
  read_html() 

text <-df2 %>% html_nodes("li p") %>% #Do check these indexes correspond to the news article body
  html_text() 

text_df <- tibble(line = 1:length(text), text = text)

tidy_df <-  text_df %>% 
  unnest_tokens(word, text)

news_Sentiment <- tidy_df %>% 
inner_join(get_sentiments("bing")) %>% 
  count(index = line, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

hist(news_Sentiment$sentiment, xlab = "Sentiment" , main = "Histogram Facebook Sentiment Analysis")
