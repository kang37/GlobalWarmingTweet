# Package ----
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(showtext)
showtext_auto()

# Data ----
case <- read.csv("RawData/ForKansaiConf/case202001.csv") %>% 
  as_tibble()
tw.agg <- read.csv("RawData/ForKansaiConf/twAgg201201_202112.csv") %>% 
  as_tibble()

# Analysis ----
par(mfrow = c(3, 1))
plot(case$refGW, type = "l")
plot(case$refCC, type = "l")
plot(case$temp_anom, type = "l")
par(mfrow = c(1, 1))

case %>% 
  select(temp_anom, refGW, refCC) %>% 
  chart.Correlation()

par(mfrow = c(2, 1))
plot(tw.agg$GW2CC, type = "l")
plot(tw.agg$verGW2CC, type = "l")
par(mfrow = c(1, 1))

plot(tw.agg$GW2CC, tw.agg$verGW2CC)
