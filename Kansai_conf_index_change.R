# Package ----
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(patchwork)
library(showtext)
showtext_auto()

# Analysis ----
## Index change of warm winter 2020 ----
# 读取数据
case.2020 <- read.csv("RawData/ForKansaiConf/case202001.csv") %>% 
  as_tibble()
tw.agg <- read.csv("RawData/ForKansaiConf/twAgg201201_202112.csv") %>% 
  as_tibble()

# 作图
par(mfrow = c(3, 1))
plot(case.2020$refGW, type = "l")
plot(case.2020$refCC, type = "l")
plot(case.2020$temp_anom, type = "l")
par(mfrow = c(1, 1))

case.2020 %>% 
  select(temp_anom, refGW, refCC) %>% 
  chart.Correlation()

par(mfrow = c(2, 1))
plot(tw.agg$GW2CC, type = "l")
plot(tw.agg$verGW2CC, type = "l")
par(mfrow = c(1, 1))

plot(tw.agg$GW2CC, tw.agg$verGW2CC)

## Index change of winter in 2021 ----
case.2021 <- read.csv("RawData/ForKansaiConf/case202110_11.csv") %>% 
  as_tibble() %>% 
  # 提取日期列
  mutate(date = as.Date(substr(JSTdate, 1, 10)))

png("ProcData/ForKansaiConf/Tweetnum_change_202110_202111.png", 
    width = 900, height = 500, res = 150)
(ggplot(case.2021) + 
  geom_line(aes(x = date, y = refGW))) / 
  (ggplot(case.2021) + 
     geom_line(aes(x = date, y = refCC))) 
dev.off()

png("ProcData/ForKansaiConf/Rate_change_202110_202111.png", 
    width = 900, height = 500, res = 150)
(ggplot(case.2021) + 
    geom_line(aes(x = date, y = GW2CC))) / 
  (ggplot(case.2021) + 
     geom_line(aes(x = date, y = verGW2CC))) 
dev.off()

## Monthly index change from 2012-2021 ----
nort.gw.cc <- read.csv("RawData/ForKansaiConf/twAgg201201_202112_nort.csv") %>% 
  as_tibble()
png("ProcData/ForKansaiConf/Nort_gw_cc_rate_2012_2021.png", 
    width = 900, height = 500, res = 150)
ggplot(nort.gw.cc) + 
  geom_line(aes(1 : nrow(nort.gw.cc), NoRT_GWtoCC)) + 
  labs(x = "")
dev.off()

nort <- read.csv("RawData/ForKansaiConf/twCount201201_202112_nort.csv") %>% 
  as_tibble()
png("ProcData/ForKansaiConf/Nort_gw_cc_2012_2021.png", 
    width = 900, height = 500, res = 150)
(ggplot(nort) + 
    geom_line(aes(1 : nrow(nort.gw.cc), NoRT_GW_sum)) + 
  labs(x = "")) / 
  (ggplot(nort) + 
     geom_line(aes(1 : nrow(nort.gw.cc), NoRT_CC_sum)) + 
     labs(x = ""))
dev.off()
