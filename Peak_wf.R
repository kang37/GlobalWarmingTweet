# Statement ----
# 基于小兔提供的每天的推文数量等数据，计算人均推文平均值超过一定基线的日期中，推文数量占总推文数的比例。
# 如果超过上述基线的日期连续天数超过3天，则称之为峰值时期，它们往往对应某些事件。本代码同时提取了峰值时期的原始推文，并分析其高频词，以此辅助确定峰值时期的话题和相关事件。

# Preparation ----
# use twarc2 in terminal to download Twitter raw data, an example for Jan of 2018: 
# twarc2 search "(#気候変動 OR #地球温暖化 OR #温暖化 OR #ClimateChange OR GlobalWarming OR 地球温暖化 OR 温暖化 OR 気候変動) lang:ja" --archive --start-time "2017-12-31T15:00:00" --end-time "2018-01-31T15:00:00" 201801.json
# should be noted the time above is not UTC time

# then turn the *.json to *.csv in terminal command line, which including from *.json to *.jsonl, then from *.jsonl to *.csv, an example for Jan of 2018: 
# twarc2 flatten 201801.json 201801.jsonl
# twarc2 csv --no-inline-referenced-tweets --output-columns "id,author.id,created_at,text,author.verified,author.username,author.name,conversation_id,public_metrics.like_count,public_metrics.quote_count,public_metrics.reply_count,public_metrics.retweet_count,referenced_tweets.replied_to.id,referenced_tweets.retweeted.id,referenced_tweets.quoted.id" 202101.jsonl 202101.csv

# I then deleted the original *.json and middle *.jsonl file to save storage

# Package ----
library(dplyr)
library(tidyr)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(lubridate)
library(stopwords)
library(parallel)
library(ggplot2)
library(showtext)

# Setting ----
showtext_auto()

# Read data ----
## Twitter stat data ----
# daily tweet number number, intensity, etc. 
var.raw <- read.csv("RawData/tw_Tdif_user12_22.csv") %>% 
  tibble() %>% 
  rename_with(tolower) %>% 
  rename(num_tw = agg, num_gw = refgw, num_cc = refcc, t_diff = jptdiff) %>% 
  mutate(date = as.Date(jstdate), 
         year = year(date), 
         month = month(date), 
         day = day(date), 
         t_diff_2 = t_diff^2) %>% 
  select(date, year, month, day, t_diff, t_diff_2, 
         num_tw, num_gw, num_cc, tw_perhk, gw_perhk, cc_perhk)
# calculate per use daily tweet num high value baseline 
var.raw.stat <- left_join(
  var.raw %>% 
    select(date, year, month, day, starts_with("num")) %>% 
    pivot_longer(
      cols = starts_with("num"), names_to = "dt_set", values_to = "num"
    ) %>% 
    mutate(dt_set = substr(dt_set, 5, 6)), 
  var.raw %>% 
    select(date, year, month, day, ends_with("perhk")) %>% 
    pivot_longer(
      cols = ends_with("perhk"), names_to = "dt_set", values_to = "perhk"
    ) %>% 
    mutate(dt_set = substr(dt_set, 1, 2))
) %>% 
  # add columns for peak period base lines for each scope 
  group_by(dt_set, year) %>% 
  mutate(int90 = quantile(perhk, 0.90)) %>% 
  ungroup()

# subset peak period stat data
var.peak <- var.raw.stat %>% 
  mutate(comp = perhk > int90) %>% 
  arrange(dt_set, year, month, day, comp) %>% 
  # bug: 只有在日期连续的情况下才可以用这个算法进行分组
  mutate(comp_oppo = !comp) %>% 
  mutate(grp = cumsum(comp_oppo)) %>% 
  subset(comp) %>% 
  group_by(grp) %>% 
  # get the max continuous day of each peak period 
  mutate(nth_day = row_number(), 
         high_days = max(nth_day)) %>% 
  ungroup() %>% 
  # identified as peak period if continuous date of high value > 3 days 
  subset(high_days >= 3) %>% 
  # rename peak period according to the start date 
  group_by(grp) %>% 
  mutate(
    year_min = min(year), 
    month_min = min(month), 
    day_min = min(day), 
    peak_grp = paste0(year_min, month_min, day_min)
  ) %>% 
  ungroup() %>% 
  select(-c(int90, comp_oppo, year_min, month_min, day_min, grp))

## Dictionary ----
dict <- dictionary(list(
  diversity = "生物 多様 性", 
  global_warming = "地球 温暖 化", 
  warming = "温暖 化", 
  asoushi = "麻生 氏", 
  asoutarou = "麻生 太郎", 
  climate_change = "気候 変動", 
  liberal_democratic_party = "自 民 党", 
  a = "砂漠 化", 
  a = "CO2", 
  a = "二酸化炭素", 
  a = "国 連", 
  a = "再 稼働", 
  a = "降 雨", 
  a = "降 雪", 
  a = "消費 税", 
  a = "安 倍", 
  a = "安倍 晋三", 
  a = "衆議 院", 
  a = "参議 院", 
  a = "米 大統領", 
  a = "ト ラ ン プ", 
  a = "熱中 症", 
  a = "電気 代", 
  a = "松岡 修造", 
  a = "外務 相", 
  a = "経産 省", 
  a = "経済 産業 省", 
  a = "史上 最大", 
  a = "環境 大臣", 
  a = "化石 賞", 
  a = "自 民", 
  a = "副 総裁", 
  a = "真 鍋", 
  a = "寒冷 化", 
  a = "真鍋 淑郎", 
  a = "真鍋 氏", 
  a = "真鍋 淑郎 氏"
))

## Twitter text data ----
# read raw tweet data
# *.csv files for raw data 
tw.file <- 
  list.files("RawData/Twitter")[grepl(".csv", list.files("RawData/Twitter"))]
tw.raw <- 
  mclapply(tw.file, ReadTwRaw, file.dir = "RawData/Twitter", mc.cores = 4) %>% 
  do.call(rbind, .) %>% 
  # turn to a corpus
  corpus(text_field = "text")

# test: can also read the cash file - faster, 27 secs
# readRDS("Tw_raw.rds")
# check top 2 rows
# summary(tw.raw, 2)
# bug: Why the unique id of tweet raw data less than rows of tweet raw data? Reason: some tweets belongs to different themes. 
# length(unique(tw.raw$id))
# ndoc(tw.raw)
# only keep the target date when peaks happen 
# bug: take all tweet (no GW or CC) as an example
# get tweet data of peak period for "global warming"
tw.peak.gw <- 
  corpus_subset(tw.raw, date %in% subset(var.peak, dt_set == "gw")$date)
# add peak period group name
docvars(tw.peak.gw) <- left_join(
  docvars(tw.peak.gw), 
  (subset(var.peak, dt_set == "gw") %>% select(date, peak_grp))
)

# get tweet data of peak period for "climate change"
tw.peak.cc <- 
  corpus_subset(tw.raw, date %in% subset(var.peak, dt_set == "cc")$date)
# add peak period group name
docvars(tw.peak.cc) <- left_join(
  docvars(tw.peak.cc), 
  (subset(var.peak, dt_set == "cc") %>% select(date, peak_grp))
)


## Token ----
toc.peak.gw <- TocPeak(tw.peak.gw)
docvars(toc.peak.gw) <- docvars(tw.peak.gw)
toc.peak.cc <- TocPeak(tw.peak.cc)
docvars(toc.peak.cc) <- docvars(tw.peak.cc)

## DTM ----
dfm.peak.gw <- dfm(toc.peak.gw) %>% 
  dfm_remove(pattern = stopwords("ja", source = "marimo")) %>% 
  dfm_remove(pattern = "")
dfm.peak.cc <- dfm(toc.peak.cc) %>% 
  dfm_remove(pattern = stopwords("ja", source = "marimo")) %>% 
  dfm_remove(pattern = "")

# Analysis ----
## Dates with continue high value days ----
# 查看高值在3个口径中的重合情况
var.peak %>% 
  subset(dt_set %in% c("gw", "cc")) %>% 
  # 添加作图辅助日期
  mutate(date_dummy = as.Date(paste("9999", month, day, sep = "-"))) %>% 
  ggplot() + 
  geom_tile(aes(date_dummy, dt_set), fill = "darkred") + 
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        panel.grid = element_blank()) +
  scale_x_date(date_labels = "%b") + 
  scale_y_discrete(breaks = c("tw", "gw", "cc"), 
                   labels = c("Total", "Global Warming", "Climate Change")) + 
  labs(x = "Date", y = "Dataset") + 
  facet_wrap(.~ year, ncol = 1, strip.position = "right")

## High-freq words of peak period ----
# get high freq words of each peak period 
# bug: need improvement - tokens_compound() and change object names
PltHighFreqWord(dfm.peak.gw, "GW")
PltHighFreqWord(dfm.peak.cc, "CC")
# test
# check meaningless words and improve the above figure 
# corpus_subset(tw.peak, peak_grp == "201211") %>% 
#   kwic(pattern = "本")

## Stat plot ----
# Tweet num int for GW and CC ----
var.raw %>% 
  select(date, gw_perhk, cc_perhk) %>% 
  pivot_longer(cols = c(gw_perhk, cc_perhk), 
               names_to = "dt_set", values_to = "val") %>% 
  ggplot() + 
  geom_line(aes(date, val, col = dt_set)) + 
  scale_color_discrete(
    name = "Data set", 
    limits = c("gw_perhk", "cc_perhk"), 
    labels = c("Global Warming", "Climate Change")
  ) + 
  theme(legend.position = c(0.2, 0.8)) + 
  labs(x  = "", y = "Tweet number per 100000 user")

# Tweet num int and Temperature anomaly ----
# tweet number and temperature anomaly change for 2020 before-spring and 2022 summer
dt.plt <- list(subset(var.raw, year == 2020 & month == 1), 
               subset(var.raw, date < "2022-07-15" & date > "2022-06-16"))
names(dt.plt) <- c("2020_b4spring", "2022_summer")
for (i in c(1:2)) {
  # GW tweet num ~ temperature diff or its square 
  PlotColLine(x = dt.plt[[i]], 
              file = names(dt.plt)[[i]], 
              var_col = "gw_perhk", var_line = "t_diff", times = 10, 
              name_y1 = "Global warming tweet number  \nper 100000 users", 
              name_y2 = "Temperature anomaly (°C)")
  PlotColLine(x = dt.plt[[i]], 
              file = names(dt.plt)[[i]], 
              var_col = "gw_perhk", var_line = "t_diff_2", times = 1, 
              name_y1 = "Global warming tweet number  \nper 100000 users", 
              name_y2 = "Temperature anomaly square (°C^2)")
  # CC tweet num ~ temperature diff or its square 
  PlotColLine(x = dt.plt[[i]], 
              file = names(dt.plt)[i], 
              var_col = "cc_perhk", var_line = "t_diff", times = 0.25, 
              name_y1 = "Climate change tweet number  \nper 100000 users", 
              name_y2 = "Temperature anomaly (°C)")
  PlotColLine(x = dt.plt[[i]], 
              file = names(dt.plt)[[i]], 
              var_col = "cc_perhk", var_line = "t_diff_2", times = 0.1, 
              name_y1 = "Climate change tweet number  \nper 100000 users", 
              name_y2 = "Temperature anomaly square (°C^2)")
}

for (i in c("gw_perhk", "cc_perhk")) {
  for (j in c("t_diff", "t_diff_2")) {
    cat("----------\n")
    cat("\n", i, j)
    a <- var.raw %>% 
      subset(year == 2020 & month == 1)
    cor.test(a[[i]], a[[j]]) %>% print()
  }
}



