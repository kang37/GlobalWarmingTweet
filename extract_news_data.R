# Preparation ----
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Extract data ----
# 函数：用于从手动下载的每月的新闻文本中提取所需数据。
extract_news_data <- function(file_x) {
  # 读取文档内容。
  res <- read_file(paste0("data_raw/txt/", file_x)) 
  # 构建结果数据框。
  res <- 
    # 按照文章分割文本。
    data.frame(raw = unlist(strsplit(res, "\nNo.", fixed = TRUE))) %>% 
    # 提取各列信息。
    mutate(
      lines = strsplit(raw, "\n"), 
      no = lapply(lines, function(x) str_extract(x[1], "\\d+")) %>% unlist(), 
      date = lapply(lines, function(x) x[2]) %>% unlist(), 
      ampm = lapply(lines, function(x) x[3]) %>% unlist(), 
      sec = lapply(lines, function(x) x[4]) %>% unlist(), 
      page_char = lapply(lines, function(x) x[5]) %>% unlist(), 
      headline = lapply(lines, function(x) x[6]) %>% unlist(), 
      text = 
        lapply(lines, function(x) paste(x[7:length(x)], collapse = "\n")) %>% 
        unlist()
    ) %>% 
    separate(page_char, into = c("page", "char_count"), sep = ",") %>% 
    select(-raw, -lines) %>% 
    mutate(
      page = as.numeric(gsub("ページ", "", page)), 
      char_count = as.numeric(gsub("文字", "", char_count))
    )
  # 返回结果。
  return(res)
}

# 转化提取并导出结果。
list.files("data_raw/txt/") %>% 
  lapply(
    ., 
    function(x) {
      extract_news_data(x) %>% 
        write.csv(paste0("data_proc/csv/", gsub("txt", "csv", x)))
    }
  )

# Analysis ----
# 构建分析数据。
ash <- list.files("data_raw/txt/") %>% 
  lapply(., function(x) extract_news_data(x)) %>% 
  bind_rows() %>% 
  mutate(year = substr(date, 1, 4), month = substr(date, 6, 7)) %>% 
  filter(!is.na(no)) %>% 
  # Bug: Remove 2012 data for now. 
  filter(year != "2012")

# 每年新闻数量变化。
ash %>% 
  group_by(year) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot() + 
  geom_col(aes(year, n)) + 
  labs(x = NULL, y = "Tweet number") + 
  theme_bw()

# 每个月新闻数量变化。
ash %>% 
  group_by(year, month) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  ggplot() + 
  geom_line(aes(month, n, col = year, group = year)) + 
  labs(x = "Month", y = "Tweet number", col = "Year") + 
  theme_bw()
