# 加载包。
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# 函数：用于从手动下载的每月的新闻文本中提取所需数据。
extract_news_data <- function(dir_x) {
  # 读取文档内容。
  res <- read_file(dir_x) 
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
lapply(
  paste0("data_raw/", c("hs201201.txt", "hs201202.txt", "hs201203.txt")), 
  function(x) extract_news_data(x) %>% write.csv(gsub("txt", "csv", x))
)
