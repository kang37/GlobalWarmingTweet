# Preparation ----
pacman::p_load(
  readr, stringr, lubridate, DescTools, dplyr, tidyr, stopwords, topicmodels,
  tidytext, quanteda, quanteda.textstats, LSX, ggplot2, showtext
)
showtext_auto()

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
# list.files("data_raw/txt/") %>% 
#   lapply(
#     ., 
#     function(x) {
#       extract_news_data(x) %>% 
#         write.csv(paste0("data_proc/csv/", gsub("txt", "csv", x)))
#     }
#   )

# Number analysis ----
# 构建分析数据。
ash <- list.files("data_raw/txt/") %>% 
  lapply(., function(x) extract_news_data(x)) %>% 
  bind_rows() %>% 
  mutate(year = substr(date, 1, 4), month = substr(date, 6, 7)) %>% 
  filter(!is.na(no)) %>% 
  # Bug: Remove 2012 data for now. 
  filter(year != "2012") %>% 
  mutate(id = paste(year, month, no, sep = "-")) %>% 
  # Remove duplicated entries. 
  mutate(dup = duplicated(id)) %>% 
  filter(!dup)

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
  labs(x = "Month", y = "News number", col = "Year") + 
  theme_bw()

# 每个月字数变化。
ash %>% 
  group_by(year, month) %>% 
  summarise(char_count = sum(char_count), .groups = "drop") %>% 
  ggplot() + 
  geom_line(aes(month, char_count, col = year, group = year)) + 
  labs(x = "Month", y = "News character number", col = "Year") + 
  theme_bw()

# 条数和字数的相关性。
news_mth_n_char <- left_join(
  ash %>% 
    group_by(year, month) %>% 
    summarise(char_count = sum(char_count), .groups = "drop"), 
  ash %>% 
    group_by(year, month) %>% 
    summarise(n = n(), .groups = "drop"), 
  by = c("year", "month")
)
news_mth_n_char %>% 
  ggplot() + 
  geom_point(aes(char_count, n, col = year), alpha = 0.7) + 
  labs(x = "News number", y = "Character count") + 
  theme_bw()
cor.test(news_mth_n_char$n, news_mth_n_char$char_count)

# Text mining data ----
# 停止词：在分词之后去除的不重要的日语词汇。
jp_stop_word <- tibble(
  word = c(
    stopwords("ja", source = "marimo"), 
    # Hiragana in Japanese: define Unicode code points for hiragana characters and convert code points to UTF-8 characters. 
    strsplit(intToUtf8(c(12353:12435)), "")[[1]], 
    "amp", "ます", "です", "こと", "って", "てい", "という", "んで", "ので", 
    "なく", "など", "なる", "せん", "しま", "とか", "しょう", "ろう", "けど", 
    "さん", "あっ", "られる", "ぜひ", "てる", "なら", "思い", "思う", "れる", 
    "たく", "なので", "ただ", "ほうが", "もの", "かも", "たら", "そう", " ",
    "いと", "とも", "どちら", "にし", "しく", "しか", "しな", "すぎ", "ほしい", 
    "おい", "なか"
  )
)

# Stopwords for sentiment analysis, inherit from last part. . 
quan_jp_stop_word <- jp_stop_word$word

# Corpus. 
quan_corp <- corpus(ash, text_field = "text")

# 自定义词典。
quan_dict <- 
  dictionary(list(
    global_warming_1 = "地球　温暖　化", 
    global_warming_1 = "地球　温暖化"
  ))

# Tokenization. 
quan_tok <- 
  tokens(
    quan_corp, 
    remove_symbols = TRUE, 
    remove_numbers = TRUE, 
    remove_url = TRUE, 
    remove_separators = TRUE, 
    remove_punct = TRUE
  ) %>% 
  # Bug: Should delete text with just a few words? 
  tokens_compound(pattern = quan_dict, concatenator = "") %>% 
  # Keep tokens in Japanese. 
  # Bug: Need to keep the words in other language? 
  tokens_select(
    pattern = "^[ぁ-んァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE
  ) %>% 
  tokens_remove(pattern = quan_jp_stop_word)

# Text ID. 
text_id <- tibble(text = names(quan_tok), id = docvars(quan_tok)$id)

# Topic analysis ----
# 生成term-document矩阵。
quan_dtm <- dfm(quan_tok)
quan_dtm_tm <- convert(quan_dtm, to = "tm")

# 要选取几个主题呢？当话题数量太少时，大部分人的回答属于各个主题的概率差不多，这显然不合理；主题数很多时，各个回答属于各个话题的概率会更加有区分度，但是主题也不应太多。
# 函数：基于自定义主题数量，获得各个文档属于不同主题的概率。
quan_test_k_topic <- function(k_x) {
  # 生成LDA数据。
  lda <- LDA(quan_dtm_tm, k_x, control = list(seed = 1234))
  
  # 各篇文章属于各个主题的概率。
  id_topic_res <- tidy(lda, matrix = "gamma") %>% 
    mutate(k = k_x)
  return(id_topic_res)
}
# 测试主题数量思路：如果区分度越高的话，一个文档被划分到各个主题下的概率就越离散，基尼系数就越高。所以，可以给定一定范围的自定义主题数量，计算不同主题数量下，各个文档被划分到各个主题中的概率。看在那个自定义主题数量下，平均基尼系数最高，或者看看基尼系数在什么时候突变。
# 要测试的自定义主题数量范围。
# range_k <- 2:8
# 存储测试结果。
# quan_id_topic_test <- 
#   lapply(range_k, quan_test_k_topic) %>% 
#   setNames(as.character(range_k)) %>% 
#   bind_rows() %>% 
#   left_join(text_id, by = c("document" = "text"))
# 计算基尼系数之前，先直观观察不同主题下文档主题划分的区分度。如果区分度越高，格子之间的颜色差异就越明显。
# quan_id_topic_test %>% 
#   bind_rows() %>% 
#   ggplot() + 
#   geom_tile(aes(document, as.integer(topic), fill = gamma)) + 
#   scale_fill_gradient(high = "red", low = "green") + 
#   theme(axis.text.x = element_blank()) + 
#   facet_wrap(.~ k, scales = "free_y")
# 计算基尼系数并比较不同自定义主题数量下基尼系数的差异。
# quan_id_topic_test %>% 
#   group_by(k, document) %>% 
#   summarise(gini = Gini(gamma), .groups = "drop") %>% 
#   mutate(k = factor(k, levels = as.character(range_k))) %>% 
#   ggplot() + 
#   geom_boxplot(aes(k, gini), alpha = 0.5) + 
#   theme_bw() + 
#   labs(x = "Topic number", y = "Gini")

# 正式进行主题模型分析。
# 获得测试范围内的最佳自定义主题数量：基尼系数最大，区分度最高。
# 计算突变点：平均基尼系数突然增加的点对应的主题数，就是目标主题数。
# quan_gini_chg_rate <- quan_id_topic_test %>% 
#   group_by(k, document) %>% 
#   summarise(gini = Gini(gamma), .groups = "drop") %>% 
#   group_by(k) %>% 
#   summarise(gini = mean(gini), .groups = "drop") %>% 
#   mutate(
#     gini_lag = lag(gini), gini_mean_chg_rate = (gini - gini_lag) / gini_lag
#   )
# ggplot(quan_gini_chg_rate) + 
#   geom_point(aes(k, gini_mean_chg_rate))
# (
#   quan_tar_k <- quan_gini_chg_rate %>% 
#     filter(gini_mean_chg_rate == max(gini_mean_chg_rate, na.rm = TRUE)) %>% 
#     pull(k)
# )

# Bug: 手动选择主题数量。
quan_tar_k <- 6

# 构建LDA数据。
quan_lda <- LDA(quan_dtm_tm, k = quan_tar_k, control = list(seed = 1234))

# 评估区分度。
# 每个回答属于各个主题的概率。
quan_id_topic <- tidy(quan_lda, matrix = "gamma") %>% 
  # 计算基尼系数。
  rename(text = document) %>% 
  group_by(text) %>% 
  mutate(gini = Gini(gamma)) %>% 
  ungroup() %>% 
  # 区分回答：有偏向型的还是均衡型的。基尼系数越高，回答越偏向某个主题，系数越低，回答和多个主题相关的可能性越大。
  mutate(gini_cls = case_when(
    gini <= quantile(gini, 1/3) ~ "1", 
    gini <= quantile(gini, 2/3) ~ "2", 
    gini <= quantile(gini, 3/3) ~ "3"
  )) %>% 
  left_join(text_id, by = "text") %>% 
  left_join(ash %>% select(id, date), by = "id")
# 基尼系数越高的组，概率越离散。
quan_id_topic %>% 
  ggplot() + 
  geom_tile(aes(text, topic, fill = gamma)) + 
  facet_wrap(.~ gini_cls, scales = "free_x", ncol = 1) + 
  theme(axis.text.x = element_blank()) + 
  scale_fill_gradient(high = "red", low = "green")

# 转化成可阅读的主题数据，并取每个主题的前几位关键词。
# 漏洞：需要再增加停止词，并且统一日语词汇如“鹿”和“しか”。
(
  topic_word <- tidy(quan_lda, matrix = "beta") %>% 
    # Bug: Why there are empty term?
    filter(term != "") %>% 
    group_by(topic) %>% 
    slice_max(beta, n = 30) %>% 
    mutate(term = reorder_within(term, beta, topic)) %>% 
    summarise(term = list(term), .groups = "drop") %>% 
    mutate(
      term = unlist(lapply(term, function(x) paste0(x, collapse = ", ")))
    ) %>% 
    mutate(term = gsub("_", "", term), term = gsub("[0-9]", "", term))
)
# 导出结果。
openxlsx::write.xlsx(
  topic_word, 
  paste0("data_proc/topic_word_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
)

# 抽出和各个主题匹配度最高的前10条回答，解读各个主题的含义。
topic_text <- quan_id_topic %>% 
  group_by(topic) %>% 
  arrange(topic, -gamma) %>% 
  slice_head(n = 5) %>% 
  ungroup() %>% 
  # 漏洞：需要提前更改id的类型。
  left_join(
    ash %>% 
      rename(text_content = text) %>% 
      left_join(rename(text_id, text_id = text), by = "id"), 
    by = c("text" = "text_id", "id")
  ) %>% 
  select(id, topic, gamma, headline, text, text_content) %>% 
  # 选取每篇文章正文内容的前400个字。
  mutate(text_content = substr(text_content, 1, 400))
View(topic_text)

# 2022年1-6月每天各主题显著度变化。
quan_id_topic %>% 
  mutate(
    year = substr(date, 1, 4), 
    month = substr(date, 6, 7) %>% as.numeric(), 
    day = substr(date, 9, 10), 
    date = as_date(paste(year, month, day, sep = "-"))
  ) %>% 
  filter(year == "2022", month <= 6) %>% 
  group_by(date, topic) %>% 
  summarise(gamma = sum(gamma), .groups = "drop") %>% 
  group_by(date) %>% 
  mutate(tot_gamma = sum(gamma), gamma_score = gamma / tot_gamma) %>% 
  ggplot() + 
  geom_line(aes(date, gamma_score, col = as.character(topic), group = topic)) + 
  facet_wrap(.~ topic)

# LSS ----
# Seed words for LSS model. 
seed_word <- 
  c(rep(1, 12), rep(-1, 11)) %>% 
  setNames(c(
    c("恵み", "絶賛", "創出", "改善", "配慮", 
      "対話", "効果的", "支持", "機会", "成功",  
      "効果", "成功例"), 
    c("破壊", "危険", "壊滅", "被害", "貧相", 
      "酷評", "悪徳", 
      "危険性", "壊滅的", "農業被害", "豪雨被害")
  ))

context_word <- char_context(quan_tok, "温暖化", p = 0.05)

# LSS model. 
lss <- 
  textmodel_lss(
    quan_dtm, 
    seeds = seed_word, 
    # terms = context_word, 
    k = 300, 
    include_data = TRUE, 
    group_data = TRUE
  )
# 词语极性。
textplot_terms(lss)

# 情感得分。
lss_score <- 
  docvars(quan_dtm) %>% 
  mutate(fit = predict(lss, newdata = quan_dtm))

lss_score_smooth <- 
  lss_score %>% 
  mutate(
    year = substr(date, 1, 4), 
    month = substr(date, 6, 7) %>% as.numeric(), 
    day = substr(date, 9, 10), 
    date = as_date(paste(year, month, day, sep = "-"))
  ) %>% 
  select(date, fit) %>% 
  smooth_lss(., engine = "locfit", span = 0.2) %>% 
  rename("se" = "se.fit")

ggplot(lss_score_smooth) + 
  geom_line(aes(date, fit)) + 
  geom_ribbon(aes(x = date, ymin = fit - se, ymax = fit + se), alpha = 0.2) + 
  theme_bw()

# Twitter comparison ----
# 加载数据。
library(targets)
tar_make()
tar_load(general_plot_dt)
tar_load(dtm_2022)
tar_load(lda_2022)
tar_load(topic_word_2022)

# 推文每月数量变化。
general_plot_dt %>% 
  mutate(year = year(date), month = month(date)) %>% 
  group_by(year, month) %>% 
  summarise(tw_num = sum(tw_num), .groups = "drop") %>% 
  ggplot() + 
  geom_line(aes(month, tw_num, col = as.character(year), group = year)) + 
  labs(x = "Month", y = "Tweet number", col = "Year") + 
  theme_bw()

tw_id_topic <- tidy(lda_2022, matrix = "gamma") %>% 
  rename(text = document) %>% 
  # Bug: 这里的对应关系是否正确？
  mutate(
    id = rep(docvars(dtm_2022)$id, 6), 
    date = rep(docvars(dtm_2022)$date, 6)
  )

# 各个话题的显著度变化。
tw_id_topic %>% 
  group_by(date, topic) %>% 
  summarise(gamma = sum(gamma), .groups = "drop") %>% 
  group_by(date) %>% 
  mutate(tot_gamma = sum(gamma), gamma_score = gamma / tot_gamma) %>% 
  ggplot() + 
  geom_line(aes(date, gamma_score, col = as.character(topic), group = topic)) + 
  facet_wrap(.~ topic)
