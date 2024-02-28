pacman::p_load(dplyr, ggplot2)

tw_count <- 
  rbind(
    read.csv("RawData/twCount202201-202209.csv") %>% 
      select(date = X, tweet), 
    read.csv("RawData/twCount201201-202111.csv") %>% 
      select(date = X, tweet)
  ) %>% 
  tibble() %>% 
  # Bug: Take mean value of 2021.11 and 2022.01 as tweet count of 2022.12. 
  rbind(data.frame(date = "202112", tweet = (213791 + 103344) / 2)) %>% 
  mutate(
    year = as.integer(substr(date, 1, 4)), 
    month = as.integer(substr(date, 5, 6)), 
    .before = 1
  ) %>% 
  arrange(year, month)

tw_count %>% 
  ggplot(aes(month, tweet, col = as.factor(year))) + 
  geom_point() + 
  geom_line()

tw_count %>% 
  filter(tweet <= 1.5e5) %>% 
  ggplot(aes(month, tweet, col = as.factor(year))) + 
  geom_point() + 
  geom_line()

tw_count %>% 
  filter(year <= 2018, month >= 10) %>% 
  ggplot(aes(as.factor(month), tweet, col = as.factor(year))) + 
  geom_point() + 
  geom_line(aes(group = year)) + 
  labs(x = "Month", y = "Number of Tweet") + 
  theme_bw() + 
  scale_color_discrete(guide = guide_legend("Year"))

# Take 10-year means. 
tw_count %>% 
  filter(year <= 2018, month >= 10) %>% 
  group_by(month) %>% 
  summarise(num_tweet = max(tweet), .groups = "drop") %>% 
  mutate(num_tweet_1.5 = num_tweet * 1.5) %>% 
  rbind(colSums(.)) %>% 
  mutate(month = as.character(month)) %>% 
  mutate(month = case_when(
    month == "33" ~ "総計", TRUE ~ month
  )) %>% 
  knitr::kable() 

# Sample data. 
data.table::fread("RawData/jst202206full.csv") %>% 
  tibble() %>% 
  head(10) %>% 
  write.csv("data_sample_kang.csv")
