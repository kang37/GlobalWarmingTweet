# Statement ----
# 基于小兔提供的每天的推文数量等数据，计算人均推文平均值超过一定基线的日期中，推文数量占总推文数的比例。

# Package ----
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data ----
tw.num <- read.csv("RawData/tw_Tdif_user12_22.csv") %>% 
  tibble() %>% 
  rename_with(tolower) %>% 
  rename(num_tw = agg, num_gw = refgw, num_cc = refcc) %>% 
  mutate(date = as.Date(jstdate), 
         year = year(date), 
         month = month(date), 
         day = day(date)) %>% 
  select(date, year, month, day, 
         num_tw, num_gw, num_cc, tw_perhk, gw_perhk, cc_perhk)
tw.num <- left_join(
  tw.num %>% 
    select(date, year, month, day, starts_with("num")) %>% 
    pivot_longer(
      cols = starts_with("num"), names_to = "dt_set", values_to = "num"
    ) %>% 
    mutate(dt_set = substr(dt_set, 5, 6)), 
  tw.num %>% 
    select(date, year, month, day, ends_with("perhk")) %>% 
    pivot_longer(
      cols = ends_with("perhk"), names_to = "dt_set", values_to = "perhk"
    ) %>% 
    mutate(dt_set = substr(dt_set, 1, 2))
)

# 计算推文强度的高值基准线
tw.int <- tw.num %>% 
  group_by(dt_set, year) %>% 
  summarise(
    int90 = quantile(perhk, 0.90), 
    int85 = quantile(perhk, 0.85), 
    int80 = quantile(perhk, 0.80), 
    int75 = quantile(perhk, 0.75)
  ) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = starts_with("int"), names_to = "int_idx",  values_to = "int_val"
  )

# 不考虑年份差异的分位数
tw.int.ori <- tw.num %>% 
  group_by(dt_set) %>% 
  summarise(
    int90 = quantile(perhk, 0.90), 
    int85 = quantile(perhk, 0.85), 
    int80 = quantile(perhk, 0.80), 
    int75 = quantile(perhk, 0.75)
  ) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = starts_with("int"), names_to = "int_idx",  values_to = "int_val_ori"
  )

# Analysis ----
## Not consider yearly diff ----
# 计算推文强度的高值基准线
tw.ana.ori <- tw.num %>% 
  left_join(tw.int.ori) %>% 
  # 挑出推文强度超过这些基准线的日期
  mutate(
    comp = perhk > int_val_ori
  )

# 可视化：全部、体积全球变暖、气候变化的推文强度的变化，及对应的推文强度高值基准线
ggplot(tw.ana.ori) + 
  geom_line(aes(date, log(perhk))) + 
  geom_line(aes(date, log(int_val_ori)), col = "pink") + 
  facet_grid(int_idx ~ dt_set, scales = "free") 

# 可视化超过基准线的日期分布
subset(tw.ana.ori, dt_set = "tw") %>% 
  ggplot() + 
  geom_tile(aes(month, day, fill = comp)) + 
  facet_grid(year ~ int_idx)
subset(tw.ana.ori, dt_set = "gw") %>% 
  ggplot() + 
  geom_tile(aes(month, day, fill = comp)) + 
  facet_grid(year ~ int_idx)
subset(tw.ana.ori, dt_set = "cc") %>% 
  ggplot() + 
  geom_tile(aes(month, day, fill = comp)) + 
  facet_grid(year ~ int_idx)

# 算出超过基准的日期对应的推文条数
tw.sum.ori <- 
  tw.ana.ori %>% 
  group_by(dt_set, int_idx) %>% 
  summarise(
    up_num = sum(num * comp)
  )

# 汇总计算超过高值基准线的日期中推文数量
pivot_wider(
  tw.sum.ori, 
  id_cols = "int_idx", 
  names_from = "dt_set", 
  values_from = "up_num"
)

# 汇总计算超过高值基准线的日期中推文数量占推文数的比例
tw.sum.ori %>% 
  left_join(
    tw.num %>% 
      group_by(dt_set) %>% 
      summarise(num = sum(num)) %>% 
      ungroup(), 
    by = "dt_set"
  ) %>% 
  mutate(prop = up_num / num) %>% 
  pivot_wider(
    id_cols = int_idx, 
    names_from = dt_set, 
    values_from = prop
  )

## Consider yearly diff ----
# 查看新旧分位数的差异
tw.int %>% 
  left_join(tw.int.ori) %>% 
  mutate(comp = int_val < int_val_ori) %>% 
  ggplot() + 
  geom_tile(aes(int_idx, year, fill = comp)) + 
  facet_wrap(.~ dt_set)
# 2020-2022年的新分位数反而比原本旧的、不考虑年份的分位数要高，意味着能够达到这个标准的日期更少，同时，2020年之后的推文数更多，因此相比于原来不考虑年份差异的结果而言，超过基准的推文数就会更少

# 计算推文强度的高值基准线
tw.ana <- tw.num %>% 
  left_join(tw.int) %>% 
  # 挑出推文强度超过这些基准线的日期
  mutate(
    comp = perhk > int_val
  )

# 可视化：全部、体积全球变暖、气候变化的推文强度的变化，及对应的推文强度高值基准线
ggplot(tw.ana) + 
  geom_line(aes(date, log(perhk))) + 
  geom_line(aes(date, log(int_val)), col = "pink") + 
  facet_grid(int_idx ~ dt_set, scales = "free") 

# 可视化超过基准线的日期分布
subset(tw.ana, dt_set = "tw") %>% 
  ggplot() + 
  geom_tile(aes(as.factor(month), day, fill = comp)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_grid(year ~ int_idx)
subset(tw.ana, dt_set = "gw") %>% 
  ggplot() + 
  geom_tile(aes(as.factor(month), day, fill = comp)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_grid(year ~ int_idx)
subset(tw.ana, dt_set = "cc") %>% 
  ggplot() + 
  geom_tile(aes(as.factor(month), day, fill = comp)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_grid(year ~ int_idx)

# 算出超过基准的日期对应的推文条数
tw.sum <- 
  tw.ana %>% 
  group_by(dt_set, int_idx) %>% 
  summarise(
    up_num = sum(num * comp)
  )

# 汇总计算超过高值基准线的日期中推文数量
pivot_wider(
  tw.sum, 
  id_cols = "int_idx", 
  names_from = "dt_set", 
  values_from = "up_num"
)

# 汇总计算超过高值基准线的日期中推文数量占推文数的比例
tw.sum %>% 
  left_join(
    tw.num %>% 
      group_by(dt_set) %>% 
      summarise(num = sum(num)) %>% 
      ungroup(), 
    by = "dt_set"
  ) %>% 
  mutate(prop = up_num / num) %>% 
  pivot_wider(
    id_cols = int_idx, 
    names_from = dt_set, 
    values_from = prop
  )

# Dates with continue high value days ----
# 以90%为准，挑选连续3天或以上均为高值的日期
tw.conthigh <- tw.ana %>% 
  subset(int_idx == "int90") %>% 
  arrange(dt_set, year, month, day, comp) %>% 
  # 漏洞：只有在日期连续的情况下才可以用这个算法进行分组
  mutate(comp_oppo = !comp) %>% 
  mutate(grp = cumsum(comp_oppo)) %>% 
  subset(comp) %>% 
  group_by(grp) %>% 
  # 计算是第几个连续天数，以及总连续天数，以此作为之后的筛选基准
  mutate(nth_day = row_number(), 
         high_days = max(nth_day)) %>% 
  ungroup() %>% 
  # 挑出连续高值天数大于3天的日期
  subset(high_days >= 3) %>% 
  # 基于每个峰值期的起点日期重命名分组名称
  group_by(grp) %>% 
  mutate(
    year_min = min(year), 
    month_min = min(month), 
    day_min = min(day), 
    peak_grp = paste0(year_min, month_min, day_min)
  ) %>% 
  ungroup() %>% 
  select(-c(int_idx, int_val, comp_oppo, year_min, month_min, day_min, grp))

# 查看高值在3个口径中的重合情况
tw.conthigh %>% 
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
  labs(x = "Dataset", y = "Date") + 
  facet_wrap(.~ year, ncol = 1, strip.position = "right")
