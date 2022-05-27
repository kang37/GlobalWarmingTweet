# Packages ----
library(dplyr)
library(ggplot2)
library(showtext)
showtext_auto()

# Function ----
# 函数：基于一个数据框，对3个变量作图
# 参数：
# x：数据框，需要包含名为“label”列以映射横轴标签
# name_var_1：列名1，红色柱状图
# name_var_1：列名2，蓝色柱状图
# name_var_3：列名3，黑色折线图
# times_axia_right：右侧纵轴缩放倍数
# name_yaxis_left：左侧纵轴标题
# name_yaxis_right：右侧纵轴标题
fun_plot3var <- function(
  x, name_var_1, name_var_2, name_var_3, 
  times_axia_right, name_yaxis_left, name_yaxis_right, 
  method = "mix") {
  # 对数据进行缩放
  x[[name_var_3]] <- x[[name_var_3]] * times_axia_right
  
  # 开始绘图
  ggplot() + 
    geom_col(data = x, aes_string(x = "date", y = name_var_1), fill = "red") + 
    geom_col(data = x, aes_string(x = "date", y = name_var_2), fill = "blue") + 
    labs(x = "", y = name_yaxis_left) +
    theme(axis.ticks.x = element_blank(), 
          panel.grid.major = element_blank()) + 
    scale_x_discrete(labels = x$label) +
    geom_line(data = if (method == "mix") na.omit(x) else x, 
              aes_string(x = "date", y = name_var_3, group = 1)) + 
    scale_y_continuous(
      sec.axis = sec_axis(~./times_axia_right, name = name_yaxis_right))
}

# Data ---- 
# 读取月度数据
mth_data <- read.csv("RawData/ForKansaiConf/TwAgg1201_2112.csv") %>% 
  as_tibble() %>% 
  rename(date = Unnamed..0) %>% 
  mutate(date = as.character(date)) %>% 
  select(-X)
# 读取年度数据
yr_data <- read.csv("RawData/ForKansaiConf/TwUser2012-2021.csv") %>% 
  as_tibble() %>% 
  rename(date = X) %>% 
  mutate(date = paste0(as.character(date), "06"))
# 综合两个数据
plot_data_ori <- mth_data %>% 
  left_join(yr_data, by = "date") %>% 
  mutate(year = substr(date, 1, 4), month = substr(date, 5, 6)) %>% 
  mutate(label = ifelse(month == "01", year, ""))
plot_data_proc <- plot_data_ori %>% 
  mutate(
    # 将推文数量单位转化为“x10000”
    tweet = tweet / 1000, 
    retweet = retweet / 1000, 
    tweetNoRT = tweetNoRT / 1000, 
    refGW_sum = refGW_sum / 1000, 
    refCC_sum = refCC_sum / 1000, 
    # 将用于折线图的数据转化为差不多的数量级
    # predicted.Number.of.Users = predicted.Number.of.Users / 100, 
    # tempAnom = tempAnom * 10, 
    # 构建提及“全球变暖”和“气候变化”推文数量之和的列以作图
    refGW_CC_per_10000 = refGW_per_10000 + refCC_per_10000, 
    refGW_CC = refGW_sum + refCC_sum
  )

# Analysis ----
## Figure 1 ----
png("ProcData/ForKansaiConf/无转推推文数_转推数_用户数.png", 
    width = 900, height = 500, res = 150)
fun_plot3var(
  plot_data_proc, 
  name_var_1 = "tweet", 
  name_var_2 = "tweetNoRT", 
  name_var_3 = "predicted.Number.of.Users", 
  times_axia_right = 0.01, 
  name_yaxis_left = "Number of tweet (x1000)", 
  name_yaxis_right = "Predicted number of user")
dev.off()

## Figure 2 ----
png("ProcData/ForKansaiConf/提及GW_提及CC_用户数.png", 
    width = 900, height = 500, res = 150)
fun_plot3var(
  plot_data_proc, 
  name_var_1 = "refGW_CC", 
  name_var_2 = "refCC_sum", 
  name_var_3 = "predicted.Number.of.Users", 
  times_axia_right = 0.01, 
  name_yaxis_left = "Number of ref tweet (x1000)", 
  name_yaxis_right = "Predicted number of user")
dev.off()

## Figure 3 ----
png("ProcData/ForKansaiConf/万人提及GW_万人提及CC_温度偏差.png", 
    width = 900, height = 500, res = 150)
fun_plot3var(
  x = plot_data_proc, 
  name_var_1 = "refGW_CC_per_10000", 
  name_var_2 = "refCC_per_10000", 
  name_var_3 = "tempAnom", 
  times_axia_right = 10, 
  name_yaxis_left = "Number of ref tweet (x1000)", 
  name_yaxis_right = "Temperature diff", 
  method = "non-mix"
)
dev.off()

## Word freq month figures ----
# 十年间提及气候变动的词频
png("ProcData/ForKansaiConf/12_21气候变动词频.png", 
    width = 900, height = 500, res = 150)
read.csv("RawData/ForKansaiConf/CC_Freq12_21.csv") %>% 
  as_tibble() %>% 
  rename_with(tolower) %>% 
  ggplot() + 
  geom_col(aes(y = reorder(term, txt), x = txt)) + 
  labs(y = "Word", x = "Frequency")
dev.off()

# 十年间提及全球变暖的词频
png("ProcData/ForKansaiConf/12_21全球变暖词频.png", 
    width = 900, height = 500, res = 150)
read.csv("RawData/ForKansaiConf/GW_Freq12_21.csv") %>% 
  as_tibble() %>% 
  rename_with(tolower) %>% 
  ggplot() + 
  geom_col(aes(y = reorder(term, txt), x = txt)) + 
  labs(y = "Word", x = "Frequency")
dev.off()
