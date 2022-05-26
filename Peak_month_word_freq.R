# Package ----
library(RMeCab)
library(dplyr)
library(ggplot2)
library(showtext)
showtext_auto()

# Data ----
# 用户自定义词典
userdic <- "RawData/Global_warming_tweet.dic"

# 设置停止词
stopwords <- c(
  "する","それ","なる","ない","そこ","これ","ある", "さん", "なん", "の", "ん", 
  "いる", "思う", "そう", "れる", "くる", "考える", "言う", "ー", 
  "できる", "てる", "でる", "世界の財界", "一", "いい", "何", "いう", "できる", 
  "られる", "n", "RT", letters, LETTERS, 
  "+", "<", ">", "><", "!!", "#", "!?", "@", "(", "/", "\\"
)

# Analysis ----
## Get peak month word freq *.csv and plot ---- 
# 函数：基于文本*.csv文件，生成文本*.txt文件，再解析并写出词频*.csv文件
# 参数：
# dir.csv：文本*.csv文件路径及文件名称
# name.tar：最终生成词频*.csv文件路径及文件名称
# dir.dic：用户自定义词典路径及文件名称
# stopword：停止词，为字符向量
Csv2WfCsv <- function(dir.csv, name.tar, dir.dic, stopword) {
  # 读取文本*.csv文件
  text.csv <- read.csv(dir.csv) %>% 
    as_tibble()
  
  # 区分提及全球变暖和气候变化的推文
  text.csv.gw <- subset(text.csv, refGW == 1)
  text.csv.cc <- subset(text.csv, refCC == 1)
  
  # 将两类推文文本列的内容分别写入*.txt文件
  write(text.csv.gw$textClr, 
        paste0("ProcData/ForKansaiConf/Text_", name.tar, "_gw.txt"))
  write(text.csv.cc$textClr, 
        paste0("ProcData/ForKansaiConf/Text_", name.tar, "_cc.txt"))
  
  # 对两类推文，分别读取文本*.txt文件并解析成词频
  for (i in c("gw", "cc")) {
    wf <- docDF(
      target = paste0("ProcData/ForKansaiConf/Text_", name.tar, "_", i, ".txt"), 
      type = 1, dic = dir.dic) %>% 
      as_tibble() %>% 
      # 进一步筛除不需要的词语
      subset(
        POS1 %in% c("名詞","動詞","形容詞") & 
          !(POS2 %in% c("非自立","接尾", "数")) &
          !(TERM %in% stopwords)
      ) %>% 
      rename_with(~ c("term", "pos1", "pos2", "freq")) %>% 
      arrange(-freq) 
    # 写入*.csv文件
    write.csv(
      wf, paste0("ProcData/ForKansaiConf/Wf_", name.tar, "_", i, ".csv"))
  }
}

# 写出词频*.csv文件
Csv2WfCsv(dir.csv = "RawData/ForKansaiConf/test_Clr202001lite.csv", 
          name.tar = "202001", dir.dic = userdic, stopword = stopwords)
Csv2WfCsv(dir.csv = "RawData/ForKansaiConf/test_Clr202110lite.csv", 
          name.tar = "202110", dir.dic = userdic, stopword = stopwords)
Csv2WfCsv(dir.csv = "RawData/ForKansaiConf/test_Clr202111lite.csv", 
          name.tar = "202111", dir.dic = userdic, stopword = stopwords)

# 各目标月份的词频图
PlotWf <- function(name.tar, name.keyword) {
  # 构建文件路径前半段
  dir.part <- paste0("ProcData/ForKansaiConf/Wf_", name.tar, "_", name.keyword)
  
  # 生成画布并作图
  png(paste0(dir.part, ".png"), width = 900, height = 500, res = 150)
  read.csv(
    paste0(dir.part, ".csv")) %>% 
    head(10) %>% 
    ggplot() + 
    geom_col(aes(y = reorder(term, freq), x = freq)) + 
    labs(y = "Word", x = "Frequency")
}

PlotWf(name.tar = "202001", name.keyword = "gw")
dev.off()
PlotWf(name.tar = "202001", name.keyword = "cc")
dev.off()

PlotWf(name.tar = "202110", name.keyword = "gw")
dev.off()
PlotWf(name.tar = "202110", name.keyword = "cc")
dev.off()

PlotWf(name.tar = "202111", name.keyword = "gw")
dev.off()
PlotWf(name.tar = "202111", name.keyword = "cc")
dev.off()

## Get 2020 warm winter word freq *.csv ----
# 目标日期字符串
tar.date <- c(paste0(rep("2020-01-0", 5), 5:9), 
              paste0(rep("2020-01-", 6), 10:15))

# 构建目标数据集：2020年1月5日-15日的数据
text.202001.part <- read.csv("RawData/2020/jst202001lite.csv") %>% 
  as_tibble() %>% 
  mutate(date = substr(created_at, 1, 10)) %>% 
  subset(date %in% tar.date)

# 将目标数据根据提及“”和“”进行分类，并且写出推文文本为*.txt文件
text.202001.part %>% 
  subset(grepl("温暖化", text)) %>% 
  .$text %>% 
  write(file = "ProcData/ForKansaiConf/Text_202001_part_gw.txt")
text.202001.part %>% 
  subset(grepl("気候変動", text)) %>% 
  .$text %>% 
  write(file = "ProcData/ForKansaiConf/Text_202001_part_cc.txt")

# 读取*.txt文件并进行分词
Wf2020JanPart <- function(name.keyword) {
  docDF(target = paste0("ProcData/ForKansaiConf/Text_202001_part_", 
                        name.keyword, ".txt"), 
        type = 1, dic = userdic) %>% 
    rename_with(~ c("term", "pos1", "pos2", "freq")) %>% 
    subset(
      pos1 %in% c("名詞","動詞","形容詞") & 
        !(pos2 %in% c("非自立","接尾", "数")) &
        !(term %in% stopwords)
    ) %>% 
    arrange(-freq) %>% 
    write.csv(file = paste0("ProcData/ForKansaiConf/Wf_202001_part_", 
                            name.keyword, ".csv"))
}
Wf2020JanPart("cc")
Wf2020JanPart("gw")
