# Package ----
library(RMeCab)
library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)
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
  "+", "<", ">", "><", "!!", "#", "!?", "@", "(", "/", "\\", "://"
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

# 将目标数据根据提及“温暖化”和“気候変動”进行分类，并且写出推文文本为*.txt文件
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

## Get 2021 events word freq *.csv ----
# 目标日期字符串
tar.date.1 <- paste0(rep("2021-10-", 5), 26:30)
tar.date.2 <- paste0(rep("2021-11-0", 4), 2:5)
tar.date.3 <- paste0(rep("2021-11-", 2), 16:17)

# 构建目标数据集
text.2021.parts <- read.csv("RawData/2021/jst202111lite.csv") %>% 
  rbind(read.csv("RawData/2021/jst202110lite.csv")) %>% 
  as_tibble() %>% 
  mutate(date = substr(created_at, 1, 10)) 
text.2021.part.1 <- text.2021.parts %>% 
  subset(date %in% tar.date.1)
text.2021.part.2 <- text.2021.parts %>% 
  subset(date %in% tar.date.2)
text.2021.part.3 <- text.2021.parts %>% 
  subset(date %in% tar.date.3)

# 将目标数据根据提及“”和“”进行分类，并且写出推文文本为*.txt文件
WriteTxt2021 <- function(x, name.tar) {
  x %>% 
    subset(grepl("温暖化", text)) %>% 
    .$text %>% 
    write(file = paste0("ProcData/ForKansaiConf/Text_", name.tar, "_gw.txt"))
  x %>% 
    subset(grepl("気候変動", text)) %>% 
    .$text %>% 
    write(file = paste0("ProcData/ForKansaiConf/Text_", name.tar, "_cc.txt"))
}

WriteTxt2021(x = text.2021.part.1, name.tar =  "2021_part_1")
WriteTxt2021(x = text.2021.part.2, name.tar =  "2021_part_2")
WriteTxt2021(x = text.2021.part.3, name.tar =  "2021_part_3")

# 读取*.txt文件并进行分词
Wf2021Parts <- function(order.file, name.keyword) {
  docDF(target = paste0("ProcData/ForKansaiConf/Text_2021_part_", order.file, 
                        "_", name.keyword, ".txt"), 
        type = 1, dic = userdic) %>% 
    rename_with(~ c("term", "pos1", "pos2", "freq")) %>% 
    subset(
      pos1 %in% c("名詞","動詞","形容詞") & 
        !(pos2 %in% c("非自立","接尾", "数")) &
        !(term %in% stopwords)
    ) %>% 
    arrange(-freq) %>% 
    write.csv(file = paste0("ProcData/ForKansaiConf/Wf_2021_part_", order.file, 
                            "_", name.keyword, ".csv"))
}

Wf2021Parts(order.file = "1", name.keyword = "cc")
Wf2021Parts(order.file = "1", name.keyword = "gw")

Wf2021Parts(order.file = "2", name.keyword = "cc")
Wf2021Parts(order.file = "2", name.keyword = "gw")

Wf2021Parts(order.file = "3", name.keyword = "cc")
Wf2021Parts(order.file = "3", name.keyword = "gw")

## Get 202001 co-occurrence ----
# 函数：读取原始数据并将推文内容写出为*.txt文件
Csv2Txt <- function(dir.csv, name.tar) {
  # 读取文本*.csv文件
  text.csv <- read.csv(dir.csv) %>% 
    as_tibble()
  
  # 区分提及全球变暖和气候变化的推文
  text.csv.gw <- subset(text.csv, grepl("温暖化", text))
  text.csv.cc <- subset(text.csv, grepl("気候変動", text))
  
  # 将两类推文文本列的内容分别写入*.txt文件
  write(text.csv.gw$text, paste0("ProcData/Txtgen/", name.tar, "_gw.txt"))
  write(text.csv.cc$text, paste0("ProcData/Txtgen/", name.tar, "_cc.txt"))
}
Csv2Txt(dir.csv = "RawData/2020/jst202001lite.csv", name.tar = "202001")

# 共现数据计算生成
for (i in c("gw", "cc")) {
  # 构建N-gram列表：
  # 假设N=1，且只选取三类词性
  # 待办：有些不需要的字符没有清理干净，影响共现判断
  ngram <- 
    docDF(target = paste0("ProcData/TxtGen/202001_", i, ".txt"), 
          type = 1, pos = c("名詞", "形容詞", "動詞"), 
          nDF = 1, N = 2, dic = userdic) %>% 
    # 进一步去除停用词
    subset(!N1 %in% stopwords) %>% 
    subset(!N2 %in% stopwords) 
  names(ngram)[5] <- "freq"
  ngram <- ngram %>% 
    arrange(-freq) %>% 
    # 保留出现次数排名前50的连接
    head(50)
  
  # 导出共现图
  png(
    filename = paste0("ProcData/ForKansaiConf/202001_", i, "_共现.png"), 
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1600,
    bg = "transparent" # 透明背景
  )
  # 提取作图数据
  plotdata_ngram <- graph_from_data_frame(ngram)
  # 作共现图
  print(ggraph(ngram, layout = "fr") +
          geom_edge_link(aes(edge_alpha = freq), show.legend = FALSE) +
          geom_node_point(color = "lightblue", size = 5) +
          geom_node_text(
            aes(label = name), size = 3, repel=TRUE, family="HiraKakuProN-W3") +
          theme_graph(base_size=12))
  dev.off()
}

## Get separated words of text ----
# 函数：将日语字符串分解成逗号分隔的分词
# 参数：
# x：日语字符串
SepWord <- function(x) {
  # 对一条推文进行分词
  words <- gsub("[[:punct:]]", "", x) %>%  # 去除原文中的英文标点符号
    RMeCabC(str = ., dic = userdic) %>%  # 进行分词
    unlist() %>% 
    .[names(.) %in% c("名詞", "動詞", "形容詞")] %>%  # 去除日语标点符号
    .[!. %in% stopwords]  # 去除停用词
  # 将分词合成字符串并用逗号区隔开
  words.output <- paste(words, collapse = ",")
  return(words.output)
}

# 函数：将各条推文文本分解成用逗号分隔的分词
# 参数：
# dir：包含初步清洗后的推文列“textClr”的文件路径
ApplySepWord <- function(dir) {
  # 读取包含推文内容的文件
  test.ori <- read.csv(dir) %>% 
    as_tibble()
  # 进行分词
  test.sepword <- test.ori %>% 
    subset(textClr != "") %>% # 去除推文为空的行
    subset(textClr != " ") %>% # 去除推文为空格的行
    mutate(sepword = apply(.["textClr"], 1, SepWord))
  return(test.sepword)
}

# 对各月份进行分词
test.202001.sepword <- 
  ApplySepWord("RawData/ForKansaiConf/test_Clr202001lite.csv")
write.csv(test.202001.sepword, 
          "ProcData/ForKansaiConf/Test_202001_sepword.csv")

test.202110.sepword <- 
  ApplySepWord("RawData/ForKansaiConf/test_Clr202110lite.csv")
write.csv(test.202110.sepword, 
          "ProcData/ForKansaiConf/Test_202110_sepword.csv")

test.202111.sepword <- 
  ApplySepWord("RawData/ForKansaiConf/test_Clr202111lite.csv")
write.csv(test.202111.sepword, 
          "ProcData/ForKansaiConf/Test_202111_sepword.csv")
