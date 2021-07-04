# install mecab in cmd
# brew install mecab
# brew install mecab-ipadic

# install the required packages 

library(readr)
library(RMeCab)
library(ggplot2)
library(igraph)
library(ggraph)

# 设置停用词
stopwords <- c(
  "する","それ","なる","ない","そこ","これ","ある", "さん", "なん", "の", "ん", 
  "n", "RT", letters, LETTERS, 
  "+", "<", ">", "><", 
  "地球温暖化"
)

# 设置启动参数
cash_readcsv <- FALSE

if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_ALL", "Japanese")
  userdic <- "C:/data/kangdict.dic"
} else {
  userdic <- "/Users/VickyWang/kang.dic"
}

# 要分析的文件夹路径和文件名
csvdir <- "liteNoRT_url/"
csvfiles <- list.files("liteNoRT_url")

# 构建原始数据列表
twtdata <- vector("list", length(csvfiles))
names(twtdata) <- csvfiles

# 开始出图
starttime <- Sys.time()
for (i in csvfiles) {
  # 读取原始数据并筛出子集以测试代码
  if (cash_readcsv == FALSE) {
    twtdata[[i]] <- read_csv(paste0(csvdir, i))
  }
  
  twtdata_sub <- twtdata[[i]]
  
  # 清洗推文数据
  # Remove mentions, urls, emojis, numbers, punctuations, etc.
  twtdata_sub$textClr <- gsub("@\\w+", "", twtdata_sub$textClr)
  twtdata_sub$textClr <- gsub("https?://.+", "", twtdata_sub$textClr)
  twtdata_sub$textClr <- gsub("\\d+\\w*\\d*", "", twtdata_sub$textClr)
  twtdata_sub$textClr <- gsub("#\\w+", "", twtdata_sub$textClr)
  twtdata_sub$textClr <- gsub("[[:punct:]]", " ", twtdata_sub$textClr)
  twtdata_sub$textClr <- gsub("[0-9]", "", twtdata_sub$textClr)
  
  # 将推文写入文件
  write(twtdata_sub$textClr, "twt.txt")
  # 提取词频
  freq_ori <- docDF("twt.txt", type = 1, dic = userdic)
  # 进一步清除停止词
  freq <- freq_ori
  freq <- freq[freq$POS1 %in% c("名詞","動詞","形容詞") & 
                 !(freq$POS2 %in% c("非自立","接尾", "数")) &
                 !(freq$TERM %in% stopwords),]
  
  # 归并相同的词语
  freq <- aggregate(twt.txt ~ TERM, data = freq, sum)
  
  # 按照出现次数排序
  freq <- freq[order(freq$twt.txt, decreasing = TRUE), ]
  head(freq)
  
  # 条形图
  png(
    filename = paste0("bar_liteNoRT_url/", i, "词频", ".png"), 
    type = "cairo", # 抗锯齿
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1600,
    bg = "transparent" # 透明背景
  )
  theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))
  print(
    ggplot(head(freq, 10), aes(x = reorder(TERM, twt.txt), y = twt.txt)) + 
      geom_bar(stat = "identity") + 
      labs(x = i) + 
      coord_flip()
  )
  dev.off()
  
  # 词汇共现图 ----
  # 构建N-gram列表：
  # 假设N=1，且只选取三类词性
  # 问题：有些不需要的字符没有清理干净，影响共现判断
  ngram <- docDF("twt.txt", type = 1, pos = c("名詞", "形容詞", "動詞"), 
                 nDF = 1, N = 2, dic = userdic)
  # 进一步去除停用词
  ngram <- ngram[!ngram$N1 %in% stopwords, ]
  ngram <- ngram[!ngram$N2 %in% stopwords, ]
  dim(ngram)
  # 查看各种词汇共现出现的次数
  table(ngram$twt.txt)
  
  # 保留出现次数排名前50的连接
  ngram_sub <- 
    ngram[ngram$twt.txt %in% names(tail(table(ngram$twt.txt), 50)), ]
  dim(ngram_sub)
  
  # 提取作图数据
  plotdata_ngram_sub <- graph_from_data_frame(ngram_sub)
  # tkplot(plotdata_ngram_sub)
  # plot(plotdata_ngram_sub, vertex.size = 20, 
  #      vertex.label.family="HiraKakuProN-W3")
  # 输出图片
  png(
    filename = paste0("co_liteNoRT_url/", i, "共现图", ".png"), 
    type = "cairo", # 抗锯齿
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1600,
    bg = "transparent" # 透明背景
  )
  print(
    ggraph(plotdata_ngram_sub, layout = "fr") +
      geom_edge_link(aes(edge_alpha = twt.txt), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(
        aes(label = name), size = 3, repel=TRUE, family="HiraKakuProN-W3") +
      theme_graph(base_size=12)
  )
  dev.off()
  print(i)
  print(Sys.time() - starttime)
}
Sys.time() - starttime

