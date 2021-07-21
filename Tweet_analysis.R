# install mecab in cmd
# brew install mecab
# brew install mecab-ipadic

# install the required packages 

library(readr)
library(RMeCab)
library(ggplot2)
library(igraph)
library(ggraph)
library(reshape2)

# 设置启动参数
# 是否已有数据读取缓存
cash_readcsv <- FALSE
# 要分析的文件夹路径和文件名
csvdir <- "liteNoRT_url"
csvfiles <- list.files(csvdir)
# 要存储结果图的路径
vernum <- 10
barpngdir <- paste0("bar_", csvdir, vernum)
copngdir <- paste0("co_", csvdir, vernum)
# 创建文件夹
dir.create(barpngdir)
dir.create(copngdir)

# 设置停用词
stopwords <- c(
  "する","それ","なる","ない","そこ","これ","ある", "さん", "なん", "の", "ん", 
  "いる", "思う", "そう", "れる", "くる", "考える", "言う", "ー", 
  "できる", "てる", "でる", "世界の財界", 
  "一", 
  "いい", 
  "何", 
  "いう", 
  "できる", 
  "られる", 
  "n", "RT", letters, LETTERS, 
  "+", "<", ">", "><"
)

if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_ALL", "Japanese")
  userdic <- "C:/data/kangdict.dic"
} else {
  userdic <- "/Users/VickyWang/kang.dic"
}

# 数据读取
if (cash_readcsv == FALSE) {
  # 如果无数据读取缓存，则构建原始数据列表
  twtdata <- vector("list", length(csvfiles))
  names(twtdata) <- csvfiles
  # 开始读取数据
  for (i in csvfiles) {
    twtdata[[i]] <- read_csv(paste0(csvdir, "/", i))
  }
}

# 补充数据清洗
# 去除特定的推文
for (i in csvfiles) {
  twtdata[[i]] <- twtdata[[i]][
    grepl("世界の財界", twtdata[[i]]$text) == FALSE & 
      grepl("無実", twtdata[[i]]$text) == FALSE & 
      grepl("誓約", twtdata[[i]]$text) == FALSE & 
      grepl("露骨", twtdata[[i]]$text) == FALSE , ]
}

# 构建变量用于存放词频结果
freq <- vector("list", length(csvfiles))
names(freq) <- csvfiles
ngram <- vector("list", length(csvfiles))
names(ngram) <- csvfiles
ngram_sub <- vector("list", length(csvfiles))
names(ngram_sub) <- csvfiles

# 生成词频和共现图数据 ----
starttime <- Sys.time()
for (i in csvfiles) {
  # 导入数据
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
  
  # 词频数据 ----
  freq[[i]] <- docDF("twt.txt", type = 1, dic = userdic)
  # 进一步清除停止词
  freq[[i]] <- freq[[i]]
  freq[[i]] <- freq[[i]][freq[[i]]$POS1 %in% c("名詞","動詞","形容詞") & 
                 !(freq[[i]]$POS2 %in% c("非自立","接尾", "数")) &
                 !(freq[[i]]$TERM %in% stopwords),]
  
  # 归并相同的词语
  freq[[i]] <- aggregate(twt.txt ~ TERM, data = freq[[i]], sum)
  
  # 按照出现次数排序
  freq[[i]] <- freq[[i]][order(freq[[i]]$twt.txt, decreasing = TRUE), ]
  
  # 词汇共现图数据 ----
  # 构建N-gram列表：
  # 假设N=1，且只选取三类词性
  # 问题：有些不需要的字符没有清理干净，影响共现判断
  ngram[[i]] <- docDF("twt.txt", type = 1, pos = c("名詞", "形容詞", "動詞"), 
                      nDF = 1, N = 2, dic = userdic)
  # 进一步去除停用词
  ngram[[i]] <- ngram[[i]][!ngram[[i]]$N1 %in% stopwords, ]
  ngram[[i]] <- ngram[[i]][!ngram[[i]]$N2 %in% stopwords, ]
  dim(ngram[[i]])
  # 查看各种词汇共现出现的次数
  table(ngram[[i]]$twt.txt)
  
  # 保留出现次数排名前50的连接
  ngram_sub[[i]] <- 
    ngram[[i]][ngram[[i]]$twt.txt %in% names(tail(table(ngram[[i]]$twt.txt), 50)), ]
  dim(ngram_sub[[i]])
  
  # 提示进程
  print(i)
}

# 作图导出 ----
for (i in csvfiles) {
  # 条形图导出 ----
  png(
    filename = paste0(barpngdir, "/", i, "词频", ".png"), 
    type = "cairo", # 抗锯齿
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1600,
    bg = "transparent" # 透明背景
  )
  theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))
  print(
    ggplot(head(freq[[i]], 30), aes(x = reorder(TERM, twt.txt), y = twt.txt)) + 
      geom_bar(stat = "identity") + 
      labs(x = i) + 
      coord_flip()
  )
  dev.off()
  
  ## 共现图导出 ----
  # 提取作图数据
  plotdata_ngram_sub <- graph_from_data_frame(ngram_sub[[i]])
  # tkplot(plotdata_ngram_sub)
  # plot(plotdata_ngram_sub, vertex.size = 20, 
  #      vertex.label.family="HiraKakuProN-W3")
  # 输出图片
  png(
    filename = paste0(copngdir, "/", i, "共现图", ".png"), 
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
}

# 分词频率历史变化 ----
# 词频数据标准化
freq_std <- lapply(freq, function(x) {
  data.frame(x["TERM"], x["twt.txt"]/max(x["twt.txt"])) 
})

# 合并词频数据
func_multimerge <- function(varls) {
  outdf <- varls[[1]]
  for (i in c(2: length(varls))) {
    names(varls[[i]]) <- c("TERM", names(varls)[i])
    outdf <- merge(outdf, varls[[i]], by = "TERM")
  }
  outdf
}
freq_stddf <- func_multimerge(freq_std)

# 按照出现频数标准值之和排序
freq_stddf$freqsum <- rowSums(
  freq_stddf[names(freq_stddf)[names(freq_stddf) %in% csvfiles]])
freq_stddf <- freq_stddf[order(freq_stddf$freqsum, decreasing = TRUE), ]

# 删除最后一列
freq_stddf <- freq_stddf[-length(freq_stddf)]
# 选取排名前15的词
freq_stddf <- head(freq_stddf, 16)
# 重命名各列为序号
names(freq_stddf) <- c("TERM", gsub("[a-zA-Z_.]", "", csvfiles))
head(freq_stddf)

# 变形成长数据
freq_stddf_long <- melt(freq_stddf, id = "TERM")
freq_stddf_long$variable <- as.numeric(freq_stddf_long$variable)

# 作图展示高频词随时间变化
png(
  filename = paste0("词频历史变化.png"), 
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 1600, height = 1600,
  bg = "transparent" # 透明背景
)
ggplot(freq_stddf_long, aes(variable, value)) + 
  geom_col(aes(fill = TERM, color = TERM)) + 
  facet_wrap(.~TERM) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.title = element_blank())
dev.off()

# 删除最高的两个词看什么情况
freq_stddf_long_2 <- 
  freq_stddf_long[freq_stddf_long$TERM %in% c("地球温暖化", "温暖化") == FALSE, ]
ggplot(freq_stddf_long_2, aes(variable, value)) + geom_line(aes(color = TERM)) + 
  facet_wrap(.~TERM)

