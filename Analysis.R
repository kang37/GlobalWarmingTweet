# 准备工作：
# 通过终端安装mecab：
# brew install mecab
# brew install mecab-ipadic

# 安装所需的R包
# 注意安装RMeCab的时候可以用如下语句：
# install.packages ("RMeCab", repos = "http://rmecab.jp/R", type = "source")

library(PerformanceAnalytics)
library(ggplot2)
library(patchwork)
library(readr)
library(RMeCab)
library(igraph)
library(ggraph)
library(reshape2)
library(showtext)

# 基本设置 ----
set_txtexit <- TRUE  # 是否已有推文内容的*.txt文件
set_picexp <- TRUE # 是否出图
showtext_auto()

ana_yr <- 2012:2020

# 数据读取 ----
##. 推文数据及清洗 ----
# 读取推文存储在年和月双层数据列表中
raw <- vector("list", length = length(ana_yr))
names(raw) <- ana_yr

for (i in names(raw)) {
  raw[[i]] <- vector("list", length = 12)
  names(raw[[i]]) <- 1:12
}

for (i in ana_yr) {
  file2read <- grep("csv",  list.files(paste0(i)), value = TRUE)
  for (j in 1:12) {
    raw[[paste0(i)]][[j]] <- read.csv(paste0("RawData", i, "/", file2read[j]))
  }
}

# 统计各年内推文总数
# 统计年内总数的函数
# 输入列表
fun_yrnum <- function(x) {
  num <- vector("numeric")
  for (i in 1:12) {
    num <- c(num, dim(x[[i]])[1])
  }
  num <- sum(num)
  return(num)
}

num <- vector("numeric")
for (i in ana_yr) {
  num <- c(num, fun_yrnum(raw[[paste(i)]]))
}
num_yr <- data.frame(
  yr = ana_yr, 
  num = num
)

# 构建年度数据
raw_yr <- lapply(raw, function(x) Reduce(rbind, x))

##. 气象数据 ----
# 读取日本气象局下载的气候异常指标数据
file2read <- list.files("JMA")
jma <- vector("list", length(file2read))
names(jma) <- file2read

for (i in 1:length(file2read)) {
  jma[[i]] <- read.csv(paste0("JMA/", file2read[i]), fileEncoding = "Shift-JIS")
  names(jma[[i]]) <- c("year", gsub(".csv", "", file2read[i]))
}

fun_merge <- function(x, y) {
  output <- merge(x, y, by = "year")
}
jma_df <- Reduce(fun_merge, jma)
num_yr <- merge(num_yr, jma_df, by.x = "yr", by.y = "year")

# 年度分析 ----
##. 概况可视化 ----
num_yr <- merge(num_yr, read.csv("RawData/UserCount2012-2021.csv"), 
                by.x = "yr", by.y = "X")
# 各年各月份转推、非转推、用户比例图
if(set_picexp) png("ProcData/各年推文及用户数.png", 
                   width = 900, height = 500, res = 150)
temp_num_yr <- num_yr
temp_num_yr$date <- paste0(temp_num_yr$yr, "06")
temp_num_mth <- num_mth
temp_num_mth <- merge(temp_num_mth, temp_num_yr, by = "date", all.x = TRUE)
temp_num_mth$label <- ""
temp_num_mth$label[which(temp_num_mth$mth == 1)] <- 
  temp_num_mth$yr.x[which(temp_num_mth$mth == 1)]
ggplot(temp_num_mth) + 
  geom_col(aes(x = as.factor(date), y = tweet/1000), fill = "red") + 
  geom_col(aes(x = as.factor(date), y = tweetNoRT/1000), fill = "blue") + 
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "User percentage (%)")) +
  labs(x = "", y = "Number of tweet (x1000)") +
  theme(axis.ticks.x = element_blank(), 
        panel.grid.major = element_blank()) + 
  scale_x_discrete(labels = temp_num_mth$label) +
  geom_line(data = na.omit(temp_num_mth), 
            aes(x = as.factor(date), y = Percentage*10, group = 1))
if(set_picexp) dev.off()

##. 词频分析和共现 ----
# 设置停用词
stopwords <- c(
  "する","それ","なる","ない","そこ","これ","ある", "さん", "なん", "の", "ん", 
  "いる", "思う", "そう", "れる", "くる", "考える", "言う", "ー", 
  "できる", "てる", "でる", "世界の財界", 
  "一", "いい", "何", "いう", "できる", "られる", 
  "n", "RT", letters, LETTERS, 
  "+", "<", ">", "><"
)
# 选择用户自定义词典
userdic <- "/Users/Kang/Documents/R/GlobalWarmingTweet/kang.dic"

# 写出txt文件以供后续词频和共现分析
if(set_txtexit == FALSE) {
  for (i in names(raw_yr)) {
    # 导入数据
    twtdata_sub <- raw_yr[[i]]
    
    # 清洗推文数据
    # Remove mentions, urls, emojis, numbers, punctuations, etc.
    twtdata_sub$text <- gsub("@\\w+", "", twtdata_sub$text)
    twtdata_sub$text <- gsub("https?://.+", "", twtdata_sub$text)
    twtdata_sub$text <- gsub("\\d+\\w*\\d*", "", twtdata_sub$text)
    twtdata_sub$text <- gsub("#\\w+", "", twtdata_sub$text)
    twtdata_sub$text <- gsub("[[:punct:]]", " ", twtdata_sub$text)
    twtdata_sub$text <- gsub("[0-9]", "", twtdata_sub$text)
    
    # 将推文写入*.txt文件
    write(twtdata_sub$text, file = paste0("ProcData/TxtGen/", i, "_text.txt"))
  }
}

# 词频分析
# 构建变量用于存放词频结果
freq <- vector("list", length(raw_yr))
names(freq) <- names(raw_yr)

for (i in names(raw_yr)) {
  freq[[i]] <- 
    docDF(target = paste0("ProcData/TxtGen/", i, "_text.txt"), 
          type = 1, dic = userdic)
  
  # 进一步清除停止词
  freq[[i]] <- freq[[i]][freq[[i]]$POS1 %in% c("名詞","動詞","形容詞") & 
                           !(freq[[i]]$POS2 %in% c("非自立","接尾", "数")) &
                           !(freq[[i]]$TERM %in% stopwords), ]
  
  # 归并相同的词语
  names(freq[[i]])[length(freq[[i]])] <- "txt"
  freq[[i]] <- aggregate(txt ~ TERM, data = freq[[i]], sum)
  # 按照出现次数排序
  freq[[i]] <- freq[[i]][order(freq[[i]]$txt, decreasing = TRUE), ]
}

# 词频图片导出
for (i in names(raw_yr)) {
  png(
    filename = paste0("ProcData/WordFreqBar", "/", i, "词频", ".png"), 
    res = 300, # 300ppi分辨率
    width = 1600, height = 1600,
    bg = "transparent" # 透明背景
  )
  theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))
  print(
    ggplot(head(freq[[i]], 30), aes(x = reorder(TERM, txt), y = txt)) + 
      geom_bar(stat = "identity") + 
      labs(x = i) + 
      coord_flip()
  )
  dev.off()
}

# 共现分析
# 构建变量用于存放共现分析结果
ngram <- vector("list", length(raw_yr))
names(ngram) <- names(raw_yr)
ngram_sub <- vector("list", length(raw_yr))
names(ngram_sub) <- names(raw_yr)

# 共现数据计算生成
for (i in names(raw_yr)) {
  # 构建N-gram列表：
  # 假设N=1，且只选取三类词性
  # 待办：有些不需要的字符没有清理干净，影响共现判断
  ngram[[i]] <- docDF(target = paste0("ProcData/TxtGen/", i, "_text.txt"), 
                      type = 1, pos = c("名詞", "形容詞", "動詞"), 
                      nDF = 1, N = 2, dic = userdic)
  # 进一步去除停用词
  ngram[[i]] <- ngram[[i]][!ngram[[i]]$N1 %in% stopwords, ]
  ngram[[i]] <- ngram[[i]][!ngram[[i]]$N2 %in% stopwords, ]
  dim(ngram[[i]])
  names(ngram[[i]])[5] <- "text"
  
  # 保留出现次数排名前50的连接
  ngram_sub[[i]] <- 
    ngram[[i]][ngram[[i]]$text %in% names(tail(table(ngram[[i]]$text), 50)), ]
  dim(ngram_sub[[i]])
}

# 共现图导出
for (i in names(raw_yr)) {
  # 提取作图数据
  plotdata_ngram_sub <- graph_from_data_frame(ngram_sub[[i]])
  # tkplot(plotdata_ngram_sub)
  # plot(plotdata_ngram_sub, vertex.size = 20, 
  #      vertex.label.family="HiraKakuProN-W3")
  # 输出图片
  png(
    filename = paste0("ProcData/CoOccNet/", i, "共现", ".png"), 
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1600,
    bg = "transparent" # 透明背景
  )
  print(
    ggraph(plotdata_ngram_sub, layout = "fr") +
      geom_edge_link(aes(edge_alpha = text), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(
        aes(label = name), size = 3, repel=TRUE, family="HiraKakuProN-W3") +
      theme_graph(base_size=12)
  )
  dev.off()
}

##. 分词频率历史变化 ----
# 待办：尚未测试
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

##. 气候异常相关分析 ----
# 检测各个自变量的正态性
for(i in gsub(".csv", "", names(jma))) {
  cat(i, shapiro.test(num_yr[[i]])$p.value, "\n")
}
# 待办：大部分不符合正态性，之后可以考虑进行变换

# 检测自变量自相关性
if(set_picexp) png("ProcData/自变量自相关图.png", 
                   width = 900, height = 900, res = 150)
chart.Correlation(
  num_yr[c("num", "amdday200mm_p", "amdday400mm_p", "amdhour50mm_p", 
           "amdhour80mm_p", "max30up_p", "max35up_p", "min0down_p", "min25up_p")])
if(set_picexp) dev.off()

# 统计定量分析：单因素线性回归
for (i in names(jma_df)[-1]) {
  res <- cor.test(num_yr$num, num_yr[, i])
  cat(i, "\t", "p", res$p.value, "\t", "cor", res$estimate, "\n")
}
# 对p值显著的结果画图
fit <- lm(num ~ amdday400mm_p, data = num_yr)
if(set_picexp) png("ProcData/单因素回归分析图.png", 
                   width = 1000, height = 1000, res = 150)
plot(num_yr$amdday400mm_p, num_yr$num)
abline(fit)
if(set_picexp) dev.off()

##. 情感分析结果图 ----
# 情感分析输入数据来自Python分析输出
senti <- read.csv("RawData/oseti201201-202111.csv")
if(set_picexp) png("ProcData/2012-2021各月份情感分析.png", 
                   width = 900, height = 500, res = 150)
senti_lng$variable <- 
  factor(senti_lng$variable, levels = c("Positive", "Neutral", "Negative"))
senti_lng <- reshape2::melt(senti, id = "X")
senti_lng$month <- substr(senti_lng$X, 5, 6)
senti_lng$label <- ""
senti_lng$label[which(senti_lng$month == "01")] <- 
  substr(senti_lng$X[which(senti_lng$month == "01")], 1, 4)
ggplot(senti_lng) + 
  geom_col(aes(as.factor(X), value, fill = variable, color = variable)) +
  labs(x = "", y = "Percentage") +
  theme(axis.ticks.x = element_blank(), 
        panel.grid.major = element_blank()) + 
  scale_x_discrete(labels = temp_num_mth$label) 
if(set_picexp) dev.off()

# 月度数据 ----
##. 概况可视化 ----
# 绝对值读取和整理
num_mth <- read.csv("RawData/TwCount201201-202111.csv")
names(num_mth)[1] <- "date"
num_mth$yr <- substr(num_mth$date, 1, 4)
num_mth$mth <- as.numeric(substr(num_mth$date, 5, 6))

# 相对值可视化
# 汇总计算年度数据
num_mth_smry <- aggregate(tweet ~ yr, data = num_mth, FUN = sum)
names(num_mth_smry)[2] <- "tweet_tot"
for (i in c("tweetNoRT", "retweet", "tweetReply", "tweetQt", "tweetOrg", 
            "tweet_No_RT_url", "tweetOrg_No_url")) {
  num_mth_smry[paste0(i, "_tot")] <- 
    aggregate(num_mth[[i]] ~ num_mth$yr, FUN = sum)[2]
}

# 将年度数据合并到月份并且计算各项指标标准化值
num_mth_std <- merge(num_mth, num_mth_smry, by = "yr")
for (i in c("tweet", "tweetNoRT", "retweet", "tweetReply", "tweetQt", "tweetOrg", 
            "tweet_No_RT_url", "tweetOrg_No_url")) {
  num_mth_std[[i]] <- 
    num_mth_std[[i]] / num_mth_std[[paste0(i, "_tot")]]
}
num_mth_std$mth <- as.factor(num_mth_std$mth)

# 点图
for (i in c("tweet", "tweetNoRT", "retweet", "tweetReply", "tweetQt", 
            "tweetOrg", "tweet_No_RT_url", "tweetOrg_No_url")) {
  if(set_picexp) png(paste("ProcData/月度数据标准值点图", i, ".png"), 
                     width = 1000, height = 1000, res = 150)
  print(ggplot(num_mth_std) + 
          geom_point(aes_string(x = "mth", y = i, color = "yr")) +
          labs(x = "Month", y = paste0("Percentage of ", i)))
  if(set_picexp) dev.off()
}
# 各月份箱型图
for (i in c("tweet", "tweetNoRT", "retweet", "tweetReply", "tweetQt", 
            "tweetOrg", "tweet_No_RT_url", "tweetOrg_No_url")) {
  if(set_picexp) png(paste("ProcData/月度数据标准值箱型图", i, ".png"), 
                     width = 1000, height = 1000, res = 150)
  print(ggplot(num_mth_std) + 
          geom_boxplot(aes_string(x = "mth", y = i)) +
          labs(x = "Month", y = paste0("Percentage of ", i)))
  if(set_picexp) dev.off()
}
# 各年份箱型图
for (i in c("tweet", "tweetNoRT", "retweet", "tweetReply", "tweetQt", 
            "tweetOrg", "tweet_No_RT_url", "tweetOrg_No_url")) {
  if(set_picexp) png(paste("ProcData/年度数据箱型图", i, ".png"), 
                     width = 1000, height = 1000, res = 150)
  print(ggplot(num_mth) + 
          geom_boxplot(aes_string(x = "yr", y = i)) + 
          labs(x = "Year", y = paste0("Number of ", i)))
  if(set_picexp) dev.off()
}
# 特定推文类型占特定推文类型比例折线图
if(set_picexp) png(paste("ProcData/原创推文占比折线图", i, ".png"), 
                   width = 1000, height = 1000, res = 150)
temp_num_mth <- num_mth
temp_num_mth$label <- ""
temp_num_mth$label[which(temp_num_mth$mth == 1)] <- 
  temp_num_mth$yr[which(temp_num_mth$mth == 1)]
ggplot(num_mth) + 
  geom_line(aes(x = as.factor(date), 
                y = tweetOrg/tweetNoRT, group = 1), color = "red") + 
  geom_line(aes(x = as.factor(date), 
                y = tweetOrg_No_url/tweetNoRT, group = 1), color = "blue") + 
  labs(x = "", y = "Percentage") +
  theme(axis.ticks.x = element_blank(), 
        panel.grid.major = element_blank()) + 
  scale_x_discrete(labels = temp_num_mth$label)
if(set_picexp) dev.off()

##. 2021年9-11月词频及共现 ----
# 读取原始数据
raw_mth_2021 <- vector("list", length = 3)
names(raw_mth_2021) <- 
  c("Clr202109lite.csv", "Clr202110lite.csv", "Clr202111lite.csv")
for (i in names(raw_mth_2021)) {
  # 读取*.csv数据
  raw_mth_2021[[i]] <- read.csv(paste0("RawData/ClrCsv/", i))
  
  # 导入数据
  twtdata_sub <- raw_mth_2021[[i]]
  
  # 将推文写入*.txt文件
  write(as.character(twtdata_sub$textClr), 
        file = paste0("ProcData/TxtGen/Mth2021/", i, "_text.txt"))
}

# 词频分析
# 待办：无法顺利读取2021年10月的数据

# 共现分析
# 构建变量用于存放共现分析结果
ngram_2021 <- vector("list", length = 3)
names(ngram_2021) <- 
  c("Clr202109lite.csv", "Clr202110lite.csv", "Clr202111lite.csv")
ngram_2021_sub <- vector("list", length = 3)
names(ngram_2021_sub) <- 
  c("Clr202109lite.csv", "Clr202110lite.csv", "Clr202111lite.csv")

# 共现数据计算生成
for (i in names(ngram_2021)) {
  # 构建N-gram列表：
  # 假设N=1，且只选取三类词性
  # 待办：有些不需要的字符没有清理干净，影响共现判断
  ngram_2021[[i]] <- 
    docDF(target = paste0("ProcData/TxtGen/Mth2021/", i, "_text.txt"), 
          type = 1, pos = c("名詞", "形容詞", "動詞"), 
          nDF = 1, N = 2, dic = userdic)
  # 进一步去除停用词
  ngram_2021[[i]] <- ngram_2021[[i]][!ngram_2021[[i]]$N1 %in% stopwords, ]
  ngram_2021[[i]] <- ngram_2021[[i]][!ngram_2021[[i]]$N2 %in% stopwords, ]
  dim(ngram_2021[[i]])
  names(ngram_2021[[i]])[5] <- "text"
  
  # 保留出现次数排名前50的连接
  ngram_2021_sub[[i]] <- 
    ngram_2021[[i]][ngram_2021[[i]]$text %in% 
                      names(tail(table(ngram_2021[[i]]$text), 50)), ]
  dim(ngram_2021_sub[[i]])
}

# 共现图导出
for (i in names(ngram_2021)) {
  # 提取作图数据
  plotdata_ngram_2021_sub <- graph_from_data_frame(ngram_2021_sub[[i]])
  # tkplot(plotdata_ngram_2021_sub)
  # plot(plotdata_ngram_2021_sub, vertex.size = 20, 
  #      vertex.label.family="HiraKakuProN-W3")
  # 输出图片
  png(
    filename = paste0("ProcData/CoOccNet/Mth2021/", i, "共现", ".png"), 
    res = 300, # 300ppi 分辨率
    width = 1600, height = 1600,
    bg = "transparent" # 透明背景
  )
  print(
    ggraph(plotdata_ngram_2021_sub, layout = "fr") +
      geom_edge_link(aes(edge_alpha = text), show.legend = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(
        aes(label = name), size = 3, repel=TRUE, family="HiraKakuProN-W3") +
      theme_graph(base_size=12)
  )
  dev.off()
}
