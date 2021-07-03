# install mecab in cmd
# brew install mecab
# brew install mecab-ipadic

# install the required packages 

library(readr)
library(RMeCab)
library(ggplot2)

if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_ALL", "Japanese")
} 

# 读取原始数据并筛出子集以测试代码
twtdata <- read_csv("jst201909lite.csv")
twtdata_sub <- twtdata
twtdata_sub

# 清洗推文数据
# Remove mentions, urls, emojis, numbers, punctuations, etc.
twtdata_sub$text <- gsub("@\\w+", "", twtdata_sub$text)
twtdata_sub$text <- gsub("https?://.+", "", twtdata_sub$text)
twtdata_sub$text <- gsub("\\d+\\w*\\d*", "", twtdata_sub$text)
twtdata_sub$text <- gsub("#\\w+", "", twtdata_sub$text)
twtdata_sub$text <- gsub("[[:punct:]]", " ", twtdata_sub$text)
twtdata_sub$text <- gsub("[0-9]", "", twtdata_sub$text)

# 将推文写入文件
write(twtdata_sub$text, "twt.txt")

# 提取词频
start <- Sys.time()
freq_ori <- docDF("twt.txt", type = 1, dic = "C:/data/kangdict.dic")
Sys.time() - start

# 进一步清除停止词
stopwords <- c("する","それ","なる","ない","そこ","これ","ある", 
               "n", "RT", letters, LETTERS, 
               "+", "<", ">", "><", 
               "地球温暖化")
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
ggplot(head(freq, 30), aes(x = reorder(TERM, twt.txt), y = twt.txt)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# 输出目前结果，进一步手动挑选
write_csv(head(freq, 400), "请挑选停用词.csv")

# 共现图 ----
library(igraph)
library(ggraph)

# 构建N-gram列表：
# 假设N=1
# 只选取三类词性
# 问题：有些不需要的字符没有清理干净，影响共现判断
ngram <- docDF("twt.txt", type = 1, pos = c("名詞", "形容詞", "動詞"), 
               nDF = 1, N = 2, dic = "C:/data/kangdict.dic")
dim(ngram)
# 查看各种词汇共现出现的次数
table(ngram$twt.txt)

# 去除只出现次数少的连接
ngram_sub <- ngram[ngram$twt.txt > 50, ]
dim(ngram_sub)

# 提取作图数据
plotdata_ngram_sub <- graph_from_data_frame(ngram_sub)
# tkplot(plotdata_ngram_sub)
# plot(plotdata_ngram_sub, vertex.size = 20)

ggraph(plotdata_ngram_sub, layout = "fr") +
  geom_edge_link(aes(edge_alpha = twt.txt), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

grep("禿", twtdata_sub$text, value = TRUE)[1:60]
twtdata_sub$text[1:6]


# 用
library(jiebaR)
segment(twtdata_sub$text[1], worker())

source("http://rmecab.jp/R/Aozora.R")
x <- Aozora("http://www.aozora.gr.jp/cards/000121/files/637_ruby_4095.zip")

RMeCabFreq()

S <- data.frame(S= "メロスは激怒した", stringsAsFactors = FALSE)
a <- docDF(S, column = "S", type = 1, N = 1, nDF = 1)
a
S <- data.frame(user = "mine", text = head(twtdata_sub, 1)$text)
a <- docDF(S, column = "S", type = 1, N = 2, nDF = 1, dic = "./jisho.csv")
a
a <- docDF(head(twtdata_sub, 1), column = 2, type = 1)



res <- RMeCabC("すもももももももものうち")
res

res <- RMeCabC("石田基広")
res

res <- RMeCabC("石田基広", dic = "C:/data/ishida.dic")
res


write("前より太ったよね って聞くのがコミュニケーションだと思ってるオッサンは何なん  この間より禿げましたよね いや絶対禿げましたよ 前は眉から 地球温暖化の影響ですか  って聞かれたら切れるだろ 頭に除草剤まくぞ ", "test.txt")

RMeCabC("前より太ったよね って聞くのがコミュニケーションだと思ってるオッサンは何なん  この間より禿げましたよね いや絶対禿げましたよ 前は眉から 地球温暖化の影響ですか  って聞かれたら切れるだろ 頭に除草剤まくぞ ", dic = "C:/data/kangdict.dic")

a <- docDF("test.txt", type = 1, dic = "C:/data/kangdict.dic")
a
"地球温暖化" %in% a$TERM
"影響" %in% a$TERM
"基広" %in% a$TERM

