library(rJava)
library(Rwordseg)
library(tmcn)
library(wordcloud2)  
data0 <- read.csv("D:/R_study/d.csv",encoding="utf-8")  
data0 <- unique(data)# 去除重复的数据
 
#去除评论中含有的英文和数字

text0 <- gsub('[a-zA-Z0-9]','',data0$comment)

#插入自定义词汇

words <- c('范冰冰')

insertWords(strwords=words)

#分词

segword <- segmentCN(strwords=text)

#创建停止词库,并转为向量格式

mystopwords <- read.table("D:\\R_study\\分词\\StopWords.txt",stringsAsFactors=FALSE)

mystopwords <- as.vector(mystopwords[,1])

#自定义删除停止词函数

removewords <- function(target_words,stop_words)
    
{
    
    target_words <- target_words[target_words%in%stop_words==FALSE]
    
    return(target_words)
    
}

segword2 <- sapply(X=segword,FUN=removewords,mystopwords)


#绘制文字云

word_freq <- createWordFreq(unlist(segword2))

#一切使用默认参数

wordcloud2(word_freq)

wordcloud2(word_freq[0:500,],size=1,shape='star',fontFamily="微软雅黑")









