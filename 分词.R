library(rJava)  
library(Rwordseg)
library(wordcloud2)

#读取数据
hlzj <-readLines("D://R_study//分词//侠客行.txt",encoding ="UTF-8")  
length(hlzj)  

segment.options(isNameRecognition=TRUE) #人名识别为真 

hlzjTemp <- gsub("[0-9０１２３４５６７８９ < > ~]","",hlzj)  
hlzjTemp <- segmentCN(hlzjTemp)  
hlzjTemp[1:2]  


stopwords<- unlist(read.table("D:\\R_study\\分词\\StopWords.txt",stringsAsFactors=F))  
stopwords[50:100]  

#移除停词
removeStopWords <- function(x,stopwords) {  
    temp <- character(0)  
    index <- 1  
    xLen <- length(x)  
    while (index <= xLen) {  
        if (length(stopwords[stopwords==x[index]]) <1)  
            temp<- c(temp,x[index])  
        index <- index +1  
    }  
    temp  
} 

hlzjTemp2 <-lapply(hlzjTemp,removeStopWords,stopwords)  
hlzjTemp2[1:2] 

library('RColorBrewer')

words <- lapply(hlzjTemp2,strsplit," ")  
wordsNum <- table(unlist(words))  
wordsNum <- sort(wordsNum) #排序  
wordsData <- data.frame(words =names(wordsNum), freq = wordsNum)  
weibo.top150 <- tail(wordsData,300) #取前150个词  
colors=brewer.pal(8,"Dark2")  
wordcloud2(weibo.top150$words),weibo.top150$freq,scale=c(8,0.5),colors=colors,random.order=F)


library(wordcloud2)  

wordcloud2(demoFreq, size = 1,shape = 'star')  




