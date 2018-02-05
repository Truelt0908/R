library('readr')
library(rJava)
library(Rwordseg)
library(wordcloud2)  
library(stringr)
#读取数据
data <- read_csv("D:/python_study/python学习/数据资料/R_data.csv")
text <- unique(data[,c('地市级政区代码','党委书记姓名')])
text <- text[,2]
text <- text[complete.cases(text$党委书记姓名),]

#去掉英文字母及数字
text <- gsub('[a-zA-Z0-9]','',text$党委书记姓名)

#去掉姓
name <- str_sub(text,2,-1)

#分词
segword <- segmentCN(strwords = name)

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
library('tmcn')
word_freq <- createWordFreq(unlist(segword))

#tmcn和segword包函数名称一样，有冲突，统计完词频后，将其设置成不加载
detach(package:tmcn)


#指定图片
china_black = system.file("examples/china_black.jpg",package = "wordcloud2") 

t = system.file("examples/t.png",package = "wordcloud2") 

#绘制词云图
wordcloud2(word_freq, figPath = china_black, color = "random-light",backgroundColor = 'random-dark')

#一切使用默认参数
wordcloud2(word_freq,size=1,shape='star',fontFamily="微软雅黑")

letterCloud(word_freq,word ='R')

