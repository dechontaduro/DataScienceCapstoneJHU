#require(devtools)
#install_version("tm", version = "0.6-2", repos = "http://cran.us.r-project.org")
#install.packages("quanteda")

require(stringi) #contar palabras
library(tm)
#require(quanteda)
#detach("package:quanteda", unload = T)
require(RWeka)

library(ggplot2)
library(slam)

#install.packages("bigmemory")
#library(bigmemory)

#objToRemove <- ls()
#objToRemove[objToRemove %IN% ('news.sample')]

rm(list = ls())

gc()

proyPath <- "C:\\Users\\juanc\\Dropbox\\DataScience\\Capstone\\ExploratoryDataAnalysis"
urlData <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fileData <- "C:\\Dev\\R\\TextMining\\Coursera-SwiftKey.zip"
fileTwitter <- "C:\\Dev\\R\\TextMining\\final\\en_US\\en_US.twitter.txt"
fileBlog <- "C:\\Dev\\R\\TextMining\\final\\en_US\\en_US.blogs.txt"
fileNews <- "C:\\Dev\\R\\TextMining\\final\\en_US\\en_US.news.txt"
RData <- "C:\\Dev\\R\\TextMining\\CapstoneExploratory.RData"

setwd(proyPath)

downloadAndUnzip <- function (url, filename, filenameIntoZip){
  if(!file.exists(filename)) { 
    download.file(url, destfile=filename, method="curl") 
  }

  if(!file.exists(filenameIntoZip)) { 
    unzip(filename)
  }
}

downloadAndUnzip(urlData, fileData, fileTwitter)

readFile <- function(filename){
    con <- file(filename, "r")
    fileobj <- readLines(con)
    close(con)
    return(fileobj)
}


news <- readFile(fileNews)
blog <- readFile(fileBlog)
twitter <- readFile(fileTwitter)

fileStats <- data.frame(File = c("news", "blogs", "twitter")
           , t(sapply(list(news, blog, twitter), stri_stats_general))
           , t(sapply(list(news, blog, twitter), stri_stats_latex))
           , t(sapply(list(nchar(news), nchar(blog), nchar(twitter)), summary))
           )

fileStats <- subset(fileStats, select = -c(LinesNEmpty, CharsNWhite, CharsWord, CharsCmdEnvir
                                          , CharsWhite,  Cmds, Envirs
                                          , X1st.Qu., Median, X3rd.Qu.))

colnames(fileStats) <- c("File", "Lines", "Chars", "Words", "Chars/line Min.", "Mean", "Max.")



set.seed(1234)
news.sample <- sample(news, size = round(length(news) * 0.1), replace =FALSE)
blog.sample <- sample(blog, size = round(length(blog) * 0.1), replace =FALSE)
twitter.sample <- sample(twitter, size = round(length(twitter) * 0.1), replace =FALSE)


fileStats.sample <- data.frame(File = c("news", "blogs", "twitter")
                        , t(sapply(list(news.sample, blog.sample, twitter.sample), stri_stats_general))
                        , t(sapply(list(news.sample, blog.sample, twitter.sample), stri_stats_latex))
                        , t(sapply(list(nchar(news.sample), nchar(blog.sample), nchar(twitter.sample)), summary))
)

fileStats.sample <- subset(fileStats.sample, select = -c(LinesNEmpty, CharsNWhite, CharsWord, CharsCmdEnvir
                                           , CharsWhite,  Cmds, Envirs
                                           , X1st.Qu., Median, X3rd.Qu.))

colnames(fileStats.sample) <- c("File", "Lines", "Chars", "Words", "Chars/line Min.", "Mean", "Max.")


#rm(list=c("news.dfm", "blog.dfm"))
#news.dfm <- dfm(news)
#blog.dfm <- dfm(blog)
#twitter.dfm <- dfm(twitter)
#news.dfm
#kwic(news, "terror")
#summary(news.dfm)

news.corpus <- Corpus(VectorSource(news.sample))
blog.corpus <- Corpus(VectorSource(blog.sample))
twitter.corpus <- Corpus(VectorSource(twitter.sample))

#all.corpus <- paste(news.corpus, blog.corpus, twitter.corpus)
#all.corpus

cleanCorpus <- function (corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  #corpus <- tm_map(corpus, stemDocument)
  #corpus <- tm_map(corpus, removeWords, stopwords())
  #if(!is.null(stopwordsAditional))
  #  corpus <- tm_map(corpus, removeWords, stopwordsAditional)
  corpus <- tm_map(corpus, PlainTextDocument)
  return(corpus)
}

news.corpus.clean <- cleanCorpus(news.corpus)
blog.corpus.clean <- cleanCorpus(blog.corpus)
twitter.corpus.clean <- cleanCorpus(twitter.corpus)
#all.corpus.clean <- c(news.corpus.clean, blog.corpus.clean, twitter.corpus.clean)


news.tdm <-  TermDocumentMatrix(news.corpus.clean)
blog.tdm <- TermDocumentMatrix(blog.corpus.clean)
twitter.tdm <- TermDocumentMatrix(twitter.corpus.clean)

#all.tdm <- TermDocumentMatrix(all.corpus.clean)

save.image(RData)
load(RData)


news.tdm.tot <- row_sums(news.tdm, na.rm = TRUE)
blog.tdm.tot <- row_sums(blog.tdm, na.rm = TRUE)
twitter.tdm.tot <- row_sums(twitter.tdm, na.rm = TRUE)


news.tdm.tot <- sort(news.tdm.tot, decreasing = TRUE)


getCoverage <- function (freqs,coverage) {
  counts = 0
  freqAcum = sum(freqs)*coverage
  for (i in 1:length(freqs)){
    if (counts < freqAcum){
      counts = counts + freqs[i]
    }
    else {
      return(i)
    }
  }
}

length(news.tdm.tot)

getCoverage(news.tdm.tot,0.5)
getCoverage(blog.tdm.tot,0.5)
getCoverage(twitter.tdm.tot,0.5)


coverage.table <- data.frame(File = c("news", "blogs", "twitter")
                             , C0.5 = sapply(list(news.tdm.tot, blog.tdm.tot, twitter.tdm.tot), getCoverage, coverage=0.5)
                             , C0.9 = sapply(list(news.tdm.tot, blog.tdm.tot, twitter.tdm.tot), getCoverage, coverage=0.9))


t(sapply(list(news.tdm.tot, blog.tdm.tot, twitter.tdm.tot), getCoverage, coverage=0.5))




news.tdm.rs <- removeSparseTerms(news.tdm, 0.999)
blog.tdm.rs <- removeSparseTerms(blog.tdm, 0.995)
twitter.tdm.rs <- removeSparseTerms(twitter.tdm, 0.997)

news.wordfrec <- sort(rowSums(as.matrix(news.tdm.rs)), decreasing=TRUE)
blog.wordfrec <- sort(rowSums(as.matrix(blog.tdm.rs)), decreasing=TRUE)
twitter.wordfrec <- sort(rowSums(as.matrix(twitter.tdm.rs)), decreasing=TRUE)

news.wordfrec[1:50]

news.wordfrec.n <- news.wordfrec / news.wordfrec[1]
blog.wordfrec.n <- blog.wordfrec / blog.wordfrec[1]
twitter.wordfrec.n <- twitter.wordfrec / twitter.wordfrec[1]

news.wordfrec.n[1:50]
blog.wordfrec.n[1:50]
twitter.wordfrec.n[1:50]




ggplot(as.data.frame(news.wordfrec), aes(news.wordfrec)) + 
  geom_histogram(fill = "red", alpha = 0.8)

  geom_histogram(data = as.data.frame(blog.wordfrec), fill = "red", alpha = 0.8) + 
  geom_histogram(data = as.data.frame(twitter.wordfrec), fill = "blue", alpha = 0.8) 

  
  
  ggplot(news.wordfrec, aes(x=reorder(Term,-Freq),Freq)) + 
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  

par(mfrow=c(1,3)) 
  
#Histogramas dependen de la cantidad de muestras????
hist(news.wordfrec
     #, breaks = 1000, xlim = c(0,0.05)
     , breaks = 1000, xlim = c(0,150)
     , main = "News term frecuency", xlab = "Term's frecuency")
hist(blog.wordfrec
     #, breaks = 1000, xlim = c(0,0.05)
     , breaks = 1000, xlim = c(0,5000)
     , main = "Blog term frecuency", xlab = "Term's frecuency")
hist(twitter.wordfrec
     #, breaks = 1000, xlim = c(0,0.05)
     , breaks = 1000, xlim = c(0,5000)
     , main = "Twitter term frecuency", xlab = "Term's frecuency")
  
news.wordfrec[1:25]
names(news.wordfrec[1:25])


news.wordfrec.tb <- table(news.wordfrec[1:25],names(news.wordfrec[1:25]))


par(las=2) # make label text perpendicular to axis
par(mfrow=c(1,3)) 

barplot(news.wordfrec[1:25]
        , names.arg=names(news.wordfrec[1:25])
        , main="News - 25 most frecuent Words"
        , horiz=TRUE
        , cex.names=1.2)
barplot(blog.wordfrec[1:25]
        , names.arg=names(blog.wordfrec[1:25])
        , main="Blog - 25 most frecuent Words"
        , horiz=TRUE
        , cex.names=0.8)
barplot(twitter.wordfrec[1:25]
        , names.arg=names(twitter.wordfrec[1:25])
        , main="Blog - 25 most frecuent Words"
        , horiz=TRUE
        , cex.names=0.8)




BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

news.tdm2 <- TermDocumentMatrix(news.corpus.clean, control = list(tokenize = BigramTokenizer))
blog.tdm2 <- TermDocumentMatrix(blog.corpus.clean, control = list(tokenize = BigramTokenizer))
twitter.tdm2 <- TermDocumentMatrix(twitter.corpus.clean, control = list(tokenize = BigramTokenizer))



news.tdm2.bm <-  as.big.matrix(as.matrix(news.tdm2))

news.tdm2.sm <- sparseMatrix(i=news.tdm2$i, j=news.tdm2$j, x=news.tdm2$v)

news.tdm2.ft <- findFreqTerms(news.tdm2,lowfreq = 1)
str(news.tdm2.ft)

news.tdm2.df <- data.frame(table(news.tdm2))

length(news.tdm2$dimnames$Terms)
length(news.tdm2$v)

news.tdm2.df <- as.data.frame(apply(news.tdm2,1,sum))

bigramDF <- data.frame(Term = news.tdm2$dimnames$Terms, Freq = news.tdm2$v)
dim(bigramDF)

head(news.tdm2)


news.tdm2.rs <- removeSparseTerms(news.tdm2, 0.9995)
blog.tdm2.rs <- removeSparseTerms(blog.tdm2, 0.998)
twitter.tdm2.rs <- removeSparseTerms(twitter.tdm2, 0.998)

news.bigramfrec <- sort(rowSums(as.matrix(news.tdm2.rs)), decreasing=TRUE)
blog.bigramfrec <- sort(rowSums(as.matrix(blog.tdm2.rs)), decreasing=TRUE)
twitter.bigramfrec <- sort(rowSums(as.matrix(twitter.tdm2.rs)), decreasing=TRUE)

length(news.bigramfrec)
length(blog.bigramfrec)
length(twitter.bigramfrec)

news.bigramfrec[grepl("^of", names(news.bigramfrec))]
blog.bigramfrec[grepl("^of", names(blog.bigramfrec))]
twitter.bigramfrec[grepl("^of", names(twitter.bigramfrec))]

news.bigramfrec.n <- news.bigramfrec / news.bigramfrec[1]
blog.bigramfrec.n <- blog.bigramfrec / blog.bigramfrec[1]
twitter.bigramfrec.n <- twitter.bigramfrec / twitter.bigramfrec[1]

hist(news.bigramfrec.n, breaks = 1000, xlim = c(0,0.05)
     , main = "News bigram frecuency", xlab = "Bigram's frecuency")
hist(blog.bigramfrec.n, breaks = 1000, xlim = c(0,0.5)
     , main = "Blog bigram frecuency", xlab = "Bigram's frecuency")
hist(twitter.bigramfrec, breaks = 1000, xlim = c(0,10000)
     , main = "Twitter bigram frecuency", xlab = "Bigram's frecuency")


TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

news.tdm3 <- TermDocumentMatrix(news.corpus.clean, control = list(tokenize = TrigramTokenizer))
blog.tdm3 <- TermDocumentMatrix(blog.corpus.clean, control = list(tokenize = TrigramTokenizer))
twitter.tdm3 <- TermDocumentMatrix(twitter.corpus.clean, control = list(tokenize = TrigramTokenizer))

news.tdm3.rs <- removeSparseTerms(news.tdm3, 0.9995)
blog.tdm3.rs <- removeSparseTerms(blog.tdm3, 0.9995)
twitter.tdm3.rs <- removeSparseTerms(twitter.tdm3, 0.9997)

news.trigramfrec <- sort(rowSums(as.matrix(news.tdm3.rs)), decreasing=TRUE)
blog.trigramfrec <- sort(rowSums(as.matrix(blog.tdm3.rs)), decreasing=TRUE)
twitter.trigramfrec <- sort(rowSums(as.matrix(twitter.tdm3.rs)), decreasing=TRUE)

length(news.trigramfrec)
length(blog.trigramfrec)
length(twitter.trigramfrec)


head(twitter.trigramfrec[order(names(twitter.trigramfrec))])

twitter.trigramfrec[grepl("^thanks", names(twitter.trigramfrec))]


news.trigramfrec.n <- news.trigramfrec / news.trigramfrec[1]
blog.trigramfrec.n <- blog.trigramfrec / blog.trigramfrec[1]
twitter.trigramfrec.n <- twitter.trigramfrec / twitter.trigramfrec[1]


hist(news.trigramfrec.n, breaks = 1000, xlim = c(0,0.2)
     , main = "News trigram frecuency", xlab = "Trigram's frecuency")
hist(blog.trigramfrec.n, breaks = 1000, xlim = c(0,0.2)
     , main = "Blog trigram frecuency", xlab = "Trigram's frecuency")
hist(twitter.trigramfrec.n, breaks = 1000, xlim = c(0,0.2)
     , main = "Twitter trigram frecuency", xlab = "Trigram's frecuency")


library(wordcloud)
wordcloud(names(news.wordfrec[3:50]),news.wordfrec[3:50],max.words=30,colors=brewer.pal(8,"Dark2"))
wordcloud(names(news.bigramfrec),news.bigramfrec,max.words=10,colors=brewer.pal(8,"Dark2"))
wordcloud(names(news.trigramfrec),news.bigramfrec,max.words=50,rot.per=.3,colors=brewer.pal(8,"Dark2"))
