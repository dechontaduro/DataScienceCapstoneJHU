#install.packages(c("quanteda", "data.table", "RSQLite"))
library(quanteda)
library(data.table)
#library(RSQLite)

time.globalstart <- Sys.time()

rm(list = ls())

ngramMax <- 5
ngramMin <- 1
sampleFactor <- 0.1
#dfmTrimMin <- 4
minDocfreq <- 2
proyPath <- "C:\\Users\\juanc\\Dropbox\\DataScience\\Capstone\\ExploratoryDataAnalysis"

files <- c("news"="C:\\Dev\\R\\TextMining\\final\\en_US\\en_US.news.txt"
           , "blog"="C:\\Dev\\R\\TextMining\\final\\en_US\\en_US.blogs.txt"
           , "twitter"="C:\\Dev\\R\\TextMining\\final\\en_US\\en_US.twitter.txt"
)

all.sample <- "C:\\Dev\\R\\TextMining\\final\\en_US\\all.txt"

cleanText <- function(text){
  #https://rstudio-pubs-static.s3.amazonaws.com/206767_888032bfd7454688ab3a8f95628af99d.html
  text <- iconv(text, "latin1", "ASCII", sub="")
  text <- gsub(pattern="[[:cntrl:]]", replacement="", x=text)
  text <- gsub(pattern="[[:alnum:]]+://([[:alnum:]]\\.)?(.+)\\.([[:alnum:]]+)/?([[:alnum:]]*[[:punct:]]*)*", replacement="", x=text)
  text <- gsub(pattern="[[:alnum:]]+[_|\\.]*[[:alnum:]]*@[[:alnum:]]+(_|-|\\.)*[[:alnum:]]*\\.[[:alnum:]]+", replacement="", x=text)
  text <- gsub(pattern="#[[:alpha:]]+(_*[[:alnum:]]*)*", replacement="", x=text)
  text <- gsub(pattern=" @[[:alnum:]]+_*[[:alnum:]]*", replacement="", x=text)
  text <- gsub(pattern="[[:alnum:]]+(_|-|\\.)*[[:alnum:]]*@[[:alnum:]]+", replacement="", x=text)
  text <- gsub(pattern="[-|/|\\]", replacement=" ", x=text)
  text <- gsub(pattern="[^[:alpha:]]", replacement=" ", x=text)
  return(text)
}

readFile <- function(filename){
  con <- file(filename, "r")
  fileobj <- readLines(con)
  close(con)
  return(fileobj)
}

separateTerms = function(x){
  # Pre-allocate
  firstTerms = character(length(x))
  lastTerm = character(length(x))
  
  for(i in 1:length(x)){
    posOfSpaces = gregexpr("_", x[i])[[1]]
    posOfLastSpace = posOfSpaces[length(posOfSpaces)]
    firstTerms[i] = substr(x[i], 1, posOfLastSpace-1)
    lastTerm[i] = substr(x[i], posOfLastSpace+1, nchar(x[i]))
  }
  
  list(firstTerms=firstTerms, lastTerm=lastTerm)
}

createGramDiscountLeftOverProb = function(ngram.dt){
  # Supposed table "threeGramTable" as above, we want to add a "discount" column.
  ngram.dt$discount = rep(1, nrow(ngram.dt))
  
  # Calculate the discount coefficient.
  # We only consider n-grams that have 0 < frequency <= k (5). Larger than 5: "Reliable enough".
  for(currRTimes in 5:2){
    nextRTimes = currRTimes + 1
    
    currN = nrow(ngram.dt[frecuency == currRTimes])
    nextN = nrow(ngram.dt[frecuency == nextRTimes])
    
    currd = (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
    
    print(paste(currRTimes, nextRTimes, currN, nextN, currd, sep = "-"))
    # the beauty of "data.table"!
    ngram.dt[frecuency == currRTimes, discount := currd]
  }
  
  # Calculate the remaining probability (thanks to discounting...).
  leftoverprob <- ngram.dt[, .(leftoverprob=calcLeftOverProb(lastTerm, frecuency, discount)), by=firstTerms]
  list(gram = ngram.dt, leftoverprob = leftoverprob)
}

calcLeftOverProb = function(lastTerm, frequency, discount){
  all_freq = sum(frequency)
  
  return(1-sum((discount*frequency)/all_freq))
}

setwd(proyPath)

#Create samples files
lapply(names(files), function(x){
    print(paste(x, "create sample"))
    
    time.start <- Sys.time()
    filen <- paste("",files[x], sep = "")
    file.sample <- paste(filen, "sample", sampleFactor, sep = ".")
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "reading", filen))
    file.tmp <- readFile(filen)
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "sampling", filen))
    file.tmp <- sample(file.tmp, size=length(file.tmp)*sampleFactor, replace =FALSE)
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "cleaning", filen))
    file.tmp <- cleanText(file.tmp)
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "saving", file.sample))
    write(file.tmp, file.sample)
    time.end <- Sys.time()
    print(time.end - time.start)
    
    rm(file.tmp)
  })


file.unified <- character()

#Unify samples
for (i in 1:length(files)){
  x <- names(files)[i]
  print(paste(x, "Unify samples"))
  
  filen <- paste("",files[x], sep = "")
  file.sample <- paste(filen, "sample", sampleFactor, sep = ".")
  
  time.start <- Sys.time()
  print(paste(x, "opening", file.sample))
  file.Text <- readFile(file.sample)
  time.end <- Sys.time()
  print(time.end - time.start)
  
  time.start <- Sys.time()
  print(paste(x, "unifying", file.sample))
  file.unified <- c(file.unified, file.Text)
  time.end <- Sys.time()
  print(time.end - time.start)
  
  rm(file.Text)
}
write(file.unified, paste(all.sample, "sample", sampleFactor, sep = "."))
rm(file.unified)

files <- c("all" = all.sample)

#Create Ngrams
lapply(names(files), function(x){
  print(paste(x, "create ngrams"))
  
  filen <- paste("",files[x], sep = "")
  file.sample <- paste(filen, "sample", sampleFactor, sep = ".")
  
  time.start <- Sys.time()
  print(paste(x, "opening", file.sample))
  file.Text <- readFile(file.sample)
  time.end <- Sys.time()
  print(time.end - time.start)
  
  time.start <- Sys.time()
  print(paste(x, "tokening", file.sample))
  file.tokens <- tokens(file.Text, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
  time.end <- Sys.time()
  print(time.end - time.start)
  
  time.start <- Sys.time()
  print(paste(x, "lowering", file.sample))
  file.tokens <- tokens_tolower(file.tokens)
  time.end <- Sys.time()
  print(time.end - time.start)

  rm(file.Text)
  
  for(i in ngramMin:ngramMax){
    time.start <- Sys.time()
    print(paste(x, "creating document-feature matrix ngrams:", i))
    ngrams <- dfm(file.tokens, tolower = FALSE, ngrams = i, verbose = TRUE)
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "Trimming document-feature matrix ngrams:", i, minDocfreq, sep = " "))
    ngrams <- dfm_trim(ngrams, min_docfreq = minDocfreq)
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "saving document-feature matrix ngrams:", i))
    saveRDS(ngrams, paste(file.sample, i, ".rds", sep = "_"))
    time.end <- Sys.time()
    print(time.end - time.start)
    
    rm(ngrams)
  }
  rm(file.tokens)
})

lapply(names(files), function(x){
  print(paste(x, "create dt frecuency ngrams"))
  
  filen <- paste("",files[x], sep = "")
  file.sample <- paste(filen, "sample", sampleFactor, sep = ".")
  
  file.dt <- list()
  for(i in ngramMin:ngramMax){
    time.start <- Sys.time()
    print(paste(x, "opening dataframe ngrams:", i))
    ngrams <- readRDS(paste(file.sample, i, ".rds", sep = "_"))
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "calculating dt frecuency ngrams:", i))
    ngramsfrec <- docfreq(ngrams, scheme = "count")
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "calculating dt frecuency ngrams:", i))
    ngramsfrec <- as.data.table(ngramsfrec, key=names(ngramsfrec), keep.rownames=T)
    setnames(ngramsfrec, c("gram","frecuency"))
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "ordening dt frecuency ngrams:", i))
    setorder(ngramsfrec, -frecuency)
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "saving dt frecuency ngrams:", i))
    saveRDS(ngramsfrec, paste(file.sample, i, "frec.rds", sep = "_"))
    time.end <- Sys.time()
    print(time.end - time.start)
    
    rm(ngrams)
    rm(ngramsfrec)
  }
})


lapply(names(files), function(x){
  print(paste(x, "create first and last terms ngrams"))
  
  filen <- paste("",files[x], sep = "")
  file.sample <- paste(filen, "sample", sampleFactor, sep = ".")
  
  for(i in ngramMin:ngramMax){
    time.start <- Sys.time()
    print(paste(x, "opening data.table ngrams:", i))
    ngramsfrec <- readRDS(paste(file.sample, i, "frec.rds", sep = "_"))
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "calculating first and last terms ngrams:", i))
    ngramsfrec[, c("firstTerms","lastTerm") := separateTerms(gram)]
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "saving first and last terms ngrams:", i))
    saveRDS(ngramsfrec, paste(file.sample, i, "terms.rds", sep = "_"))
    time.end <- Sys.time()
    print(time.end - time.start)
    
    rm(ngramsfrec)
  }
})

lapply(names(files), function(x){
  print(paste(x, "create GramDiscountLeftOverProb ngrams"))
  
  filen <- paste("",files[x], sep = "")
  file.sample <- paste(filen, "sample", sampleFactor, sep = ".")
  
  for(i in ngramMin:ngramMax){
    time.start <- Sys.time()
    print(paste(x, "opening first and last terms ngrams:", i))
    ngramsfrec <- readRDS(paste(file.sample, i, "terms.rds", sep = "_"))
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "calculating GramDiscountLeftOverProb ngrams:", i))
    temp <- createGramDiscountLeftOverProb(ngramsfrec)
    time.end <- Sys.time()
    print(time.end - time.start)
    
    time.start <- Sys.time()
    print(paste(x, "saving GramDiscountLeftOverProb ngrams:", i))
    saveRDS(temp[[1]], paste(file.sample, i, "frecdis.rds", sep = "_"))
    saveRDS(temp[[2]], paste(file.sample, i, "frecdislop.rds", sep = "_"))
    time.end <- Sys.time()
    print(time.end - time.start)
    
    rm(temp)
  }
})

ngramsPred <- 
lapply(names(files), function(x){
  print(paste(x, "load GramDiscountLeftOverProb ngrams"))
  
  filen <- paste("",files[x], sep = "")
  file.sample <- paste(filen, "sample", sampleFactor, sep = ".")
  
  file.dt <- list()
  file.dt.lop <- list()
  for(i in ngramMin:ngramMax){
    time.start <- Sys.time()
    print(paste(x, "loading GramDiscountLeftOverProb ngrams:", i))
    file.dt[[i]] <- readRDS(paste(file.sample, i, "frecdis.rds", sep = "_"))
    file.dt.lop[[i]] <- readRDS(paste(file.sample, i, "frecdislop.rds", sep = "_"))
    
    time.end <- Sys.time()
    print(time.end - time.start)
  }

  return (list("dis" = file.dt, "lop" = file.dt.lop))
})

names(ngramsPred) <- names(files)

time.globalend <- Sys.time()
print(time.globalend - time.globalstart)