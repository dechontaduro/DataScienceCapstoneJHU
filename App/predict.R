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


predictNextWord <- function(text, filengram){
  text <- cleanText(text)
  text <- tolower(text)
  
  words <-  unlist(strsplit(text," "))
  nwords <- length(words)
  
  if(nwords > ngramMax - 1){
    nwords <- ngramMax - 1
  }
  
  for(i in nwords:1){
    igram <- i + 1
    found <- FALSE
    firstTermsLookup <- paste(tail(words, igram - 1), collapse = "_")
    groupGrams <- ngramsPred[[filengram]][["dis"]][[igram]][firstTerms == firstTermsLookup]
    
    if(nrow(groupGrams) > 0){
      return(list("grams" = groupGrams, "nwords" = nwords, "ngram" = igram))
    }
  }
  groupGrams <- ngramsPred[[filengram]][["dis"]][[1]][1]
  return(list("grams" = groupGrams, "nwords" = nwords, "ngram" = 1))
}

finalProbKatz <- function (ngrams, nwords, ngram, filengram){
  for(i in nwords:1){
    igram <- i + 1
    found <- FALSE
    firstTermsLookup <- paste(tail(words, igram - 1), collapse = "_")
    
    groupGrams <- ngramsPred[[filengram]][["dis"]][[igram]][firstTerms == firstTermsLookup]
    
    if(nrow(groupGrams) > 0){
      all_freq <- sum(groupGrams$frecuency)
      groupGrams$finalProb <- ((groupGrams$discount * groupGrams$frecuency) / all_freq)
      setorder(groupGrams, -finalProb)
      print(groupGrams)
      found <- TRUE
    }
    else{
      firstTermsLookup <- paste(tail(words, igram - 1 - 1), collapse = "_")
      groupGramsPrev <- ngramsPred[[filengram]][["dis"]][[igram - 1]][firstTerms == firstTermsLookup]
      beta = ngramsPred[[filengram]][["dis"]][[igram - 1]][firstTerms == firstTermsLookup]$leftoverprob
      
      if(nrow(groupGramsPrev) > 0){
        groupGrams.remain = groupGrams[!(groupGramsPrev$lastTerm %in% groupGrams$lastTerm)]
        all_freq = sum(groupGram$frecuency)
        alpha = beta_leftoverprob / sum((oneGroupIn2Gram_Remain$frequency * oneGroupIn2Gram_Remain$discount) / all_freq)
        
        finalProb = alpha * ((oneRecordIn2Gram$frequency * oneRecordIn2Gram$discount ) / all_freq)
      }
    }
    
    
  }
}

