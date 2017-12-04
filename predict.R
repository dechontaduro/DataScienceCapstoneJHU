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
