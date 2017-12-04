library(shiny)
library(quanteda)
library(data.table)

source("predict.R")

shinyServer(function(input, output) {
  resultPred <- reactive({predictNextWord(input$text, input$corpus)})

  output$corpus <- renderText({
    paste("Corpus", input$corpus, sep = ":")
  })
  
  output$sentence <- renderText({
    paste("Sentence", input$text, sep = ":")
  })
  
  output$nextWord <- renderText({
    res <- resultPred()
    if(nrow(res$grams)>0){
      paste("Next word", res$grams[1]$lastTerm, sep = ":")
    }
    else{
      "Next word not found"
    }
  })
  
  output$firstTerms <- renderText({
    res <- resultPred()
    if(nrow(res$grams)>0){
      paste("Gram", res$grams[1]$firstTerms, sep = ":")
    }
    else{
      "Gram not found"
    }
  })
  
  output$ngrams <- renderDataTable({
    res <- resultPred()
    if(nrow(res$grams)>0){
      res$grams
      }
  })
  
})
