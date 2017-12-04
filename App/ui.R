library(shiny)

shinyUI(fluidPage(
  titlePanel("Predict Next Word"),
  
  sidebarLayout(
    sidebarPanel(
       radioButtons("corpus","Corpus"
                      , choices = c("All"="all", "News"="news", "Blog"="blog", "Twitter"= "twitter"))
       , textInput("text", "Text"
                 ,placeholder = "Write your text to predict next word" )
    ),
    
    mainPanel(
      h3(textOutput("corpus"))
      , h3(textOutput("sentence"))
      , h3(textOutput("nextWord"))
      , h3(textOutput("firstTerms"))
      , h3("Grams")
      , dataTableOutput("ngrams")
    )
  )
))
