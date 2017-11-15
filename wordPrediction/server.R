
library(shiny)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

        word = reactive({
                input$word
        })
        
        # load data
        biGrams = read.csv("bigrams.csv")

        # load Word prediction function
        source("wordPredict.R")
        prediction = reactive({
                if (!is.null(word())) {
                        wordPredict(word(), biGrams)
                } else {
                        " "
                }
        }) 
        
        output$predWord = renderText({
                paste("My word prediction is", prediction())
                })
  
})
