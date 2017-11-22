
library(shiny)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

        words = reactive({
                input$sentence
        })
        
        # load data
        biGrams = read.csv("bigrams.csv")

        # load Word prediction function
        source("wordPredict.R")
        prediction = reactive({
                        wordPredict(words(), biGrams)
        }) 
        
        output$predWord = renderText({
                        paste(words(), prediction()[[1]])
                })
        
        output$altWords = renderText({
                prediction()[[2]] %>% paste(collapse = ", ")
        })
  
})
