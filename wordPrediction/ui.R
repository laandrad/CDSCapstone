library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Prediction App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            textInput("word", "Type a word...")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
            textOutput("predWord")
    )
  )
))
