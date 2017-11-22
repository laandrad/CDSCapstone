library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Word Prediction App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            textInput("sentence", "Type a sentence...")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
            p(strong("My prediction of your next word is...")),
            textOutput("predWord"),
            p(),
            p(),
            p(em("Alternative suggestions...")),
            textOutput("altWords")
    )
  )
))
