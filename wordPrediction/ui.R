library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("My Word Prediction App"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            p("To get started, press button below. Be patient, it might take a little while for the data to load."),
            actionButton("load", "Load database"),
            p(""),
            textOutput("dbLoaded"),
            p(""),
            p(""),
            p("When database is loaded, start typing a sentence below."),
            textInput("sentence", "My sentence...")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
            p(strong("Your sentence with my word prediction is...")),
            textOutput("predWord"),
            p(""),
            p(em("Alternative word suggestions...")),
            plotlyOutput("freqPlot"),
            p(""),
            p(em("Or, perhaps...")),
            textOutput("altWords")
    )
  )
))
