
library(shiny)
library(dplyr)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

        words = reactive({
                input$sentence
        })
        
        # load data
        db4 = reactiveValues(data = NULL)
        
        observeEvent(input$load, {
                # data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/Capstone Project/wordPrediction/appData/"
                data.folder = "/srv/connect/apps/wordPrediction/appData/"
                db4$data = NULL
                withProgress(message = 'Loading...', value = 0, {
                        n = 100
                        for (i in 1:n) {
                                db4$data = c(db4$data,
                                            readRDS(paste0(data.folder, 
                                                           "db_", i, 
                                                           ".Rdata"))
                                            )
                                incProgress(1/n, detail = paste(i, "% completed"))
                        }
                })
        })

        # load Word prediction function
        source("./wordPredict.R", local = TRUE)
        prediction = reactive({
                        if (is.null(db4$data)) return()
                        sentence = cleanSentence(words())
                        target = sentence[length(sentence)]
                        i = which(names(db4$data) == target)
                        if (!identical(integer(0), i)) {
                                predW4G(sentence, db4$data[[i]])
                        } else {
                                list(pred = NULL,
                                     alt = NULL,
                                     all = NULL,
                                     freq = NULL
                                     )
                        }
        }) 
        
        output$predWord = renderText({
                        predW = prediction()
                        paste(words(), predW$pred)
                })
        
        output$altWords = renderText({
                        predW = prediction()
                        paste(predW$all %>% unique(), collapse = ", ")
        })
        
        output$freqPlot = renderPlotly({
                predW = prediction()
                if (is.null(predW$all)) return()
                dat = data.frame(terms = predW$all, freq = predW$freq) 
                dat = group_by(dat, terms) %>% summarise(freq = sum(freq))
                dat$perc = (dat$freq / max(dat$freq)) %>% round(1)
                dat = dat %>% arrange(desc(perc))
                dat = dat[1:8, ]
                dat$terms = factor(dat$terms)
                plot_ly(data = dat, 
                        y = ~perc, 
                        x = ~terms,
                        color = ~terms, 
                        showlegend = F,
                        type = "bar") %>% 
                        add_annotations(y = ~perc + 0.05, 
                                        x = ~terms,
                                        text = ~terms, 
                                        showarrow = F) %>%
                        add_annotations(y = ~perc - 0.05, 
                                        x = ~terms,
                                        text = ~perc, 
                                        showarrow = F) %>%
                        layout(xaxis = list(title = "", 
                                            zeroline = FALSE,
                                            showline = FALSE,
                                            showticklabels = FALSE,
                                            showgrid = FALSE), 
                               yaxis = list(title = "confidence", 
                                            zeroline = FALSE,
                                            showline = FALSE,
                                            showticklabels = FALSE,
                                            showgrid = FALSE))
        })
  
})
