library(dplyr)
library(plotly)
library(pbapply)
# library(parallel)

source("myFunctions.R")

Sys.setenv("plotly_username" = "landrad78")
Sys.setenv("plotly_api_key" = "JUuLrFcCqExiWblQbHup")

data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myObjects = list.files(data.folder, pattern = ".rds")
other = myObjects %>% grep("^(bi|tri|ex|word)", .)
myObjects = myObjects[-other]

for (i in 2:length(myObjects)) {
        tic = proc.time()
        print(paste("Reading file:", myObjects[i], "..."))
        dat = readRDS(paste0(data.folder, myObjects[i]))
        print("Finished reading file.")
        
        # word frequency
        print(paste("Calculating word frequency for:", myObjects[i], "..."))
        wordFreq = getWordFreq(myTokens)
        head(wordFreq)

        # Bigrams
        print(paste("Calculating bigrams for:", myObjects[i], "..."))
        biGrams = dat %>% getNGrams(N = 2)

        # Trigrams
        
        print(paste("Calculating trigrams for:", myObjects[i], "..."))
        triGrams = dat %>% getNGrams(N = 3)

        # Detect languages
        cl = makeCluster(4L)
        print(paste("Detecting languages for:", myObjects[i], "..."))
        languages = pbsapply(dat, detectLanguage, cl = cl)
        stopCluster(cl)
        languages = languages[!is.na(languages)] %>% table %>% data.frame()

        # Plots
        print(paste("Creating plots for:", myObjects[i], "..."))
        dtf = dtm(wordFreq)
        dtf2 = dtm(biGrams) 
        dtf3 = dtm(triGrams)
        dtl = dtm(languages)
        
        # wordFreqPlot = plot_ly(data = dtf[1:20, ],
        #                        type = "bar", orientation = "h",
        #                        x = ~term %>% factor, y = ~Freq)
        percPlot = plot_ly(data = dtf, type = "scatter", mode = "lines",
                           x = ~term, y = ~CumPerc)
        biGramPlot = plot_ly(data = dtf2[1:10, ], 
                             type = "bar", 
                             x = ~term %>% factor, y = ~Freq)
        triGramPlot = plot_ly(data = dtf3[1:10, ], 
                              type = "bar", 
                              x = ~term %>% factor, y = ~Freq)
        langPlot = plot_ly(data = dtl, type = "bar",
                           x = ~. %>% factor(), y = ~Perc)

        p = subplot(percPlot, biGramPlot, triGramPlot, langPlot, nrows = 2)
        # api_create(p, filename = paste("Frequencies", myObjects[i]),
        #            fileopt = "overwrite", sharing = "public")
        print("Finished creating plots.")

        print(paste("Saving objects for:", myObjects[i], "..."))
        saveRDS(wordFreq, paste0(data.folder, "wordFreq_", myObjects[i]))
        saveRDS(biGrams, paste0(data.folder, "biGrams_", myObjects[i]))
        saveRDS(triGrams, paste0(data.folder, "triGrams_", myObjects[i]))
        saveRDS(p, paste0(data.folder, "exPlot_", myObjects[i]))
        
        totTime = proc.time() - tic
        print(paste("Total processing time (in minutes):", 
                    (totTime[3] / 60) %>% round(2)))
}

