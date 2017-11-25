library(dplyr)
library(lubridate)
library(tokenizers)
library(parallel)
library(pbapply)
library(plotly)

source("myFunctions.R")

# get file names in folder
data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
myRDS = list.files(data.folder, pattern = "tokens")

reduce <- function(objects, subset) {
        tic = proc.time()

        set = objects %>% grepl(subset, .)
        myRDS = objects[set]
        dfs = lapply(1:length(myRDS), function(i) {
                print(paste("Reading file:", myRDS[i], "..."))
                readRDS(paste0(data.folder, myRDS[i]))
        })
        
        dt = data.frame(Term = sapply(1:length(dfs), function(i) 
                levels(dfs[[i]]$Term)) %>% 
                        unlist() %>% unique() 
        )
        
        print("reducing...")
        pb = txtProgressBar(min = 0, 
                            max = length(dfs), 
                            style = 3)
        for (i in 1:length(dfs)) {
                dt = dt %>% left_join(dfs[[i]], by = c("Term" = "Term"))
                setTxtProgressBar(pb, i + 1)
        }
        close(pb)
        
        dt[is.na(dt)] = 0
        dt = data.frame(Term = dt[, 1], Freq = apply(dt[, -1], 1, sum))

        # 12. Retrieve total processing time
        totTime = proc.time() - tic
        print(paste("Total processing time:", 
                    totTime[3] %>% round %>% seconds_to_period()))
        dt
}

wordFreq = reduce(myRDS, "Freq")
biGrams = reduce(myRDS, "biGrams")
triGrams = reduce(myRDS, "triGrams")
languages = reduce(myRDS, "language")

print(paste("Saving objects..."))
saveRDS(wordFreq, paste0(data.folder, "wordFreqTotal.Rdata"))
saveRDS(biGrams, paste0(data.folder, "biGramsTotal.Rdata"))
saveRDS(triGrams, paste0(data.folder, "triGramsTotal.Rdata"))
saveRDS(languages, paste0(data.folder, "languagesTotal.Rdata"))

# 11. Produce plots
print(paste("Creating plots for:", myRDS[i], "..."))
dtf = dtm(wordFreq)
dtf2 = dtm(biGrams)
dtf3 = dtm(triGrams)

p1 = myPlotCoverage(dtf, dtf2, dtf3, title = "Coverage")
p1

print("Finished plotting.")

p2 = languages %>% arrange(desc(Freq)) %>% .[-1, ] %>%
        plot_ly(x = ~Freq, y = ~Term, type = "bar", color = ~Term, showlegend = F)
p2

print("saving plots...")
plotly_IMAGE(p1, format = "png",
             out_file = paste0(data.folder,
                               "coverage_terms.png"))
plotly_IMAGE(p2, format = "png",
             out_file = paste0(data.folder,
                               "languages.png"))

