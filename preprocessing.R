library(dplyr)
library(lubridate)
library(tokenizers)
library(parallel)
library(pbapply)
library(plotly)

Sys.setenv("plotly_username" = "landrad78")
Sys.setenv("plotly_api_key" = "JUuLrFcCqExiWblQbHup")

source("myFunctions.R")

# get file names in folder
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = ".txt")

linesToRead = 80000

stats = NULL

i = 1
for (i in 1:length(myFiles)) {
        
        tic = proc.time()
        
        # Read a chunck of data
        print(paste("Reading file:", myFiles[i], "..."))
        con = file(paste0(data.folder, myFiles[i]), "r")
        
        # myLines = readLines(con)
        myLines = readLines(con, linesToRead)

        print("Finished reading file.")
        
        # Open parallel computing
        cl = makeCluster(4L)
        
        # compute descriptive stats for raw data
        print(paste("Calculating descriptives in raw file:", myFiles[i], "..."))
        rawStats = pblapply(myLines, describeLines, cl = cl)
        
        # sampling data
        set.seed(80537)
        partition = rbinom(length(myLines), 1, 0.01)
        myLinesSample = myLines[which(partition == 1)]
        
        # clean text
        print(paste("Cleaning sampled text in file:", myFiles[i], "..."))
        sentences = myLinesSample %>% tokenize_sentences() %>% unlist()
        myTokens = pblapply(sentences, cleanSentence, cl = cl) %>%
                Filter(f = Negate(is.null))
        
        # close parallel
        stopCluster(cl)
        
        # save objects to hard drive
        print("Saving objects...")
        objName = strsplit(myFiles[i], split = "\\.")[[1]][2]
        saveRDS(myTokens, paste0(data.folder, objName, ".rds"))
        
        # word frequency
        print(paste("Calculating word frequency for:", myFiles[i], "..."))
        wordFreq = parallelWordCount(myTokens, nPartitions = 10)
        head(wordFreq)
        print(paste("Saving objects for:", myFiles[i], "..."))
        saveRDS(wordFreq, paste0(data.folder, "wordFreq_", objName))
        
        # Bigrams
        print(paste("Calculating bigrams for:", myFiles[i], "..."))
        biGrams = parallelNGramCount(myTokens, N = 2, nPartitions = 10)
        head(biGrams)
        print(paste("Saving objects for:", myFiles[i], "..."))
        saveRDS(biGrams, paste0(data.folder, "biGrams_", objName))
        
        # Trigrams
        print(paste("Calculating trigrams for:", myFiles[i], "..."))
        triGrams = parallelNGramCount(myTokens, N = 3, nPartitions = 10)
        head(triGrams)
        print(paste("Saving objects for:", myFiles[i], "..."))
        saveRDS(triGrams, paste0(data.folder, "triGrams_", objName))
        
        # Detect languages
        cl = makeCluster(4L)
        print(paste("Detecting languages for:", myFiles[i], "..."))
        languages = pbsapply(myTokens, detectLanguage, cl = cl)
        stopCluster(cl)
        languages = languages[!is.na(languages)] %>% table %>% data.frame()
        head(languages)
        print(paste("Saving objects for:", myFiles[i], "..."))
        saveRDS(languages, paste0(data.folder, "languages_", objName))
        
        # Plots
        print(paste("Creating plots for:", myFiles[i], "..."))
        dtf = dtm(wordFreq)
        dtf2 = dtm(biGrams)
        dtf3 = dtm(triGrams)
        dtl = dtm(languages)
        
        print(paste("saving plot to:", paste0("coverage_terms_", objName, ".png")))
        myPlotCoverage(dtf, dtf2, dtf3)
        print("Finished plotting.")
        
        dtl
        
        # Total time
        totTime = proc.time() - tic
        print(paste("Total processing time:", 
                    totTime[3] %>% round %>% seconds_to_period()))
}
        