library(dplyr)
library(lubridate)
library(tokenizers)
library(parallel)
library(pbapply)
library(plotly)

source("myFunctions.R")

# get file names in folder
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = ".txt")
myFiles


linesToRead = 2000

stats = NULL

i = 3
for (i in 1:length(myFiles)) {
        
        tic = proc.time()
        
        # 1. Read a chunck of data
        print(paste("Reading file:", myFiles[i], "..."))
        con = file(paste0(data.folder, myFiles[i]), "r")
        # myLines = readLines(con, linesToRead)
        myLines = readLines(con)
        close.connection(con)
        print("Finished reading file.")
        
        # 2. Initialize parallel computing
        cl = makeCluster(2L)
        
        # # 3. Compute descriptive stats for raw data
        # print(paste("Calculating descriptives in raw file:", myFiles[i], "..."))
        # rawStats = pblapply(myLines, describeLines, cl = cl)
        # stats = rbind(stats, textStats(rawStats) %>% round)
        # print("Table: Word and character frequency in data file.")
        # print(stats)
        
        # 4. Sample data
        print("Sampling from data file...")
        set.seed(80537)
        partition = rbinom(length(myLines), 1, 0.5)
        myLinesSample = myLines[which(partition == 1)]
        
        # 5. Clean text
        print(paste("Cleaning sampled text in file:", myFiles[i], "..."))
        sentences = myLinesSample %>% tokenize_sentences() %>% unlist()
        myTokens = pblapply(sentences, cleanSentence, cl = cl) %>%
                Filter(f = Negate(is.null))
        
        # 6. Create object names for saving to hard drive
        print("Saving objects...")
        objName = strsplit(myFiles[i], split = "\\.")[[1]][2]
        data.folder2 = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
        # write.table(myLinesSample, paste0(data.folder2, objName, "_lines.txt"))
        saveRDS(myTokens, paste0(data.folder2, objName, "_ALL_tokens.Rdata"))
        
        # 7. close parallel
        stopCluster(cl)
       
        # 8. Retrieve total processing time
        totTime = proc.time() - tic
        print(paste("Total processing time:", 
                    totTime[3] %>% round %>% seconds_to_period()))
}

rownames(stats) = myFiles
stats %>% saveRDS("stats.rds")        
