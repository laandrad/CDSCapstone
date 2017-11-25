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

linesToRead = 100000

tic = proc.time()

# 1. Read a chunck of data
i = 1
print(paste("Reading file:", myFiles[i], "..."))
con = file(paste0(data.folder, myFiles[i]), "r")
myLines = readLines(con, linesToRead)
print("Finished reading file.")

cl = makeCluster(4L)
print("Sampling from data file...")
set.seed(80537)
partition = rbinom(length(myLines), 1, 0.25)
myLinesSample = myLines[which(partition == 1)]

# Clean text
print(paste("Cleaning sampled text in file:", myFiles[i], "..."))
sentences = myLinesSample %>% tokenize_sentences() %>% unlist()
myTokensStem = pblapply(sentences, cleanSentence, cl = cl, stem = T) %>%
        Filter(f = Negate(is.null))
myTokensNo = pblapply(sentences, cleanSentence, cl = cl, stem = F) %>%
        Filter(f = Negate(is.null))
stopCluster(cl)

# Compute word frequency
print(paste("Calculating word frequency for:", myFiles[i], "..."))
wordFreqStem = parallelWordCount(myTokensStem, nPartitions = 100)
wordFreqNo = parallelWordCount(myTokensNo, nPartitions = 100)


# 11. Produce plots
print(paste("Creating plots for:", myFiles[i], "..."))
dtfStem = dtm(wordFreqStem)
dtfNo = dtm(wordFreqNo)

p = plot_ly(data = dtfStem, type = "scatter", mode = "lines", name = "Stemmed",
             x = ~seq_along(CumPerc), y = ~CumPerc) %>%
        add_trace(type = "scatter", mode = "lines", name = "Unstemmed",
                  x = ~seq_along(dtfNo$CumPerc), y = ~dtfNo$CumPerc) %>%
        layout(xaxis = list(title = "Number of Terms"))
p
print("Finished plotting.")
saveRDS(p, paste0(data.folder, "coverage.RDS"))


# 12. Retrieve total processing time
totTime = proc.time() - tic
print(paste("Total processing time:", 
            totTime[3] %>% round %>% seconds_to_period()))

head(dtfNo, 20)
head(dtfStem, 20)
