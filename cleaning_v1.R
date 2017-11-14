library(dplyr)
library(parallel)
library(pbapply)

source("myFunctions.R")

con = url("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt")
badwords = readLines(con)[-1]
close(con)
saveRDS(badwords, "badWordsDict.rds")

# get file names in folder
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = ".txt")

linesToRead = 800000

stats = NULL

for (i in 1:length(myFiles)) {
        tic = proc.time()
        # Read a chunck of data
        print(paste("Reading file:", myFiles[i], "..."))
        con = file(paste0(data.folder, myFiles[i]), "r")
        # myLines = readLines(con)
        myLines = readLines(con, linesToRead)
        close(con)
        print("Finished reading file.")
        
        # Start up a parallel cluster
        cl = makeCluster(4L)
        
        # compute descriptive stats for raw data
        print(paste("Calculating descriptives in raw file:", myFiles[i], "..."))
        rawStats = pblapply(myLines, describeLines, cl = cl)

        # sample 10% of data
        set.seed(80537)
        partition = rbinom(length(myLines), 1, 0.01)
        myLinesSample = myLines[which(partition == 1)]
        
        # clean text
        print(paste("Cleaning sampled text in file:", myFiles[i], "..."))
        myText = pblapply(myLinesSample, cleanLine, cl = cl)
        
        # save objects to hard drive
        print("Saving objects...")
        objName = strsplit(myFiles[i], split = "\\.")[[1]][2]
        saveRDS(myText, paste0(data.folder, objName, ".rds"))
        
        # close cluster connection
        stopCluster(cl)
        
        # compute descriptive statistics
        stats = rbind(stats, c(objName, textStats(rawStats)))
        totTime = proc.time() - tic
        print(paste("Total processing time (in minutes):", 
                    (totTime[3] / 60) %>% round(2)))

}

cat("Raw frequencies...\n")
stats

write.csv(stats, paste0(data.folder, "rawStatistics.csv"), row.names = F)

