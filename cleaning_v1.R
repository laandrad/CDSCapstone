library(dplyr)
library(parallel)

source("myFunctions.R")

# get file names in folder
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = ".txt")

linesToRead = 10

stats = NULL

for (i in 1:length(myFiles)) {
        tic = proc.time()
        # Read a chunck of data
        print(paste("Reading file:", myFiles[i], "..."))
        con = file(paste0(data.folder, myFiles[i]), "r")
        myLines = readLines(con)
        close(con)
        print("Finished reading file.")
        
        # Start up a parallel cluster
        cl = makeCluster(detectCores())
        
        # clean text
        print(paste("Cleaning text in file:", myFiles[i], "..."))
        myText = parLapply(cl, myLines, cleanLine)
        print("Finished cleaning text.")
        
        print(paste("Calculating descriptives in file:", myFiles[i], "..."))
        fileStats = parLapply(cl, myText, describeLines)
        print("Finished calculating stats.")
        
        # save objects to hard drive
        print("Saving objects...")
        objName = strsplit(myFiles[i], split = "\\.")[[1]][2]
        saveRDS(myText, paste0(data.folder, objName, ".rds"))
        saveRDS(fileStats, paste0(data.folder, objName, "_stats.rds"))
        
        # close cluster connection
        stopCluster(cl)
        
        # compute descriptive statistics
        stats = rbind(stats, c(objName, textStats(fileStats)))
        totTime = proc.time() - tic
        print(paste("Total processing time:", totTime[3] %>% round(2)))
}

stats
write.csv(stats, paste0(data.folder, "textStatistics.csv"), row.names = F)


