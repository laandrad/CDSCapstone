library(dplyr)
library(parallel)
library(pbapply)
library(lubridate)

source("myFunctions.R")

# get file names in folder
data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
myRDS = list.files(data.folder, pattern = "tokens")

for (i in 1:length(myRDS)) {
        print(paste("reading object", myRDS[i]))
        myobj = readRDS(paste0(data.folder, myRDS[i]))
        print("Simplifying...")
        myobj = pbsapply(myobj, function(x) x %>% 
                                 unname() %>% 
                                 unlist())
        # write.table(myobj, paste0(data.folder, myRDS[i], "_.txt"), row.names = F)
        
        print("Mapping...")
        nPartitions = 100
        totalDFS = myobj %>% length()
        partLength = round(totalDFS / nPartitions)
        parts = gl(n = nPartitions, k = partLength, length = totalDFS)
        
        print("Shuffling...")
        dfs = lapply(1:totalDFS, function(i) {
                parallelWordCount(myobj[parts == as.character(i)], nPartitions)
                print(paste("finished part", i, "of", totalDFS))
        })
        
        dt = data.frame(Term = sapply(1:length(dfs), function(i) 
                levels(dfs[[i]]$Term)) %>% unlist() %>% unique() )
        colnames(dt) = "Term"
        
        pb = txtProgressBar(min = 0, 
                            max = length(dfs), 
                            style = 3)
        for (i in 1:length(dfs)) {
                dt = dt %>% left_join(dfs[[i]], by = c("Term" = "Term"))
                setTxtProgressBar(pb, i + 1)
        }
        close(pb)
        
        dt[is.na(dt)] = 0
        dt = data.frame(dt[, 1], apply(dt[, -1], 1, sum))
        colnames(dt) = c("Term", "Freq")
        write.csv(dt, paste0(data.folder, myRDS[i], "_freq.csv"), row.names = F)

}
