library(dplyr)
library(pbapply)
library(parallel)

# 1. Load data
data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
myFiles = list.files(data.folder, pattern = "triGramsTotal")
triGrams = readRDS(paste0(data.folder, myFiles))
head(triGrams)

# 2. Preprocess
print("Mapping...")
nPartitions = 1000
totalBG = triGrams %>% nrow()
partLength = round(totalBG / nPartitions)
parts = gl(n = nPartitions, k = partLength, length = totalBG)
datBG = lapply(1:partLength, function(i) triGrams[parts == as.character(i), 1])
freqBG = lapply(1:partLength, function(i) triGrams[parts == as.character(i), 2])

print("Shuffling...")
cl = makeCluster(4L)
edges = pblapply(datBG, function(x) {
                library(dplyr)
                worker <- function(dat) {
                        lapply(dat, function(x) {
                                strsplit(x %>% as.character(), " ")
                }) %>% 
                                unlist() %>% 
                                unname()
                }
                worker(x)
        },
        cl = cl)
stopCluster(cl)

print("Reducing...")
triGrams = pblapply(1:length(datBG), function(i) {
        data.frame(matrix(edges[[i]], ncol = 3, byrow = T) %>% as.data.frame(),
                             freqBG[[i]])
})

triGrams = do.call(rbind, triGrams)

colnames(triGrams) = c("first", "second", "third", "Freq")
head(triGrams)
write.csv(triGrams, paste0(data.folder, "wordPrediction-triGrams.csv"), row.names = F)

