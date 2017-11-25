library(dplyr)
library(pbapply)
library(parallel)
library(igraph)


# 1. Load data
data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
myFiles = list.files(data.folder, pattern = "biGramsTotal")
biGrams = readRDS(paste0(data.folder, myFiles))
head(biGrams)

# 2. Preprocess
print("Mapping...")
nPartitions = 1000
totalBG = biGrams %>% nrow()
partLength = round(totalBG / nPartitions)
parts = gl(n = nPartitions, k = partLength, length = totalBG)
datBG = lapply(1:partLength, function(i) biGrams[parts == as.character(i), 1])
freqBG = lapply(1:partLength, function(i) biGrams[parts == as.character(i), 2])

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
biGrams = pblapply(1:length(datBG), function(i) {
        data.frame(matrix(edges[[i]], ncol = 2, byrow = T) %>% as.data.frame(),
                             freqBG[[i]])
})

biGrams = do.call(rbind, biGrams)

colnames(biGrams) = c("first", "second", "Freq")
head(biGrams)
write.csv(biGrams, paste0(data.folder, "wordPrediction-bigrams.csv"), row.names = F)

# 3. Create exploratory network graph
set.seed(80537)
set = runif(300, 1, nrow(biGrams)) %>% round
set = set[set %% 2 == 1]
set = rbind(set, set + 1) %>% as.vector()
links = edges %>% unlist() %>% .[set]
head(links)

saveRDS(links, paste0(data.folder, "empiricalFrequencies.Rdata"))
par(mfrow = c(1, 3))
g = graph(links[1:100])
plot(g, vertex.frame.color = NA, vertex.color = NA, 
     edge.arrow.size = 0.5, edge.arrow.width = 0.5)
g = graph(links[101:200])
plot(g, vertex.frame.color = NA, vertex.color = NA, 
     edge.arrow.size = 0.5, edge.arrow.width = 0.5)
g = graph(links[201:300])
plot(g, vertex.frame.color = NA, vertex.color = NA, 
     edge.arrow.size = 0.5, edge.arrow.width = 0.5)


