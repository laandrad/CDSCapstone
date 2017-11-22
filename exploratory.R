library(dplyr)
library(parallel)
library(pbapply)
library(igraph)


# 1. Load data
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = "biGram")
i = 1
biGrams = readRDS(paste0(data.folder, myFiles[i]))
head(biGrams)

# 2. Preprocess
cl = makeCluster(4L)
edges = pbsapply(biGrams[, 1], function(x) {
        library(dplyr)
        strsplit(x %>% as.character(), " ")
        }, cl = cl) %>% 
        unlist()

biGrams = data.frame(matrix(edges, ncol = 2, byrow = T) %>% as.data.frame(), 
                 biGrams[, 2])
colnames(biGrams)[3] = "Freq"
write.csv(biGrams, "wordPrediction/bigrams.csv", row.names = F)
head(biGrams)

# 3. Create exploratory network graph
g = graph(edges[1:200])
plot(g, vertex.frame.color = NA, vertex.color = NA, 
     edge.arrow.size = 0.5, edge.arrow.width = 0.5)


# 4. Word prediction Example
biGrams = read.csv("wordPrediction/bigrams.csv")
source("wordPrediction/wordPredict.R")

word = "I am"
wordPredict(word, biGrams)[[1]]

word = "mother"
wordPredict(word, biGrams)[[1]]


