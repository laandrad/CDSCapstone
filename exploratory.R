library(dplyr)
library(parallel)
library(pbapply)
library(igraph)

# get file names in folder
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = "biGram")

i = 1

# load data
biGrams = readRDS(paste0(data.folder, myFiles[i]))

# Preprocess
cl = makeCluster(4L)
edges = pbsapply(biGrams[1:1000, 1], function(x) {
        library(dplyr)
        strsplit(x %>% as.character(), " ")
        }, cl = cl) %>% 
        unlist()

biGrams = data.frame(matrix(edges, ncol = 2, byrow = T) %>% as.data.frame(), 
                 biGrams[1:1000, 2])
colnames(biGrams)[3] = "Freq"
head(biGrams)
write.csv(biGrams, "wordPrediction/bigrams.csv", row.names = F)

# Create network graph
g = graph(sample(edges, size = 100, replace = F))
plot(g, vertex.frame.color = NA, vertex.color = NA, 
     edge.arrow.size = 0.5, edge.arrow.width = 0.5)
tMatrix = as_adj(g)
tMatrix

# Word prediction
wordPredict <- function(word, biGrams) {
        if (word %in% biGrams$V1) {
                predWord = filter(biGrams, V1 == word)
                predWord[which.max(predWord$Freq), "V2"] %>%
                        as.character()
        } else {
                base::sample(biGrams$V2, 1) %>% as.character()
        }
}

word = "because"
wordPredict(word, biGrams)

word = "general"
wordPredict(word, biGrams)


