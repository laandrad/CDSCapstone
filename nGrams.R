
library(pbapply)
library(parallel)

data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
myFiles = list.files(data.folder, pattern = "_ALL_tokens.Rdata")
myFiles

computeNGrams <- function(sentence) {
        library(dplyr)
        library(tokenizers)
        
        sentence = sentence %>% unlist() %>% paste(collapse = " ") 
        biGrams = sentence %>% tokenize_ngrams(n = 2)
        biGrams = lapply(biGrams[[1]], gsub, pattern = " ", replacement = "-") %>% 
                        unlist()
        cat(biGrams, 
            file = paste0(data.folder, "/nGramData/biGrams_All.txt"),
            sep = "\n", append = TRUE)
        
        triGrams = sentence %>% tokenize_ngrams(n = 3)
        triGrams = lapply(triGrams[[1]], gsub, pattern = " ", replacement = "-") %>% 
                unlist()
        cat(triGrams, 
            file = paste0(data.folder, "/nGramData/triGrams_All.txt"),
            sep = "\n", append = TRUE)
        
        tetraGrams = sentence %>% tokenize_ngrams(n = 4)
        tetraGrams = lapply(tetraGrams[[1]], gsub, pattern = " ", replacement = "-") %>% 
                unlist()
        cat(tetraGrams %>% unlist(), 
            file = paste0(data.folder, "/nGramData/tetraGrams_All.txt"), 
            sep = "\n", append = TRUE)
}

cl = makeCluster(2L)
for (j in 1:length(myFiles)) {
        print(paste("Reading object:", myFiles[j]))
        myTokens = readRDS(paste0(data.folder, myFiles[j]))
        N = length(myTokens)
        print(paste("Computing nGrams for", myFiles[j]))
        nG = pbsapply(1:N, function(i) {
                computeNGrams(myTokens[i])
        })
        remove(nG)
}
stopCluster(cl)

