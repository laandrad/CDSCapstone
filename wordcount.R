library(dplyr)
library(lubridate)
library(tokenizers)
library(parallel)
library(pbapply)
library(plotly)

source("myFunctions.R")

# get file names in folder
data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
myRDS = list.files(data.folder, pattern = "tokens")
other = myRDS %>% grepl("((.txt)$|Grams|lang|Freq|png)", .)
myRDS = myRDS[!other]

i = 1
for (i in 1:length(myRDS)) {
        
        tic = proc.time()
        
        # 1. Read a chunck of data
        print(paste("Reading file:", myRDS[i], "..."))
        sentences = readRDS(paste0(data.folder, myRDS[i]))
        print("Finished reading file.")
        print("Sampling from data file...")
        set.seed(80537)
        partition = rbinom(length(sentences), 1, 0.1)
        sentences = sentences[which(partition == 1)]

        # 2. Compute word frequency
        nPartitions = 100
        print(paste("Calculating word frequency for:", myRDS[i], "..."))
        wordFreq = parallelWordCount(sentences, nPartitions = nPartitions)
        print(paste("Saving objects for:", myRDS[i], "..."))
        saveRDS(wordFreq, paste0(data.folder, "wordFreq_", myRDS[i]))

        # 3. Compute bigrams
        print(paste("Calculating bigrams for:", myRDS[i], "..."))
        biGrams = parallelNGramCount(sentences, N = 2, nPartitions = nPartitions)
        print(paste("Saving objects for:", myRDS[i], "..."))
        saveRDS(biGrams, paste0(data.folder, "biGrams_", myRDS[i]))

        # 4. Compute trigrams
        print(paste("Calculating trigrams for:", myRDS[i], "..."))
        triGrams = parallelNGramCount(sentences, N = 3, nPartitions = nPartitions)
        print(paste("Saving objects for:", myRDS[i], "..."))
        saveRDS(triGrams, paste0(data.folder, "triGrams_", myRDS[i]))
        
        # 4. Compute tetraGrams
        print(paste("Calculating tetraGrams for:", myRDS[i], "..."))
        tetraGrams = parallelNGramCount(sentences, N = 4, nPartitions = nPartitions)
        print(paste("Saving objects for:", myRDS[i], "..."))
        saveRDS(tetraGrams, paste0(data.folder, "tetraGrams_", myRDS[i]))
        
        # 5. Detect languages
        cl = makeCluster(4L)
        print(paste("Detecting languages for:", myRDS[i], "..."))
        languages = pbsapply(sentences, detectLanguage, cl = cl)
        stopCluster(cl)
        languages = languages[!is.na(languages)] %>% table %>% data.frame()
        colnames(languages) = c("Term", "Freq")
        print(paste("Saving objects for:", myRDS[i], "..."))
        saveRDS(languages, paste0(data.folder, "languages_", myRDS[i]))
        
        # 6. Retrieve total processing time
        totTime = proc.time() - tic
        print(paste("Total processing time:", 
                    totTime[3] %>% round %>% seconds_to_period()))
}
