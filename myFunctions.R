# remove contractions and punctuation function
cleanLine <- function(line) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)
        
        expand <- function(word) {
                ifelse(word %in% contractions[, 1], 
                       contractions[which(contractions[, 1] %in% word), 2], 
                       word) %>% unlist
        }

        cleanSentence <- function(sentence) {
                tokenize_words(sentence) %>% lapply(expand)
        }
        
        line %>% tokenize_sentences %>% lapply(cleanSentence)
}

textStats <- function(fileStats) {
        wordStats = sapply(1:length(fileStats), function(i) {
                fileStats[[i]][[1]]
        })
        
        charStats = sapply(1:length(fileStats), function(i) {
                fileStats[[i]][[2]]
        })
        
        c(nLines = length(myLines),
          totWords = sum(wordStats),
          meanWordLine = mean(wordStats),
          maxWordLine = max(wordStats),
          totCharacters = sum(charStats),
          meanCharLine = mean(charStats),
          maxCharLine = max(charStats)
        ) 
}

describeLines <- function(line) {
        library(dplyr)
        library(tokenizers)
        line = line %>% unlist
        numbWords =  line %>% length
        lineLength = line %>% paste %>% 
                tokenize_characters(simplify = T) %>% 
                unlist %>% length
        # print(paste("Number of words in line:", numbWords))
        # print(paste("line length (in characters):", lineLength))
        return(list(numbWords, lineLength))
}