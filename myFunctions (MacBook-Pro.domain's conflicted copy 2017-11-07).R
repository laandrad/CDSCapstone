# remove contractions and punctuation function
cleanLine <- function(line) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)
        
        expand <- function(word) {
                ifelse(word %in% contractions[, 1], 
                       contractions[which(contractions[, 1] %in% word), 2], 
                       word) %>% 
                        unlist %>% 
                        paste(collapse = " ") %>% 
                        tokenize_regex("[^[:alpha:] ]| ") %>% 
                        unlist %>% 
                        iconv("latin1", "ASCII", sub = "") %>% 
                        gsub("([[:alpha:]])\\1{2,}", "\\1", .) %>%
                        gsub(".*haha*.", "ha-ha", .) %>%
                        (function(x) x[(!x == "")]) 
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

spellCheck <- function(text) {
        
        library(hunspell)
        
        replaced = NULL
        
        for (i in 1:length(text)) {
                if (!is.na(text[i]) & length(text[i] > 0)) {
                        if (!hunspell_check(text[i])) {
                                replaced = c(replaced, 
                                             hunspell_suggest(text[i])[[1]][1])
                        } else {
                                replaced = c(replaced, 
                                             text[i])
                        }
                }
        }
        
        replaced
}


replaceRedundant <- function(x){
                paste(x, collapse = " ") %>%
                strsplit(split = "(?!')[ [:punct:]]", fixed = F, perl = T) %>%
                unlist %>%
                trimws %>%
                tolower %>%
                unique %>%
                paste(collapse = " ") %>%
                gsub("list c  ", "", .)
}


getWordFreq <- function(text) {
                text %>% 
                replaceRedundant %>%
                unlist %>% 
                paste(collapse = " ") %>% 
                tokenize_words %>% 
                table
}

getBiGrams <- function(text) {
                text %>% 
                lapply(function(x) {
                        x %>%
                        replaceRedundant %>%
                        paste(collapse = " ") %>%
                        tokenize_ngrams(., n = 2) %>%
                        unlist %>%
                        table
        })  %>%
                unlist
}
