cleanSentence <- function(sentence) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)
        
        # converting sentence into tokens
        tokens = sentence %>% tokenize_words() %>% unlist()
        
        # Loading dictionaries for token cleaning
        contracted = contractions[, 1] %>% tokenize_words() %>% 
                unlist() %>% gsub("'", "~", .)
        expanded = contractions[, 2] %>% tokenize_words() %>% unlist()
        abbreviations = rbind(abbreviations, 
                              data.frame(abv = "T.V.", 
                                         rep = "Television")) 
        abbrev = abbreviations[, 1] %>% tokenize_words() %>% unlist()
        abbExp = abbreviations[, 2] %>% tokenize_words() %>% unlist()
        badwords = readRDS("badWordsDict.rds")
        
        # Define functions
        expContract <- function(tokens) {
                print("Expanding contractions...")
                sapply(1:length(tokens), function(i) {
                        condition = tokens[i] %in% contracted
                        if (condition && 
                            !is.na(condition) &&
                            !length(condition) == 0) {
                                item = (which(contracted %in% tokens[i]) - 1) * 2
                                expanded[item:(item + 1)]
                        } else {
                                tokens[i]
                        }
                }) %>% unlist()
        }
        
        expAbbv <- function(tokens) {
                print("Expanding abbreviations")
                sapply(1:length(tokens), function(i) {
                        condition = tokens[i] %in% abbrev
                        if (condition && 
                            !is.na(condition) && 
                            !length(condition) == 0) {
                                item = (which(abbrev %in% tokens[i]) - 1) * 2
                                abbExp[item:(item + 1)]  
                        } else {
                                tokens[i]
                        }
                })
        }
        
        cleanWords <- function(tokens) {
                print("Cleaning bad words...")
                sapply(1:length(tokens), function(i) {
                        condition = tokens[i] %in% badwords
                        if (condition && 
                            !is.na(condition) && 
                            !length(condition) == 0) {
                                "obscenity" 
                        } else {
                                tokens[i]
                        }
                })
        }
        
        spellCheck <- function(tokens) {
                print("Spell checking...")
                sapply(1:length(tokens), function(i) {
                        library(hunspell)
                        condition = hunspell_check(tokens[i])
                        if (!condition && 
                            !is.na(condition) && 
                            !length(condition) == 0) {
                                corrected = hunspell_suggest(tokens[i])[[1]]
                                gsub(" ", "", corrected)[1]
                        } else {
                                tokens[i]
                        }
                })
        }
        
        # Clean sentence
        tokens %>% gsub("'", "~", .) %>% 
                iconv("latin1", "ASCII", sub = "") %>%
                expContract() %>% 
                expAbbv() %>%
                cleanWords() %>%
                spellCheck() %>%
                grep("[:alpha:]", ., value = T) %>%
                hunspell_stem() %>%
                unlist()
        
}

