# word Prediction
wordPredict <- function(sentence, biGrams) {
        
        if (!is.null(sentence) &&
            !is.na(sentence) &&
            nchar(sentence) > 0) {

                sentence = cleanSentence(sentence)
                
                word = sentence %>% strsplit(., " ") %>% unlist()
                word = word[length(word)]
                
                if (word %in% biGrams$V1) {
                        predWords = filter(biGrams, V1 == word)
                        prediction = predWords[which.max(predWords$Freq), "V2"] %>%
                                        as.character()
                        return(list(prediction, predWords$V2 %>% as.vector()))
                } else {
                        return(list(NULL, sample(biGrams$V2, 1) %>% as.character()))
                }
        }
}


# remove contractions, bad words, and punctuation
cleanSentence <- function(sentence) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)
        
        # converting sentence into tokens
        # print("tokenizing...")
        if (length(sentence) == 0) {
                warning("no words in sentence! Skipping sentence")
                # print(sentence)
                sentence = list(NULL)
        }
        # print(sentence)
        tokens = sentence %>% 
                tokenize_words() %>% 
                unlist()
        # print(tokens)
        if (length(tokens) == 0) {
                warning("no tokens in sentence! Skipping sentence")
                # print(tokens)
                tokens = list(NULL)
        }
        if (any(is.null(tokens)) | any(is.na(tokens)) | length(tokens) == 0) {
                # print(tokens)
                warning("token vector is null or na!")
        }
        # print("removing numbers and non-ASCII characters...")
        tokens = tokens %>%
                gsub("'", "~", .) %>%
                iconv("latin1", "ASCII", sub = "") %>%
                gsub("[0-9]", "", .) %>%
                .[. != ""]
        if (length(tokens) == 0) {
                warning("no tokens in sentence after removing numbers!")
        }
        if (any(is.null(tokens)) | any(is.na(tokens))) {
                warning("token vector is null or na!")
        }
        
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
        stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
        stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
        
        # Define functions
        expContract <- function(tokens) {
                # print("Expanding contractions...")
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
                # print("Expanding abbreviations")
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
                # print("Cleaning bad words...")
                tokens = sapply(1:length(tokens), function(i) {
                        condition = tokens[i] %in% badwords
                        if (condition && 
                            !is.na(condition) && 
                            !length(condition) == 0) {
                                "obscenity" 
                        } else {
                                tokens[i]
                        }
                })
                # print("tokens after removing badwords:")
                tokens = sapply(tokens, function(x) x[!is.na(x)]) %>% unlist()
                tokens = Filter(function(x) !identical(character(0),x), tokens)
                badTokens = grepl("obscenity", tokens)
                tokens = tokens[!badTokens]
                # print(tokens)
                tokens
        }
        
        spellCheck <- function(tokens) {
                # print("Spell checking...")
                tokens = gsub("~", "", tokens)
                # print("tokens read for spellchecking:")
                # print(tokens)
                if (!identical(character(0), tokens) && length(tokens) > 0) {
                        tokens = sapply(1:length(tokens), function(i) {
                                # print(tokens[i])
                                if (is.na(tokens[i])) {
                                        tokens[i] = ""
                                }
                                library(hunspell)
                                condition = hunspell_check(tokens[i] %>% as.character())
                                # str(condition)
                                if (!condition && 
                                    !is.na(condition) && 
                                    !length(condition) == 0) {
                                        corrected = hunspell_suggest(tokens[i] %>% 
                                                                             as.character())[[1]]
                                        if (length(corrected) > 1) {
                                                corrected = corrected[2]
                                        }
                                        corrected = gsub(" ", "", corrected)[1]
                                        corrected
                                        
                                } else if (is.na(condition)) {
                                        ""
                                } else {
                                        tokens[i]
                                }
                        })
                        
                        tokens
                }
                
                tokens = tokens[!is.na(tokens) && !is.null(tokens)] %>% unlist()
                # print("tokens after spellchecking:")
                # print(tokens)
                if (any(grepl('\\(\\"', tokens))) {
                        warning("not separating characters after spellchecking!")
                }
                tokens
        }
        
        removeStopWords <- function(tokens) {
                # print("Removing stop words...")
                tokens = stringr::str_replace_all(tokens, stopwords_regex, '')
                # print("tokens after removing stopwords:")
                tokens = Filter(function(x) !identical(character(0),x), tokens)
                if (any(grepl('\\(\\"', tokens))) {
                        warning("not separating characters after removing stopwords!")
                }
                tokens = Filter(function(f) nchar(f) > 0, tokens)
                # print(tokens)
                tokens
        } 
        
        stemWords <- function(tokens) {
                tokens = hunspell_stem(tokens)
                tokens %>% sapply(function(x) x[length(x)])
        }
        
        # Clean sentence
        tokens = tokens %>% 
                expContract() %>% 
                expAbbv() %>%
                cleanWords() %>%
                spellCheck() %>%
                removeStopWords() %>%
                stemWords() %>%
                unlist() 
        tokens[!is.na(tokens)]
        
}