# word Prediction
wordPredict <- function(sentence, biGrams) {
        
        if (!is.null(sentence) &&
            !is.na(sentence) &&
            nchar(sentence) > 0) {

                sentence = cleanSentenceSilently(sentence)
                
                word = sentence %>% strsplit(., " ") %>% unlist()
                word = word[length(word)]
                
                if (word %in% biGrams$first) {
                        predWords = biGrams %>%
                                filter(first == word) %>%
                                arrange(desc(Freq))
                        prediction = predWords[1, "second"] %>% as.character()
                        alternatives = predWords[2:4, "second"] %>% as.vector()
                        return(list(myWordPrediction = prediction, 
                                    wordAlternatives = alternatives))
                } else {
                        return(list(myWordPrediction = NULL, 
                                    wordAlternatives = sample(biGrams$second, 3) %>% as.vector()))
                }
        }
}

# remove contractions, bad words, and punctuation
cleanSentenceSilently <- function(sentence, language = "en_US", stem = F) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)
        library(hunspell)
        
        # print("Original sentence:")
        # print(sentence)
        
        # 1 . Convert sentence into tokens
        # print("tokenizing...")
        if (length(sentence) == 0) {
                warning("no words in sentence! Skipping sentence")
                # print(sentence)
                sentence = list(NULL)
        }
        
        tokens = sentence %>% 
                tokenize_words() %>% 
                unlist()
        
        if (length(tokens) == 0) {
                warning("no tokens in sentence! Skipping sentence")
                # print(tokens)
                tokens = list(NULL)
        }
        if (any(is.null(tokens)) | any(is.na(tokens)) | length(tokens) == 0) {
                # print(tokens)
                warning("token vector is null or na!")
        }
        
        # 2. Remove numbers and non-ASCII
        # print("removing numbers and non-ASCII characters...")
        tokens = tokens %>%
                iconv("latin1", "ASCII", sub = "") %>%
                gsub("[0-9]", "", .) %>%
                .[. != ""]
        if (length(tokens) == 0) {
                warning("no tokens in sentence after removing numbers!")
        }
        if (any(is.null(tokens)) | any(is.na(tokens))) {
                warning("token vector is null or na!")
        }
        
        # 3. Load dictionaries for token cleaning
        badwords = readRDS("badWordsDict.rds")
        stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
        stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
        
        # 4. Define functions
        expContract <- function(tokens) {
                # print("Expanding contractions...")
                if (!identical(character(0), tokens)) {
                        tokens = tokens %>% gsub("'ve", " have", .) %>%
                                gsub("'s", " is", .) %>%
                                gsub("'m", " am", .) %>%
                                gsub("'ll", " will", .) %>%
                                gsub("'d", " would", .) %>%
                                gsub("n't", " not", .) %>%
                                gsub("'re", " are", .) %>%
                                tokenize_words() %>% unlist()
                }
                # print(tokens)
                tokens
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
                tokens = tokens %>% gsub("obscenity", NA, .) %>% .[!is.na(.)]
                # print(tokens)
                tokens
        }
        
        spellCheck <- function(tokens) {
                # print("Spell checking...")
                if (!identical(character(0), tokens) && length(tokens) > 0) {
                        tokens = sapply(1:length(tokens), function(i) {
                                if (is.na(tokens[i])) {
                                        tokens[i] = ""
                                }
                                library(hunspell)
                                condition = hunspell_check(tokens[i] %>% 
                                                                   as.character(),
                                                           dict = dictionary(language))
                                if (!condition && 
                                    !is.na(condition) && 
                                    !length(condition) == 0) {
                                        corrected = hunspell_suggest(tokens[i] 
                                                                     %>% 
                                                                             as.character(),
                                                                     dict = dictionary(language))[[1]]
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
                if (any(grepl('\\(\\"', tokens))) {
                        warning("not separating characters after spellchecking!")
                }
                tokens
        }
        
        removeStopWords <- function(tokens) {
                # print("Removing stopwords...")
                tokens = stringr::str_replace_all(tokens, stopwords_regex, '')
                # print("tokens after removing stopwords:")
                tokens = Filter(function(x) !identical(character(0),x), tokens)
                if (any(grepl('\\(\\"', tokens))) {
                        warning("not separating characters after removing stopwords!")
                }
                tokens = Filter(function(f) nchar(f) > 0, tokens)
                tokens
        } 
        
        stemWords <- function(tokens) {
                tokens = hunspell_stem(tokens, dict = dictionary(language))
                tokens %>% sapply(function(x) x[length(x)])
        }
        
        # 5. Perform Clean sentences
        tokens = tokens %>% 
                expContract() %>%
                cleanWords() %>%
                spellCheck() %>%
                # removeStopWords() %>%
                unlist() 
        tokens = tokens[!is.na(tokens)]
        
        if (stem == T) {
                tokens = tokens %>% 
                        stemWords() %>%
                        unlist() 
        }
        
        # print(tokens)
        tokens
}
