# prediction from bigams algorithm
predW2G <- function(sentence, db) {
               
        fdb = db %>% filter(first %in% sentence[length(sentence)])
                
        if (fdb %>% nrow() > 0) {
                print("got first")
                        
                fdb %>% getPredictions(i = 2)
                
        } else {
                print("not in database")
        }
}

# prediction from trigams algorithm
predW3G <- function(sentence, db) {
       
        sdb = db %>% filter(second %in% sentence[length(sentence)])
        if (sdb %>% nrow() > 0) {
                print("got second")
                        
                fdb = sdb %>% filter(first %in% sentence[length(sentence) - 1])
                        
                if (fdb %>% nrow() > 0) {
                        print("got first")
                                
                        fdb %>% getPredictions(i = 3)
                                
                } else {
                        sdb %>% getPredictions(i = 3)
                }
                
        } else {
                print("not in database")
        }
}

# prediction from tetragams algorithm
predW4G <- function(sentence, db) {

        tdb = db %>% filter(third %in% sentence[length(sentence)])
        
        if (tdb %>% nrow() > 0) {
                # print("got third")
                
                sdb = tdb %>% filter(second %in% sentence[length(sentence)-1])
                if (sdb %>% nrow() > 0) {
                        # print("got second")
                        
                        fdb = sdb %>% filter(first %in% sentence[length(sentence) - 2])
                        
                        if (fdb %>% nrow() > 0) {
                                # print("got first")
                                
                                fdb %>% getPredictions(i = 4)
                                
                        } else {
                                sdb %>% getPredictions(i = 4)
                        }
                        
                } else {
                        tdb %>% getPredictions(i = 4)
                }
                
        } else {
                # print("not in database")
        }
}

# help function
getPredictions <- function(db, i) {
        word = db %>% arrange(desc(freq))
        list(pred = word[1, i],
             alt = word[2:6, i],
             all = word[, i],
             freq = word[, 5]
             )
}

# accuracy function
crossCheckPrediction <- function(myPred, trueW) {
        
        if (length(myPred) == 3) {
                list(first = myPred[[1]] == trueW,
                     alt = trueW %in% myPred[[2]],
                     all = trueW %in% myPred[[3]])
        } else if (length(myPred) == 2) {
                list(first = myPred[[1]] == trueW,
                     alt = trueW %in% myPred[[2]],
                     all = FALSE)
        } else {
                list(first = myPred[[1]] == trueW,
                     alt = FALSE,
                     all = FALSE)
        }
        
}

# remove contractions, bad words, and punctuation
cleanSentence <- function(sentence, language = "en_US", stem = F) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)

        # 1 . Convert sentence into tokens
        if (length(sentence) == 0) {
                warning("no words in sentence! Skipping sentence")
                sentence = list(NULL)
        }
        
        tokens = sentence %>% 
                tokenize_words() %>% 
                unlist()
        
        if (length(tokens) == 0) {
                warning("no tokens in sentence! Skipping sentence")
                tokens = list(NULL)
        }
        if (any(is.null(tokens)) | any(is.na(tokens)) | length(tokens) == 0) {
                warning("token vector is null or na!")
        }
        
        # 2. Remove numbers and non-ASCII
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
                tokens
        }
        
        cleanWords <- function(tokens) {
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
                tokens = sapply(tokens, function(x) x[!is.na(x)]) %>% unlist()
                tokens = Filter(function(x) !identical(character(0),x), tokens)
                tokens = tokens %>% gsub("obscenity", NA, .) %>% .[!is.na(.)]
                tokens %>% unname()
        }
        
        spellCheck <- function(tokens) {
                if (!identical(character(0), tokens) && length(tokens) > 0) {
                        tokens = sapply(1:length(tokens), function(i) {
                                if (is.na(tokens[i])) {
                                        tokens[i] = ""
                                }
                                library(hunspell) %>% suppressWarnings()
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
        
        stemWords <- function(tokens) {
                tokens = hunspell_stem(tokens, dict = dictionary(language))
                tokens %>% sapply(function(x) x[length(x)])
        }
        
        # 5. Perform Clean sentences
        tokens = tokens %>% 
                expContract() %>%
                cleanWords() %>%
                spellCheck() %>%
                unlist() 
        tokens = tokens[!is.na(tokens)]
        
        if (stem == T) {
                tokens = tokens %>% 
                        stemWords() %>%
                        unlist() 
        }

        tokens
}

