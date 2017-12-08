# word Prediction
predictWord <- function(sentence, biGrams, triGrams, tetraGrams) {
        
        library(tokenizers)
        
        if (!is.null(sentence)) {
                sentence = cleanSentenceSilently(sentence)
                sentence = sentence %>% tokenize_words() %>% unlist() %>% unname()
        }
        
        # print(sentence)
        
        if (!is.null(sentence) & length(sentence) > 2) {
                wordPred = predictFromTetragram(sentence, tetraGrams)
                print(paste("tetragram prediction:", wordPred))
                word = wordPred[[1]]
                # print(word)
                alternatives = wordPred[[2]]
                
                # if (is.na(word)) warning("word is NA in tetragram")
                
                if (is.null(word) || is.na(word)) {
                        wordPred = predictFromTrigram(sentence, triGrams)
                        print(paste("triagram prediction:", wordPred))
                        word = wordPred[[1]]
                        alternatives = wordPred[[2]]
                        
                        # if (is.na(word)) warning("word is NA in Trigram")
                        
                        if (is.null(word) || is.na(word)) {
                                wordPred = predictFromBigram(sentence, biGrams)
                                print(paste("biagram prediction:", wordPred))
                                word = wordPred[[1]]
                                alternatives = wordPred[[2]]
                        }
                }
                
        } else if (!is.null(sentence) & 
                   length(sentence) == 2) {
                
                wordPred = predictFromTrigram(sentence, triGrams)
                word = wordPred[[1]]
                alternatives = wordPred[[2]]
                
                if (is.null(word) || 
                    identical(logical(0), word) ||
                    is.na(word)) {
                        wordPred = predictFromBigram(sentence, biGrams)
                        word = wordPred[[1]]
                        alternatives = wordPred[[2]]
                }
                        
        } else if (!is.null(sentence) & 
                   length(sentence) == 1) {
                
                wordPred = predictFromBigram(sentence, biGrams)
                word = wordPred[[1]]
                alternatives = wordPred[[2]]
                
        } else {
                list(word = NULL, alternatives = c("a", "the", "is"))
        }
        
        if (any(is.na(alternatives))) {
                alternatives = predictFromTrigram(sentence, triGrams)[[2]]
                
                if (any(is.na(alternatives))) {
                        alternatives = predictFromBigram(sentence, biGrams)[[2]]
                }
        }
        
        list(word, alternatives)
}

predictFromTetragram <- function(sentence, tetraGrams) {
        
        if (!is.null(sentence) &&
            !is.na(sentence)) {
                
                triGramFirst = sentence[length(sentence) - 2]
                triGramSecond = sentence[length(sentence) - 1]
                triGramThird = sentence[length(sentence)]
                
                if (triGramFirst %in% tetraGrams$first & 
                    triGramSecond %in% tetraGrams$second &
                    triGramThird %in% tetraGrams$third) {
                                        predWords = tetraGrams %>%
                                                filter(first == triGramFirst &
                                                               second == triGramSecond &
                                                               third == triGramThird) %>%
                                                arrange(desc(Freq))
                                        prediction = predWords[1, "fourth"] %>% as.character()
                                        alternatives = predWords[2:4, "fourth"] %>% as.vector()
                                        return(list(myWordPrediction = prediction, 
                                                    wordAlternatives = alternatives))
                        
                } else {
                        return(list(myWordPrediction = NULL, 
                                    wordAlternatives = sample(tetraGrams$fourth, 3) %>% 
                                            as.vector()))
                }
        }
}

predictFromTrigram <- function(sentence, triGrams) {
        
        if (!is.null(sentence) &&
            !is.na(sentence)) {
                
                bigramFirst = sentence[length(sentence) - 1]
                # print(bigramFirst)
                bigramSecond = sentence[length(sentence)]
                # print(bigramSecond)
                
                if (bigramFirst %in% triGrams$first & 
                    bigramSecond %in% triGrams$second) {
                                predWords = triGrams %>%
                                        filter(first == bigramFirst &
                                                       second == bigramSecond) %>%
                                        arrange(desc(Freq))
                                prediction = predWords[1, "third"] %>% as.character()
                                alternatives = predWords[2:4, "third"] %>% as.vector()
                                return(list(myWordPrediction = prediction, 
                                            wordAlternatives = alternatives))
                } else {
                        return(list(myWordPrediction = NULL, 
                                    wordAlternatives = sample(triGrams$third, 3) %>% 
                                            as.vector()))
                }
        }
}

predictFromBigram <- function(sentence, biGrams) {
        
        if (!is.null(sentence) &&
            !is.na(sentence)) {

                word = sentence[length(sentence)]
                
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
                                    wordAlternatives = sample(biGrams$second, 3) %>% 
                                            as.vector()))
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
        
        tokens = sentence %>% tokenize_words() %>% unlist()
        
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
