# remove contractions, bad words, and punctuation

cleanSentence <- function(sentence) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)
        
        # converting sentence into tokens
        print("tokenizing...")
        if (length(sentence) == 0) {
                warning("no words in sentence! Skipping sentence")
                print(sentence)
                sentence = list(NULL)
        }
        print(sentence)
        tokens = sentence %>% 
                tokenize_words() %>% 
                unlist()
        print(tokens)
        if (length(tokens) == 0) {
                warning("no tokens in sentence! Skipping sentence")
                print(tokens)
                tokens = list(NULL)
        }
        if (any(is.null(tokens)) | any(is.na(tokens)) | length(tokens) == 0) {
                print(tokens)
                warning("token vector is null or na!")
        }
        print("removing numbers and non-ASCII characters...")
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
                print("tokens after removing badwords:")
                tokens = sapply(tokens, function(x) x[!is.na(x)]) %>% unlist()
                tokens = Filter(function(x) !identical(character(0),x), tokens)
                badTokens = grepl("obscenity", tokens)
                tokens = tokens[!badTokens]
                print(tokens)
                tokens
        }
        
        spellCheck <- function(tokens) {
                print("Spell checking...")
                tokens = gsub("~", "", tokens)
                print("tokens read for spellchecking:")
                print(tokens)
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
                print("tokens after spellchecking:")
                print(tokens)
                if (any(grepl('\\(\\"', tokens))) {
                        warning("not separating characters after spellchecking!")
                }
                tokens
        }
        
        removeStopWords <- function(tokens) {
                print("Removing stop words...")
                tokens = stringr::str_replace_all(tokens, stopwords_regex, '')
                print("tokens after removing stopwords:")
                tokens = Filter(function(x) !identical(character(0),x), tokens)
                if (any(grepl('\\(\\"', tokens))) {
                        warning("not separating characters after removing stopwords!")
                }
                tokens = Filter(function(f) nchar(f) > 0, tokens)
                print(tokens)
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
        library(stringi)
        
        numbWords =  line %>% stri_count_words()
        lineLength = line %>% stri_length()
        
        return(list(numbWords, lineLength))
}


getWordFreq <- function(tokenList) {
        tic = proc.time()
        oldw = getOption("warn")
        options(warn = -1)
        
        freqTables = tokenList %>% lapply(table)
        terms = freqTables %>% unlist() %>% names() %>% unique() %>% sort()
        
        df = data.frame(Term = terms)
        
        pb = txtProgressBar(min = 0, max = length(freqTables), style = 3)
        
        for (i in 1:length(freqTables)) {
                # print(data.frame(freqTables[i]))
                
                if (nrow(data.frame(freqTables[i])) > 0) {
                        df = df %>% left_join(data.frame(freqTables[i]), 
                                              by = c("Term" = "Var1"))
                }
                
                setTxtProgressBar(pb, i)
        }
        close(pb)
        
        df[is.na(df)] = 0
        
        df = data.frame(df[, 1], apply(df[, -1], 1, sum))
        colnames(df) = c("Term", "Freq")
        
        options(warn = oldw)
        totTime = proc.time() - tic
        print(paste("Processing time:", 
                    totTime[3] %>% round(2) %>% seconds_to_period()))
        df
}

getNGrams <- function(tokenList, N = 2, ...) {
        
        library(dplyr)
        library(pbapply)

        createNgrams <- function(words, N = 2) {
                library(dplyr)
                library(tokenizers)
                words = paste(words, collapse = " ") %>% gsub("c", "", .)
                tokenize_ngrams(x = words, n = N, simplify = T)
        }

        # Start up a parallel cluster
        cl = makeCluster(4L)
        print("processing Ngrams...")
        nGrams = lapply(tokenList, createNgrams, N)
        head(nGrams)
        stopCluster(cl)
        
        print(cat("Total unique Ngrams:", nGrams %>% unique() %>% length()))
        
        print("Calculating Ngram frequencies...")
        
        tic = proc.time()
        oldw = getOption("warn")
        options(warn = -1)
        
        freqTables = nGrams %>% lapply(table)
        terms = freqTables %>% unlist() %>% names() %>% unique() %>% sort()
        
        df = data.frame(Term = terms)
        
        pb = txtProgressBar(min = 0, max = length(freqTables), style = 3)
        
        for (i in 1:length(freqTables)) {
                
                if (nrow(data.frame(freqTables[i])) > 0) {
                        df = df %>% left_join(data.frame(freqTables[i]), 
                                              by = c("Term" = "Var1"))
                }
                
                setTxtProgressBar(pb, i)
        }
        close(pb)
        
        df[is.na(df)] = 0
        
        df = data.frame(df[, 1], apply(df[, -1], 1, sum))
        colnames(df) = c("Term", "Freq")
        
        options(warn = oldw)
        totTime = proc.time() - tic
        print(paste("Processing time:", 
                    totTime[3] %>% round(2) %>% seconds_to_period()))
        df
}

detectLanguage <- function(words) {
        library(dplyr)
        library(cld2)
        words = paste(words, collapse = " ") %>% gsub("c", "", .)
        detect_language(words)
}

dtm <- function(wordFreq) {
        library(dplyr)
        wordFreq %>%
                mutate(Perc = Freq / sum(Freq) * 100) %>% 
                arrange(desc(Perc)) %>%
                mutate(CumPerc = cumsum(Perc)) %>% 
                arrange(desc(Perc))
}

replaceRedundant <- function(text){
        paste(text, collapse = " ") %>%
                strsplit(split = "(?!')[ [:punct:]]", fixed = F, perl = T) %>%
                unlist() %>%
                trimws() %>%
                unique() %>%
                paste(collapse = " ") %>%
                gsub("list c  ", "", .)
}
