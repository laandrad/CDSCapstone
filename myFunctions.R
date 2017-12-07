reformat <- function(line, N = 2) {
        library(dplyr)
        
        line = line %>% gsub("\\t", " ", .) %>% strsplit(" ") %>% unlist()
        nGram = line[1] %>% strsplit("-") %>% unlist() %>% matrix(ncol = N)
        data.frame(nGram, Freq = line[2])
}

makeDF <- function(myLines, N = 2) {
        cl = makeCluster(2L)
        df = myLines %>% pblapply(reformat, N, cl = cl) %>% 
                pbsapply(unlist, cl = cl) %>% t() %>% as.data.frame()
        stopCluster(cl)
        df
}

# remove contractions, bad words, and punctuation
cleanSentence <- function(sentence, language = "en_US", stem = F) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)
        # library(hunspell)
        
        print("Original sentence:")
        print(sentence)
        
        # 1 . Convert sentence into tokens
        print("tokenizing...")
        if (length(sentence) == 0) {
                warning("no words in sentence! Skipping sentence")
                print(sentence)
                sentence = list(NULL)
        }
        
        tokens = sentence %>% 
                tokenize_words() %>% 
                unlist()
        
        if (length(tokens) == 0) {
                warning("no tokens in sentence! Skipping sentence")
                print(tokens)
                tokens = list(NULL)
        }
        if (any(is.null(tokens)) | any(is.na(tokens)) | length(tokens) == 0) {
                print(tokens)
                warning("token vector is null or na!")
        }
        
        # 2. Remove numbers and non-ASCII
        print("removing numbers and non-ASCII characters...")
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
                print("Expanding contractions...")
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
                # print("tokens after removing badwords:")
                tokens = sapply(tokens, function(x) x[!is.na(x)]) %>% unlist()
                tokens = Filter(function(x) !identical(character(0),x), tokens)
                tokens = tokens %>% gsub("obscenity", NA, .) %>% .[!is.na(.)]
                # print(tokens)
                tokens %>% unname()
        }
        
        spellCheck <- function(tokens) {
                print("Spell checking...")
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
        
        removeStopWords <- function(tokens) {
                print("Removing stopwords...")
                tokens = stringr::str_replace_all(tokens, stopwords_regex, '')
                print("tokens after removing stopwords:")
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
        
        print(tokens)
        tokens
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

parallelWordCount <- function(myTokens, nPartitions = 4) {
        
        library(dplyr)
        library(parallel)
        library(pbapply)
        
        tic = proc.time()
        print("Mapping...")
        nPartitions = nPartitions
        totalSentences = myTokens %>% length()
        partLength = round(totalSentences / nPartitions)
        parts = gl(n = nPartitions, k = partLength, length = totalSentences)
        
        wordCount <- function(tokenList) {
                
                oldw = getOption("warn")
                options(warn = -1)
                
                freqTables = tokenList %>% lapply(table)
                terms = freqTables %>% unlist() %>% 
                        names() %>% unique() %>% sort()
                
                df = data.frame(Term = terms)
                
                for (i in 1:length(freqTables)) {
                        
                        if (nrow(data.frame(freqTables[i])) > 0) {
                                df = df %>% left_join(data.frame(freqTables[i]), 
                                                      by = c("Term" = "Var1"))
                        }
                }
                
                df[is.na(df)] = 0
                
                if (ncol(df) > 0) {
                        df = data.frame(df[, 1], apply(df[, -1], 1, sum))
                        colnames(df) = c("Term", "Freq")
                }
                
                options(warn = oldw)
                df
        }
        
        print("Shuffling...")
        cl = makeCluster(4L)
        termList = pblapply(1:nPartitions, function(i) {
                library(dplyr)
                myTokens[parts == as.character(i)] %>% wordCount()
        }, cl = cl)
        stopCluster(cl)
        
        print("Reduction...")
        reduce <- function(termList) {
                dt = data.frame(
                        Term = sapply(1:length(termList), function(i) 
                        levels(termList[[i]]$Term)) %>% unlist() %>% unique()
                        )

                pb = txtProgressBar(min = 0, 
                                    max = length(termList), 
                                    style = 3)
                for (i in 1:length(termList)) {
                        if (!identical(character(0), termList[[i]])) {
                                dt = dt %>% left_join(termList[[i]], 
                                                      by = c("Term" = "Term"))
                                setTxtProgressBar(pb, i + 1)
                        }
                }
                close(pb)
                
                dt[is.na(dt)] = 0
                dt = data.frame(Term = dt[, 1], Freq = apply(dt[, -1], 1, sum))
                dt
        }
        
        dt = reduce(termList)
        print(head(dt))
        
        totTime = proc.time() - tic
        print(paste("Processing time:", 
                    totTime[3] %>% round(2) %>% seconds_to_period()))
        dt
        
}

parallelNGramCount <- function(myTokens, N = 2, nPartitions = 4) {
        
        library(dplyr)
        library(parallel)
        library(pbapply)
        
        tic = proc.time()
        
        createNgrams <- function(words, N = N) {
                library(dplyr)
                library(tokenizers)
                words = paste(words, collapse = " ") %>% gsub("c ", "", .)
                tokenize_ngrams(x = words, n = N, simplify = T)
        }
        
        # Start up a parallel cluster
        cl = makeCluster(4L)
        print("processing Ngrams...")
        nGrams = pblapply(myTokens, createNgrams, N = N, cl = cl)
        stopCluster(cl)
        
        print(paste("Total unique Ngrams:", nGrams %>% unique() %>% length()))
        
        print("Calculating Ngram frequencies...")
        
        oldw = getOption("warn")
        options(warn = -1)
        
        print("Mapping...")
        nPartitions = nPartitions
        totalSentences = nGrams %>% length()
        partLength = round(totalSentences / nPartitions)
        parts = gl(n = nPartitions, k = partLength, length = totalSentences)
        
        wordCount <- function(tokenList) {
                
                oldw = getOption("warn")
                options(warn = -1)
                
                freqTables = tokenList %>% lapply(table)
                terms = freqTables %>% unlist() %>% names() %>% unique() %>% sort()
                
                df = data.frame(Term = terms)
                
                for (i in 1:length(freqTables)) {
                        
                        if (nrow(data.frame(freqTables[i])) > 0) {
                                df = df %>% left_join(data.frame(freqTables[i]), 
                                                      by = c("Term" = "Var1"))
                        }
                }
                
                df[is.na(df)] = 0
                
                if (ncol(df) > 0) {
                        df = data.frame(
                                Term = df[, 1], 
                                Freq = apply(df[, -1], 1, sum)
                                )
                }
                
                options(warn = oldw)
                df
        }
        
        print("Shuffling...")
        cl = makeCluster(2L)
        termList = pblapply(1:nPartitions, function(i) {
                library(dplyr)
                nGrams[parts == as.character(i)] %>% wordCount()
        }, cl = cl)
        stopCluster(cl)
        
        print("Reduction...")
        reduce <- function(termList) {
                dt = data.frame(Term = sapply(1:length(termList), function(i) 
                        levels(termList[[i]]$Term)) %>% unlist() %>% unique() )
                colnames(dt) = "Term"
                
                pb = txtProgressBar(min = 0, 
                                    max = length(termList), 
                                    style = 3)
                for (i in 1:length(termList)) {
                        if (!identical(character(0), termList[[i]])) {
                                dt = dt %>% left_join(termList[[i]], 
                                                      by = c("Term" = "Term"))
                                setTxtProgressBar(pb, i + 1)
                        }
                }
                close(pb)
                
                dt[is.na(dt)] = 0
                dt = data.frame(Term = dt[, 1], Freq = apply(dt[, -1], 1, sum))
                dt
        }
        
        dt = reduce(termList)
        
        totTime = proc.time() - tic
        print(paste("Processing time:", 
                    totTime[3] %>% round(2) %>% seconds_to_period()))
        dt
        
}


detectLanguage <- function(words) {
        library(dplyr)
        library(cld2)
        words = paste(words, collapse = " ") %>% gsub("c\\(", "", .)
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

myPlotCoverage <- function(dtf, dtf1, dtf2, title) {
        
        library(plotly)
        p1 = plot_ly(data = dtf, type = "scatter", mode = "lines", name = "Terms",
                     x = ~seq_along(CumPerc), y = ~CumPerc) %>%
                layout(xaxis = list(title = "Number of Terms"))
        p2 = plot_ly(data = dtf2, type = "scatter", mode = "lines", name = "BiGrams",
                     x = ~seq_along(CumPerc), y = ~CumPerc) %>%
                layout(xaxis = list(title = "Number of BiGrams"))
        p3 = plot_ly(data = dtf3, type = "scatter", mode = "lines", name = "TriGrams",
                     x = ~seq_along(CumPerc), y = ~CumPerc) %>%
                layout(xaxis = list(title = "Number of TriGrams"))
        p = subplot(p1, p2, p3, nrows = 1, shareY = T, titleX = T) %>%
                layout(title = title,
                       yaxis = list(title = "% Coverage"))
        p
}