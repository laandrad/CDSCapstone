
# remove contractions, bad words, and punctuation
cleanSentence <- function(sentence, language = "en_US", stem = F) {
        library(dplyr)
        library(tokenizers)
        library(qdapDictionaries)
        library(hunspell)
        
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
        
        # 3. Load dictionaries for token cleaning
        contracted = contractions[, 1] %>% tokenize_words() %>% 
                unlist() %>% gsub("'", "~", .)
        expanded = contractions[, 2] %>% tokenize_words() %>% unlist()
        abbreviations = rbind(abbreviations,
                              data.frame(abv = "T.V.",
                                         rep = "Television"))
        abbrev = abbreviations[, 1] %>% tokenize_words() %>% unlist()
        abbExp = abbreviations[, 2] %>% tokenize_words() %>% unlist()
        source.folder = "//client/C$/Users/alejandro/Dropbox (Personal)/Coursera Data Science/Capstone Project/"
        badwords = readRDS(paste0(source.folder, "badWordsDict.rds"))
        stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
        stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
        
        # 4. Define functions
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
                                condition = hunspell_check(tokens[i] %>% 
                                                                as.character(),
                                                           dict = dictionary(language))
                                # str(condition)
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
                tokens = hunspell_stem(tokens, dict = dictionary(language))
                tokens %>% sapply(function(x) x[length(x)])
        }
        
        # 5. Perform Clean sentences
        if (stem == T) {
                tokens = tokens %>% 
                        expContract() %>% 
                        expAbbv() %>%
                        cleanWords() %>%
                        spellCheck() %>%
                        removeStopWords() %>%
                        stemWords() %>%
                        unlist() 
                tokens[!is.na(tokens)]
        } else {
                tokens = tokens %>% 
                        expContract() %>% 
                        expAbbv() %>%
                        cleanWords() %>%
                        spellCheck() %>%
                        removeStopWords() %>%
                        unlist() 
                tokens[!is.na(tokens)]
        }
        
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
        # print(totalSentences)
        # print(nPartitions)
        partLength = round(totalSentences / nPartitions)
        # print(partLength)
        parts = gl(n = nPartitions, k = partLength, length = totalSentences)
        # print(parts)
        
        wordCount <- function(tokenList) {
                
                oldw = getOption("warn")
                options(warn = -1)
                
                # print(tokenList)
                freqTables = tokenList %>% lapply(table)
                terms = freqTables %>% unlist() %>% names() %>% unique() %>% sort()
                
                df = data.frame(Term = terms)
                # print(head(df))
                
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
                # print(head(df))
                
                df
        }
        
        print("Shuffling...")
        cl = makeCluster(4L)
        tl = pbsapply(1:nPartitions, function(i) {
                library(dplyr)
                myTokens[parts == as.character(i)] %>% wordCount()
        }, cl = cl)
        stopCluster(cl)
        # print(str(tl))
        
        print("Reduction...")
        dt = data.frame(Term = tl[1])
        colnames(dt) = "Term"
        
        # print(head(dt))
        # print(length(seq(1, length(tl), 2)))
        
        pb = txtProgressBar(min = 0, max = length(seq(1, length(tl), 2)), style = 3)
        for (i in seq(1, length(tl), 2)) {
                        # print(head(tl[[i]]))
                        # print(head(tl[[i + 1]]))
                
                        df = data.frame(Term = tl[i], Freq = tl[i + 1])
                        # print(i)
                        # print(head(df))
                        colnames(df) = c("Term", "Freq")
                        dt = dt %>% left_join(df, by = c("Term" = "Term"))
                setTxtProgressBar(pb, i + 1)
        }
        close(pb)
        
        dt[is.na(dt)] = 0
        dt = data.frame(dt[, 1], apply(dt[, -1], 1, sum))
        colnames(dt) = c("Term", "Freq")
        
        totTime = proc.time() - tic
        print(paste("Processing time:", 
                    totTime[3] %>% round(2) %>% seconds_to_period()))
        
        # print(head(dt))
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
        nGrams = pblapply(myTokens, createNgrams, N = N)
        # print(head(nGrams))
        stopCluster(cl)
        
        print(paste("Total unique Ngrams:", nGrams %>% unique() %>% length()))
        
        print("Calculating Ngram frequencies...")
        
        oldw = getOption("warn")
        options(warn = -1)
        
        print("Mapping...")
        nPartitions = nPartitions
        totalSentences = nGrams %>% length()
        # print(totalSentences)
        # print(nPartitions)
        partLength = round(totalSentences / nPartitions)
        # print(partLength)
        parts = gl(n = nPartitions, k = partLength, length = totalSentences)
        # print(parts)
        
        wordCount <- function(tokenList) {
                
                oldw = getOption("warn")
                options(warn = -1)
                
                # print(tokenList)
                freqTables = tokenList %>% lapply(table)
                terms = freqTables %>% unlist() %>% names() %>% unique() %>% sort()
                
                df = data.frame(Term = terms)
                # print(head(df))
                
                for (i in 1:length(freqTables)) {
                        
                        if (nrow(data.frame(freqTables[i])) > 0) {
                                # print(data.frame(freqTables[i]))
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
                # print(head(df))
                
                df
        }
        
        print("Shuffling...")
        cl = makeCluster(4L)
        tl = pbsapply(1:nPartitions, function(i) {
                library(dplyr)
                nGrams[parts == as.character(i)] %>% wordCount()
        }, cl = cl)
        stopCluster(cl)
        # print(str(tl))
        
        print("Reduction...")
        dt = data.frame(Term = tl[1])
        colnames(dt) = "Term"
        
        # print(head(dt))
        # print(length(seq(1, length(tl), 2)))
        
        pb = txtProgressBar(min = 0, max = length(seq(1, length(tl), 2)), style = 3)
        for (i in seq(1, length(tl), 2)) {
                df = data.frame(Term = tl[i], Freq = tl[i + 1])
                # print(i)
                # print(head(df))
                colnames(df) = c("Term", "Freq")
                dt = dt %>% left_join(df, by = c("Term" = "Term"))
                setTxtProgressBar(pb, i + 1)
        }
        close(pb)
        
        dt[is.na(dt)] = 0
        dt = data.frame(dt[, 1], apply(dt[, -1], 1, sum))
        colnames(dt) = c("Term", "Freq")
        
        totTime = proc.time() - tic
        print(paste("Processing time:", 
                    totTime[3] %>% round(2) %>% seconds_to_period()))
        
        # print(head(dt))
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

myPlotCoverage <- function(dtf, dtf1, dtf2) {
        
        library(plotly)
        
        Sys.setenv("plotly_username" = "landrad78")
        Sys.setenv("plotly_api_key" = "JUuLrFcCqExiWblQbHup")
        
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
                layout(title = myFiles[i],
                       yaxis = list(title = "% Coverage"))
        plotly_IMAGE(p, format = "png", 
                     out_file = paste0(data.folder, "coverage_terms_", objName, ".png"))
        api_create(p, paste0(data.folder, "coverage_terms_", objName), 
                   fileopt = "overwrite", sharing = "public")
        p
}