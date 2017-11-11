# remove contractions, bad words, and punctuation

cleanLine <- function(line) {
        library(dplyr)
        library(hunspell)
        library(tokenizers)
        library(qdapDictionaries)
        
        abbreviations = rbind(abbreviations, 
                              data.frame(abv = "T.V.", rep = "Television"))
        
        
        expandAbbreviations <- function(text) {
                
                text = text %>% strsplit(., " ") %>% unlist()
                words = character(length(text))
                for (i in 1:length(words)) {
                        if (text[i] %in% abbreviations[, "abv"]) {
                                words[i] = abbreviations[
                                                  abbreviations[, "abv"] 
                                                  %in% text, "rep"]
                        } else {
                                words[i] = text[i]
                        }
                }
                
                words %>% paste(collapse = " ")
        } 
        
        
        expandContractions <- function(word) {
                ifelse(word %in% contractions[, 1], 
                       contractions[which(contractions[, 1] %in% word), 2], 
                       word) %>% 
                        unlist() %>% 
                        paste(collapse = " ") %>% 
                        tokenize_regex("[^[:alpha:] ]| ") %>% 
                        unlist() %>% 
                        iconv("latin1", "ASCII", sub = "") %>% 
                        gsub("([[:alpha:]])\\1{2,}", "\\1", .) %>%
                        gsub(".*haha*.", "ha-ha", .) %>%
                        (function(x) x[(!x == "")]) 
        }

        
        spellCheck <- function(terms) {
                
                badwords = readRDS("badWordsDict.rds")
                
                if (is.list(terms)) {
                        terms = unlist(terms)
                }
                
                for (i in 1:length(terms)) {
                        
                        # print(terms[i])
                        
                        if (!identical(terms[i], character(0)) &&
                            !identical(terms[i], NULL) &&
                            !length(terms[i]) == 0) {
                                
                                word = terms[i]
                                condition = word %in% badwords
                                # print(cat("bad word?", condition))
                                
                                if (condition) {
                                        word = "obscenity"
                                } else {
                                        condition = hunspell_check(word)
                                        if (!condition && !is.na(condition)) {
                                                corrected = hunspell_suggest(word)[[1]]
                                                word = gsub(" ", "", corrected)[1]
                                        } else {
                                                word = word
                                        }
                                }
                                
                        # print(word)
                        terms[i] = word

                        } else {
                                
                                terms[i] = NULL
                                
                        }
                }
                
                terms
        }
         
                       
        cleanSentence <- function(sentence) {
                        tokenize_word_stems(sentence, 
                                            language = "english", 
                                            stopwords = stopwords("en"), 
                                            simplify = T) %>% 
                        sapply(expandContractions) %>%
                        unname() %>%
                        spellCheck()
        }
        
        line %>% 
                expandAbbreviations() %>%
                tokenize_sentences() %>% 
                lapply(cleanSentence)
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


getWordFreq <- function(text) {
        
        library(dplyr)
        library(parallel)
        library(pbapply)
        # Start up a parallel cluster
        cl = makeCluster(4L)
        
        print("Unlisting words...")
        terms = pbsapply(text, function(x) unlist(x, recursive = T), cl = cl) %>% unlist()
        print(cat("Finished unlisting", length(terms), "words"))

        # close cluster connection
        stopCluster(cl)
        
        print("Computing term frequency matrix...")
        # dtm = pbsapply(terms, table, cl = cl) %>% data.frame()
        dtm = numeric(terms %>% unique %>% length)
        names(dtm) = terms %>% unique %>% sort()

        print(cat("Total unique terms:", length(dtm)))

        pb = txtProgressBar(min = 0, max = length(terms), style = 3)

        for (i in 1:length(terms)) {
                if (terms[i] %in% names(dtm)) {
                        dtm[which(terms[i] == names(dtm))] =
                                dtm[which(terms[i] == names(dtm))] + 1
                }
                setTxtProgressBar(pb, i)
        }

        dtm[!is.na(names(dtm))] %>%
                data.frame(term = names(.), 
                         Freq = .) %>%
                arrange(desc(Freq))
}

getNGrams <- function(text, N = 2) {
        
        library(dplyr)
        library(pbapply)
        library(doParallel)
        library(foreach)

        getNgrams <- function(words, N) {
                library(dplyr)
                library(tokenizers)
                words = paste(words, collapse = " ") %>% gsub("c", "", .)
                tokenize_ngrams(x = words, n = N, simplify = T)
        }

        # Start up a parallel cluster
        cl = makeCluster(4L)
        print("processing Ngrams...")
        nGrams = pbsapply(text, function(x) getNgrams(x, N), cl = cl) %>% unlist()
        stopCluster(cl)
        
        print("Calculating Ngram frequencies...")
        dtm = numeric(nGrams %>% unique() %>% length())
        names(dtm) = nGrams %>% unique() %>% sort()

        cl = makeCluster(4L)
        registerDoParallel(cl)
        pb = txtProgressBar(min = 0, max = length(nGrams), style = 3)
        foreach(i = 1:length(nGrams)) %dopar% {
                if (nGrams[i] %in% names(dtm)) {
                        dtm[which(nGrams[i] == names(dtm))] =
                                dtm[which(nGrams[i] == names(dtm))] + 1
                }
                setTxtProgressBar(pb, i)
        }
        close(pb)
        stopCluster(cl)
        
        print("Processing term-frequency dataframe...")
        
        dtmProcess <- function(dtm) {
                library(dplyr)
                dtm[!is.na(names(dtm))] %>%
                        data.frame(term = names(.), 
                                   Freq = .) %>%
                        arrange(desc(Freq))
        }
        
        cl = makeCluster(4L)
        dtm = pbsapply(dtm, dtmProcess, cl = cl)
        stopCluster(cl)
        
        dtm
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
                mutate(Perc = Freq / sum(Freq) * 100,
                       CumPerc = cumsum(Perc))
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


# if (!is.null(condition) && 
#     length(condition) > 1 && 
#     !is.na(condition)) {
#         
#         word = word
#         
# }