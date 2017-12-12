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

generateSentence <- function(myLines) {
        library(dplyr)
        library(tokenizers)
        
        j = runif(n = 1, min = 2, max = length(myLines)) %>% round
        text = myLines[j] %>% tokenize_sentences() %>% unlist()
        text =  text[[length(text)]] %>% tokenize_words() %>% unlist()
        
        if (!any(is.na(text)) &
            length(text) > 1) {
                k = runif(n = 1, min = 2, max = length(text)) %>% round
                sentence = text[1:(k - 1)] %>% paste(collapse = " ")
                word = text[k]
                # print(paste(sentence, "->", word))
                list(sentence, word)
        } else {
                list(sentence = NULL, word = NULL)
        }
}

# Generic function to assign multiple variables from list at the same time

# Generic form
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
        Envir = as.environment(-1)
        
        if (length(r) > length(l))
                warning("RHS has more args than LHS. Only first", length(l), "used.")
        
        if (length(l) > length(r))  {
                warning("LHS has more args than RHS. RHS will be repeated.")
                r <- extendToMatch(r, l)
        }
        
        for (II in 1:length(l)) {
                do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
        }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
        s <- length(source)
        d <- length(destin)
        
        # Assume that destin is a length when it is a single number and source is not
        if(d==1 && s>1 && !is.null(as.numeric(destin)))
                d <- destin
        
        dif <- d - s
        if (dif > 0) {
                source <- rep(source, ceiling(d/s))[1:d]
        }
        return (source)
}

# Grouping the left hand side
g = function(...) {
        List = as.list(substitute(list(...)))[-1L]
        class(List) = 'lbunch'
        return(List)
}