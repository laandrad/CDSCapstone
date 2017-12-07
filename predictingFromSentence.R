library(dplyr)
library(tokenizers)

# 4. Word prediction Example
data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
biGrams = read.csv(paste0(data.folder, "wordPrediction-bigrams.csv"))
triGrams = read.csv(paste0(data.folder, "wordPrediction-trigrams.csv"))
tetraGrams = read.csv(paste0(data.folder, "wordPrediction-tetragrams.csv"))
source("wordPrediction/wordPredict.R")

# get file names in folder
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = ".txt")

linesToRead = 10
con = file(paste0(data.folder, myFiles[1]), "r")
myLines = readLines(con, linesToRead)
close(con)


generateSentence <- function(myLines) {
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


sentence = generateSentence(myLines)
sentence
predictWord(sentence[[1]], biGrams, triGrams, tetraGrams)


testAcc <- function(myLines, biGrams, triGrams, tetraGrams) {
        sentence = generateSentence(myLines)
        print(sentence)
        if (!is.null(sentence[[1]])) {
                sentence[[2]] == predictWord(sentence[[1]], 
                                             biGrams, 
                                             triGrams, 
                                             tetraGrams)[[1]]
        }
}

test = NULL
for (i in 1:100) {
        print(i)
        myTest = testAcc(myLines, biGrams, triGrams, tetraGrams)
        print(myTest)
        test = c(test, myTest)
}
test %>% table()
