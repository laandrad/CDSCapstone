library(dplyr)
library(tokenizers)

source("myFunctionsv2.R")
source("wordPrediction/wordPredict.R")

# get file names in folder
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = ".txt")

# load test data
linesToRead = 10000
con = file(paste0(data.folder, myFiles[1]), "r")
myLines = readLines(con, linesToRead)
close(con)

# load tetragram database
fileName = "/Users/alejandro/Coursera Data Science Capstone Data/tetraGramsFreqFinal.csv"
db4 = read.csv(fileName, row.names = 1, stringsAsFactors = F)
colnames(db4) = c("first", "second", "third", "fourth", "freq")

# load trigram database
# fileName = "/Users/alejandro/Coursera Data Science Capstone Data/triGramsFreqFinal.csv"
# db3 = read.csv(fileName, row.names = 1, stringsAsFactors = F)
# colnames(db3) = c("first", "second", "third", "freq")
# 
# # load bigram database
# fileName = "/Users/alejandro/Coursera Data Science Capstone Data/biGramsFreqFinal.csv"
# db2 = read.csv(fileName, row.names = 1, stringsAsFactors = F)
# colnames(db2) = c("first", "second", "freq")
# 

# test run
perf4 = numeric(4) %>% t() %>% as.data.frame()
colnames(perf4) = c("first", "alt", "all", "perf")

perf3 = numeric(4) %>% t() %>% as.data.frame()
colnames(perf3) = c("first", "alt", "all", "perf")

perf2 = numeric(4) %>% t() %>% as.data.frame()
colnames(perf2) = c("first", "alt", "all", "perf")

N = 200

for (i in 1:N) {
        
        print(paste("Generating sentence", i))
        sentence = generateSentence(myLines)
        
        if (!is.character(sentence[[1]])) next
        
        tokens = sentence[[1]] %>% tokenize_words() %>% unlist()
        trueW = tokens[length(tokens)]
        testSentence = tokens[-length(tokens)] %>% paste(collapse = " ")
        
        print("Making prediction from tetragrams...")
        tic = proc.time()
        myPred = predW4G(testSentence, db4)
        tot = proc.time() - tic
        g(a, b, c) %=% crossCheckPrediction(myPred, trueW)
        
        perf4 = rbind(perf4, data.frame(first = as.numeric(a),
                                        alt = as.numeric(b),
                                        all = as.numeric(c),
                                        perf = tot[3]))
        
        print("Making prediction from trigrams...")
        tic = proc.time()
        myPred = predW3G(testSentence, db4)
        tot = proc.time() - tic
        g(a, b, c) %=% crossCheckPrediction(myPred, trueW)
        
        perf3 = rbind(perf3, data.frame(first = as.numeric(a),
                                        alt = as.numeric(b),
                                        all = as.numeric(c),
                                        perf = tot[3]))
        
        
        print("Making prediction from bigrams...")
        tic = proc.time()
        myPred = predW2G(testSentence, db4)
        tot = proc.time() - tic
        perf2 = rbind(perf2, data.frame(first = as.numeric(a),
                                        alt = as.numeric(b),
                                        all = as.numeric(c),
                                        perf = tot[3]))
}

perf4 = perf4[-1, ]
perf3 = perf3[-1, ]
perf2 = perf2[-1, ]
print("Performance tetragrams...")
apply(perf4, 2, mean)
print("Performance trigrams...")
apply(perf3, 2, mean)
print("Performance bigrams...")
apply(perf2, 2, mean)
