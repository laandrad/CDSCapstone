library(dplyr)
library(tokenizers)

source("wordPrediction/wordPredict.R")

# get file names in folder
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myFiles = list.files(data.folder, pattern = ".txt")

# load test data
linesToRead = 10000
con = file(paste0(data.folder, myFiles[1]), "r")
myLines = readLines(con, linesToRead)
close(con)

# load database
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/"
db4 = readRDS(paste0(data.folder, "database4grams.Rdata"))

# test run
perf4 = numeric(4) %>% t() %>% as.data.frame()
colnames(perf4) = c("first", "alt", "all", "perf")

N = 200

for (i in 1:N) {
        
        print(paste("Generating sentence", i))
        sentence = generateSentence(myLines)
        
        if (!is.character(sentence[[1]])) next

        sentence = cleanSentence(sentence[[1]])
        print(sentence)
        
        if (!is.character(sentence)) next
        if (length(sentence) < 2) next
        
        trueW = sentence[length(sentence)]
        testSentence = sentence[-length(sentence)]
        
        print("Making prediction from tetragrams...")
        tic = proc.time()
        target = testSentence[length(testSentence)]
        i = which(names(db4) == target)
        if (identical(integer(0), i)) next
        db =  db4[[i]]
        myPred = predW4G(testSentence, db)
        tot = proc.time() - tic
        g(a, b, c) %=% crossCheckPrediction(myPred, trueW)
        
        perf4 = rbind(perf4, data.frame(first = as.numeric(a),
                                        alt = as.numeric(b),
                                        all = as.numeric(c),
                                        perf = tot[3]))
}

perf4 = perf4[-1, ]
print("Performance tetragrams...")
apply(perf4, 2, mean) %>% round(3)

