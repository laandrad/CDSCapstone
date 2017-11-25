
# 4. Word prediction Example
data.folder = "/Users/alejandro/Dropbox (Personal)/Coursera Data Science/"
biGrams = read.csv(paste0(data.folder, "wordPrediction-bigrams.csv"))
source("wordPrediction/wordPredict.R")

sentence = "I am"
wordPredict(sentence, biGrams)

word = "His father"
wordPredict(word, biGrams)

