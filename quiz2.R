library(dplyr)

source("wordPrediction/wordPredict.R")

data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/"
myFiles = list.files(data.folder, pattern = ".csv")
myFiles

biGrams = read.csv(paste0(data.folder, myFiles[1]), row.names = 1)
triGrams = read.csv(paste0(data.folder, myFiles[3]), row.names = 1)
tetraGrams = read.csv(paste0(data.folder, myFiles[2]))

head(biGrams)
colnames(biGrams) = c("first", "second", "Freq")

head(triGrams)
colnames(triGrams) = c("first", "second", "third", "Freq")

head(tetraGrams)
colnames(tetraGrams) = c("first", "second", "third", "fourth", "Freq")

# Q1
sentence = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
predictWord(sentence, biGrams, triGrams, tetraGrams)

#Q2
sentence = "You're the reason why I smile everyday. Can you follow me please? It would mean the"
predictWord(sentence, biGrams, triGrams, tetraGrams)

#Q3
sentence = "Hey sunshine, can you follow me and make me the"
predictWord(sentence, biGrams, triGrams, tetraGrams)

#Q4
sentence = "Offense still struggling but the"
predictWord(sentence, biGrams, triGrams, tetraGrams)
## this one doesn't seem correct
## chose wrong: "defense"

#Q5
sentence = "Go on a romantic date at the"
predictWord(sentence, biGrams, triGrams, tetraGrams)
## this one doesn't seem correct
## chose wrong: "movies"

#Q6
sentence = "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
predictWord(sentence, biGrams, triGrams, tetraGrams)

#Q7
sentence = "Love that film and haven't seen it in quite some"
predictWord(sentence, biGrams, triGrams, tetraGrams)

#Q8
sentence = "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
predictWord(sentence, biGrams, triGrams, tetraGrams)
## this one doesn't seem correct
## chose right: "fingers"

#Q9
sentence = "Be grateful for the good times and keep the faith during the"
predictWord(sentence, biGrams, triGrams, tetraGrams)
predictFromTrigram(cleanSentenceSilently(sentence), triGrams)
predictFromBigram(cleanSentenceSilently(sentence), biGrams)
## this one doesn't seem correct
## chose wrong: "hard"

#Q10
sentence = "If this isn't the cutest thing you've ever seen, then you must be"
predictWord(sentence, biGrams, triGrams, tetraGrams)
predictFromTrigram(cleanSentenceSilently(sentence), triGrams)
predictFromBigram(cleanSentenceSilently(sentence), biGrams)
## wrong prediction
## chose right: "insane"