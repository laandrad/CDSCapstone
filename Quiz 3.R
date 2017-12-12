library(dplyr)
library(tokenizers)

source("wordPrediction/wordPredict.R")

# load tetragram database
fileName = "/Users/alejandro/Coursera Data Science Capstone Data/tetraGramsFreqFinal.csv"
db4 = read.csv(fileName, row.names = 1, stringsAsFactors = F)
colnames(db4) = c("first", "second", "third", "fourth", "freq")

# Q1
sentence = "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
myPred = predW4G(sentence, db4)
myPred = predW3G(sentence, db3)
# tried die - right

# Q2
sentence = "I asked about dessert and he started telling me about his"
myPred = predW4G(sentence, db4)
# tried financial then marital - right

# Q3
sentence = "I'd give anything to see arctic monkeys this"
myPred = predW4G(sentence, db4)
myPred[1:2]
# tried morning then weekend - right

# Q4
sentence = "Talking to your mom has the same effect as a hug and helps reduce your"
myPred = predW4G(sentence, db4)
myPred[1:2]
# tried stress - right

# Q5
sentence = "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
myPred = predW4G(sentence, db4)
myPred[1:2]
# tried picture - right

# Q6
sentence = "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
myPred = predW4G(sentence, db4)
myPred[1:2]
# tried case then matter - right 

# Q7
sentence = "I can't even hold an uneven number of bags of groceries in each"
myPred = predW4G(sentence, db4)
myPred[1:2]
# tried hand - right

# Q8
sentence = "Every inch of you is perfect from the bottom to the"
myPred = predW4G(sentence, db4)
myPred[1:2]
# tried top - right

# Q9
sentence = "I’m thankful my childhood was filled with imagination and bruises from playing"
myPred = predW4G(sentence, db4)
myPred[1:2]
# tried outside - right

# Q10
sentence = "I like how the same people are in almost all of Adam Sandler's"
myPred = predW4G(sentence, db4)
myPred = predW3G(sentence, db3)
myPred[1:2]
# tried movies - right
