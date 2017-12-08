library(dplyr)
library(pbapply)
library(parallel)

source("myFunctions.R")

data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/"
myFiles = list.files(data.folder, pattern = "Freq")
myFiles

con = file(paste0(data.folder, myFiles[3]))
myLines = readLines(con)
close.connection(con)

# line = myLines[1]
print("Mapping...")
nPartitions = 6
totalDFS = myLines %>% length()
partLength = round(totalDFS / nPartitions)
parts = gl(n = nPartitions, k = partLength, length = totalDFS)

print("Shuffling...")
i = 2
for (i in 5) {
        ngrams = makeDF(myLines[parts == as.character(i)], N = 4)
        print(paste("finished part", i, "of", nPartitions))
        write.csv(ngrams, 
                  paste0(data.folder, "tetraGramsFreqFinal-Part", i, ".csv"))
}
