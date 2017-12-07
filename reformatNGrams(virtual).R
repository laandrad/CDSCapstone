
library(dplyr)
library(pbapply)
library(parallel)

setwd("C:/Users/Alejandro/Desktop/")
# link = "triGramsFreq.txt.tar.gz"
# untar(link, "triGramsFreq.txt")

con = file("triGramsFreq.txt")
myLines = readLines(con)
close.connection(con)

reformat <- function(line, N = 2) {
  library(dplyr)
  
  line = line %>% gsub("\\t", " ", .) %>% strsplit(" ") %>% unlist()
  nGram = line[1] %>% strsplit("-") %>% unlist() %>% matrix(ncol = N)
  data.frame(nGram, Freq = line[2])
}

makeDF <- function(myLines, N = 2) {
  cl = makeCluster(2L)
  df = myLines %>% pblapply(reformat, N, cl = cl) %>% 
    pbsapply(unlist, cl = cl) %>% t() %>% as.data.frame()
  stopCluster(cl)
  df
}

print("Mapping...")
nPartitions = 30
totalDFS = myLines %>% length()
partLength = round(totalDFS / nPartitions)
parts = gl(n = nPartitions, k = partLength, length = totalDFS)

print("Shuffling...")
i = 1
for (i in 1:10) {
  ngrams = makeDF(myLines[parts == as.character(i)], N = 3)
  print(paste("finished part", i, "of", nPartitions))
  write.csv(ngrams, paste0("triGramsFreqFinal-Part1-", i, ".csv"))
}
