
library(varhandle)

data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/nGramData/"
myFiles = list.files(data.folder, pattern = "Freq.txt")
myFiles




myReadFile <- function(fileName, outFile) {
        print(paste("Reading file:", fileName, "..."))
        dat1 = read.table(fileName, sep = "-", fill = T)
        head(dat1)
        
        writeLines(text = dat1[, 2] %>% as.character(), 
                   con = "/Users/alejandro/Desktop/ngrams.txt", 
                   sep = "\t")
        nGrams = read.delim("/Users/alejandro/Desktop/ngrams.txt", header = F, 
                            sep = "\t") %>% unname() %>% unfactor() %>% 
                matrix(ncol = 2, byrow = T) %>% as.data.frame()
        
        nGrams = data.frame(dat1[, 1] %>% as.character(), 
                            nGrams[-nrow(nGrams), 1] %>% unlist(),
                            nGrams[-nrow(nGrams), 2] %>% unlist())
        colnames(nGrams) = c("first", "second", "Freq")
        nGrams$first = nGrams$first %>% unfactor()
        nGrams$second = nGrams$second %>% unfactor()
        str(nGrams)
        nGrams
}

i = 1
fileName = paste0(data.folder, myFiles[i])
outFile = paste0(data.folder, myFiles[i], ".csv")

newFile = myReadFile(fileName, outFile)
head(newFile)
str(newFile)
write.csv(newFile, outFile, row.names = F)
