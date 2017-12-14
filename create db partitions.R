library(progress)
library(dplyr)
library(data.table)

# load database
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/nGramData/"
db4 = read.csv(paste0(data.folder, "tetraGramsFreqFinal.csv"), 
               row.names = 1, stringsAsFactors = F)
colnames(db4) = c("first", "second", "third", "fourth", "freq")
head(db4)

str(db4)
db4 = db4 %>% filter(freq > 1)
db4$third = db4$third %>% as.character()
db4 = split(db4, db4$third %>% as.factor(), drop = T)

nPartitions = 100
totalDFS = db4 %>% length()
partLength = round(totalDFS / nPartitions)
parts = gl(n = nPartitions, k = partLength, length = totalDFS)

data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/appData/"

pb = progress_bar$new(total = nPartitions)
for (i in 1:nPartitions) {
        saveRDS(db4[parts == as.character(i)], 
                paste0(data.folder, "db_", i, ".Rdata"))
        pb$tick()
}

db1 = readRDS(paste0(data.folder, "db_", 1, ".Rdata"))
db2 = readRDS(paste0(data.folder, "db_", 2, ".Rdata"))
db3 = c(db1, db2)
