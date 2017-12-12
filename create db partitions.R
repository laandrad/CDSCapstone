# load database
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/"
db = readRDS(paste0(data.folder, "database4grams.Rdata"))

nPartitions = 100
totalDFS = db %>% length()
partLength = round(totalDFS / nPartitions)
parts = gl(n = nPartitions, k = partLength, length = totalDFS)

library(progress)
pb = progress_bar$new(total = nPartitions)
for (i in 1:nPartitions) {
        saveRDS(db[parts == as.character(i)], 
                paste0(data.folder, "db_", i, ".Rdata"))
        pb$tick()
}

db1 = readRDS(paste0(data.folder, "db_", 1, ".Rdata"))
db2 = readRDS(paste0(data.folder, "db_", 2, ".Rdata"))
db3 = c(db1, db2)
