# split datasets into train and test partitions
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myObjects = list.files(data.folder, pattern = ".rds")

myObjects = myObjects[seq(2, length(myObjects), 2)]

for (i in 1:length(myObjects)) {
        
        dat = readRDS(paste0(data.folder, myObjects[i]))
        
        set.seed(80537)
        train = rbinom(length(dat), 1, 0.6)
        trainSet = dat[train == 1]
        
        testSet = dat[train == 0]
        remove(dat)
        
        saveRDS(trainSet, paste0(data.folder, "train_", myObjects[i]))
        saveRDS(testSet, paste0(data.folder, "test_", myObjects[i]))
        
}