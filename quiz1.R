
data.folder = "/Users/alejandro/Coursera Data Science Capstone Data/en_US/"
myObjects = list.files(data.folder, pattern = ".rds")

twitter = readRDS(paste0(data.folder, myObjects[[6]]))

loves = sapply(1:length(twitter), 
               function(i) twitter[[i]] %>% 
                       unlist %>% (function(x) any(x %in% "love"))
)

hates = sapply(1:length(twitter), 
               function(i) twitter[[i]] %>% 
                       unlist %>% (function(x) any(x %in% "hate"))
)

sum(loves) / sum(hates)

biostats = sapply(1:length(twitter), 
                  function(i) twitter[[i]] %>% 
                          unlist %>% (function(x) any(x %in% "biostats"))
)

twitter[[which(biostats == TRUE)]]


target = "a computer once beat me at chess but it was no match for me at kickboxing"

targetTwits = sapply(1:length(twitter), 
                     function(i) twitter[[i]] %>% 
                             unlist %>% paste(collapse = " ") == target
)

sum(targetTwits)

