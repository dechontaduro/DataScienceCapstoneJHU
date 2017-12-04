#loadNgramsPredict <- function(){

ngramMax <- 5
ngramMin <- 1
sampleFactor <- 0.1

all.sample <- "./data/all.txt"
files <- c("all" = all.sample)

ngramsPred <- 
lapply(names(files), function(x){
  print(paste(x, "load GramDiscountLeftOverProb ngrams"))
  
  filen <- paste("",files[x], sep = "")
  file.sample <- paste(filen, "sample", sampleFactor, sep = ".")
  
  file.dt <- list()
  file.dt.lop <- list()
  for(i in ngramMin:ngramMax){
    time.start <- Sys.time()
    print(paste(x, "loading GramDiscountLeftOverProb ngrams:", i))
    file.dt[[i]] <- readRDS(paste(file.sample, i, "frecdis.rds", sep = "_"))
    file.dt.lop[[i]] <- readRDS(paste(file.sample, i, "frecdislop.rds", sep = "_"))
    
    time.end <- Sys.time()
    print(time.end - time.start)
  }
  
  return (list("dis" = file.dt, "lop" = file.dt.lop))
})

names(ngramsPred) <- names(files)

#  return(ngramsPred)  
#}