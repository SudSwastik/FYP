library(dplyr)
path <- "C:/Users/Saurabh/Desktop/Data"
fs <- list.files(path, pattern = glob2rx("*.csv"))
for (f in fs) {
  fname <- file.path(path, f)             ## current file name
  df <- read.csv(fname)                   ## read file
  df <- select(df,-1)                     ## delete column B
  write.csv(df, fname, row.names = FALSE) ## write it out
}
