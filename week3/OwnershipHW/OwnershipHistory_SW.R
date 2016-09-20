files <- dir("samplefiles", full.names = TRUE)

z <- as.data.frame(matrix(NA, length(files), 15))
names(z) <- paste(rep(c("buyer", "price", "date"), 5), rep(1:5, each=3), sep="")

for (i in 1:length(files)) {
  x <- readLines(files[i])
  
  m <- grep("Ownership History", x)[1]
  startEnd <- m+grep("<(/)*table", x[m:length(x)])[1:2]-1
  tableRows <- x[startEnd[1]:startEnd[2]]
  tableRows <- tableRows[grep("$", tableRows, fixed=TRUE)]
  tableRows <- strsplit(tableRows, "</td>")
  numCols <- length(tableRows[[1]])
  numOwners <- length(tableRows) # we're only tracking the latest 5, max
  tableRows <- gsub("<[^<>]*>|\t|\\$|,", "", unlist(tableRows))
  histdata <- matrix(tableRows, ncol=numCols, byrow=TRUE)
  histdata <- histdata[1:min(5, numOwners), c(1:2, numCols), drop=FALSE]
  
  z[i, 1:length(histdata)] <- as.vector(histdata)
}

