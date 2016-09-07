#
# Sanity checking of initial New Haven hw1.csv files
#

files <- dir("CV2dropbox625", full.names = TRUE)

z <- list()
for (file in files) {
  x <- read.csv(file, as.is=TRUE)
  z[[file]] <- x
  cat(file, nrow(x), ncol(x), "\n")
}

# An example: the third file seems odd... a different number of rows
# according to Excel from what R reports?
x <- z[[3]]
dim(x)
tail(x)   # The end looks okay
anyNA(x$pid)
table(table(x$pid))             # Think about this
table(x$pid)[table(x$pid)>1]    # Useful
is.character(x$pid)             # Oops, want it numeric!
x[x$pid=="c(NA",]               # hmm....



