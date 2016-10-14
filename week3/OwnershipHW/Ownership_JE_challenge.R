#
# Ownership History
#
# The "frame" of this script is just to test the "body" of the loop -- the
# example problem is getting the most five recent sale information fields.
# So you'd need some minor adaptations to fit the code into your
# scripts (i.e. replace my "frame" with your "frame") but the work
# itself (denoted clearly below) is the interesting part.

files <- dir('samplefiles', full.names = TRUE)
files <- files[grep('[0-9]+.html$', files)]
z <- as.data.frame(matrix(NA, length(files), 15))
names(z) <- paste(rep(c("buyer", "price", "date"), 5), rep(1:5, each=3), sep="")
getctent <- function(x) gsub("(<[^<>]*>)|(\t)|(\\$)|,", "", x, perl = TRUE)

for (i in 1:length(files)) {
  file <- files[i]
  x <- scan(file, what="", sep="\n", quiet=TRUE)
  # Interesting work starts here:
  start <- grep("Ownership History", x)[1] + 1
  #Catch the column names
  ti <- gsub('<th[^<>]+>', '', trimws(x[start+4]))
  ti <- gsub('&amp;', '&', ti)
  ti <- gsub('</th>', ',', ti)
  ti <- gsub('<br>', ' ', ti)
  ti <- strsplit(ti, split = ',')[[1]]
  temp <- grep("</table>", x)
  end <- min(temp[temp > start])
  #Trim the whitespace characters
  temp <- grep("$", x[start:end], fixed = TRUE, value = TRUE)
  temp <- gsub('&amp;', '&', temp)
  temp <- strsplit(temp, "</td>", fixed = TRUE)
  temp <- lapply(temp, getctent)
  rownum <- length(temp)
  temp <- matrix(unlist(temp), ncol = rownum)
  rownames(temp) <- ti
  temp <- temp[c('Owner', 'Sale Price', 'Sale Date'), 1:min(5, rownum)]
  temp <- c(as.vector(temp), rep(NA, (5-rownum)*3))
  z[i,] <- temp    # Not right because of the row/pid issue, but....
  # End the interesting work.
}

write.table(z, file = 'Ownership_dj333.csv', row.names = FALSE, sep= ',')