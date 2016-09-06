#
# www.visionappraisal.com
#
# PLEASE do not run this script.  It is unnecessary and creates lots
# of web traffic because there are so many properties.

bad <- NULL
baseurl <- 'http://gis.vgsi.com/newhavenct/Parcel.aspx?pid=XXX'
these <- 1:27309     # Full set of queries (do not run!) 1:27309
for (i in these) {
  cat(i, "\n")
  filename <- file.path('newdata2016', paste(i, '.html', sep=""))
  if (!file.exists(filename)) {
    url <- gsub('XXX', i, baseurl)
    x <- try(scan(url, what="", sep="\n"))
    if (class(x)=="try-error") {
      bad <- c(bad, i)
    } else {
      write(x, filename)
    }
  }
}
dput(bad, "bad.txt")  # One way to save a vector, then use dget()...




