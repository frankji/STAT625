#
# www.visionappraisal.com
#
# Example: 30 Lincoln St, New Haven, CT 06511
setwd('~/Courses/STAT625/week1/RealEstate/')
baseurl <- 'http://gis.vgsi.com/newhavenct/Parcel.aspx?pid=XXX'
these <- 600:699     # Full set of queries (do not run!) 1:27309
options(show.error.messages = TRUE)
bad<-c()
for (i in these) {
  cat(i, "\n")
  url <- gsub('XXX', i, baseurl)
  x<-try(scan(url, what="", sep="\n"), silent = TRUE)
  if(inherits(x, "try-error")){
    bad<-c(bad, i)
    print(paste('errors in No.', as.character(i)))
  }
  else{
    write(x, file.path('newdata', paste(i, '.html', sep="")))
  }
}

# The block of code above may or may not fail on file 602.  It
# doesn't work for me, but it has worked for students in the past
# (perhaps Windows is more robust to a "file not found" sort of thing
# when querying the web).  Don't worry about it.
#
# Challenge: fix up the code above so it doesn't die upon getting the
# BAD_REQUEST response.  Use try() and read the help pages.  Collect
# the parcel IDs which failed.  HINT: 602 fails.  In the range from
# 600:699, which other(s) failed?  Try to find this/these yourself
# on visionappraisal.com.
#
# Goal: I'm not interested in the answer.  And I don't want you to
# "scrape" all of New Haven yourself (I'll give you the files).
# But I want you to understand every single line of code, above,
# so that you can do this sort of thing on your own.
#
################################################################################
# Towards Homework 1.  I'm going to try to extract the year built from a single
# file.  Obviously this works, below.  Maybe it will work for all properties,
# in which case the code below could be cleaned up, simplified, and used in
# a loop over all properties.

x <- scan("newdata/600.html", what="", sep="\n")

thisline <- grep("MainContent_ctl01_lblYearBuilt", x)
thisline

x[thisline]

temp <- gsub("<[^<>]*>", "", x[thisline])
temp
as.numeric(temp)

