#
# Flight Delay Exploration
#
# http://stat-computing.org/dataexpo/2009/the-data.html

##
## Aside: the following code was used to create a subset of the
## data for STAT 230/530.  I include the code for completeness.
## The original 1995 full data could be downloaded, above, but
## are too large (about 500MB) to be a good idea for us right now!
## So please don't try to run the following code; if you want to
## do something, start with the United Airlines subset that I
## provide and begin to explore.
##
setwd('/Users/Frank/Courses/stat530/week1/FlightDelays/')
x <- read.csv("UA1995.csv", as.is=TRUE)

dim(x)                    # How big is this data set?
names(x)                  # What are the variable names?
table(x$UniqueCarrier)    # A one-way contingency table of the carriers

# Here, I subset those rows of the data set corresponding to United
# Airlines (UA) AND having a non-missing value in DepDelay.  If you
# have used R before and are familiar with the subset() command, you
# might be in the wrong class.  Consider STAT 361 next semester.
# Please do not use subset() prior to the Fall Break.  After the Fall
# Break, if you really like it and know what you are doing, fine.

y1 <- x[x$UniqueCarrier=="UA" & !is.na(x$DepDelay),]    # Preferred
y2 <- subset(x, UniqueCarrier=="UA" & !is.na(DepDelay)) # Okay after Fall Break
identical(y1, y2)   # Proving the two lines above are equivalent

# Strongly recommended instead of using read.csv():
write.table(y1, "UA1995.csv", row.names=F, col.names=T, sep=",")

##
## Now starting with United Airlines
##

x <- read.csv("UA1995.csv", as.is=TRUE)

dim(x)
names(x)


