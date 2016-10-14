# Case Studies
# Thutsday September 29
#

# Load the data you trust and reduce it to "residential" properties:
x <- read.csv("NH_Merged/NH_dj333.csv", as.is=TRUE)

# You probably want something different here for selecting your
# set of properties for modeling and prediction:
x <- x[!is.na(x$model) & x$model!="Multi-Fam 5-12",]
dim(x)
table(x$model)

#x$bedrooms <- (x$bedrooms - mean(x$bedrooms, na.rm = TRUE)) / sd(x$bedrooms, na.rm = TRUE)
#x$finalbath <- (x$finalbath - mean(x$finalbath, na.rm = TRUE)) / sd(x$bedrooms, na.rm = TRUE)
# This really is not so great on many levels:

# Distance Calculation
d <- function(x, y) sqrt(sum((x - y)^2))
ds <- function(x, mat) as.numeric(apply(mat, 1, d, y = x))

dis <- ds(x, x[, c(43, 44)])
top10 <- as.numeric(quantile(dis, 0.1))
mygreatlm <- lm(totval ~ sqft + bedrooms + finalbath + latitude + longitude , data=x)
mygreatlm <- lm(totval ~ sqft, data=x)
## END PRELIMINARY WORK:

# Build upon this function:
IsItFair <- function(x, pid) {
  i <- which(x$pid==pid)
  if (length(i)==0) return(NA)

  # To make it easier to look at the source, you could do this.
  # You will have to modify this for your computer.
  fileurl <- gsub("XXX", pid,
                  file.path("/Users/Frank/Courses/STAT625/week1/HW1/VisionAppraisal/newdata2016/XXX.html"))
  totval <- x$totval[i]
  cat(pid, "Browser: ", fileurl, "\n")
  
  ## YOUR LOCAL WORK STARTS HERE.  The following code is just to get some
  ## numbers into place and would be pathological on many levels.
  ## You can do anything you want to get some estval and do any type
  ## of computation for statistic and pvalue.  The only guideline is
  ## that if your pvalue isn't a p-value, it should have the scale
  ## interpretation that 0 = Grossly Unfair and 1 = No Problem!
  ##
  ## Let's all agree that we'll live in a one-sided world here,
  ## where by "fair" we're thinking "not too high".
  ## So my p.value below is wrong in many ways and you should
  ## consider it very carefully.  And conf.int should be a
  ## traditional two-sided 95% confidence interval.
  ## You can do something fancier than I do with mygreatlm at your
  ## own risk, but the structure of the return should remain unchanged
  ## unless Susan and I agree on a change.

  location <- x[i, c(43, 44)]
  dis <- ds(location, x[, c(43, 44)])
  top10 <- as.numeric(quantile(dis, 0.1))
  mygreatlm <- lm(totval ~ sqft + bedrooms + finalbath, data=x[which(dis <= top10), ])
  estval <- as.numeric(predict(mygreatlm, newdata=x[i,, drop=FALSE]))
  statistic <- (estval - totval) / sd(x$totval)
  p.value <- 2*pnorm(-abs(statistic))
  conf.int <- estval + c(-2, 2) * sd(x$totval)

  ## END YOUR WORK
  
  return(list(pid=x$pid[i],
              totval=x$totval[i],
              estval=estval,
              statistic=statistic,
              p.value=p.value,
              conf.int=conf.int))
}

# To use this, x must be your reduced data frame of residential
# properties, where you are asking it to focus on the parcel id.
# So the second argument is NOT a row number (unless you are doing
# predictions for the entire city of PIDs).

IsItFair(x, 12345)
