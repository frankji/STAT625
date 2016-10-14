# Case Studies
# Thutsday September 29
#

# Load the data you trust and reduce it to "residential" properties:
x <- read.csv("NH_Merged_Oct4.csv", as.is = TRUE)

## PRELIMINARY WORK:

# You probably want something different here for selecting your
# set of properties for modeling and prediction:
fam <- c('Multi-Fam 2-4', 'Multi-Fam 5-12',
         'Res Condo', 'Single Family')
fam <- which(x$model %in% fam)
x <- x[fam, ]
x <- x[which(!x$multibuilding), ]
dim(x)
table(x$model)

# This really is not so great on many levels:
library(ranger)
x <- model.frame(totval ~ sqft + pctgood + style + model + acres + neighborhood +
                     grade + bedrooms + finalbaths + latitude + longitude + actype +
                     kstyle + occupancy + bathstyle + zone + pid,
                    data=x, na.action = na.omit)
dat <- x[, names(x) != 'pid']
for(i in c('style', 'model', 'neighborhood', 'actype', 'kstyle', 'bathstyle', 'zone', 'grade')){
  dat[, i] <- as.factor(dat[, i])
}
mygreatlm <- lm(totval ~ ., data = dat)
# Evaluation
losses <- c()
tests <- cut(sample(1:nrow(dat)), breaks = 10, labels = FALSE)
for(i in 1:10){
  test <- which(tests == i)
  training <- which(tests != i)
  mymdl <- ranger(totval ~ ., data = dat[training, ], write.forest = TRUE, importance = 'impurity')
  ans <- predict(mymdl, data = dat[test,, drop = FALSE])$predictions
  loss <- sqrt(mean((dat[test, 'totval'] - ans)^2))
  losses <- c(losses, loss)
}
meanloss <- mean(losses)
## END PRELIMINARY WORK

# Build upon this function:
IsItFair <- function(x, pid, verbose=FALSE) {
  i <- which(x$pid==pid)
  if (length(i)==0) return(NA)

  # To make it easier to look at the source, you could do this.
  # You will have to modify this for your computer.
  fileurl <- gsub("XXX", pid,
                  file.path("~/Courses/STAT625/week1/HW1/VisionAppraisal/newdata2016/XXX.html"))
  totval <- x$totval[i]
  if (verbose) cat(pid, "Browser: ", fileurl, "\n")
  
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
  
  estval <- as.numeric(predict(mygreatlm, newdata=x[i,, drop=FALSE]))
  statistic <- (estval - totval) / sd(x$totval)
  p.value <- pnorm(statistic, lower.tail = FALSE)
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

IsItFair(x, 3)
IsItFair(x, 3, verbose=TRUE)

################################################################################
# The last thing you need to do (a freebie from us), which will only attempt
# predictions for pid values from your reduced residential data set x:

zzz <- data.frame(pid=1:27307, estval=NA, statistic=NA,
                  p.value=NA, conf.int.L=NA, conf.int.U=NA)
for (pid in x$pid) {
  ans <- IsItFair(x, pid)
  if (!is.na(ans$estval)) {
    zzz$estval[pid] <- ans$estval
    zzz$statistic[pid] <- ans$statistic
    zzz$p.value[pid] <- ans$p.value
    if (!anyNA(ans$conf.int)) {
      zzz$conf.int.L[pid] <- ans$conf.int[1]
      zzz$conf.int.U[pid] <- ans$conf.int[2]
    }
  }
}







