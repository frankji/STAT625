# Case Studies
# Thursday September 29
# Reviewed by Dingjue Ji dj333
# Load the data you trust and reduce it to "residential" properties:
x <- read.csv("NH_training.csv", as.is=TRUE)
## PRELIMINARY WORK:
hist(log(x$totval))
# Linear model -- did not include style or grade - number of coefficients
# was less than the rank when one or both of these variables were included
### dj333 ### It might be better if you do not name the object with an existed function name.
lm <- lm(totval ~ sqft + bedrooms + finalbaths + pctgood + yearbuilt + 
		occupancy + acres + model, data=x)  
summary(lm)
IsItFair <- function(x, pid, verbose=FALSE) {
  i <- which(x$pid==pid) ### dj333 ### spacing, 'x$pid == pid'
  if (length(i)==0) return(NA) 
  fileurl <- gsub("XXX", pid,
                  file.path("~/Desktop/stat625/week3/newdata2016/XXX.html"))
  totval <- x$totval[i]
  if (verbose) cat(pid, "Browser: ", fileurl, "\n")
  
  estval <- as.numeric(predict(lm, newdata=x[i,, drop=FALSE]))
  statistic <- (estval - totval) / sd(x$totval)
  p.value <- pnorm(statistic)
  conf.int <- estval + c(-1.96, 1.96) * sd(x$totval)
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
# Examining the distributions of totval and predicted value
par(mfrow=c(2,1))
hist(predict(lm))
hist(x$totval)
# Sanity Check -- run through a couple of numbers -- see if the p-values
# are consistent with the estval and totvals
IsItFair(x, 18000)
IsItFair(x, 1700, verbose=TRUE)
################################################################################
# The last thing you need to do (a freebie from us), which will only attempt
# predictions for pid values from your reduced residential data set x:
zzz <- data.frame(pid=1:27307, estval=NA, statistic=NA,
                  p.value=NA, conf.int.L=NA, conf.int.U=NA)
### dj333 ### It is not computationally efficient
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
# Only include the rows from our "residential" dataset
zzz <- zzz[!is.na(zzz$estval),]
write.csv(zzz, "NH_Prediction_8.csv", row.names=FALSE)

### dj333 ### Using the model to do prediction on test set
### dj333 ### This part is added by dj333
test <- read.csv('NH_test.csv', as.is = TRUE)
zzz <- as.data.frame(matrix(NA, nrow = nrow(test), ncol = 5))
colnames(zzz) <- c('estval', 'statistic', 'p.value', 'conf.int.L', 'conf.int.R')
zzz$estval <- as.numeric(predict(lm, test))
zzz$statistic <- (zzz$estval - test$totval) / sd(test$totval)
zzz$p.value <- pnorm(zzz$statistic)
zzz$conf.int.L <- zzz$estval - 1.96 * sd(test$totval)
zzz$conf.int.R <- zzz$estval + 1.96 * sd(test$totval)
write.table(zzz, file = 'NH_Prediction_8.csv', sep = ',', row.names = FALSE)