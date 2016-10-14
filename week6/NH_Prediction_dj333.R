# Case Studies
# Thutsday September 29
# Written by Dingjue Ji

# Load the data you trust and reduce it to "residential" properties:
x <- read.csv("NH_training.csv", as.is = TRUE)
test <- read.csv('NH_test.csv', as.is = TRUE)
trset <- 1:nrow(x)
teset <- (1:nrow(test)) + nrow(x)
alldat <- rbind(x, test)
for(i in c('style', 'model', 'neighborhood', 'grade', 'zone')) {
  alldat[, i] <- factor(alldat[, i])
}
## PRELIMINARY WORK:

# You probably want something different here for selecting your
# set of properties for modeling and prediction:
dim(x)
table(x$model)

alldat$logval <- log(alldat$totval + 1)
alldat$logsqft <- log(alldat$sqft + 1)
alldat$newacres <- (alldat$acres)^(1/4)


mygreatlm <- lm(logval ~ logsqft + style + model + newacres +
                  grade + bedrooms + finalbaths + zone + pctgood, data = alldat[trset, ])

## END PRELIMINARY WORK

for(i in c('estval', 'statistic', 'p.value', 'conf.int.L', 'conf.int.U')) assign(i, rep(NA, 27307))
estval[x$pid] <- exp(as.numeric(predict(mygreatlm)))
statistic[x$pid] <- (estval[x$pid] - x$totval) / sd(x$totval)
p.value[x$pid] <- pnorm(statistic[x$pid])
conf.int.L[x$pid] <- estval[x$pid] - 2 * sd(x$totval)
conf.int.U[x$pid] <- estval[x$pid] + 2 * sd(x$totval)

################################################################################
# The last thing you need to do (a freebie from us), which will only attempt
# predictions for pid values from your reduced residential data set x:
zzz <- data.frame(pid=1:27307, estval=NA, statistic=NA,
                  p.value=NA, conf.int.L=NA, conf.int.U=NA)
for(i in names(zzz)) zzz[, i] <- get(i)

write.table(zzz, file = 'NH_Prediction_dj333.csv', sep = ',', row.names = FALSE)

### dj333 ### Prediction for test set

# May not work because there will be new levels of neighborhood

zzz <- as.data.frame(matrix(NA, nrow = nrow(test), ncol = 5))
colnames(zzz) <- c('estval', 'statistic', 'p.value', 'conf.int.L', 'conf.int.R')
zzz$estval <- exp(as.numeric(predict(mygreatlm, alldat[teset, ])))
zzz$statistic <- (zzz$estval - test$totval) / sd(test$totval)
zzz$p.value <- pnorm(zzz$statistic)
zzz$conf.int.L <- zzz$estval - 1.96 * sd(test$totval)
zzz$conf.int.R <- zzz$estval + 1.96 * sd(test$totval)
write.table(zzz, file = 'NH_Prediction_8.csv', sep = ',', row.names = FALSE)

