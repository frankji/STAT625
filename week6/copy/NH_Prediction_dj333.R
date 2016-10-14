# Case Studies
# Thutsday September 29
# Written by Dingjue Ji

# Load the data you trust and reduce it to "residential" properties:
x <- read.csv("NH_Merged_Oct4.csv", as.is = TRUE)

## PRELIMINARY WORK:

# You probably want something different here for selecting your
# set of properties for modeling and prediction:
dim(x)
table(x$model)

x$logval <- log(x$totval + 1)
x$logsqft <- log(x$sqft + 1)
x$newacres <- (x$acres)^(1/4)

mygreatlm <- lm(logval ~ logsqft + style + model + newacres + neighborhood +
                  grade + bedrooms + finalbaths + zone + pctgood, data = x)

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



