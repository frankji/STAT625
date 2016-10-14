nh <- read.csv('NH_Merged/NH_dj333.csv', as.is = TRUE)

# You probably want something different here for selecting your
# set of properties for modeling and prediction:
#nh <- nh[!is.na(nh$model) & nh$model!="Multi-Fam 5-12",]

fam <- c('Multi-Fam 2-4', 'Multi-Fam 5-12',
         'Res Condo', 'Single Family')
fam <- which(nh$model %in% fam)
nh <- nh[fam, ]
apply(nh, 2, x/sum(x))
ebys <- nh$totval/nh$sqft
nh$ebys <- ebys
d <- function(x, y) sqrt(sum((x - y)^2))
ds <- function(x, mat) as.numeric(apply(mat, 1, d, y = x))

test <- c(41.25467, -72.8913)
myeval <- function(x, value) {
  dis <- ds(x, nh[, c(43, 44)])
  top10 <- as.numeric(quantile(dis, 0.1))
  mysample <- sample(ebys[which(dis <= top10)], 100000, replace = TRUE)
  sum(mysample >= value)/100000
}

system.time(myeval(c(41.25442, -72.89126), 155400/864))

p.values <- apply(nh, 1, function(x) myeval(x[1:2], x[3]))