geo <- read.csv('geocode.csv', as.is = TRUE)
bu <- read.csv('address_lat_long.csv', as.is = TRUE)
address <- bu[, 2]
arc <- read.csv('ArcGISgeocoded.csv', as.is = TRUE)
arc <- arc[, c('pid', 'latitude_ArcGIS', 'longitude_ArcGIS')]
nhshape <- read.csv('shape_newhaven.csv', as.is = TRUE)
nhcshape <- read.csv('shape_newhavencounty.csv', as.is = TRUE)
colnames(nhshape) <- c('lon', 'lat')
colnames(nhcshape) <- c('lon', 'lat')
bu <- bu[, c('pid', 'latitude', 'longitude')]
colnames(bu) <- c('pid', 'lat', 'lon')
colnames(arc) <- c('pid', 'lat', 'lon')


distpoly <- function(x, polyg) {
  temp <- apply(polyg, 1, function(x, y) sqrt(sum((y-x)^2)), y = x)
  return(min(temp, na.rm = TRUE))
}

cleanpoly <- function(polyg) {
  polyg <- as.matrix(polyg)
  n <- nrow(polyg)
  cleanp <- rep(TRUE, n)
  j <- 1
  line0 <- polyg[2, ] - polyg[1, ]
  line0 <- line0/(sqrt(sum(line0^2)))
  for (i in 1:(n-1)) {
    line1 <- polyg[i+1, ] - polyg[i, ]
    line1 <- line1/(sqrt(sum(line1^2)))
    if (as.numeric(abs(line1 %*% line0)) < 0.95) {
      line0 <- line1
      j <- i
    }
    else{
      cleanp[i] <- FALSE
      line0 <- polyg[i+1, ] - polyg[j, ]
    }
  }
  return(polyg[cleanp, ])
}

inrange <- function(nums, k) {
  if (prod(k-range(nums)) < 0) {
    return(TRUE)
  } else return(FALSE)
}

get.intersect <- function(ind, x3, mat) {
  x2 <- mat[ind[2], ]
  x1 <- mat[ind[1], ]
  v12 <- x2 - x1
  x3 <- x1+v12*(x3[2]-x1[2])/v12[2]
  return(x3)
}

svet <- function(x, mat){
  test <- (mat[x[2], 2] - mat[x[1], 2]) * (mat[x[2], 2] - mat[x[3], 2])
  if(test > 0){
    return(TRUE)
  } else return(FALSE)
}

tandem <- function(x, n) {
  n <- length(x)
  if (n <= 1) return(x)
  del <- which((x[c(2:n)] - x[1:n-1]) %% n == 1) + 1
  return(x[-del])
}

pinpoly<-function(x, polyg, verbose = FALSE){
  n <- nrow(polyg)
  edges <- cbind(polyg[1:n, 2], polyg[c(2:n, 1), 2])
  inrg <- apply(edges, 1, inrange, k = x[2])
  if (sum(inrg) == 0) return(FALSE)
  thelines <- cbind(which(inrg), (which(inrg)) %% n + 1)
  inters <- t(apply(thelines, 1, get.intersect, x3 = x, mat = polyg))
  left <- sum(inters[,1] < x[1]) %% 2
  right <- sum(inters[,1] > x[1]) %% 2
  if(verbose){
    coinc <- which(polyg[, 2] == x[2])
    if (sum(coinc) > 0) {
      pvet<-cbind(coinc-1, coinc, coinc+1)
      coinc <- coinc[!apply(pvet, 1, svet, mat = polyg)]
    }
    coinc <- tandem(coinc)
    left <- (left + sum(polyg[coinc, 1] < x[1])) %% 2
    right <- (right + sum(polyg[coinc, 1] > x[1])) %% 2
  }
  if (left*right == 1) {
    return(TRUE)
  } else return(FALSE)
}

nhshape <- cleanpoly(nhshape)
nhcshape <- cleanpoly(nhcshape)
#Geo
geo.intown <- rep(NA, nrow(geo))
geo.notnull <- as.matrix(geo[!is.na(geo[, 2]), c(3, 2)])
geo.intown[!is.na(geo$lat)] <- apply(geo.notnull, 1, pinpoly, polyg = as.matrix(nhshape), verbose = TRUE)
geo.outdist <- apply(geo[which(!geo.intown), c(3,2)], 1, distpoly, polyg = as.matrix(nhshape))
geo.bound <- as.numeric(names(which(geo.outdist < 5E-3)))

geo.incounty <- rep(NA, nrow(geo))
geo.notnull <- as.matrix(geo[!is.na(geo[, 2]), c(3, 2)])
geo.incounty[!is.na(geo$lat)] <- apply(geo.notnull, 1, pinpoly, polyg = as.matrix(nhcshape), verbose = TRUE)

geo.nh <- geo.intown
geo.nh[geo.bound] <- TRUE
geo.ss <- as.numeric(geo.nh) + as.numeric(geo.intown) + as.numeric(geo.incounty)

plot(lat ~ lon, data = rbind(nhshape, nhshape[1, ]), type = 'l')
points(geo[!geo.nh, c(3, 2)], col='red', pch = 2)

#BU
bu.intown <- rep(NA, nrow(bu))
bu.notnull <- as.matrix(bu[!is.na(bu[, 2]), c(3, 2)])
bu.intown[!is.na(bu$lat)] <- apply(bu.notnull, 1, pinpoly, polyg = as.matrix(nhshape), verbose = TRUE)
bu.outdist <- apply(bu[which(!bu.intown), c(3,2)], 1, distpoly, polyg = as.matrix(nhshape))
bu.bound <- as.numeric(names(which(bu.outdist < 5E-3)))

bu.incounty <- rep(NA, nrow(bu))
bu.notnull <- as.matrix(bu[!is.na(bu[, 2]), c(3, 2)])
bu.incounty[!is.na(bu$lat)] <- apply(bu.notnull, 1, pinpoly, polyg = as.matrix(nhcshape), verbose = TRUE)

bu.nh <- bu.intown
bu.nh[bu.bound] <- TRUE
bu.ss <- as.numeric(bu.nh) + as.numeric(bu.intown) + as.numeric(bu.incounty)

#Arc
arc.intown <- rep(NA, nrow(arc))
arc.notnull <- as.matrix(arc[!is.na(arc[, 2]), c(3, 2)])
arc.intown[!is.na(arc$lat)] <- apply(arc.notnull, 1, pinpoly, polyg = as.matrix(nhshape), verbose = TRUE)
arc.outdist <- apply(arc[which(!arc.intown), c(3,2)], 1, distpoly, polyg = as.matrix(nhshape))
arc.bound <- as.numeric(names(which(arc.outdist < 5E-3)))

arc.incounty <- rep(NA, nrow(arc))
arc.notnull <- as.matrix(arc[!is.na(arc[, 2]), c(3, 2)])
arc.incounty[!is.na(arc$lat)] <- apply(arc.notnull, 1, pinpoly, polyg = as.matrix(nhcshape), verbose = TRUE)

arc.nh <- arc.intown
arc.nh[arc.bound] <- TRUE
arc.ss <- as.numeric(arc.nh) + as.numeric(arc.intown) + as.numeric(arc.incounty)

s.scores <- cbind(geo.ss, arc.ss, bu.ss)
s.agree <- apply(s.scores, 1, function(x) sum(x >= 2, na.rm = TRUE) > 1)
all.null <- apply(s.scores, 1, function(x) all(is.na(x)))
s.agree[all.null] <- NA
l.agree <- rep(FALSE, nrow(geo))
l.agree[all.null] <- NA
choice <- rep(NA, nrow(geo))
final.score <- rep(NA, nrow(geo))

for(i in 1:nrow(s.scores)){
  if(!is.na(s.agree[i])){
    final.score[i] <- max(s.scores[i, ], na.rm = TRUE)
  }
  if (s.agree[i] & !is.na(s.agree[i])) {
    final.score[i] <- final.score[i] + 1
    three  <- rbind(geo[i, c(3, 2)], arc[i, c(3, 2)], bu[i, c(3, 2)])
    l.agree[i] <- any(dist(three) < 5E-3, na.rm = TRUE)
    if (l.agree[i]) {
      final.score[i] <- final.score[i] + 1
      choice[i] <- which(dist(three) < 5E-3)[1] %% 2 + 1
    } else {choice[i] <- which(s.scores[i, ] == max(s.scores[i, ], na.rm = TRUE))[1]}
  } 
  if (is.na(s.agree[i])) {
    choice[i] <- NA
  } else {
    choice[i] <- which(s.scores[i, ] == max(s.scores[i, ], na.rm = TRUE))[1]
  }
}

choices <- c('geo', 'arc', 'bu')
latlon <- matrix(NA, nrow = nrow(geo), ncol = 2)
for (i in 1:length(choice)){
  if(!is.na(choice[i])){
    latlon[i, ] <- as.numeric(get(choices[choice[i]])[i, c(3,2)])
  }
}
colnames(latlon) <- c('lon', 'lat')

defect <- as.numeric(which(sapply(1:27307, function(x) s.scores[x, choice[x]]) < 2))
d.add <- paste(address[defect], ' Connecticut', sep = ',')
addresses <- sapply(d.add, getlatlon, polyg = nhcshape)
plot(lat~lon, data= nhshape)
points(latlon, col='red')

nhprop <- read.csv('625_dj333.csv', as.is = TRUE)
nhprop[, 'latitude'] <- latlon[, 2]
nhprop[, 'longitude'] <- latlon[, 1]
nhprop[, 'locquality'] <- final.score
write.table(nhprop, file = 'NH_dj333.csv', row.names = FALSE, sep = ',')

plot(lat ~ lon, data = rbind(nhshape, nhshape[1, ]), type = 'l')
points(geo[!geo.nh, c(3, 2)], col='red', pch = 2)
points(bu[!bu.nh, c(3, 2)], col='blue', pch = 5)
points(arc[!arc.nh, c(3, 2)], col='green', pch = 4)

plot(lat ~ lon, data = rbind(nhcshape, nhcshape[1, ]), type = 'l', lwd = 2, col = 'blue')
points(lat ~ lon, data = rbind(nhshape, nhshape[1, ]), type = 'l', lwd = 2, col = 'red')
points(lat ~ lon, data = latlon, col = 'gold', pch = 20)