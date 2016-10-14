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

getlatlon <- function(add, polyg) {
  json <- geocode(add, output = 'all', sensor = FALSE)
  if(json[[2]] != 'OK'){
    return(c(NA, NA))
  }
  for(i in 1:length(json[[1]])){
    js <- json[[1]][[i]]
    city <- js$formatted_address
    loc <- c(js$geometry$location$lng, js$geometry$location$lat)
    if(pinpoly(loc, polyg)) return(loc)
  }
  return(c(NA, NA))
}

maybe <- which(!geo.intown & (geo$lat < 41 | geo$lat > 41.5) & (geo$lon < -73.2 | geo$lon > -72.6))
locations <- read.csv(file = 'address_lat_long.csv', as.is = TRUE)
locs <- maybe
locations <- locations[locs, ]
locations[, 2] <- paste(locations[, 2], ' ,New Haven, CT', sep = '')
results <- sapply(locations[, 2], getlatlon)
