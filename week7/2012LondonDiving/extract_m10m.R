myfile <- scan(file = '2012WorldCupLondon.csv', what = character(), sep = '\n')

getfirst <- function(txt, label, t) {
  first <- grep(label, txt, perl = TRUE)
  min(first[first - t > 0])
}
cap <- function(x) gsub("([A-Z])([A-Za-z]+)", "\\1\\L\\2", x, perl=TRUE)
m3m <- grep("Men's 10m Platform", myfile, perl = TRUE)
Event <- 'M10mPF'
rounds <- c('Preliminary', 'Semifinal', 'Final')
dives <- 6
Judge <- sapply(m3m, getfirst, txt = myfile, label = 'Panel of Judges')
Judge <- unique(Judge[Judge < Inf])
j.page <- sapply(Judge, getfirst, txt = myfile, label = 'Page')
sink('M10mPF_judges.txt')
for (i in 1:length(Judge)) {
  m3m.info <- paste(myfile[Judge[i]:j.page[i]], collapse = '\n')
  m3m.info <- gsub(',+', ',', m3m.info)
  cat(m3m.info)
}
sink()

judge.info <- scan(file = 'M10mPF_judges.txt', what = character(), sep = '\n')
pages <- grep('Page', judge.info, value = TRUE)
pages <- gsub(".+(Page [0-9]+ of [0-9]+).*", "\\1", pages)
pages <- sapply(pages, function(x) strsplit(x, split = '\\s')[[1]])
breaks <- which(as.numeric(pages[4, ]) == 1)
starts <- grep('Page', judge.info)[breaks]
tables <- list()
temp <- grep('Judge [0-9]+', judge.info)
for(i in 1:(length(starts)-1)){
  selected <- temp[temp > starts[i] & temp < starts[i+1]]
  if (length(selected) > 0) {
    tables[[i]] <- judge.info[selected]
  } else tables[[i]] <- NA
}

judges.names <- unlist(tables[1:3])
judges.names <- gsub('Judge [0-9]+\\s+(.+),$', '\\1', judges.names)
judges.n <- sapply(judges.names, function(x) strsplit(x, split = ' ')[[1]])
judges.n <- lapply(judges.n, function(x) gsub("([A-Za-z])([A-Za-z]+)", "\\U\\1\\E\\L\\2", x, perl=TRUE))
judges.n <- lapply(judges.n, function(x) grep(paste(c('Judge', x[1:2]), collapse = '.+'), myfile, perl = TRUE))
judges.c <- lapply(judges.n, function(x) gsub('.+,+([A-Z]+),+FINA.+', '\\1', myfile[x]))
judges.s <- lapply(judges.n, function(x) gsub('.+,+([MF]),+.+', '\\1', myfile[x]))
judges.c <- unlist(lapply(judges.c, function(x) ifelse(length(x) == 0, NA, x)))
judges.s <- unlist(lapply(judges.s, function(x) ifelse(length(x) == 0, NA, x)))
rd <- c(rep('Preliminary', 14), rep('Semifinal', 7), rep('Final', 7))
jno <- c(1:14, 1:7, 1:7)
judges <- cbind(names(judges.n), unlist(judges.c), unlist(judges.s), rd, jno)
colnames(judges) <- c('Judge', 'Jcountry', 'JGender', 'Round' ,'Jno')
write.table(judges, file = 'M10mPF_judges.csv', sep = ',', row.names = FALSE)
###########################################################################

Judges <- read.csv('M10mPF_judges.csv', as.is = TRUE)
sink("M10mPF_divers.txt")
n.page <- sapply(m3m, getfirst, txt = myfile, label = 'Page')
for(i in 1:length(m3m)){
  m3m.info <- paste(myfile[m3m[i]:n.page[i]], collapse = '\n')
  m3m.info <- gsub(',+', ',', m3m.info)
  cat(m3m.info)
}
sink()

m3m.info <- scan(file = 'M10mPF_divers.txt', what = character(), sep = '\n')
getlen <- function(x) length(strsplit(x, split = ',', perl = TRUE)[[1]])
test <- as.numeric(sapply(m3m.info, getlen))
data.rows <- which(test == max(test))
gap <- which(data.rows[2:length(data.rows)] - data.rows[1:length(data.rows)-1] > 1)
m3m.round <- rep(NA, length(data.rows))
m3m.round[1:gap[1]] <- 'Preliminary'
m3m.round[(gap[1] + 1):gap[2]] <- 'Semi'
m3m.round[(gap[2] + 1):length(m3m.round)] <- 'Final'
m3m.info <- as.matrix(sapply(m3m.info[data.rows], function(x) strsplit(x, split = ',')[[1]]))
m3m.info <- t(m3m.info)
rownames(m3m.info) <- 1:nrow(m3m.info)
m3m.info <- cbind(m3m.info, m3m.round)
divers <- m3m.info[m3m.round == 'Preliminary', 2]
divers.first <- sapply(divers, function(x) strsplit(x, split = '\\s')[[1]][1])
divers.first.cap <- cbind(divers.first, cap(divers.first))
divers.first.cap <- apply(divers.first.cap, 1, function(x) paste(c('(', x[1], '|', x[2], ')'), collapse = ''))
divers.first.cap <- gsub('^(.+)$', '[ ,]\\1[ ,]', divers.first.cap)
divers.lst <- gsub('.+ ([^ ]+)$', '\\1', divers)
divers.lst.cap <- cbind(divers.lst, toupper(divers.lst))
divers.lst.cap <- apply(divers.lst.cap, 1, function(x) paste(c('(', x[1], '|', x[2], ')'), collapse = ''))
divers.lst.cap <- gsub('^(.+)$', '[ ,]\\1[ ,]', divers.lst.cap)
divers.pat <- paste(divers.first.cap, collapse = '|')

start <- min(sapply(m3m, getfirst, txt = myfile, label = 'Detailed Results'))
details <- grep('Detailed Results', myfile, perl = TRUE)
details <- details[details >= start]
pos <- unique(sapply(details, getfirst, txt = myfile, label = divers.pat)) - 5
n.page <- sapply(pos, getfirst, txt = myfile, label = 'Page')
sink('M10mPF.txt')
for(i in 1:length(pos)){
  page.s <- ifelse(length(grep('Page', myfile[pos[i]], perl = TRUE)) == 1, pos[i] + 1, pos[i])
  m3m.info <- paste(myfile[page.s:n.page[i]], collapse = '\n')
  m3m.info <- gsub(',+', ',', m3m.info, perl = TRUE)
  m3m.info <- gsub('<ca>|"', '', m3m.info, perl = TRUE)
  m3m.info <- gsub('(Page [0-9]+ of [0-9]+,)', '\\1\n', m3m.info)
  if(length(grep(paste(divers.lst.cap, collapse = '|'), myfile[pos[i]+5:9], perl = TRUE)) > 0)
    cat(m3m.info)
}
sink()

m3m.info <- scan(file = 'M10mPF.txt', what = character(), sep = '\n')
pages <- grep('Page', m3m.info, value = TRUE)
pages <- gsub(".+(Page [0-9]+ of [0-9]+).*", "\\1", pages)
pages <- sapply(pages, function(x) strsplit(x, split = '\\s')[[1]])
breaks <- which(as.numeric(pages[2, ]) / as.numeric(pages[4, ]) == 1)
starts <- c(1, grep('Page', m3m.info)[breaks])
following <- '.*[A-Z]{3},[0-9]+[A-Z]+,[0-9\\.]+,[0-9\\.]+,'
first.pat <- paste(paste(divers.first.cap, following, sep = ''), collapse = '|')
temp <- grep(first.pat, m3m.info, perl = TRUE)
tables <- list()
for(i in 1:(length(starts)-1)){
  selected <- temp[temp > starts[i] & temp < starts[i+1]]
  if (length(selected) > 0) {
    tables[[i]] <- m3m.info[as.vector(sapply(selected, function(x) seq(x, x+dives-1)))]
  } else tables[[i]] <- NA
}

gettab <- function(x, dives) {
  x <- gsub('[^A-Za-z0-9\\., ]', '', x)
  if (length(grep('([0-9]+[A-Za-z]+,){2,}', x)) == 1) {
    dat <- strsplit(x, split = ',')[[1]]
    diver <- dat[1:3]
    lst <- grep('^[^0-9\\.]+$', dat[-c(1, 2, 3)])
    diver[2] <- trimws(paste(diver[2], lst))
    tab <- matrix(grep('^[0-9\\.]+$', dat[-c(1, 2, 3, lst + 3)], value = TRUE), nrow = dives)
    Difficulty <- rep(tab[, 1], each = 7)
  }
  else{
    dat <- strsplit(x, split = '[0-9]+[A-Z]')[[1]]
    dat <- gsub('^,+|,+$', "", dat)
    diver <- strsplit(dat[1], split = ',')[[1]][1:3]
    tab <- t(sapply(dat[-1], function(x) strsplit(x, split = ',')[[1]][1:9]))
    lst <- grep('[A-Za-z]+', dat[-1], perl = TRUE)
    if(length(lst) == 1) {
      lst <- gsub('.+,([A-Za-z]+)$', '\\1', dat[lst + 1])
      diver[2] <- trimws(paste(diver[2], lst))
      dat[-1] <- gsub('[A-Za-z]+', '', dat[-1])
    }
    Difficulty <- rep(gsub('^([0-9\\.]+).+', '\\1', dat[-1]), each = 7)
  }
  JScore <- as.vector(t(tab[, 2:8]))
  diver <- as.data.frame(t(matrix(rep(diver, dives*7), nrow = 3)), stringsAsFactors = FALSE)
  names(diver) <- c('Rank', 'Diver', 'Country')
  DiveNo <- rep(1:dives, each = 7)
  diver <- cbind(diver, DiveNo, JScore, Difficulty)
  return(diver)
}

#Something's wrong with the last one of Final
B <- grep('[0-9]+,[A-Za-z ]+,[A-Z]{3},[0-9]+[A-Z]$', m3m.info)
if(length(B) > 0) {
  B.index <- sapply(B, function(x) max(which(starts - x < 0)))
  B.ends <- getfirst(txt = m3m.info, t = B, label = '[^0-9 ,]{2,}') - 1
  for(z in 1:length(B.index)) 
    tables[[B.index[z]]] <- c(tables[[B.index[z]]], paste(m3m.info[B[z]:B.ends[z]], collapse = ','))
}
#######################################################

alltabs <- list()
tables <- tables[sapply(tables, length) > 1]
for (i in 1:3) {
  judges <- Judges[Judges$Round == rounds[i], ]
  temp.table <- paste(tables[[i]], collapse = ',')
  temp.table <- gsub('\"', '', temp.table, perl = TRUE)
  temp.table <- gsub(',+', ',', temp.table, perl = TRUE)
  temp.table <- gsub(",([0-9]+,[A-Za-z ]+)", "\n\\1", temp.table)
  temp.table <- strsplit(temp.table, split = '\n')[[1]]
  temp.table <- do.call(rbind, lapply(temp.table, gettab, dives = dives))
  preornot <- rep(rounds[i] == 'Preliminary', nrow(temp.table))
  panel <- ifelse(preornot, temp.table$DiveNo %in% c(1, 2, 3), rep(TRUE, nrow(temp.table)))
  temp.table$Judge[panel] <- rep(judges[1:7, 1], sum(panel)/7)
  temp.table$JCountry[panel] <- rep(judges[1:7, 2], sum(panel)/7)
  if (rounds[i] == 'Preliminary') {
    temp.table$Judge[!panel] <- rep(judges[8:14, 1], sum(!panel)/7)
    temp.table$JCountry[!panel] <- rep(judges[8:14, 2], sum(!panel)/7)
  }
  temp.table$Round <- rounds[i]
  temp.table$Event <- Event
  alltabs[[i]] <- temp.table
}

finaltab <- do.call(rbind, alltabs)
cols <- c('Event', 'Round', 'Diver', 'Country', 'Rank', 'DiveNo', 'Difficulty', 'JScore', 'Judge', 'JCountry')
finaltab <- finaltab[, cols]
write.table(finaltab, file = 'M10mPF.csv', sep = ',', row.names = FALSE)