Judge <- sapply(m3m, getfirst, txt = myfile, label = 'Panel of Judges')
Judge <- unique(Judge[Judge < Inf])
j.page <- sapply(Judge, getfirst, txt = myfile, label = 'Page')
sink('w3m_judges.txt')
for (i in 1:length(Judge)) {
  m3m.info <- paste(myfile[Judge[i]:j.page[i]], collapse = '\n')
  m3m.info <- gsub(',+', ',', m3m.info)
  cat(m3m.info)
}
sink()

judge.info <- scan(file = 'w3m_judges.txt', what = character(), sep = '\n')
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
write.table(judges, file = 'w3m_judges.csv', sep = ',', row.names = FALSE)