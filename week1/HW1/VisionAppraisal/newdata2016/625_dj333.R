files <- grep('([0-9])\\.html', dir(), value = TRUE)
pids <- sort(as.numeric(gsub("([0-9]+)\\.html", "\\1", files)))
null.pids <- sapply(1:27307, function(i) i %in% pids)
init_list <- function(ids){
  mydata <- NA
  if (null.pids[ids]) {
    fname <- paste(ids, "html", sep = '.')
    mydata <- scan(file=fname, what = '', sep = '\n', quiet = TRUE)
  }
  list(i = ids, data = mydata)
}
flist <- lapply(1:27307, init_list)

col.names <- c('pid', 'location','totval', 'address', 
               'yearbuilt', 'sqft', 'replcost', 'pctgood', 
               'style', 'model', 'grade', 'occupancy', 
               'actype', 'bedrooms', 'bathrooms', 'halfbaths', 
               'bathstyle', 'kstyle', 'exval', 'acres', 'zone', 
               'neighborhood', 'landval', 'garagesqft')
saleinfo <- rep(c('buyer', 'price', 'date'), 5)
saleinfo <- apply(cbind(saleinfo, 1:5), 1, function(x) paste(x, collapse = ''))
col.names[25:41] <- c(saleinfo, 'multibuilding', 'nineplus')
col.patterns <- c('', '"MainContent_lblLocation"', '"MainContent_lblGenAppraisal"', 
                  '"MainContent_lblAddr1"', '"MainContent_ctl01_lblYearBuilt"', 
                  '"MainContent_ctl01_lblBldArea"', '"MainContent_ctl01_lblRcn"', 
                  '"MainContent_ctl01_lblPctGood"', '<td>[Ss][Tt][Yy][Ll][Ee][^<>]*</td>', 
                  '<td>[Mm][Oo][Dd][Ee][Ll][^<>]*</td>', '<td>[Gg][Rr][Aa][Dd][Ee][^<>]*</td>', 
                  '<td>[Oo][Cc][Cc][Uu][Pp][Aa][Nn][Cc][Yy][^<>]*</td>', 'AC Type[^<>]*', 
                  'To*ta*l Be*dro*ms', 'To*ta*l Ba*thr*o*m*s', 'To*ta*l Half Ba*ths', 
                  'Bath Style[^<>]*', 'Kitchen Style[^<>]*', '', '"MainContent_lblLndAcres"', 
                  '"MainContent_lblZone"', '"MainContent_lblNbhd"', '"MainContent_lblLndAppr"', '')
col.repat <- c('', rep('<span[^<>]*>[^<>]*</span>', 7), rep('<td>[^<>]*</td>$', 10), 
               '', rep('<span[^<>]*>[^<>]*</span>', 4), '')
getsale <- function(tab){
  #Function to retrieve information in table of sale history
  #Including first 5 'Owner','Sale Price','Sale Date'
  tab[, 'Owner'] <- gsub('&amp[;]*', '&', tab[, 'Owner'])
  tab[, 'Sale Price'] <- as.numeric(gsub('[^0-9\\.]', '', (tab[, 'Sale Price'])))
  sales <- as.vector(t(as.matrix(tab[1:5, c('Owner', 'Sale Price', 'Sale Date')])))
  return(trimws(sales))
}
col.num <- rep(FALSE, 41)
names(col.num) <- col.names
col.num[c("totval", "sqft", "replcost", "pctgood", "halfbaths", "acres", "landval", 
          "garagesqft", "price1", "price2", "price3" ,"price4", "price5")] <- TRUE

usearch<-which(col.patterns != '')
myreg <- function(pat, x, regpat, numonly=FALSE, usefirst=TRUE, nextline=0){
  #Function to retrieve string from a list of strings in html
  #Only for pid location totval
  #Usage: myreg(KEYWORDS, LIST, REGPATTERN, NUMERIC)
  ln<-grep(pat, x)
  if (length(ln) == 0) return(NA)
  ln<-ln+nextline
  reg <- regmatches(x[ln], regexpr(regpat, x[ln]))
  if (usefirst) reg<-reg[1]
  # Remove useless characters
  re.out <- gsub('<[^<>]*>', '', reg)
  re.out <- trimws(re.out)
  if (is.na(re.out) || re.out == '') return(NA)
  if (numonly)
    re.out<-as.numeric(gsub('[^0-9\\.]', '', re.out))
  return(re.out)
}

catchtab <- function(x, tabname){
  #Function to catch the only table with the name
  ln<-grep('^\\s+<table.*>', x)
  cap <- ln[trimws(x[ln+2]) == tabname] + 2
  if (length(cap) >= 1) {
    cap <- cap[1]
    term <- grep('</table>', x[cap:length(x)])[1] + cap - 2
    content <- x[cap:term]
    if (length(grep('No Data for', content)) > 0) return(NA)
    ti <- gsub('<th[^<>]+>', '', trimws(x[cap+2]))
    ti <- gsub('</th>', ',', ti)
    ti <- gsub('<br>', ' ', ti)
    ti <- strsplit(ti, split = ',')[[1]]
    content <- content[grep('<td>.+</td>', content)]
    content <- gsub('<td[^<>]*>|</td>$', '', content)
    content <- gsub('</td>', '\t', content)
    tab <- t(sapply(content, function(x) strsplit(x, split = '\t')[[1]]))
    tab <- as.data.frame(tab, stringsAsFactors = FALSE)
    colnames(tab) <- ti
    rownames(tab) <- 1:nrow(tab)
    tab[tab == ''] <- NA
    tab[tab == '&nbsp;'] <- NA
    return(tab)
  } else return(NA)
}

getgsize <- function(x){
  if (length(x) > 0) {
    return(sum(as.numeric(gsub('^[^0-9]|[^0-9]$', '', x))))
  } else return(NA)
}

myparse <- function(file.list){
  #Function to parse htmls and retrieve information
  #Only for sapply
  myinfo <- c(file.list['i'][[1]], rep(NA, 38), rep(0, 2))
  names(myinfo)<-col.names
  x<-file.list['data'][[1]]
  if (class(x) != 'character') return(myinfo)
  gsize <- c()
  getit <- function(i){
    myreg(col.patterns[i], x, col.repat[i], numonly = col.num[i])
  }
  myinfo[usearch] <- sapply(usearch, getit) 
  #Get tables
  subtab <- catchtab(x, 'Building Sub-Areas (sq ft)')
  extab <- catchtab(x, 'Extra Features')
  saletab <- catchtab(x, 'Ownership History')
  outtab <- catchtab(x, 'Outbuildings')
  if (class(extab) == 'data.frame') {
    exval <- extab[, grep('[Vv][Aa][Ll][Uu][Ee]', colnames(extab))]
    myinfo['exval'] <- sum(as.numeric(gsub('[^0-9\\.]', '', exval))) 
    grs <- unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, extab[,1])))
    gsize <- c(gsize, extab[grs, 'Size'])
  }
  if (class(outtab) == 'data.frame') {
    grs <- unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, outtab[,1])))
    gsize <- c(gsize, outtab[grs, 'Size'])
  }
  if (class(subtab) == 'data.frame') {
    grs <- unlist(sapply(c('FCP', 'FGR', 'OPA', 'UGR'), function(i) grep(i, subtab[,1])))
    gsize <- c(gsize, subtab[grs, 'Gross Area'])
  }
  myinfo["garagesqft"] <- getgsize(gsize)
  if (class(saletab) == 'data.frame') 
    myinfo[saleinfo] <- getsale(saletab)
  if (length(grep('Building 2', x)) > 0) 
    myinfo["multibuilding"] <- 1
  if (length(grep('\\+', myinfo["bedrooms"])) > 0)
    myinfo["nineplus"] <- 1
  myinfo["bedrooms"] <- as.numeric(gsub('[^0-9\\.]', '', myinfo['bedrooms']))
  #Assign value to info
  return(myinfo)
}
trans.html <- function(x){
  x <- gsub('&lt;', '<', x)
  x <- gsub('&gt;', '>', x)
  x <- gsub('&nbsp;', NA, x)
  x <- gsub('&#39;', "'", x)
  x <- gsub('&amp;', '&', x)
  x <- gsub('<br>', ' ,', x)
  return(x)
}
result.list <- lapply(flist, myparse)
result.mat <- t(sapply(result.list, function(x) x))
result.mat <- t(apply(result.mat, 1 , trans.html))
info <- as.data.frame(result.mat, stringsAsFactors = FALSE)
col.num['bedrooms'] <- TRUE
info[, col.num] <- t(apply(info[, col.num], 1, as.numeric))
info[, "multibuilding"] <- info[, "multibuilding"] != 0
info[is.na(info[, "location"]), "multibuilding"] <- NA
info[, "nineplus"] <- info[, "nineplus"] != 0
info[is.na(info[, "bedrooms"]), "nineplus"] <- NA
info[, 'bathrooms'] <- gsub('[^0-9\\.\\/]', '', info[, 'bathrooms'])
eparse <- function(x) return(as.numeric(eval(parse(text = x))))
info[, 'bathrooms'] <- sapply(info[, 'bathrooms'], eparse, USE.NAMES = FALSE)
info[, "grade"] <- gsub('GRADE[_:\\s]*', '', info[, "grade"])
info[, 'finalbath'] <- apply(info[, c('bathrooms', 'halfbaths')], 1, function(x) sum(c(x[1], 0.5*x[2]), na.rm = TRUE))
info[is.na(info[, "bathrooms"]) & is.na(info[, "halfbaths"]), "finalbath"] <- NA
write.table(x = info, file = '625_dj333.csv', row.names = FALSE, sep=',')