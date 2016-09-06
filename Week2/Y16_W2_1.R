#
# STAT 325/625
# Week 2: Tuesday, September 6
#
# 1. Jay provided a slide deck with some tips and best practices for
#    working with R and R Studio that he used in STAT 230.  We'll review
#    _briefly_ and perhaps will work through an example or two.  We assume
#    you've used R before, but some of these tips may be new to you.
#
# 2. Homework 1: Earlier this morning, Susan and I collected a bunch of CSV
#    files from you folks (fingers crossed).  We are providing you with a
#    folder of them.  Your task: a sanity check!  Are these files identical?
#    Probably not.  How do they differ?  Some of the differences may be
#    trivial.  Others may be very important.  Perhaps there are some errors
#    or suspected errors.  Explore.  Perhaps you can identify a few (or a set)
#    of the most worrying parcel ids from the City of New Haven with respect
#    to our scraping of these first two variables?  Or maybe you can identify
#    specific parsing/cleaning problems that explain the differences you see.
#
#    With statistics and data analysis, the saying is (or should be)
#    "garbage in, garbage out."  It doesn't matter what your methodology is,
#    or how well you can prove a theorem or make assumptions.
#
# YOUR CODE BELOW.  Nice code.  Documented code.  Indented code...
# What code?  That's up to you.  We aren't telling you exactly what to do.
# We don't even have a specific "correct" body of work in mind or a "correct"
# version of these variables.  Understand the problem.  Figure out how
# to do something useful.  Do it!

# Information compare problem
# Written by Dingjue Ji 09/06/2016

files<-dir('CV2dropbox625/', full.names = TRUE)
rcnum<-matrix(nrow=length(files),ncol=3)
i<-1

comp<-vector('list', 22)
for (file in files){
  x<-read.csv(paste('CV2dropbox625/', file, sep = ''), as.is=TRUE)
  comp[[i]]['data']<-x
  rcnum[i,]<-c(file, nrow(x), ncol(x))
  i<-i+1
}
test1<-files[1]
test2<-files[2]
table1<-read.csv(paste('CV2dropbox625/',test1, sep=''), as.is = TRUE)
table2<-read.csv(paste('CV2dropbox625/', test2, sep=''), as.is = TRUE)

fhead<-function(csv1){
  if(all(colnames(csv1) == c('pid', 'location', 'totval'))){
    return(0)
  }
  else{
    return(1)
  }
}

ahead<-function(csv1){
  if(fhead(csv1) == 1){
    colnames(csv1)<-c('pid', 'location', 'totval')
  }
}

fcol<-function(csv1, csv2, i){
  dif<-which(csv1[,i] != csv2[,i])
  return(paste(dif, collapse=','))
}

fcolc<-function(myfcol){
  return(length(strsplit(myfcol, split=',')[[1]]))
}



compare<-function(tab1){
  csv1<-read.csv(paste('CV2dropbox625/', tab1, sep=''), as.is = TRUE)
  for(csv2 in files){
    if(csv1 != csv2){
      myfcol<-c()
      myfcolc<-c()
      for(i in 1:3){
        myfcol<-c(myfcol, fcol(csv1,csv2,i))
        myfcolc<-c(myfcolc, fcolc(myfcol = myfcol))
      }
      print(myfcol)
    }
  }
}
