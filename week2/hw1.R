#STAT625 Homework1 
#Real estate information extraction
#Written by Dingjue Ji, 09/05/2016

#Important Note:
#For those with several buildings, the number of rooms
#will be summed up. Besides, the room numbers will be 
#counted as 0 only if they exist and are 0. The others
#without information will be recorded as NA. During the
#summation, only number with information will be counted.

mysum<-function(x){
  if(all(is.na(x))){
    return(NA)
  }
  else{
    sum(x, na.rm = TRUE)
  }
}


#Create the data.frame for the dataset
info<-matrix(NA, nrow = 27307, ncol = 6)
info<-as.data.frame(info)
colnames(info)<-c('pid','location','totval','bedrooms','bathrooms','halfbaths')
myreg<-function(pat, x, regpat, numonly=FALSE){
  #Function to retrieve string from a list of strings in html
  #Only for pid location totval
  #Usage: myreg(KEYWORDS, LIST, REGPATTERN, NUMERIC)
  #Get the line with the string and extract it
  ln<-grep(pat,x)
  if(length(ln) == 0){
    return(NA)
  }
  reg<-regmatches(x[ln], regexpr(regpat, x[ln]))
  #Remove useless characters
  o<-gsub('<[^<>]*>','',reg)
  if(numonly == TRUE){
    o<-mysum(as.numeric(gsub('[^0-9]', '', o)))
  }
  return(o)
}

myparse<-function(i){
  #Function to parse htmls and retrieve information
  #Only for sapply
  fname<-paste(i, '.html', sep = '')
  loc<-NA
  aval<-NA
  beds<-NA
  baths<-NA
  halfs<-NA
  #Read files when they exist
  if(file.exists(fname)){
    x<-scan(file=fname, what='', sep='\n', quiet = TRUE)
    loc<-myreg('"MainContent_lblLocation"',x,'<span.*>.+</span>')
    aval<-myreg('"MainContent_lblGenAppraisal"',x,'<span.*>.+</span>', numonly = TRUE)
    beds<-myreg('To*ta*l Be*dro*ms', x, '<td>.+</td>', numonly = TRUE)
    baths<-myreg('To*ta*l Ba*thr*o*m*s', x, '<td>.+</td>', numonly = TRUE)
    halfs<-myreg('To*ta*l Half Ba*ths', x, '<td>.+</td>', numonly = TRUE)
  }
  #Assign value to info
  info[i,]<<-c(i, loc, aval, beds, baths, halfs)
}
sapply(1:27307, myparse)

#Tranform the format of numeric values
tonum<-function(x, trans){
  #Funtion to transform 'trans' columns of x
  #Usage tonum(DATA, COLUMNS)
  for(i in trans){
   x[,i]<-as.numeric(x[,i]) 
  }
  return(x)
}

info<-tonum(info, c(1,3,4,5,6))

#Write data into csv file
write.csv(x = info, file = 'hw1.csv', quote = FALSE, row.names = FALSE)