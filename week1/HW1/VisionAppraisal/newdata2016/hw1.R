#STAT625 Homework1 
#Real estate information extraction
#Written by Dingjue Ji, 09/05/2016

#Create the data.frame for the dataset
info<-matrix(NA, nrow = 27307, ncol = 3)
info<-as.data.frame(info)
colnames(info)<-c('pid','location','totval')
myreg<-function(pat, x){
  #Function to retrieve string from a list of strings in html
  #Only for pid location totval
  #Usage: myreg(PATTERN, LIST)
  #Get the line with the string and extract it
  ln<-grep(pat,x)
  if(length(ln) == 0){
    return(NA)
  }
  reg<-regmatches(x[ln], regexpr('<span.*>.+</span>', x[ln]))
  #Remove useless characters
  o<-gsub('<[^<>]*>','',reg)
  return(o)
}

myparse<-function(i){
  #Function to parse htmls and retrieve information
  #Only for sapply
  fname<-paste(i, '.html', sep = '')
  loc<-NA
  aval<-NA
  #Read files when they exist
  if(file.exists(fname)){
    x<-scan(file=fname, what='', sep='\n', quiet = TRUE)
    loc<-myreg('"MainContent_lblLocation"',x)
    aval<-myreg('"MainContent_lblGenAppraisal"',x)
  }
  #Assign value to info
  info[i,]<<-c(i, loc, aval)
}
sapply(1:27307, myparse)

#Tranform the format of numeric values
info$pid<-as.numeric(info$pid)
info$totval<-as.numeric(info$totval)

#Write data into csv file
write.csv(x = info, file = 'hw1.csv', quote = FALSE, row.names = FALSE)