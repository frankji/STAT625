files<-dir()
files<-files[grep('csv', files)]
x<-as.data.frame(matrix(NA,ncol = 4, nrow = 1))
names(x)<-c('pid', 'location', 'lat', 'lon')

for(file in files){
  print(file)
  x<-rbind(x, read.csv(file, as.is=TRUE))
}
x<-x[-1,]
#rowchar<-sapply(1:nrow(x), function(i) {paste(x[i,], collapse = ',')})
#counts<-as.data.frame(table(rowchar))
#table(rowchar)
test<-unique(x)
no.na<-test[!is.na(test[,3]), ]
all.na<-test[is.na(test[,3]), ]
all.na<-all.na[order(all.na[,1]), ]
nums<-as.data.frame(table(test[,1]))
nanums<-which(nums$Freq==1)
allrows<-rbind(no.na, all.na[nanums,])
allrows<-allrows[order(allrows[,1]), ]
rownames(allrows)<-1:nrow(allrows)
allrows[is.na(allrows$location),c(3,4)]<-NA
still.na<-which(!is.na(allrows$location) & 
                  apply(allrows[,c(3,4)],1,function(x) all(is.na(x))))
locs<-paste(allrows[still.na,'location'], ", New Haven, CT", sep="")
ans<-geocode(locs)
allrows[still.na,'lat']<-ans$lat
allrows[still.na, 'lon']<-ans$lon