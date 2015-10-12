CodesAndRegsPermits<-read.csv('CodesAndRegsPermits.csv')
CodesAndRegsPermits$IssueDate <- as.Date(as.character(CodesAndRegsPermits$IssueDate), format='%m/%d/%Y %H:%M')
uniqueYear<-unique(as.POSIXlt(CodesAndRegsPermits$IssueDate)$year)
uniqueYear<-sort(uniqueYear)
permitsByYear<-lapply(uniqueYear,function(i){ sum(i == as.POSIXlt(CodesAndRegsPermits$IssueDate)$year)})
permitsByYear<-data.frame(uniqueYear,unlist(permitsByYear))
colnames(permitsByYear)<-c('year','permits')
permitsByYear$year<-2003:2015
