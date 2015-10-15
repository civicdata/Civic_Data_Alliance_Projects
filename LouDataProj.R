Crime<-read.csv('../Crime_Data_All.csv')
Doctors<-read.csv('../National_Downloadable_File_-_Simplified_View.csv')
Population<-read.csv('../ACS_13_5YR population est.csv')


Doctors$State<-as.character(Doctors$State)
Doctors$Primary.specialty<-as.character(Doctors$Primary.specialty)
Doctors$Zip.Code<-as.numeric(substr(as.character(Doctors$Zip.Code),1,5))
PCPs<-subset(Doctors,Primary.specialty=='INTERNAL MEDICINE'|Primary.specialty=='FAMILY PRACTICE'|Primary.specialty=='NURSE PRACTITIONER'|Primary.specialty=='PHYSICIAN ASSISTANT'|
                 Primary.specialty=='GENERAL PRACTICE')
KYPCPs<-subset(PCPs,State=='KY')
KYPCPs$numLocations<-lapply(KYPCPs$NPI,function(i){
  sum(Doctors$NPI==i)
})
KYPCPs$numLocations<-as.numeric(KYPCPs$numLocations)
KYPCPs$FTEEquiv<-1/KYPCPs$numLocations
KYPCPs$Zip.Code<-as.numeric(substr(as.character(KYPCPs$Zip.Code),1,5))

KYZips<-unique(subset(Doctors, State=='KY',select=Zip.Code))
CrimeMD<-KYZips
CrimeMD<-unlist(CrimeMD)
CrimeMD<-as.numeric(substr(as.character(CrimeMD),1,5))
CrimeMD<-unique(CrimeMD)
CrimeMD<-data.frame(CrimeMD)
colnames(CrimeMD)<-'Zip.Code'
CrimeMD$MDFTE<-lapply(CrimeMD$Zip.Code,function(i){
  tmpNPI<-subset(Doctors,Zip.Code==i,select=NPI)
  if(length(which(KYPCPs$NPI %in% tmpNPI$NPI)>0)){
    return(sum(KYPCPs$FTEEquiv[which(KYPCPs$NPI %in% tmpNPI$NPI)]))
  } else {
    return(0)
  }
})


Crime$DATE_OCCURED<-as.Date(Crime$DATE_OCCURED)
Crime2014<-subset(Crime,DATE_OCCURED<as.Date('2015-01-01')&DATE_OCCURED>=as.Date('2014-01-01'))
Crime2014<-Crime2014[Crime2014$ZIP_CODE %in% CrimeMD$Zip.Code, ]
CrimeMD$numCrimes<-lapply(CrimeMD$Zip.Code,function(i){
  sum(i == Crime2014$ZIP_CODE)
})

LouisvilleZips <- c('40212','40211','40216','40258','40272','40203','40210','40208','40215','40214','40118','40209','40202','40206','40204','40217',
                               '40213','40219','40229','40205','40218','40228','40291','40220','40299','40023','40245','40059','40241','40223','40243',
                               '40242','40025','40222','40207','40177','40231','40041','40280')

CrimeMD<-CrimeMD[CrimeMD$Zip.Code %in% LouisvilleZips,]
CrimeMD$population<-lapply(CrimeMD$Zip.Code,function(i){
  return(Population$Total..Estimate..Total.population[Population$Id2 == i])
})
CrimeMD$MDFTE<-as.numeric(CrimeMD$MDFTE)
CrimeMD$numCrimes<-as.numeric(CrimeMD$numCrimes)
CrimeMD$population<-as.numeric(CrimeMD$population)

CrimeMD$MDFTEPerT <- CrimeMD$MDFTE/(CrimeMD$population/1000)
CrimeMD$CrimeRatePerT <- CrimeMD$numCrimes/(CrimeMD$population/1000)
