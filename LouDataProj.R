##################################################################
#                                                                # 
#   Modeling impact of PCP Density on Crime Rate in Louisville   #
#                                                                #
#                            Robert Kahne                        #
#                                                                #
##################################################################


library('xtable')
library(choroplethr)
library(choroplethrZip)

# Read in this data.  It's about 700 MB total, so it's not hosted.
Crime<-read.csv('../Crime_Data_All.csv') # Crime Data from Louisville Data (portal.louisvilleky.gov/)
Doctors<-read.csv('../National_Downloadable_File_-_Simplified_View.csv') # Physician information from CMS (https://dnav.cms.gov/Views/Search.aspx)
# Next 3: American Community Survey Data (http://factfinder.census.gov/faces/nav/jsf/pages/download_center.xhtml)
Population<-read.csv('../ACS_13_5YR population est.csv')
Race<-read.csv('../ACS13_5YR_Race.csv')
Income<-read.csv('../ACS_13_5YR Income.csv')

# Reconfigure variables so they are useful for us
Doctors$State<-as.character(Doctors$State)
Doctors$Primary.specialty<-as.character(Doctors$Primary.specialty)
Doctors$Zip.Code<-as.numeric(substr(as.character(Doctors$Zip.Code),1,5))

# Get the subset of physicians who are PCPs
PCPs<-subset(Doctors,Primary.specialty=='INTERNAL MEDICINE'|Primary.specialty=='FAMILY PRACTICE'|Primary.specialty=='NURSE PRACTITIONER'|Primary.specialty=='PHYSICIAN ASSISTANT'|
                 Primary.specialty=='GENERAL PRACTICE')

# Refine that subset to just PCPs in KY
KYPCPs<-subset(PCPs,State=='KY')

# Determine how many FTEs each physician is worth at each location they practice at.
KYPCPs$numLocations<-lapply(KYPCPs$NPI,function(i){
  sum(Doctors$NPI==i)
})
KYPCPs$numLocations<-as.numeric(KYPCPs$numLocations)
KYPCPs$FTEEquiv<-1/KYPCPs$numLocations

# These zipcodes are 9 digit (dumb), so we need to fix them
KYPCPs$Zip.Code<-as.numeric(substr(as.character(KYPCPs$Zip.Code),1,5))

# Get all the zips for which we have data in KY
KYZips<-unique(subset(Doctors, State=='KY',select=Zip.Code))

# Start framing up the data.  Start with the Zip Codes
CrimeMD<-KYZips
CrimeMD<-unique(CrimeMD)
CrimeMD<-data.frame(CrimeMD)
colnames(CrimeMD)<-'Zip.Code'

# Create a column for PCP FTEs per zip code
CrimeMD$MDFTE<-sapply(CrimeMD$Zip.Code,function(i){
  tmpNPI<-subset(Doctors,Zip.Code==i,select=NPI)
  if(length(which(KYPCPs$NPI %in% tmpNPI$NPI)>0)){
    return(sum(KYPCPs$FTEEquiv[which(KYPCPs$NPI %in% tmpNPI$NPI)]))
  } else {
    return(0)
  }
})

# Start working on the crime data.  Fix the dates so they work with us.
Crime$DATE_OCCURED<-as.Date(Crime$DATE_OCCURED)
#Refine to JUST 2014
Crime2014<-subset(Crime,DATE_OCCURED<as.Date('2015-01-01')&DATE_OCCURED>=as.Date('2014-01-01'))
# Only get the Zip Codes for which we have physician data.
Crime2014<-Crime2014[Crime2014$ZIP_CODE %in% CrimeMD$Zip.Code, ]
CrimeMD$numCrimes<-sapply(CrimeMD$Zip.Code,function(i){
  sum(i == Crime2014$ZIP_CODE)
})

# Start refining to just Louisville Zips
LouisvilleZips <- c('40212','40211','40216','40258','40272','40203','40210','40208','40215','40214','40118','40209','40202','40206','40204','40217',
                               '40213','40219','40229','40205','40218','40228','40291','40220','40299','40023','40245','40059','40241','40223','40243',
                               '40242','40025','40222','40207','40177','40231','40041','40280')

# Subset of our frame that is in Louisville.
CrimeMD<-CrimeMD[CrimeMD$Zip.Code %in% LouisvilleZips,]

#CrimeMD<-merge(CrimeMD,LouisvilleZips, by.x=Zip.Code,by.y=LouisvilleZips, all.y=TRUE)

# Do a bunch of sapplys with data from the census to control for various demographics in the zip codes.
CrimeMD$population<-sapply(CrimeMD$Zip.Code,function(i){
  return(Population$Total..Estimate..Total.population[Population$Id2 == i])
})
CrimeMD$percentMale<-sapply(CrimeMD$Zip.Code,function(i){
  return(Population$Male..Estimate..Total.population[Population$Id2 == i]/Population$Total..Estimate..Total.population[Population$Id2 == i])
})
CrimeMD$percentUnder5<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...Under.5.years[Population$Id2 == i]))/100)
})
CrimeMD$percentUnder5<-as.numeric(CrimeMD$percentUnder5)
CrimeMD$percent5to9<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...5.to.9.years[Population$Id2 == i]))/100)
})
CrimeMD$percent10to14<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...10.to.14.years[Population$Id2 == i]))/100)
})
CrimeMD$percent15to19<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...15.to.19.years[Population$Id2 == i]))/100)
})
CrimeMD$percent20to24<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...20.to.24.years[Population$Id2 == i]))/100)
})
CrimeMD$percent25to29<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...25.to.29.years[Population$Id2 == i]))/100)
})
CrimeMD$percent30to34<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...30.to.34.years[Population$Id2 == i]))/100)
})
CrimeMD$percent35to39<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...35.to.39.years[Population$Id2 == i]))/100)
})
CrimeMD$percent40to44<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...40.to.44.years[Population$Id2 == i]))/100)
})
CrimeMD$percent45to49<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...45.to.49.years[Population$Id2 == i]))/100)
})
CrimeMD$percent50to54<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...50.to.54.years[Population$Id2 == i]))/100)
})
CrimeMD$percent55to59<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...55.to.59.years[Population$Id2 == i]))/100)
})
CrimeMD$percent60to64<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...60.to.64.years[Population$Id2 == i]))/100)
})
CrimeMD$percent65to69<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...65.to.69.years[Population$Id2 == i]))/100)
})
CrimeMD$percent70to74<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...70.to.74.years[Population$Id2 == i]))/100)
})
CrimeMD$percent75to79<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...75.to.79.years[Population$Id2 == i]))/100)
})
CrimeMD$percent80to84<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...80.to.84.years[Population$Id2 == i]))/100)
})
CrimeMD$percent85Plus<-sapply(CrimeMD$Zip.Code,function(i){
  return(as.numeric(as.character(Population$Total..Estimate..AGE...85.years.and.over[Population$Id2 == i]))/100)
})
CrimeMD$percentWhiteAlone<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....White.alone[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$percentAsianAlone<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....Asian.alone[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$percentBlackAAAlone<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....Black.or.African.American.alone[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$percentAmericanIndianAlone<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....American.Indian.and.Alaska.Native.alone[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$percentHPIAlone<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....Native.Hawaiian.and.Other.Pacific.Islander.alone[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$percent2OrMoreRaces<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....Two.or.more.races.[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$percentSomeOtherRaceAlone<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....Some.other.race.alone[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$percent2OrMoreIncSomeOtherRace<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....Two.or.more.races....Two.races.including.Some.other.race[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$percent2OrMoreExcSomeOtherRace<-sapply(CrimeMD$Zip.Code,function(i){
  return(Race$Estimate..Total....Two.or.more.races....Two.races.excluding.Some.other.race..and.three.or.more.races[Race$Id2 == i]/Race$Estimate..Total.[Race$Id2 == i])
})
CrimeMD$income<-sapply(CrimeMD$Zip.Code,function(i){
  return(Income$Households..Estimate..Total[Income$Id2 == i])
})

# Calculate PCP per Thousands and Crime Rate per thousands to control for population
CrimeMD$MDFTEPerT <- CrimeMD$MDFTE/(CrimeMD$population/1000)
CrimeMD$CrimeRatePerT <- CrimeMD$numCrimes/(CrimeMD$population/1000)

# Start analyzing
#attach(CrimeMD)
#CrimeByMD<-lm(CrimeRatePerT~MDFTEPerT+percentMale+percentUnder5+percent5to9+percent10to14+percent15to19+percent20to24+percent25to29+percent30to34+
#     percent35to39+percent40to44+percent45to49+percent50to54+percent55to59+percent60to64+percent65to69+percent70to74+percent75to79+
#     percent80to84+percent85Plus+percentWhiteAlone+percentAsianAlone+percentBlackAAAlone+percentAmericanIndianAlone+percentHPIAlone+
#     percent2OrMoreRaces+percentSomeOtherRaceAlone+percent2OrMoreIncSomeOtherRace+CrimeMD$percent2OrMoreExcSomeOtherRace+income)
#
#summary(CrimeByMD)

# Build out some more consolidated categories.
CrimeMD$percentUnder20<-CrimeMD$percentUnder5+CrimeMD$percent5to9+CrimeMD$percent10to14+CrimeMD$percent15to19
CrimeMD$percent20to40<-CrimeMD$percent20to24+CrimeMD$percent25to29+CrimeMD$percent30to34+CrimeMD$percent35to39
CrimeMD$percent40to60<-CrimeMD$percent40to44+CrimeMD$percent45to49+CrimeMD$percent50to54+CrimeMD$percent55to59
CrimeMD$percent60to80<-CrimeMD$percent60to64+CrimeMD$percent65to69+CrimeMD$percent70to74+CrimeMD$percent75to79
CrimeMD$percent80Plus<-CrimeMD$percent80to84+CrimeMD$percent85Plus

CrimeMD$NotBlackWhiteAsian<-CrimeMD$percentAmericanIndianAlone+CrimeMD$percentHPIAlone+CrimeMD$percent2OrMoreRaces+CrimeMD$percentSomeOtherRaceAlone+
  CrimeMD$percent2OrMoreIncSomeOtherRace+CrimeMD$percent2OrMoreExcSomeOtherRace

# Write out what we just did.  This is what you can use.
write.table(CrimeMD,file='CrimeMD.csv', sep='\t', row.names = FALSE)

# Run a model.  
attach(CrimeMD)
CrimeByMDAbbr<-lm(CrimeRatePerT~MDFTEPerT+percentMale+percentUnder20+percent20to40+percent40to60+percent60to80+percent80Plus+
                    percentWhiteAlone+percentBlackAAAlone+percentAsianAlone+NotBlackWhiteAsian+income)4
out<-print(xtable(summary(CrimeByMDAbbr)),type='HTML')
cat('CrimeByMDAbbr',out, file='CrimeByMDAbbrModel.txt',sep='n',append=TRUE)

# Start mapping
choroMD<-data.frame(CrimeMD$Zip.Code,CrimeMD$MDFTEPerT)
colnames(choroMD)<-c('region','value')
choroMD$region<-as.character(choroMD$region)

zip_choropleth(choroMD,
               county_zoom = '21111',
               title = 'Primary Care Providers in Louisville',
               legend = '# PCPs/Thousand',
               num_colors = 1)

choroCrime<-data.frame(CrimeMD$Zip.Code,CrimeMD$CrimeRatePerT)
colnames(choroCrime)<-c('region','value')
choroCrime$region<-as.character(choroMD$region)

zip_choropleth(choroCrime,
               county_zoom = '21111',
               title = 'Crime Rate by Zip in Louisville',
               legend = '# Crimes/Thousand',
               num_colors = 1)

