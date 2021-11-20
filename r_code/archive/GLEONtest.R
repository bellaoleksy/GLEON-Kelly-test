#Load packages
library(tidyverse)
library(lubridate)
library(reshape2)
#Source DB scripts
dbdir=file.path("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE")
db="MFEdb_20190612.db"  
sensordb="MFEsensordb.db" 
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_dbUtil.R")
source("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/000_sensordbTable.R")

# Old way -----------------------------------------------------------------
start_time <- Sys.time()
WL_oldway<-mfeMetab(lakeID = 'WL', minDate = '2014-06-01', maxDate = '2014-08-31', 
                           bootstrap = 'yes', outName = 'WestLong_db_output_',
                           dirDump = '/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results')
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
MO_oldway<-mfeMetab(lakeID = 'MO', minDate = '2014-06-01', maxDate = '2014-08-31', 
                    bootstrap = 'yes', outName = 'Morris_db_output_',
                    dirDump = '/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results')
end_time <- Sys.time()
end_time - start_time





# Pull data from DB -----------------------------------------------------------------

WL_DO<-sensordbTable("DO_CORR",lakeID="WL",minDate = '2014-06-01', maxDate = '2014-08-31')
# write.table(WL_DO, file="GLEON/data/WL_DO.txt", sep = "\t", row.names = FALSE)

WL_DO_YSI<-sensordbTable("YSI_CORR",lakeID="WL",minDate = '2014-06-01', maxDate = '2014-08-31')
# write.table(WL_DO_YSI, file="GLEON/data/WL_DO_YSI.txt", sep = "\t", row.names = FALSE)

WL_metData<-sensordbTable("HOBO_METSTATION_CORR",lakeID="WL",minDate = '2014-06-01', maxDate = '2014-08-31')
# write.table(WL_metData, file="GLEON/data/WL_metData.txt", sep = "\t", row.names = FALSE)

WL_temp<-sensordbTable("HOBO_TCHAIN_CORR",lakeID="WL",minDate = '2014-06-01', maxDate = '2014-08-31')
# write.table(WL_temp, file="GLEON/data/WL_temp.txt", sep = "\t", row.names = FALSE)

# 
# MO_DO<-sensordbTable("DO_CORR",lakeID="MO",minDate = '2014-06-01', maxDate = '2014-08-31')
# write.table(MO_DO, file="GLEON/data/MO_DO.txt", sep = "\t", row.names = FALSE)
# 
# MO_DO_YSI<-sensordbTable("YSI_CORR",lakeID="MO",minDate = '2014-06-01', maxDate = '2014-08-31')
# write.table(MO_DO_YSI, file="GLEON/data/MO_DO_YSI.txt", sep = "\t", row.names = FALSE)
# 
# MO_temp<-sensordbTable("HOBO_TCHAIN_CORR",lakeID="MO",minDate = '2014-06-01', maxDate = '2014-08-31')
# write.table(MO_temp, file="GLEON/data/MO_temp.txt", sep = "\t", row.names = FALSE)

# Testing function code here -----------------------------------------------------------------

dataRaw=c("/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/data/WL_DO.txt",
          "/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/data/WL_DO_YSI.txt",
          "/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/data/WL_metData.txt",
          "/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/data/WL_temp.txt")

########################################
#Read and organize data from database - assumes database functions have been sourced
#DO
rawDOmd<-read.table(dataRaw[1],header=T,sep='\t',colClasses=c(dateTime="POSIXct"))
rawDOysi<-read.table(dataRaw[2],header=T,sep='\t',colClasses=c(dateTime="POSIXct"))
rawDO<-rbind(rawDOmd[,c("dateTime","cleanedDO_mg_L")],rawDOysi[,c("dateTime","cleanedDO_mg_L")])
dataDO<-aggregate(x=as.numeric(rawDO$cleanedDO_mg_L),by=list(rawDO$dateTime),FUN=mean,na.rm=TRUE)
colnames(dataDO)<-c("datetime","DO")

#Water temp at depth of DO sensor
# if(lakeID=="WA"){
rawSensorTemp<-rbind(rawDOmd[,c("dateTime","cleanedTemp_C")],rawDOysi[,c("dateTime","cleanedTemp_C")])
dataSensorTemp<-aggregate(x=as.numeric(rawSensorTemp$cleanedTemp_C),by=list(rawSensorTemp$dateTime),FUN=mean,na.rm=TRUE)
colnames(dataSensorTemp)<-c("datetime","TEMP")
# }else{
#   dataSensorTemp<-rawDO[,c("dateTime","cleanedTemp_C")]
#   colnames(dataSensorTemp)<-c("datetime","TEMP")
# }
#Temp profile
tempChain<-read.table(dataRaw[4],header=T,sep='\t',colClasses=c(dateTime="POSIXct"))
tempChain2<-tempChain[,c("dateTime","depth_m","cleanedTemp_C")]
colnames(tempChain2)=c('datetime','depth_m','temp')
dataTempProfile<-reshape(tempChain2,timevar="depth_m",idvar="datetime",direction="wide",sep="")

metData<-read.table(dataRaw[3],header=T,sep='\t',colClasses=c(dateTime="POSIXct"))
# metDataWL<-sensordbTable("HOBO_METSTATION_CORR",lakeID="WL",minDate=minDate,maxDate=maxDate)
# metData=rbind(metDataEL,metDataWL)
# metData=metData[!duplicated(metData$dateTime),]
#PAR
#Divide PAR by 1000 to convert from measured units (umol m-2 s-1) to model units (mmol m-2 s-1)
dataPAR<-aggregate(x=as.numeric(metData$cleanedPAR_uE_m2_s),by=list(metData$dateTime),FUN=mean,na.rm=TRUE)
colnames(dataPAR)=c("datetime","PAR")
dataPAR$PAR <- dataPAR$PAR/1000
#Wind speed
dataWind<-aggregate(x=as.numeric(metData$cleanedWindSpeed_m_s),by=list(metData$dateTime),FUN=mean,na.rm=TRUE)
colnames(dataWind)=c("datetime","WS")

##
#Display some info about time grain of measurements

#Print first five time readings for each variable
firstFiveTimes <- data.frame(DO=dataDO$datetime[1:5],PAR=dataPAR$datetime[1:5],
                             windSpeed=dataWind$datetime[1:5],sensorTemp=dataSensorTemp$datetime[1:5],
                             tempProfile=dataTempProfile$datetime[1:5])
print('First five time readings for each variable'); print(firstFiveTimes)

#Calculate first differences of time readings, display unique values
cat('\n','First differences of datetime', '\n')
difTimesDO <- diff(dataDO$datetime); print(table(difTimesDO))
difTimesPAR <- diff(dataPAR$datetime); print(table(difTimesPAR))
difTimesWindSpeed <- diff(dataWind$datetime); print(table(difTimesWindSpeed))
difTimesSensorTemp <- diff(dataSensorTemp$datetime); print(table(difTimesSensorTemp))
difTimesTempProfile <- diff(dataTempProfile$datetime); print(table(difTimesTempProfile))

#Function to find duplicate datetime stamps
findNotDupRows <- function(dataInName)
{
  #This function returns the indexes of rows where the datetime is NOT a duplicate
  #of the datetime in a previous row
  #dataInName is character, e.g. "dataPAR"
  dataIn <- eval(parse(text=dataInName))
  #Find duplicated time stamps
  dups <- duplicated(dataIn$datetime)
  #If no duplicated time stamps, notDupRows=all rows in dataIn
  if (all(dups==FALSE))
  {
    notDupRows <- c(1:dim(dataIn)[1])
  } else
    #If at least one time stamp is duplicated, warn, and notDupRows is indexes
    #of rows where datetime is not duplicate of the datetime in a previous row
  {
    notDupRows <- which(dups==FALSE)
    nDups <- dim(dataIn)[1]-length(notDupRows)
    print(paste("Warning:",nDups,"rows with duplicate time stamps in",dataInName,"will be removed"))
  }
  #Return dupRows
  return(notDupRows)
}

##
#Remove rows with duplicate datetime stamps (and warn)
notDupRows <- findNotDupRows("dataDO")
dataDO <- dataDO[notDupRows,]
notDupRows <- findNotDupRows("dataPAR")
dataPAR <- dataPAR[notDupRows,]
notDupRows <- findNotDupRows("dataWind")
dataWind <- dataWind[notDupRows,]
notDupRows <- findNotDupRows("dataSensorTemp")
dataSensorTemp <- dataSensorTemp[notDupRows,]
notDupRows <- findNotDupRows("dataTempProfile")
dataTempProfile <- dataTempProfile[notDupRows,]

#### support functions
#Round all time down to nearest timeStep (e.g. if timeStep is 5, round 00:07 to 00:05)
floorMins <- function(dataIn)
{
  #Pull out datetime column and name it x
  x <- dataIn$datetime
  nRows <- length(x)
  #Truncate each datetime to hour; convert to class numeric
  floorHour <- as.POSIXct(trunc(x[1:nRows],"hour"),tz="America/Chicago")
  floorNumeric <- as.numeric(floorHour)
  #Create sequence from floorNumeric to next hour by timeStep (in seconds)
  seqSec <- seq(0,3600,60*timeStep)
  #Create matrix where each row is floorNumeric + the elements of seqSec
  matSec <- matrix(rep(seqSec,nRows),nrow=nRows,byrow=T)
  matSec <- floorNumeric + matSec
  #Calculate abs(time difference) between each element of x and the timeStep intervals
  difs <- abs(as.numeric(x) - matSec)
  #Find the minimum absolute difference in each row and select the corresponding time from matSec
  whichMin <- apply(difs,1,which.min)
  rowNames <- as.numeric(rownames(data.frame(whichMin)))
  matIndex <- (whichMin-1)*nRows + rowNames
  matSecFlat <- matrix(matSec,ncol=1)
  outTime <- as.POSIXct(matSecFlat[matIndex],origin="1970-01-01",tz="America/Chicago")
  #Return outTime
  return(outTime)
}
lat=46.16
elev=535
windHeight=2
timeStep=10
sensorDepth=0.7
dataDO$dateTime <- floorMins(dataDO)
dataPAR$dateTime <- floorMins(dataPAR)
dataWind$dateTime <- floorMins(dataWind)
dataSensorTemp$dateTime <- floorMins(dataSensorTemp)
dataTempProfile$dateTime <- floorMins(dataTempProfile)

#Repeat check for dup rows in case any introduced during floorMins
notDupRows <- findNotDupRows("dataDO")
dataDO <- dataDO[notDupRows,]
notDupRows <- findNotDupRows("dataPAR")
dataPAR <- dataPAR[notDupRows,]
notDupRows <- findNotDupRows("dataWind")
dataWind <- dataWind[notDupRows,]
notDupRows <- findNotDupRows("dataSensorTemp")
dataSensorTemp <- dataSensorTemp[notDupRows,]
notDupRows <- findNotDupRows("dataTempProfile")
dataTempProfile <- dataTempProfile[notDupRows,]

#Find the latest first time point and the earliest last time point of all the data
startTime <- max(min(dataDO$datetime),min(dataPAR$datetime),min(dataWind$datetime),min(dataSensorTemp$datetime),min(dataTempProfile$datetime))
endTime <- min(max(dataDO$datetime),max(dataPAR$datetime),max(dataWind$datetime),max(dataSensorTemp$datetime),max(dataTempProfile$datetime))

#Data.frame with one column "datetime" which is sequence of times at time interval of timeStep, from startTime to endTime
completeTimes <- data.frame(datetime=seq(startTime,endTime,paste(timeStep,"mins")))

#Merge all of input data.frames with completeTimes, so that they now all extend from startTime to endTime by timeStep
dataDO <- merge(completeTimes,dataDO,by="datetime",all.x=T)
dataPAR <- merge(completeTimes,dataPAR,by="datetime",all.x=T)
dataWind <- merge(completeTimes,dataWind,by="datetime",all.x=T)
dataSensorTemp <- merge(completeTimes,dataSensorTemp,by="datetime",all.x=T)
dataTempProfile <- merge(completeTimes,dataTempProfile,by="datetime",all.x=T)

########################################
#Calculate sunrise, sunset

#Days of year for which to calculate sunrise and sunset
daysVec <- seq.POSIXt(trunc(startTime,"day"),trunc(endTime,"day"),"1 day")
day <- as.numeric(format(daysVec,format="%j"))

#Factors to convert degrees to radians and vice versa
degToRad <- 2*pi/360
radToDeg <- 180/pi

#Day angle "gamma" (radians). Iqbal 1983 Eq. 1.2.2
dayAngle <- 2*pi*(day-1)/365

#Declination of the sun "delta" (radians). Iqbal 1983 Eq. 1.3.1
dec <- 0.006918 - 0.399912*cos(dayAngle) + 0.070257*sin(dayAngle) - 0.006758*cos(2*dayAngle) +  0.000907*sin(2*dayAngle) - 0.002697*cos(3*dayAngle) + 0.00148*sin(3*dayAngle)

#Sunrise hour angle "omega" (degrees). Iqbal 1983 Eq. 1.5.4
latRad <- lat*degToRad
sunriseHourAngle <- acos(-tan(latRad)*tan(dec))*radToDeg

#Sunrise and sunset times (decimal hours, relative to solar time) Iqbal 1983 Ex. 1.5.1 - +5 is to put it in America/Chicago
sunrise <- 12 - sunriseHourAngle/15+5
sunset <- 12 + sunriseHourAngle/15+5
# As number of seconds from midnight
sunrise <- sunrise/24*86400
sunset <- sunset/24*86400
# As number of seconds from beginning of year
sunrise <- sunrise+(day-1)*86400
sunset <- sunset+(day-1)*86400
# Convert to POSIXct and round to nearest minute
yr <- format(daysVec,format="%Y")
origin <- paste(yr,"01","01",sep="-")
sunrise <- round(as.POSIXct(sunrise,origin=origin,tz="America/Chicago"),"mins")
sunset <- round(as.POSIXct(sunset,origin=origin,tz="America/Chicago"),"mins")

# One final note: I found out a while ago that the metabolism code doesn't take daylight 
# savings time into account.  All the datasets that Chris was getting for GLEON were in 
# normal time, not daylight savings time.  If you're using CDT times with the pelagic data 
# you've been running through the codes, you should check the sunrise and sunset times the 
# code is calculating.  They'll probably be offset by one hour, which would affect the GPP 
# and R results.  If the data you're inputting is exclusively in daylight savings time (like 
# our data from the summer), you can just add these two lines of code into script:
# #
sunrise <- sunrise + 3600
sunset <- sunset + 3600
# #
# Comment the shit out of those lines so you don't forget about them later.  
# Those lines would go right before:
# #Create data.frame with sunrise, sunset times for each day
# sun <- data.frame(day=daysVec,sunrise,sunset)

# NOT QUITE SURE ABOUT THIS YET...
#sunrise <- sunrise + 3600
#sunset <- sunset + 3600


#Create data.frame with sunrise, sunset times for each day
sun <- data.frame(day=daysVec,sunrise,sunset)


########################################
#Re-trim data sets so that they start at or after first sunrise, and end at last time before last sunrise
# i.e. lop off partial day at end

#Trim
startTrim <- min(sun$sunrise)
endTrim <- max(sun$sunrise)
dataDO <- dataDO[dataDO$datetime >= startTrim & dataDO$datetime < endTrim,]
dataPAR <- dataPAR[dataPAR$datetime >= startTrim & dataPAR$datetime < endTrim,]
dataWind<- dataWind[dataWind$datetime >= startTrim & dataWind$datetime < endTrim,]
dataSensorTemp <- dataSensorTemp[dataSensorTemp$datetime >= startTrim & dataSensorTemp$datetime < endTrim,]
dataTempProfile <- dataTempProfile[dataTempProfile$datetime >= startTrim & dataTempProfile$datetime < endTrim,]
completeTimes <- data.frame(datetime=completeTimes[completeTimes$datetime >= startTrim & completeTimes$datetime < endTrim,],stringsAsFactors=FALSE)

#(Useful later) Vector giving which solar day each time in completeTimes belongs to
solarDaysBreaks <- unique(sun$sunrise[sun$sunrise <= endTrim])
solarDaysVec <- cut.POSIXt(completeTimes$datetime,breaks=solarDaysBreaks)


########################################
#Fill gaps in data

##
#DO - do not fill gaps

fillHoles <- function(dataIn,maxLength,timeStep){
  
  #Number of rows in dataIn
  nObs <- dim(dataIn)[1]
  
  #Express maxLength as number of time steps instead of number of minutes
  maxLength <- maxLength/timeStep
  
  #Temporarily replace NAs in data with -99999
  whichNA <- which(is.na(dataIn[,2]))
  if(length(whichNA)){
    dataIn[whichNA,2] <- -99999
  }
  #Identify strings of NA (-99999) values
  rleOut <- rle(dataIn[,2])
  which9 <- which(rleOut$values==-99999)
  
  #If no NA strings in data, return dataIn
  if (length(which9)==0)
  {
    return(dataIn)
  } else
    
    #Otherwise, continue
  {
    
    #Interpolate valus for each string of NA values
    for (i in 1:length(which9))
    {
      
      #Determine start and end index of string i, and calculate length of string
      if (which9[i]==1)
      {
        stringStart <- 1
      } else
        
      {
        stringStart <- 1 + sum(rleOut$lengths[1:(which9[i]-1)])
      }
      
      stringEnd <- sum(rleOut$lengths[1:which9[i]])
      stringLength <- stringEnd-stringStart+1
      
      #Skip to next i if:
      #  -length of string exceeds maxLength,
      #  -stringStart is the first obs in dataIn
      #  -stringEnd is the last obs in dataIn
      if (stringLength > maxLength | stringStart==1 | stringEnd==nObs) next else
      {
        
        #Linearly interpolate missing values
        interp <- approx(x=c(dataIn[stringStart-1,"datetime"],dataIn[stringEnd+1,"datetime"]),
                         y=c(dataIn[stringStart-1,2],dataIn[stringEnd+1,2]),
                         xout=dataIn[stringStart:stringEnd,"datetime"],
                         method="linear")
        dataIn[stringStart:stringEnd,2] <- interp$y
      }
    }
    
    #Replace any remaining -99999 with NA
    dataIn[which(dataIn[,2]==-99999),2] <- NA
    
    #Return result
    return(dataIn)
  }
  
}

##
#PAR - linearly interpolate gaps up to 60 min long
dataPAR <- fillHoles(dataPAR,maxLength=60,timeStep=timeStep)

##
#sensorTemp - linearly interpolate gaps up to 60 min long
dataSensorTemp <- fillHoles(dataSensorTemp,maxLength=200,timeStep=timeStep)

##
#windSpeed - fill with daily average as long as at least 80% of data are available

#Loop over days
for (i in 1:length(unique(solarDaysVec))){
  cat(i, " ")
  #Extract data between sunrise on day i and sunrise on day i+1
  timeSlice <- c(sun$sunrise[i], sun$sunrise[i+1])
  dataTemp <- dataWind[dataWind$datetime>=timeSlice[1] & dataWind$datetime<timeSlice[2],]
  
  #Determine total number of observations, and number that are NA
  nTot <- length(dataTemp$WS)
  nNA <- length(which(is.na(dataTemp$WS)))
  
  #If >20% of obs are NA, skip to next i
  if(nrow(dataTemp)>0){
    if (nNA/nTot > 0.20){
      next
    }else{
      #Calculate mean windSpeed and sub in for NA values
      meanSpeed <- mean(dataTemp$WS,na.rm=T)
      naRows <- as.numeric(row.names(dataTemp[is.na(dataTemp$WS),]))
      dataWind$WS[naRows] <- meanSpeed
    }
  }else{
    next
  }
}

##
#tempProfile - linearly interpolate gaps up to 60 min long 

nCols <- dim(dataTempProfile)[2]

#Loop over the columns of dataTempProfile
for (i in 2:nCols)
{
  dataTemp <- dataTempProfile[,c(1,i)]
  dataTemp[,2]=as.numeric(dataTemp[,2])
  dataTempFilled <- fillHoles(dataTemp,maxLength=1000000,timeStep=timeStep)	#***** set maxLength to enable long interpolations or not
  dataTempProfile[,i] <- dataTempFilled[,2]
}

########################################
#Calculate zMix and fluxDummy

#If temperature measured at only one depth, use maxZMix as zMix at every time
if (ncol(dataTempProfile) <= 2)
{
  dataZMix <- data.frame(datetime=dataTempProfile$datetime,zMix=rep(maxZMix,length(dataTempProfile$datetime)))
} else
  
  #Otherwise calculate zMix from data
{
  #Convert tempProfile data to density
  #Density of water (kg m-3) as function of temp from McCutcheon (1999)
  #Note there is a different formula if salinity is appreciable; formula below ignores that
  dataDensProfile <- dataTempProfile 
  dataDensProfile[,-1] <- 1000*(1-((dataDensProfile[,-1]+288.9414)/(508929.2*(dataDensProfile[,-1]+68.12963)))*(dataDensProfile[,-1]-3.9863)^2)
  
  #Calc zMix
  dataZMix <- calcZMixDens(dataDensProfile)
}

#Plot zMix
maxDepth <- max(as.numeric(substr(colnames(dataTempProfile)[2:nCols],5,10)))
pdf(file=paste(outName,'zMix.pdf'))
plot(zMix~datetime,data=dataZMix,ylim=c(maxDepth,0))
dev.off()

#if using fluxDummy, then...
if(fluxDummyToggle){
  #Identify when to shut off atmospheric flux
  # If zMix > sensorDepth, then sensor is in mixed layer and fluxDummy = 1
  # If zMix <= sensorDepth, then there is stratification at or above sensor and fluxDummy = 0 -- shut off atmosphere at this time step
  fluxDummy <- as.numeric(dataZMix$zMix>sensorDepth)
}else{
  fluxDummy<-rep(1,nrow(dataTempProfile))
}

########################################
#Merge data for convenience

#Merge
data1 <- merge(dataDO,dataPAR,by="datetime",all=T)
data1 <- merge(data1,dataWind,by="datetime",all=T)
data1 <- merge(data1,dataSensorTemp,by="datetime",all=T)
data1 <- merge(data1,dataZMix,by="datetime",all=T)





#Combine new & old DFs ----------------------------------------------------------------

#### 
#WEST LONG
#### 
WL_newway <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/WL_2014 GPPFitOut.txt', sep=' ')
# WL <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/MFEdb/metabModelTest/EastLong 201605to201609/WestLong_ GPPFitOut.txt', sep=' ')

WL_newway$lakeID <- 'WL_GLEON'
WL_newway <- WL_newway %>%
  mutate(solarDay = ymd(solarDay))

#Note for the optimOut.txt file, had to open in Excel and fix the column headings. 
WL_optimout_newway <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/WL_2014 optimOut.txt', sep=' ')
WL_optimout_newway$lakeID <- 'WL_GLEON'
WL_optimout_newway <- WL_optimout_newway %>%
  mutate(solarDay = ymd(solarDay))

WL_newway <- left_join(WL_newway, WL_optimout_newway, by = c("solarDay","lakeID"))


WL_both<-bind_rows(WL_newway, WL_oldway)

WL_oldway <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/WestLong_db_output_ GPPFitOut.txt', sep=' ')
# WL <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/MFEdb/metabModelTest/EastLong 201605to201609/WestLong_ GPPFitOut.txt', sep=' ')

WL_oldway$lakeID <- 'WL_db'
WL_oldway <- WL_oldway %>%
  mutate(solarDay = ymd(solarDay))

#Note for the optimOut.txt file, had to open in Excel and fix the column headings. 
WL_optimout_oldway <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/WestLong_db_output_ optimOut.txt', sep=' ')
WL_optimout_oldway$lakeID <- 'WL_db'
WL_optimout_oldway <- WL_optimout_oldway %>%
  mutate(solarDay = ymd(solarDay))

WL_oldway <- left_join(WL_oldway, WL_optimout_oldway, by = c("solarDay","lakeID"))


#### 
#MORRIS
#### 
MO_newway <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/MO_2014 GPPFitOut.txt', sep=' ')
# MO <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/MFEdb/metabModelTest/EastLong 201605to201609/WestLong_ GPPFitOut.txt', sep=' ')

MO_newway$lakeID <- 'MO_GLEON'
MO_newway <- MO_newway %>%
  mutate(solarDay = ymd(solarDay))

#Note for the optimOut.txt file, had to open in Excel and fix the column headings. 
MO_optimout_newway <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/MO_2014 optimOut.txt', sep=' ')
MO_optimout_newway$lakeID <- 'MO_GLEON'
MO_optimout_newway <- MO_optimout_newway %>%
  mutate(solarDay = ymd(solarDay))

MO_newway <- left_join(MO_newway, MO_optimout_newway, by = c("solarDay","lakeID"))

MO_oldway <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/Morris_db_output_ GPPFitOut.txt', sep=' ')

MO_oldway$lakeID <- 'MO_db'
MO_oldway <- MO_oldway %>%
  mutate(solarDay = ymd(solarDay))

#Note for the optimOut.txt file, had to open in Excel and fix the column headings. 
MO_optimout_oldway <- read.csv('/Users/solomonlab/Google Drive/Research (common)/Research/Data/R/MFE/GLEON/results/Morris_db_output_ optimOut.txt', sep=' ')
MO_optimout_oldway$lakeID <- 'MO_db'
MO_optimout_oldway <- MO_optimout_oldway %>%
  mutate(solarDay = ymd(solarDay))

MO_oldway <- left_join(MO_oldway, MO_optimout_oldway, by = c("solarDay","lakeID"))



MO_both<-bind_rows(MO_newway, MO_oldway)


# Compare dataframes ------------------------------------------------------
#### 
#WEST LONG
#### 
#do NLL estimates match?
WL_both %>%
  ggplot(aes(x=solarDay, y=nll, color=lakeID)) +
  geom_point()

# write.table(WL_both, file="GLEON/results/WL_both.txt", sep = "\t", row.names = FALSE)

#How far off are we?
WL_both_wide <- left_join(WL_newway,WL_oldway, by=c("solarDay"))

WL_both_wide %>%
  ggplot(aes(x=nll.x, y=nll.y)) +
  geom_point()+
  labs(x="WL_GLEON", y="WL_db", title="NLL") +
  geom_abline(slope=1,intercept = 0)

#do GPP estimates match?
WL_both %>%
  ggplot(aes(x=solarDay, y=GPP, color=lakeID)) +
  geom_point()

WL_both_wide %>%
  ggplot(aes(x=GPP.x, y=GPP.y)) +
  geom_point()+
  labs(x="WL_GLEON", y="WL_db", title="GPP") +
  geom_abline(slope=1,intercept = 0)

#### 
#MORRIS
#### 
#do NLL estimates match?
MO_both %>%
  ggplot(aes(x=solarDay, y=nll, color=lakeID)) +
  geom_point()

#How far off are we?
MO_both_wide <- left_join(MO_newway,MO_oldway, by=c("solarDay"))

MO_both_wide %>%
  ggplot(aes(x=nll.x, y=nll.y)) +
  geom_point()+
  labs(x="MO_GLEON", y="MO_db", title="NLL") +
  geom_abline(slope=1,intercept = 0)

#do GPP estimates match?
MO_both %>%
  ggplot(aes(x=solarDay, y=GPP, color=lakeID)) +
  geom_point()

MO_both_wide %>%
  ggplot(aes(x=GPP.x, y=GPP.y)) +
  geom_point()+
  labs(x="MO_GLEON", y="MO_db", title="GPP") +
  geom_abline(slope=1,intercept = 0)

