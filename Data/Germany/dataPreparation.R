# load all data files with german corona data

listFiles <- dir("/home/nathan/Corona/Data/Germany")
listFiles <- listFiles[grepl("BundeslandCases",listFiles)]

library(data.table)

oldDataCollection <- readRDS("/home/nathan/Corona/Data/Germany/consolidatedOldData.rds")


allSavedData <- rbindlist(lapply(listFiles, 
       function(x){
         readRDS(file.path("/home/nathan/Corona/Data/Germany",x))
       }))

allSavedData[grepl("Baden",Bundesland), Bundesland:="Baden-Württemberg"]
allSavedData[grepl("Meck",Bundesland), Bundesland:="Mecklenburg-Vorpommern"]
allSavedData[grepl("Pfalz",Bundesland), Bundesland:="Rheinland-Pfalz"]
allSavedData[grepl("Holstein",Bundesland), Bundesland:="Schleswig-Holstein"]
allSavedData[grepl("Nord",Bundesland), Bundesland:="Nordrhein-Westfalen"]
allSavedData[grepl("Bran",Bundesland), Bundesland:="Brandenburg"]
allSavedData[grepl("Nieder",Bundesland), Bundesland:="Niedersachsen"]

allSavedData[,Bundesland:=gsub("\\** *", "", Bundesland)]

allSavedData <- allSavedData[order(dateofcollection)]

# assign a day for each date of collection, as much as possible assure that there is one data point per day

allSavedData[,dateWoTime :=as.Date(dateofcollection)]
checkForMissingOrExtraData <- allSavedData[Bundesland=="Gesamt",.N, by="dateWoTime"][data.table(dateWoTime=seq.Date(from=min(allSavedData$dateWoTime), to=Sys.Date(), by="1 day")), on="dateWoTime"]
checkForMissingOrExtraData[is.na(N),N:=0L]

if (sum(checkForMissingOrExtraData$N==0L)>0L | sum(checkForMissingOrExtraData$N > 1L)>0L){
  errorDate <- checkForMissingOrExtraData[N==0 | N>1,min(dateWoTime)]
  allSavedData <- allSavedData[dateWoTime < errorDate]
  if (checkForMissingOrExtraData[dateWoTime==errorDate]$N == 0){
    errorMessage <- paste0("Data not available for ",errorDate)
  } else {
    errorMessage <- paste0("Too much data for ",errorDate)
  }
  
} else {
  errorMessage <- NULL
}

allSavedData <- rbindlist(list(allSavedData,oldDataCollection),fill = TRUE)

# calculate statistics per unit of population

populationData <- fread("/home/nathan/Corona/Data/Germany/Population/BevoelkerungBundeslaenderDez2019.csv",skip=5,nrows=16,encoding = "UTF-8")
colnames(populationData)<- c("Bundesland","Bevoelkerung20191231")
populationData[grepl("Baden",Bundesland),Bundesland:="Baden-Württemberg"]
populationData[grepl("ringen",Bundesland),Bundesland:="Thüringen"]
populationData <- rbindlist(list(populationData,populationData[,.(Bundesland="Gesamt",Bevoelkerung20191231=sum(Bevoelkerung20191231))]))

allSavedData <- merge(allSavedData,populationData,by="Bundesland")

allSavedData[,':='(FaellePro100K = Anzahl / (Bevoelkerung20191231 / 100000))]
allSavedData[,':='(Faelle7TagenPro100K = FaelleLetzten7Tagen / (Bevoelkerung20191231 / 100000))]
allSavedData[,':='(TodesfaellePro100K = Todesfaelle / (Bevoelkerung20191231 / 100000))]


# prepare the data ready for easy use in a shiny app
longTable <- melt(allSavedData, id.vars = c("Bundesland","dateWoTime","dateofcollection"), variable.factor = FALSE)

saveRDS(longTable,"/home/nathan/ShinyApps/Corona/data/timeSeries.rds")
#saveRDS(longTable,"/home/nathan/Corona/ShinyApp/data/timeSeries.rds")


simulationSIRweekday <- function(parameterList, maxPeriods = 365){
  # first step initially select the infected in each population
  
  # with recovered as input the effective susceptible population is N-R
  # can treat the population as having that size
  
  
  
  t <- 0
  I <- parameterList$I0
  R <- parameterList$R0
  N <- parameterList$N
  lastDay <- parameterList$lastDay
  
  beta <- parameterList$weekdayBeta
  delta <- parameterList$delta
  
  Icounts <- c(I,integer(maxPeriods-1L))
  Rcounts <- c(R,integer(maxPeriods-1L))
  deltaI <- c(NA_integer_,integer(maxPeriods-1L))
  
  dayTable <- data.table(dateWoTime = seq.Date(from = lastDay + 1, to = lastDay + maxPeriods, by="1 day"))
  dayTable[, weekday:=weekdays(dateWoTime)]
  
  dayTable <- merge(dayTable, beta[,.(weekday, beta)],by="weekday")[order(dateWoTime)]
  # then the looping step, at each time period collect and store counts
  
  env <- list2env(list(t=t,I=I,R=R,N=N,Icounts=Icounts,Rcounts=Rcounts,deltaI=deltaI))
  
  #simResults <- data.table(t, matrix(Icounts,nrow=1), matrix(Rcounts,nrow=1), countInfected, countRecovered)
  
  
  while (env$t < maxPeriods & env$I > 0){
    with(env, {
      
      
      # number of susceptibles at the start of the period
      S <- N - R - I
      # calculate the transition probabilities for susceptibles in each population
      infectionProbabilities <- dayTable[t+1,beta] %*% (I / N)
      # randomly assign transitions
      
      newInfections <- rbinom(1L,S,infectionProbabilities)
      newRecovered <- rbinom(1L,I,delta)
      
      I <- I + newInfections - newRecovered
      R <- R + newRecovered
      
      
      t <- t+1
      Icounts[t+1] <- I
      Rcounts[t+1] <- R
      deltaI[t+1] <- newInfections
    })
    
  }
  
  simResults <- data.table(noInfected = env$Icounts[1:(env$t +1)], noRecovered = env$Rcounts[1:(env$t +1)], newCases = env$deltaI[1:(env$t +1)])
  
  return(simResults)
}




# calculate the daily values for the model parameters

lastSevenDayParameters <- function(DT, infectiousPeriod = 14L){
  ## to test
  #DT <- timeSeries[Bundesland == "Baden-Württemberg"]
  #infectiousPeriod <- 14L
  
  DT <- DT[order(dateWoTime)]
  DT[ , ':='(weekday = weekdays(dateWoTime))]
  # need the number of infected on each day and the number of susceptibles
  wideDT <- dcast(DT, dateWoTime + dateofcollection ~ variable) 
  
  # count the sum of new cases over the last #infectiousPeriod days, to give the 
  # number of infected
  
  lastDay <- wideDT[,max(dateWoTime)]
  lastSevenDays_plusOne <- data.table(lastWeek=seq.Date(lastDay - 6, lastDay+1, by=1))
  
  lastSevenDays_plusOne <- lastSevenDays_plusOne[,.(infectiousPeriod=seq.Date(lastWeek-infectiousPeriod,to=lastWeek-1,by=1)),keyby=lastWeek]
  infectiousCases <- merge(lastSevenDays_plusOne, wideDT[,.(dateWoTime,DifferenzZumVortag)],by.x = "infectiousPeriod",by.y = "dateWoTime")
  
  infectiousCases <- infectiousCases[,.(infectiousCases = sum(DifferenzZumVortag)),keyby=.(lastWeek)]
  
  betaCalc <- merge(wideDT[ , .(dateWoTime, DifferenzZumVortag) ], infectiousCases[lastWeek <= lastDay], by.x="dateWoTime", by.y = "lastWeek")
  betaCalc[ , ':='(beta=(DifferenzZumVortag / infectiousCases),
                   weekday = weekdays(dateWoTime))]
  
  
  I0 <- infectiousCases[lastWeek==max(lastWeek),infectiousCases]
  R0 <- wideDT[dateWoTime == lastDay,Anzahl] - I0
  N <- wideDT[dateWoTime == lastDay,Bevoelkerung20191231]
  
  # take the calculated probabilities and project forward
  parameterList <- list(
    I0 = I0,
    R0 = R0,
    N = N,
    weekdayBeta = betaCalc,
    lastDay = lastDay,
    delta = 1/infectiousPeriod
  )
  
  anzahlFaelle <- wideDT[dateWoTime == lastDay, Anzahl]
  
  simData <- simulationSIRweekday(parameterList, 180)
  
  simData <- simData[-1]
  simData[,':='(dateWoTime = seq.Date(lastDay+1,length.out = nrow(simData),by="1 day"),
                Anzahl = anzahlFaelle + cumsum(newCases))]
  
  setnames(simData,"newCases","DifferenzZumVortag")
  simData[,DifferenzZumVortag:=as.numeric(DifferenzZumVortag)]
  
  simData <- melt(simData, id.vars = c("dateWoTime"), variable.factor = FALSE, verbose=FALSE)
  simData[,':='(DataType = "Simuliert")]
  
  # merge with actual data and create a column that separates simulated from real
  realData <- DT[
    variable %in% c("Anzahl", "DifferenzZumVortag"),.(dateWoTime,variable,value)]
  realData[,':='(DataType = "Real")]
  
  allData <- rbindlist(list(realData, simData), use.names = TRUE, fill = TRUE)
  
}

simResults <- longTable[ , lastSevenDayParameters(.SD),keyby="Bundesland"]

saveRDS(simResults,"/home/nathan/ShinyApps/Corona/data/simResults.rds")
#saveRDS(simResults,"/home/nathan/Corona/ShinyApp/data/simResults.rds")

