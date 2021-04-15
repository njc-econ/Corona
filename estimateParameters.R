
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
  DT <- timeSeries[Bundesland == "Baden-Württemberg"]
  infectiousPeriod <- 14L
  
  DT[ , ':='(weekday = weekdays(dateWoTime))]
  # need the number of infected on each day and the number of susceptibles
  wideDT <- dcast(DT,Bundesland + dateWoTime + dateofcollection ~ variable) 
  
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

