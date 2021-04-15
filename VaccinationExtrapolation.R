# extrapolate vaccination data prior to 15th February

library(openxlsx)

Bundesland_15thFeb <- read.xlsx("Corona/Data/Germany/Vaccination/Impfquotenmonitoring2021_2_15_123348.xlsx", sheet = 2)
TimeSeries_15thFeb <- read.xlsx("Corona/Data/Germany/Vaccination/Impfquotenmonitoring2021_2_15_123348.xlsx", sheet = 4)

TimeSeries_15thFeb$Datum <- convertToDate(TimeSeries_15thFeb$Datum)
TimeSeries_15thFeb$TotalGeimpft <- c(cumsum(TimeSeries_15thFeb$Erstimpfung[-51]),NA_real_)

Bundesland_Erstimpfung <- Bundesland_15thFeb[c(3:(nrow(Bundesland_15thFeb)-1)),c(2,4,8)]

library(data.table)

Bundesland_Erstimpfung <- data.table(Bundesland_Erstimpfung)
TimeSeries_15thFeb <- data.table(TimeSeries_15thFeb)
setnames(Bundesland_Erstimpfung, c("X8"),c("ErstimpfungDifferenzZumVortag"))

Bundesland_Erstimpfung[,':='(Erstimpfung = as.numeric(Erstimpfung),
                             ErstimpfungDifferenzZumVortag = as.numeric(ErstimpfungDifferenzZumVortag))]

Bundesland_Erstimpfung[,':='(PZ_Deutschland = Erstimpfung / Bundesland_Erstimpfung[Bundesland=="Gesamt",Erstimpfung])]

# use PZ for Bundesland and the overall time series to create a time series for the bundesland prior to 15th February

TimeSeriesCreate <- TimeSeries_15thFeb[!is.na(Datum),.(Bundesland=Bundesland_Erstimpfung$Bundesland, PZGeimpft=Bundesland_Erstimpfung$PZ_Deutschland),keyby=.(Datum,Erstimpfung)]
TimeSeriesCreate[,':='(Erstimpfung = round(Erstimpfung * PZGeimpft))]

TimeSeriesCreate[ ,':='(GesamtErstimpfung=cumsum(Erstimpfung)),keyby="Bundesland"]
TimeSeriesCreate[ ,':='(PZGeimpft = NULL)]
TimeSeriesCreate <- TimeSeriesCreate[Datum != as.Date("2021-02-14")]


setnames(TimeSeriesCreate, "Erstimpfung", "Differenz_zum_Vortag")


saveRDS(TimeSeriesCreate, "Corona/Data/Germany/Vaccination/ExtrapolatedData_Erstimpfung.rds")


collectDailyData <- function(excelName){
  excelName <- paste0("Corona/Data/Germany/Vaccination/",excelName)
  
  
  TimeSeries <- read.xlsx(excelName, sheet = 4)
  TimeSeries$Datum <- convertToDate(TimeSeries$Datum)
  TimeSeries <- data.table(TimeSeries)
  statusDate <- TimeSeries[,max(Datum, na.rm=TRUE)]
  
  
  if (statusDate < as.Date("2021-04-07")){
    Bundesland <- read.xlsx(excelName, sheet = 2)
    Bundesland_Erstimpfung_Data <- Bundesland[c(3:(nrow(Bundesland)-1)),c(2,4,8)]
    Bundesland_Erstimpfung_Data <- data.table(Bundesland_Erstimpfung_Data)
    
    setnames(Bundesland_Erstimpfung_Data, c("X8"),c("ErstimpfungDifferenzZumVortag"))
    Bundesland_Erstimpfung_Data[, Datum := statusDate]
    Bundesland_Erstimpfung_Data <- Bundesland_Erstimpfung_Data[,.(Datum, Differenz_zum_Vortag = ErstimpfungDifferenzZumVortag, Bundesland, GesamtErstimpfung = Erstimpfung)]
    
    Bundesland_Erstimpfung_Data[,':='(Differenz_zum_Vortag=as.numeric(Differenz_zum_Vortag),
                                      GesamtErstimpfung=as.numeric(GesamtErstimpfung))]
  } else {
    Bundesland <- read.xlsx(excelName, sheet = 3)
    Bundesland_Erstimpfung_Data <- Bundesland[c(4:(nrow(Bundesland)-3)),c(2,3,7,13,18)]
    Bundesland_Erstimpfung_Data <- data.table(Bundesland_Erstimpfung_Data)
    
    cols <- c("GesamtImpfungNichtArztPraxis","ErstimpfungDifferenzNichtArzt",
              "GesamtImpfungArztPraxis","ErstimpfungDifferenzArzt")
    setnames(Bundesland_Erstimpfung_Data, c(2,3,4,5),cols)
    
    Bundesland_Erstimpfung_Data[,':='(
      GesamtImpfungNichtArztPraxis = as.numeric(GesamtImpfungNichtArztPraxis),
      ErstimpfungDifferenzNichtArzt = as.numeric(ErstimpfungDifferenzNichtArzt),
      GesamtImpfungArztPraxis = as.numeric(GesamtImpfungArztPraxis),
      ErstimpfungDifferenzArzt = as.numeric(ErstimpfungDifferenzArzt)
    )]
    # set missing values to zero
    Bundesland_Erstimpfung_Data[ , (cols):= lapply(.SD, function(x){x[is.na(x)] <- 0; return(x)}), .SDcols=cols]
    
    Bundesland_Erstimpfung_Data[,':='(Differenz_zum_Vortag = ErstimpfungDifferenzNichtArzt + ErstimpfungDifferenzArzt,
                                      GesamtErstimpfung = GesamtImpfungNichtArztPraxis + GesamtImpfungArztPraxis,
                                      ErstimpfungDifferenzNichtArzt = NULL,
                                      ErstimpfungDifferenzArzt = NULL,
                                      GesamtImpfungNichtArztPraxis = NULL,
                                      GesamtImpfungArztPraxis = NULL)]
    Bundesland_Erstimpfung_Data[, Datum := statusDate]
  }  
    
  return(Bundesland_Erstimpfung_Data)
}

listFiles <- dir("/home/nathan/Corona/Data/Germany/Vaccination")
listFiles <- listFiles[grepl("Impfquotenmonitoring",listFiles)]


#dailyData <- list()
#for (i in 1:length(listFiles)){
#  dailyData[[i]] <- collectDailyData(listFiles[[i]])
#}

dailyData <- rbindlist(lapply(listFiles,collectDailyData), use.names = TRUE)

impfungData <- rbindlist(list(TimeSeriesCreate,dailyData),use.names = TRUE)[order(Bundesland,Datum)]
# calculate statistics per unit of population

populationData <- fread("/home/nathan/Corona/Data/Germany/Population/BevoelkerungBundeslaenderDez2019.csv",skip=5,nrows=16,encoding = "UTF-8")
colnames(populationData)<- c("Bundesland","Bevoelkerung20191231")
populationData[grepl("Baden",Bundesland),Bundesland:="Baden-Württemberg"]
populationData[grepl("ringen",Bundesland),Bundesland:="Thüringen"]
populationData <- rbindlist(list(populationData,populationData[,.(Bundesland="Gesamt",Bevoelkerung20191231=sum(Bevoelkerung20191231))]))

impfungData <- merge(impfungData,populationData,by="Bundesland")

impfungData[,':='(ErstimpfungPZ = GesamtErstimpfung / (Bevoelkerung20191231))]


# data is not published on Sunday, so the daily data at Bundesland level are not directly available, but can be reconstructed using the
# totals from the days either side and the "Differenz vom Vortag"

allDates <- seq.Date(from=min(impfungData$Datum),to=max(impfungData$Datum),by="1 day")
missingDates <- allDates[!(allDates %in% impfungData$Datum)]

missingDates <- missingDates[order(missingDates, decreasing = TRUE)]

while (length(missingDates) > 0){
  fillInData <- function(missingdate){
    # find the data from the day after extrapolate
    newData <- impfungData[Datum == missingdate + 1]
    newData[ ,':='(Datum = Datum -1,
                   GesamtErstimpfung = GesamtErstimpfung - Differenz_zum_Vortag,
                   ErstimpfungPZ = GesamtErstimpfung / Bevoelkerung20191231,
                   Differenz_zum_Vortag = NA_real_)]
    newData[ ,':='(
      ErstimpfungPZ = GesamtErstimpfung / Bevoelkerung20191231
    )]
    oldDataDate <- max(impfungData$Datum[impfungData$Datum < missingdate])
    oldData <- impfungData[Datum == oldDataDate]
    
    daysMissing <- missingdate - oldDataDate
    
    #if (daysMissing == 1){
    newData <- merge(newData, oldData[,.(Bundesland,oldGesamtErstimpfung=GesamtErstimpfung)], by="Bundesland")
    newData[,':='(Differenz_zum_Vortag = (GesamtErstimpfung - oldGesamtErstimpfung)/as.integer(daysMissing))]
    newData[,oldGesamtErstimpfung:=NULL]
    #}
    return(newData)
  }
  
  impfungData <- rbindlist(list(impfungData, fillInData(missingDates[1])))
  missingDates <- allDates[!(allDates %in% impfungData$Datum)]
  
  missingDates <- missingDates[order(missingDates, decreasing = TRUE)]
}






impfungData <- melt(impfungData, id.vars = c("Bundesland","Datum"), variable.factor = FALSE)
saveRDS(impfungData,"/home/nathan/ShinyApps/Corona/data/impfungData.rds")
