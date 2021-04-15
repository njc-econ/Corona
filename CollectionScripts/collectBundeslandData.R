Sys.sleep(10)
setwd("~/Corona/Data/Germany")
library("rvest")
library(data.table)

library('RSelenium')
remDr<-remoteDriver(remoteServerAddr = "localhost",
                    port = 4445L,
                    browserName = "chrome")
remDr$open()

isOpened <- try(remDr$open())

attempt <- 1

if (class(isOpened)=="try-error"){
  # try to open a connection up to ten times 
  
  while (class(isOpened)=="try-error" & attempt <= 10){
    isOpened <- try(remDr$open())
    attempt <- attempt + 1
  }
}

if (class(isOpened)=="try-error" & attempt > 10){
  stop("No connection with docker engine")
} else {goodToGo <- TRUE}



rkicasesbybundesland <- "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html"

remDr$navigate(rkicasesbybundesland)

Sys.sleep(10)



wholepage <- read_html(remDr$getPageSource()[[1]])
forstamp <- Sys.time()
casedata <- html_table(wholepage)[[1]]

savedata <- TRUE

if ("lastImport.rds" %in% list.files()){
  casedataOld <- readRDS("lastImport.rds")
  if (identical(casedata, casedataOld)){
    savedata <- FALSE  
  }
} 

if (savedata){
  saveRDS(casedata,"lastImport.rds")  
  colnames(casedata)[2:6]<-casedata[1,2:6]
  
  
  colnames(casedata) <- c("Bundesland",
                          "Anzahl",
                          "DifferenzZumVortag",
                          "FaelleLetzten7Tagen",
                          "7TageInzidenz",
                          "Todesfaelle"
  )
  
  casedata <- casedata[-c(1), ]
  casedata <- data.table(casedata)
  casedata[ ,':='(Anzahl = as.integer(gsub("\\.","",Anzahl)),
                  DifferenzZumVortag = as.integer(gsub("+", "", gsub("\\.", "", gsub("\\*","",DifferenzZumVortag)))),
                  FaelleLetzten7Tagen = as.integer(gsub(",", ".", gsub("\\.", "",FaelleLetzten7Tagen))),
                  `7TageInzidenz` = as.numeric(gsub(",", ".", gsub("\\.", "",`7TageInzidenz`))),
                  Todesfaelle = as.integer(gsub("\\.","",Todesfaelle)))]
  
  casedata[ ,':='(dateofcollection = forstamp)]
  saveRDS(casedata, paste0("BundeslandCases",year(forstamp),"_",month(forstamp),"_",as.integer(format(forstamp, "%d")),"_",format(forstamp,"%H%M%S"),".rds"))
  source("/home/nathan/Corona/Data/Germany/dataPreparation.R")
}

