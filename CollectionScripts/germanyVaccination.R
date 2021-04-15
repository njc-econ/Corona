

library("rvest")
library("data.table")
impfungPage <- read_html("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Impfquotenmonitoring.html")
url <- paste0("https://www.rki.de",html_attr(html_nodes(impfungPage,css = 'a.more'),"href"))

forstamp <- Sys.time()
savename <- paste0("~/Corona/Data/Germany/Vaccination/Impfquotenmonitoring",year(forstamp),"_",month(forstamp),"_",as.integer(format(forstamp, "%d")),"_",format(forstamp,"%H%M%S"),".xlsx")
download.file(url, savename, method = "curl")

library("openxlsx")
ImpfungDataNew <- read.xlsx(savename, sheet = 2, colNames = FALSE)

savedata <- TRUE
oldDataName <- paste0("~/Corona/Data/Germany/Vaccination/lastNewDownloadImpfung",".xlsx")

ImpfungDataOld <- read.xlsx(oldDataName, sheet = 2, colNames = FALSE)
if (identical(ImpfungDataNew, ImpfungDataOld)){
  savedata <- FALSE  
}

if(savedata){
  file.copy(savename,oldDataName,overwrite=TRUE)
  source("~/Corona/VaccinationExtrapolation.R")
} else {
  file.remove(savename)
}



