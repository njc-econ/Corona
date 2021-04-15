
library(data.table)
importData <- fread("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data", fill = TRUE)

# to get the number of new cases to match reported (Differenz zum Vortag)
importData[NeuerFall!=0L,sum(AnzahlFall), keyby="Bundesland"]

# to get the number of total cases
importData[,sum(AnzahlFall), keyby="Bundesland"]
# to get the number of deaths
importData[,sum(AnzahlTodesfall), keyby="Bundesland"]


importData[,':='(Meldedatum=as.POSIXct(Meldedatum),
                 Refdatum = as.POSIXct(Refdatum))]
importData[,.(daysDiff = difftime(Refdatum, Meldedatum, units = "days"))][,.N,keyby=daysDiff]
