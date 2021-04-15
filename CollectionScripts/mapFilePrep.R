longTable <- readRDS("/home/nathan/ShinyApps/Corona/data/timeSeries.rds")
germanyBundeslandMap <- readRDS("/home/nathan/Maps/Germany/gadm36_DEU_1_sp.rds")

library(ggplot2)
library(data.table)
library(sp)

series <- "Anzahl"
date <- as.Date("2020-08-07")

theme_nothing <- function (base_size = 12, legend = FALSE) 
{
  if (legend) {
    theme(axis.text = element_blank(), axis.title = element_blank(), 
          panel.background = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.ticks.length = unit(0, 
                                                                       "cm"), panel.spacing = unit(0, "lines"), plot.margin = unit(c(0, 
                                                                                                                                     0, 0, 0), "lines"))
  }
  else {
    theme(line = element_blank(), rect = element_blank(), 
          text = element_blank(), axis.ticks.length = unit(0, 
                                                           "cm"), legend.position = "none", panel.spacing = unit(0, 
                                                                                                                 "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines"))
  }
}



mapData <- fortify(germanyBundeslandMap, region = "NAME_1")

baseMap <- ggplot(mapData, aes(x=long, y=lat, group=group)) + coord_map() + theme_nothing(legend=TRUE) +
  geom_path() +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=12)) + annotate("text",label="Quelle: Robert Koch-Institut; GADM",x=11,y=47)

saveRDS(list(mapData=mapData, baseMap=baseMap), "/home/nathan/ShinyApps/Corona/data/spatialInputs.rds")


selectedData <- longTable[variable==series & dateWoTime == date & Bundesland != "Gesamt"]

maxValue <- max(selectedData$value, na.rm = TRUE)
minValue <- min(selectedData$value, na.rm = TRUE)

completeMapData <- merge(selectedData, mapData, by.x="Bundesland", by.y="id")

baseMap +
  scale_fill_gradient(limits = c(minValue, maxValue), name = series, low="white", high="dark blue") +
  geom_polygon(data = completeMapData[Bundesland %in% completeMapData[hole]$Bundesland], aes(x=long, y=lat, group=group, fill = value),color="red") +
  geom_polygon(data = completeMapData[!(Bundesland %in% completeMapData[hole]$Bundesland)], aes(x=long, y=lat, group=group, fill = value),color="red")
  
