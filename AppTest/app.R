sourceData <- readRDS("data/timeSeries.rds")
mapData <- readRDS("data/spatialInputs.rds")
simData <- readRDS("data/simResults.rds")
impfungData <- readRDS("data/impfungData.rds")

bundeslandList <- unique(sourceData$Bundesland)
bundeslandList <- bundeslandList[!(bundeslandList=="Gesamt")]
bundeslandList <- c("Gesamt",bundeslandList[order(bundeslandList)])

mainColours <- c("#1642AD","#D72727","#5f6063")

library(ggplot2)
library(data.table)
library(gridExtra)
library(plotly)


sourceData[variable=="Anzahl",choiceNumber:=2L]
sourceData[variable=="FaelleLetzten7Tagen",choiceNumber:=1L]
sourceData[variable=="Todesfaelle",choiceNumber:=3L]
sourceData[variable=="FaellePro100K",choiceNumber:=4L]
sourceData[variable=="Faelle7TagenPro100K",choiceNumber:=5L]
sourceData[variable=="TodesfaellePro100K",choiceNumber:=6L]
impfungData[variable=="GesamtErstimpfung",choiceNumber:=7L]
impfungData[variable=="ErstimpfungPZ",choiceNumber:=8L]


options(scipen=80000000)

# calcualte I0 and R0 for the simulation

latestDay <- sourceData[,max(dateWoTime)]
latestDayImpfung <- impfungData[,max(Datum)]




simulationSIR <- function(parameterList, maxPeriods = 365){
  # first step initially select the infected in each population
  
  # with recovered as input the effective susceptible population is N-R
  # can treat the population as having that size
  
  t <- 0
  I <- parameterList$I0
  R <- parameterList$R0
  N <- parameterList$N
  
  betaMat <- parameterList$betaMat
  delta <- parameterList$delta
  
  Icounts <- c(I,integer(maxPeriods-1L))
  Rcounts <- c(R,integer(maxPeriods-1L))
  deltaI <- c(NA_integer_,integer(maxPeriods-1L))  
  # then the looping step, at each time period collect and store counts
  
  env <- list2env(list(t=t,I=I,R=R,N=N,Icounts=Icounts,Rcounts=Rcounts,deltaI=deltaI))
  
  #simResults <- data.table(t, matrix(Icounts,nrow=1), matrix(Rcounts,nrow=1), countInfected, countRecovered)
  
  
  while (env$t < maxPeriods & env$I > 0){
    with(env, {
      
      
      # number of susceptibles at the start of the period
      S <- N - R - I
      # calculate the transition probabilities for susceptibles in each population
      infectionProbabilities <- betaMat %*% (I / N)
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




ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  titlePanel("Corona Monitoring"),
  
  #mainPanel(
    
    tags$style(HTML(paste0(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: ",mainColours[1],"}"))),
    
    sliderInput(inputId = "statusDate",
                label = "Tag",
                min=min(sourceData$dateWoTime),
                max=max(sourceData$dateWoTime),
                value = max(sourceData$dateWoTime)),
    
    fluidRow(column(4,
                    h4("Neue Fälle letzter Tag"),
                    h3(textOutput("newCasesLastDay"))),
             column(4,
                    h4("Neue Fälle letzte 7 Tage"),
                    h3(textOutput("newCasesLastSevenDays"))),
             column(4,
                    h4("Gesamtfälle \n"),
                    h3(textOutput("TotalCases")))),
  fluidRow(
             column(4,
                    h4("Todesfälle \n"),
                    h3(textOutput("TotalDeaths"))),
             column(4,
                    h4("Anzahl Geimpfte"),
                    h3(textOutput("TotalVaccinated"))),
             column(4,
                    h4("Geimpfte PZ"),
                    h3(textOutput("VaccinatedPC"))),
             ),
    
    selectInput(inputId = "mapDisplayInfo",
                label = "Datentyp",
                choices = c("Neue Fälle letzte 7 Tage"=1L,
                            "Gesamtfälle" = 2L,
                            "Todesfälle" = 3L,
                            "Fälle pro 100K Bewohner"=4L,
                            "Fälle letzte 7 Tage pro 100K Bewohner"=5L,
                            "Todesfälle pro 100K Bewohner" = 6L,
                            "Anzahl Geimpfte" = 7L,
                            "Geimpfte: PZ der Bevolkerung"=8L
                            
                            )),
    
    
    
    fluidRow(
      column(9,
             plotOutput(outputId = "GermanyMap")),
      column(3,
             tableOutput(outputId = "OrderedTable"))
    )
    ,
    
    fluidRow(column(6,
                    selectInput(inputId = "Bundesland",
                                label = "Bundesland",
                                choices = bundeslandList)),
             column(6,selectInput(inputId = "Vergleich",
                                  label = "Vergleich",
                                  choices = c("Keiner", bundeslandList)))),
    
    plotOutput(outputId = "selectedBundeslandTotalCases", height = "950px"),
    
  #### Add in simulation of how it goes if like the last week
  
  h2("Wenn es weiter geht wie in der letzten Woche..."),
  fluidRow(column(6,selectInput(inputId = "BundeslandSim1",
                                label = "Bundesland",
                                choices = bundeslandList))),
  
  plotOutput(outputId = "simulationGraphs1"),         
  
  
  #### Add in simulation of future for particular bundesland
    h2("Epidemie Weiterentwicklung: Simulation"),
    fluidRow(column(6, tags$p("Zwei Faktoren sind kritisch für die Entwicklung einer Pandemie, wie oft Menschen potenzielle ansteckende Kontakte haben und wie lang infizierte Menschen ansteckend sind. Social Distancing beeinflusst das Erste und die Corona-Warn-App kann das Zweite beeinflussen. Hier kann man sehen, wie Änderungen in diesen Faktoren die Weiterentwicklung des Virus beeinflussen könnten.")))
    ,
    fluidRow(column(6,selectInput(inputId = "BundeslandSim",
                                  label = "Bundesland",
                                  choices = bundeslandList)),
             column(3,numericInput(inputId = "SIRBeta", value = 0.09, min = 0, label = "Kontakte pro Person pro Tag", step=0.01)),
             column(3,numericInput(inputId = "SIRDelta",value=13L, min=0, label = "Infektionsperiode (Tage)"))),

    plotOutput(outputId = "simulationGraphs")
  #)
  
  
)


server <- function(input, output){
  
  
  
  output$newCasesLastDay <- renderText({format(sourceData[variable=="DifferenzZumVortag" &
                                                     dateWoTime == input$statusDate & Bundesland =="Gesamt",value],big.mark = ".",decimal.mark = ",")})
  
  output$newCasesLastSevenDays <- renderText({format(sourceData[variable=="FaelleLetzten7Tagen" &
                                                           dateWoTime == input$statusDate & Bundesland =="Gesamt",value],big.mark = ".",decimal.mark = ",")})
  
  output$TotalCases <- renderText({format(sourceData[variable=="Anzahl"&
                                                dateWoTime == input$statusDate & Bundesland =="Gesamt",value],big.mark = ".",decimal.mark = ",")})
  
  output$TotalDeaths <- renderText({format(sourceData[variable=="Todesfaelle"&
                                                   dateWoTime == input$statusDate & Bundesland =="Gesamt",value],big.mark = ".",decimal.mark = ",")})
  output$TotalVaccinated <- renderText({format(impfungData[variable=="GesamtErstimpfung" & 
                                                             Datum == min(input$statusDate-1,latestDayImpfung) & Bundesland =="Gesamt",value],big.mark = ".",decimal.mark = ",")})
  output$VaccinatedPC <- renderText({format(round(impfungData[variable=="ErstimpfungPZ" & 
                                                             Datum == min(input$statusDate-1,latestDayImpfung) & Bundesland =="Gesamt",value]*100,2),big.mark = ".",decimal.mark = ",",nsmall=2)})
  
  
  
  
  
  dataInput <- reactive({
    if (input$mapDisplayInfo <= 6){
      sourceData[choiceNumber == input$mapDisplayInfo & Bundesland!="Gesamt" &
                   dateWoTime == input$statusDate]
    } else {
      impfungData[choiceNumber == input$mapDisplayInfo & Bundesland!="Gesamt" &
                    Datum == min(input$statusDate-1,latestDayImpfung),.(Bundesland, value=value*(1+99*(as.numeric(input$mapDisplayInfo==8))))]
    }
  })
  
  output$GermanyMap <- renderPlot({
    
    maxValue <- max(dataInput()$value, na.rm = TRUE)
    minValue <- min(dataInput()$value, na.rm = TRUE)
    
    if (input$mapDisplayInfo == 1L){seriesname <- "Neue Fälle \nletzte 7 Tage"}
    if (input$mapDisplayInfo == 2L){seriesname <- "Gesamtfälle"}
    if (input$mapDisplayInfo == 3L){seriesname <- "Todesfälle"}
    if (input$mapDisplayInfo == 4L){seriesname <- "Fälle pro 100K Bewohner"}
    if (input$mapDisplayInfo == 5L){seriesname <- "Fälle letzte 7 Tage\n pro 100K Bewohner"}
    if (input$mapDisplayInfo == 6L){seriesname <- "Todesfälle pro \n 100K Bewohner"}
    if (input$mapDisplayInfo == 7L){seriesname <- "Anzahl Geimpfte"}
    if (input$mapDisplayInfo == 8L){seriesname <- "Geimpfte PZ"}
    
    
    
    completeMapData <- merge(dataInput(), mapData[[1]], by.x="Bundesland", by.y="id")
    
    p <- mapData[[2]] +
      scale_fill_gradient(limits = c(minValue, maxValue), name = seriesname, low="white", high=mainColours[2]) +
      geom_polygon(data = completeMapData[Bundesland %in% completeMapData[hole]$Bundesland], aes(x=long, y=lat, group=group, fill = value),color=mainColours[1]) +
      geom_polygon(data = completeMapData[!(Bundesland %in% completeMapData[hole]$Bundesland)], aes(x=long, y=lat, group=group, fill = value),color=mainColours[1])
    
    p
    
  })
  
  output$OrderedTable <- renderTable({
    
    
    if (input$mapDisplayInfo < 4L | input$mapDisplayInfo == 7){
      outputData <- dataInput()[order(value, decreasing = TRUE),
                                .(Bundesland, value=format(value, big.mark = ".",decimal.mark = ","))]
    } else if (input$mapDisplayInfo < 8){
      outputData <- dataInput()[order(value, decreasing = TRUE),
                                .(Bundesland, value=formatC(value, digits=1L, format="f",big.mark = ".", decimal.mark = "," ))]
    } else {
      outputData <- dataInput()[order(value, decreasing = TRUE),
                                .(Bundesland, value=formatC(value, digits=2L, format="f",big.mark = ".", decimal.mark = "," ))]
    }
    
    
    if (input$mapDisplayInfo == 1L){colnames(outputData)[2]<-"Neue Fälle letzte 7 Tage"}
    if (input$mapDisplayInfo == 2L){colnames(outputData)[2]<-"Gesamtfälle"}
    if (input$mapDisplayInfo == 3L){colnames(outputData)[2]<-"Todesfälle"}
    if (input$mapDisplayInfo == 4L){colnames(outputData)[2] <- "Fälle pro 100K Bewohner"}
    if (input$mapDisplayInfo == 5L){colnames(outputData)[2] <- "Fälle letzte 7 Tage\n pro 100K Bewohner"}
    if (input$mapDisplayInfo == 6L){colnames(outputData)[2] <- "Todesfälle pro \n 100K Bewohner"}
    if (input$mapDisplayInfo == 7L){colnames(outputData)[2] <- "Anzahl Geimpfte"}
    if (input$mapDisplayInfo == 8L){colnames(outputData)[2] <- "Geimpfte PZ"}
    
    outputData
  })
  
  output$selectedBundeslandTotalCases <- renderPlot({
    
    baseData <- sourceData[Bundesland %in% input$Bundesland]
    baseImpfung <- impfungData[Bundesland %in% input$Bundesland]
    if (input$Vergleich != "Keiner"){
      baseData <- rbindlist(list(baseData, sourceData[Bundesland %in% input$Vergleich]))
      baseImpfung <- rbindlist(list(baseImpfung,impfungData[Bundesland %in% input$Vergleich]))
    }
    
    
    
    plotOne<- ggplot(baseData[variable == "Anzahl"],
           aes(x=dateWoTime, y=value, col=Bundesland)) +
      geom_line() + xlab("") + ylab("Anzahl Fälle") + labs(title = "Gesamtfälle") +
      #scale_x_date(date_labels = "%d %b")+
      theme_minimal()+
      scale_color_manual(values=mainColours) +
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    plotTwo <- ggplot(baseData[variable == "DifferenzZumVortag" & !is.na(value)],
                      aes(x=dateWoTime, y=value, col=Bundesland, fill=Bundesland)) +
      geom_bar(stat="identity", position = position_dodge2(preserve="single")) + xlab("") + ylab("Anzahl Fälle") + labs(title = "Fälle pro Tag")+ 
      theme_minimal()+
      scale_fill_manual(values = mainColours)+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    
    
    plotThree <- ggplot(baseData[ variable == "Todesfaelle"],
                      aes(x=dateWoTime, y=value, col=Bundesland)) +
      geom_line() + xlab("") + ylab("Anzahl Todesfälle")+ labs(title = "Gesamte Todesfälle")+
      theme_minimal()+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    
    plotFour <- ggplot(baseData[variable == "Faelle7TagenPro100K"],
                       aes(x=dateWoTime, y=value, col = Bundesland)) +
      geom_line() + xlab("") + labs(title="Fälle letzte 7 Tage pro 100K Bewohner ")+ylab("7 Tage Inzidenz")+
      theme_minimal()+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    
    plotFive <- ggplot(baseData[variable == "FaellePro100K"],
                       aes(x=dateWoTime, y=value, col = Bundesland)) +
      geom_line() + xlab("") + labs(title="Fälle pro 100K Bewohner ")+ylab("Fälle pro 100K")+
      theme_minimal()+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))  
    
    
    
    plotSix <- ggplot(baseData[variable == "TodesfaellePro100K"],
                       aes(x=dateWoTime, y=value, col = Bundesland)) +
      geom_line() + xlab("") + labs(title="Anzahl Todesfälle pro 100K Bewohner ")+ylab("Todesfälle pro 100K")+
      theme_minimal()+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    
    plotSeven <- ggplot(baseImpfung[variable == "GesamtErstimpfung"],
                      aes(x=Datum, y=value, col = Bundesland)) +
      geom_line() + xlab("") + labs(title="Anzahl Geimpfte ")+ylab("Anzahl Geimpfte")+
      theme_minimal()+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    
    plotEight <- ggplot(baseImpfung[variable == "ErstimpfungPZ"],
                        aes(x=Datum, y=value, col = Bundesland)) +
      geom_line() + xlab("") + labs(title="PZ Geimpfte ")+ylab("PZ Geimpfte")+
      theme_minimal()+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) paste(format(round(100*x,2), big.mark = ".", decimal.mark = ",", scientific = FALSE),"%"))
    
    grid.arrange(plotOne, plotTwo, plotThree, plotFour, plotFive, plotSix,plotSeven,plotEight, ncol=1)
  })
  
  
  
  # plot for simulation results
  simulationData <- reactive({
    startParams <- merge(sourceData[Bundesland == input$BundeslandSim & variable == "DifferenzZumVortag"
                                        & dateWoTime >= (latestDay - round(input$SIRDelta,0)),.(I0=sum(value)),keyby="Bundesland"],
                             sourceData[Bundesland == input$BundeslandSim & variable == "Anzahl" & dateWoTime == latestDay,.(Bundesland, AnzahlFaelle = value)],
                             by = "Bundesland")[,':='(R0 = AnzahlFaelle - I0)]
    
    startParams <- merge(startParams, 
                             sourceData[Bundesland == input$BundeslandSim & variable == "Bevoelkerung20191231" & dateWoTime == latestDay, .(Bundesland, N = value)],
                             by = "Bundesland")
    
    simData <- simulationSIR(parameterList = list(I0 = startParams$I0,
                                       R0 = startParams$R0,
                                       N = startParams$N,
                                       betaMat = matrix(input$SIRBeta),
                                       delta = 1/input$SIRDelta),
                            maxPeriods = 180L)
    simData <- simData[-1]
    simData[,':='(dateWoTime = seq.Date(latestDay+1,length.out = nrow(simData),by="1 day"),
                  Anzahl = startParams$AnzahlFaelle + cumsum(newCases))]
    
    setnames(simData,"newCases","DifferenzZumVortag")
    simData <- melt(simData, id.vars = c("dateWoTime"), variable.factor = FALSE, verbose=FALSE)
    simData[,':='(DataType = "Simuliert")]
    
    # merge with actual data and create a column that separates simulated from real
    realData <- sourceData[Bundesland == input$BundeslandSim & 
                 variable %in% c("Anzahl", "DifferenzZumVortag")]
    realData[,':='(DataType = "Real")]
    
    allData <- rbindlist(list(realData, simData), use.names = TRUE, fill = TRUE)
  })
  
  output$simulationGraphs1 <- renderPlot({
    
    plotOne <- ggplot(simData[variable == "Anzahl" & Bundesland == input$BundeslandSim1], aes(x=dateWoTime, y =value, col = DataType)) +
      geom_line() + xlab("") + ylab("Anzahl Fälle") + labs(title = "Gesamtfälle")+
      scale_x_date(date_labels = "%b %y")+
      theme_minimal()+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    plotTwo <- ggplot(simData[variable == "DifferenzZumVortag" & Bundesland == input$BundeslandSim1], aes(x=dateWoTime, y =value, col = DataType, fill = DataType)) +
      geom_bar(stat="identity", position = position_dodge2(preserve="single")) + xlab("") + ylab("Anzahl Fälle") + labs(title = "Fälle pro Tag")+
      scale_x_date(date_labels = "%b %y")+
      theme_minimal()+
      scale_fill_manual(values = mainColours)+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    
    grid.arrange(plotOne, plotTwo, ncol=1)
    
  })
  
  
  output$simulationGraphs <- renderPlot({
    plotOne <- ggplot(simulationData()[variable == "Anzahl"], aes(x=dateWoTime, y =value, col = DataType)) +
      geom_line() + xlab("") + ylab("Anzahl Fälle") + labs(title = "Gesamtfälle")+
      scale_x_date(date_labels = "%b %y")+
      theme_minimal()+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    plotTwo <- ggplot(simulationData()[variable == "DifferenzZumVortag"], aes(x=dateWoTime, y =value, col = DataType, fill = DataType)) +
      geom_bar(stat="identity", position = position_dodge2(preserve="single")) + xlab("") + ylab("Anzahl Fälle") + labs(title = "Fälle pro Tag")+
      scale_x_date(date_labels = "%b %y")+
      theme_minimal()+
      scale_fill_manual(values = mainColours)+
      scale_color_manual(values=mainColours)+
      scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
    
    grid.arrange(plotOne, plotTwo, ncol=1)
  })
}

shinyApp(ui = ui, server = server)