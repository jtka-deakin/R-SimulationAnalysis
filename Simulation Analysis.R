#### Produce Scatter Plots and Simulation Regression Plots
# Import required library files
library(ggplot2) 
library(MASS)
library(relaimpo)
library(scatterD3)
library(stargazer)
library(broom)
library(reshape2)
library(RColorBrewer)
library(plotly)
library(ggplot2)
library(gridExtra)

source("C:\\projlib\\R\\multiPlot.R")


stateSAData<-read.csv(file = "C:\\projlib\\R\\FallsCreek\\FIFOAndZoneWithState.csv",head=TRUE,sep=",")
stateSAData <- subset( stateSAData, ( X.Rule == "FIFO" & gcVehicleVelocity/277.67 <=21 ))
statePlotsCapacity <- FCplots(subset( stateSAData, ( state==1 )), "", "Capacity", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")
statePlotsNoCapacity <- FCplots(subset( stateSAData, ( state==0 )), "", "No Capacity", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")
statePlotsCapacity$LMGCompareAll
plot(statePlotsCapacity$niceplot)
statePlotsNoCapacity$LMGCompareAll
statePlotsNoCapacity$plotDemandVsThroughput

# All data from 960 original scenarios
# Only looking at first 3 hours of simulation
# No warm up time
prunedSAData <- rbind(
  read.csv(file = "C:\\projlib\\FallsCreek\\AUTORUNNEROUTPUT\\FIFO\\pruned\\FIFO.csv",head=TRUE,sep=",")
  ,
  read.csv(file = "C:\\projlib\\FallsCreek\\AUTORUNNEROUTPUT\\FIFO+Zone\\pruned\\FIFO+Zone.csv",head=TRUE,sep=","))
# Remove Zone and only look at velocities less than 20km/hr
allFIFO <- subset( prunedSAData, ( X.Rule == "FIFO" & gcVehicleVelocity/277.67 <=21 ))
allFIFO[1,"averageWaitTimeATS"]
# Chi squared ===========================
chiData <- lengthOfWait(allFIFO)
table("vehicel"=chiData$gcNoVehicleUsedForPikUp,"velocity"=chiData$gcVehicleVelocity,"length"=chiData$Length)

table("vehicel"=chiData$gcVehicleVelocity,"length"=chiData$Length)

table("vehicel"=chiData$IATProfileMean,"length"=chiData$Length)




  
  lengthOfWait <- function(X) {
    lengthOfATS<- as.data.frame(rep(1, nrow(allFIFO)))
    colnames(lengthOfATS) = "Length"
    RData <- (cbind(allFIFO,lengthOfATS))
    for(i in 1:nrow(RData)) {
      WaitData <- RData[i,"averageWaitTimeATS"]
      if (WaitData/60 < 15) {
        RData[i,ncol(RData)] <- "short"
      }
      if (WaitData/60 >= 15 & WaitData/60 < 30) {
        RData[i,ncol(RData)] <- "med"
      }
      if (WaitData/60 >= 30) {
        RData[i,ncol(RData)] <- "long"
      }
    }
    return(RData)
  }  
  
# =======================================
  

ALLplotsHighVel <- FCplots(subset(prunedSAData, ((gcNoVehicleUsedForPikUp==6 |gcNoVehicleUsedForPikUp==5 )&X.Rule == "FIFO" )), 
                           "", "All ScenariosOnly6Vehicle", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")


ALLplotsHighVel$plotDemandVsTime

table(subset(prunedSAData, ((gcNoVehicleUsedForPikUp==6 |gcNoVehicleUsedForPikUp==5 )&X.Rule == "FIFO" ))$gcVehicleVelocity/277.67)

# To create data for combined and qartiles qartiles(TestGrouping)
# To generate FCplots <- function(plotsData, rule, pdfFileName, saveFolder)
# To generate heatmaps FuncationSA(allFIFO, "All FIFO")

# All scenarios 
ALLplots <- FCplots(allFIFO, "", "All Scenarios", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")


combATIS <-compareLMG(ALLplots$plotDemandVsTime,ALLplots$plotDemandVsThroughput,ALLplots$plotDemandVsCost,ALLplots$LMGCompareAll)
ggsave(filename = paste("combATIS","ALL.pdf",collapse = ""),
       plot = combATIS,
       path = "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\",
       units = "mm",
       width = 200, height = 150)

StateTableDLVL <- subset( allFIFO, (gcNoVehicleUsedForPikUp <= 3 & IATProfileMean >=90))
LL <- FCplots(StateTableDLVL, "low demand and low service", "low service and low demand", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")
LL$LMGCompareAll
StateTableDHVL <- subset( allFIFO, (gcNoVehicleUsedForPikUp <= 3 & IATProfileMean <90) ) 
HL <- FCplots(StateTableDHVL, "high demand and low service", "high demand and low service", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")



plot <- HL$plotDemandVsThroughput
g <- ggplotGrob(plot + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
grid.arrange(legend)

plot <- ALLplots$plotDemandVsTime
g <- ggplotGrob(plot + theme(legend.position="bottom", legend.box = "vertical"))$grobs
legendALL <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
grid.arrange(legendALL)


StateTableDHVH <- subset( allFIFO, (gcNoVehicleUsedForPikUp > 3 & IATProfileMean <90) ) 
HH <- FCplots(StateTableDHVH, "high demand and high service", "high demand and high service", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")

StateTableDLVH <- subset( allFIFO, (gcNoVehicleUsedForPikUp > 3 & IATProfileMean >=90) ) 
LH <- FCplots(StateTableDLVH, "low demand and high service", "low demand and high service", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")

FigPlot1 <- subset( allFIFO, (gcNoVehicleUsedForPikUp == 1 & round(gcVehicleVelocity/277.67, 0) == 5))
FigPlot2 <- subset( allFIFO, (gcNoVehicleUsedForPikUp == 6 & round(gcVehicleVelocity/277.67, 0) == 5))
FigPlot3 <- subset( allFIFO, (gcNoVehicleUsedForPikUp == 6 & round(gcVehicleVelocity/277.67, 0) == 20))
FigData <- rbind(FigPlot1,FigPlot2,FigPlot3)

PFig <- FCplotsFig(FigData, "PFig", "PFig", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")

PFig$plotDemandVsThroughputTwo

nice <-compareLMG(HL$LMGCompareAll,HH$LMGCompareAll,LL$LMGCompareAll,LH$LMGCompareAll)
ggsave(filename = paste("LMGCompareAll","ServiceDemand.pdf",collapse = ""),
       plot = nice,
       path = "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\",
       units = "mm",
       width = 200, height =130)

compareHHLLtime <-compareLMG(HL$plotSimVsRegTime,HH$plotSimVsRegTime,LL$plotSimVsRegTime,LH$plotSimVsRegTime)
ggsave(filename = paste("ATIS","levels.pdf",collapse = ""),
       plot = compareHHLLtime,
       path = "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\",
       units = "mm",
       width = 200, height = 250)

HH$plotDemandVsThroughput

########################################################


###################################################
# SA and charts
# Rename data columns to something eaisier to work with
colnames(StateTableDHVH)[1]<-"rule"
colnames(StateTableDHVH)[2]<-"scenario"
colnames(StateTableDHVH)[3]<-"N"
colnames(StateTableDHVH)[4]<-"V"
colnames(StateTableDHVH)[5]<-"I"


averageTimeInSystem.model.simple <- lm(averageTimeInSystem/60 ~ 
                                         (N + 
                                            V + 
                                            I)^2 
                                       , data = StateTableDHVH)

averageThroughput.model.simple <- lm(averageThroughput ~ 
                                       (N + 
                                          V + 
                                          I)^2
                                     , data = StateTableDHVH)

averageCost.model.simple <- lm(totalTripTimes/60/60 ~ 
                                 (N + 
                                    V + 
                                    I)^2
                               , data = StateTableDHVH)

LMGaverageTimeInSystem <- calc.relimp(averageTimeInSystem.model.simple,type=c("lmg"),rela=TRUE)
LMGaverageTimeInSystemFalse <- calc.relimp(averageTimeInSystem.model.simple,type=c("lmg"),rela=FALSE)

LMGaverageThroughput <- calc.relimp(averageThroughput.model.simple,type=c("lmg"),rela=TRUE)
LMGaverageThroughputFalse  <- calc.relimp(averageThroughput.model.simple,type=c("lmg"),rela=FALSE)


averageCost <- calc.relimp(averageCost.model.simple,type=c("lmg"),rela=TRUE)
averageCostFalse <- calc.relimp(averageCost.model.simple,type=c("lmg"),rela=FALSE)

LMGWait <- as.data.frame(LMGaverageTimeInSystem@lmg)
LMGWaitFalse <- as.data.frame(LMGaverageTimeInSystemFalse@lmg)


LMGThroughput <- as.data.frame(LMGaverageThroughput@lmg)
LMGThroughputFalse <- as.data.frame(LMGaverageThroughputFalse@lmg)

LLMGCost <- as.data.frame(averageCost@lmg)
LLMGCostFalse <- as.data.frame(averageCostFalse@lmg)


ImportanceDataMatrix <- as.matrix(abs(cbind(LMGWait,
                                            LMGThroughput,
                                            LLMGCost,
                                            LMGWaitFalse,
                                            LMGThroughputFalse,
                                            LLMGCostFalse
)))



colnames(ImportanceDataMatrix) <- c("Time","Throughput","Cost","TimeF","ThroughputF","CostF")
ImportanceDataScale <- as.data.frame(apply(ImportanceDataMatrix[,1:3], 2, scale01))
ImportanceData <- cbind(ImportanceDataScale,ImportanceDataMatrix[,4:6],names = c(row.names(ImportanceDataMatrix)))
#ImportanceData <- as.data.frame(cbind(ImportanceDataMatrix,names = c(row.names(ImportanceDataMatrix))))

fillMelt <- melt(ImportanceData, measure.vars = c("Time","Throughput","Cost"))
valueMelt <- melt(ImportanceData, measure.vars = c("TimeF","ThroughputF","CostF"))
ImportanceData <-cbind(fillMelt[,c("names","variable","value")], valueMelt[,"value"])

#ImportanceData <- melt(ImportanceData, measure.vars = c("Time","Throughput","Cost"))
colnames(ImportanceData) <- c("coef","metric","importance","value")
levels(ImportanceData$metric)<-c("Time in \nsystem", "Average \n throughput\n", "Total \ncost")
#levels(ImportanceData$metric)<-c(paste("R=",summary(averageTimeInSystem.model.simple)$r.squared), "Average \n throughput\n", "Total \ncost")
#levels(ImportanceData$metric)<-c(expression(delta ~paste("hhh","llll")), "Average \n throughput\n", "Total \ncost")
#levels(ImportanceData$metric)[1]<-expression(paste("Value is ", sigma,",", R^{2},'=',r2.value))


#paste("Time in \nsystem\nR",summary(averageTimeInSystem.model.simple)$r.squared,collapse = "")

titlename <- paste("Time in \nsystem",summary(averageTimeInSystem.model.simple)$r.squared)
testing<-summary(averageTimeInSystem.model.simple)$r.squared
testLMG<-ggplot(as.data.frame(ImportanceData[]), aes(x=metric, y=coef)) + 
  geom_tile(aes(fill = importance),colour = "white") + 
  scale_fill_gradient(name = "Realative importance\nof each regressor", low = "white",high = "steelblue") +
  geom_text(aes(fill = importance, label = rlabel(value)), size=4) +
  labs(title =  bquote("                       test             \n"
                       ~R^{2}~"="~.(round(summary(averageTimeInSystem.model.simple)$r.squared,2))~
                         "                            "~ 
                         R^{2}~"="~.(round(summary(averageThroughput.model.simple)$r.squared,2))~
                         "                            "~ 
                         R^{2}~"="~.(round(summary(averageCost.model.simple)$r.squared,2))
                       )) +
  theme(axis.title.x = element_text(size=8)) + labs(x="Performance Metric") +
  theme(axis.title.y = element_text(size=8, angle=90)) + labs(y="Regressor") +
  theme(axis.text.x = element_text(size=8, angle=0)) +
  theme(axis.text.y = element_text(size=8, angle = 0)) +
  theme(text = element_text(size=8))+
  theme(legend.position="bottom", legend.box = "horizontal")+
  theme(plot.margin = unit(c(3,1,1,1), "lines")) 

ggsave(filename = paste("testLMG","testLMG.pdf",collapse = ""),
       plot = testLMG,
       path = "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\",
       units = "mm",
       width = 110, height =110)

#######################################################




#########################
# Functions
#########################
qartiles <- function(dataForCounts) {
  originalNC <- ncol(dataForCounts)
  # Cost
  cost <- dataForCounts$totalTripTimes/60/60
  for(i in 1:nrow(dataForCounts)) {
    costqartiles <- as.data.frame(t(quantile(dataForCounts$totalTripTimes/60/60, c(.0, .25, .5,.75))))
    
    if ( cost[i]>0 & cost[i] <= costqartiles$`25%`) {
      dataForCounts[i,originalNC+1] = 1
    }
    else if ( cost[i]>costqartiles$`25%` & cost[i] <= costqartiles$`50%`) {
      dataForCounts[i,originalNC+1] = 2
    }
    else if ( cost[i]>costqartiles$`50%` & cost[i] <= costqartiles$`75%`) {
      dataForCounts[i,originalNC+1] = 3
    }
    else if ( cost[i]>costqartiles$`75%`) {
      dataForCounts[i,originalNC+1] = 4
    }
    # do stuff with row
  }
  # Time in system
  cost <- dataForCounts$averageTimeInSystem/60
  for(i in 1:nrow(dataForCounts)) {
    costqartiles <- as.data.frame(t(quantile(dataForCounts$averageTimeInSystem/60, c(.0, .25, .5,.75))))
    
    if ( cost[i]>0 & cost[i] <= costqartiles$`25%`) {
      dataForCounts[i,originalNC+2] = 1
    }
    else if ( cost[i]>costqartiles$`25%` & cost[i] <= costqartiles$`50%`) {
      dataForCounts[i,originalNC+2] = 2
    }
    else if ( cost[i]>costqartiles$`50%` & cost[i] <= costqartiles$`75%`) {
      dataForCounts[i,originalNC+2] = 3
    }
    else if ( cost[i]>costqartiles$`75%`) {
      dataForCounts[i,originalNC+2] = 4
    }
    # do stuff with row
  }
  # Throughput
  cost <- dataForCounts$averageThroughput
  for(i in 1:nrow(dataForCounts)) {
    costqartiles <- as.data.frame(t(quantile(dataForCounts$averageThroughput, c(.0, .25, .5,.75))))
    
    if ( cost[i]>0 & cost[i] <= costqartiles$`25%`) {
      dataForCounts[i,originalNC+3] = 1
    }
    else if ( cost[i]>costqartiles$`25%` & cost[i] <= costqartiles$`50%`) {
      dataForCounts[i,originalNC+3] = 2
    }
    else if ( cost[i]>costqartiles$`50%` & cost[i] <= costqartiles$`75%`) {
      dataForCounts[i,originalNC+3] = 3
    }
    else if ( cost[i]>costqartiles$`75%`) {
      dataForCounts[i,originalNC+3] = 4
    }
    # do stuff with row
  }
  # Throughput
  cost <- dataForCounts$averageThroughput*dataForCounts$averageTimeInSystem/60*dataForCounts$totalTripTimes/60/60
  for(i in 1:nrow(dataForCounts)) {
    costqartiles <- as.data.frame(t(quantile(cost, c(.0, .25, .5,.75))))
    
    if ( cost[i]>0 & cost[i] <= costqartiles$`25%`) {
      dataForCounts[i,originalNC+4] = 1
    }
    else if ( cost[i]>costqartiles$`25%` & cost[i] <= costqartiles$`50%`) {
      dataForCounts[i,originalNC+4] = 2
    }
    else if ( cost[i]>costqartiles$`50%` & cost[i] <= costqartiles$`75%`) {
      dataForCounts[i,originalNC+4] = 3
    }
    else if ( cost[i]>costqartiles$`75%`) {
      dataForCounts[i,originalNC+4] = 4
    }
    # do stuff with row
  }
  
  
  dataForCounts[,originalNC+5]<- dataForCounts$averageThroughput*dataForCounts$averageTimeInSystem/60*dataForCounts$totalTripTimes/60/60
  colnames(dataForCounts)[originalNC+1]<-"cost"
  colnames(dataForCounts)[originalNC+2]<-"tis"
  colnames(dataForCounts)[originalNC+3]<-"through"
  colnames(dataForCounts)[originalNC+4]<-"comb"
  colnames(dataForCounts)[originalNC+5]<-"combMetric"
  return(dataForCounts)
}

# Create gaphs
FCplots <- function(plotsData, rule, pdfFileName, saveFolder) {
  
  averageTimeInSystem.model.simple <- lm(averageTimeInSystem/60 ~ 
                                           (gcNoVehicleUsedForPikUp + 
                                              gcVehicleVelocity + 
                                              IATProfileMean)^2 
                                         , data = plotsData)
  
  averageThroughput.model.simple <- lm(averageThroughput ~ 
                                         (gcNoVehicleUsedForPikUp + 
                                            gcVehicleVelocity + 
                                            IATProfileMean)^2
                                       , data = plotsData)
  
  averageCost.model.simple <- lm(totalTripTimes/60/60 ~ 
                                   (gcNoVehicleUsedForPikUp + 
                                      gcVehicleVelocity + 
                                      IATProfileMean)^2
                                 , data = plotsData)

  
  compare <-cbind(plotsData,
                  RTimeInSystem = averageTimeInSystem.model.simple$fitted.values,
                  RThrough = averageThroughput.model.simple$fitted.values,
                  RCost = averageCost.model.simple$fitted.values
  )
  
  l <- list(
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2,
    x = 1.2,
    y = 0.5,
    title = "test"
  )
  Xaxis_template <- list(title="Simuation result")
  Yaxis_template <- list(title="Regression result")
  # D3 plot
  plotTIS <- plot_ly(compare, x = averageTimeInSystem/60, y = RTimeInSystem, mode = "markers",
                     group = round(gcVehicleVelocity/277.67, digits = 0), symbol = as.factor(gcNoVehicleUsedForPikUp), opacity = 0.8, text = paste("scenarioNo = ",scenarioNo, "Demand = ", round(3600/IATProfileMean, digits = 0), "Vehicles = ", gcNoVehicleUsedForPikUp)) %>% 
    layout( xaxis = Xaxis_template, yaxis = Yaxis_template, title = paste(rule, " Compare simulated and regression average time in system"), legend = l)
  
  # Time in system Paper plot
  
  plotDemandVsTime <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageTimeInSystem/60)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
    theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
    theme(text = element_text(size=12)) +
    theme(legend.position="bottom", legend.box = "horizontal")+
    scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    guides(colour = guide_legend(nrow = 1))+
    theme(aspect.ratio=1/3)
  
  ggsave(filename = paste(pdfFileName,"plotDemandVsTime.pdf",collapse = ""),
         plot = plotDemandVsTime,
         path = saveFolder,
         units = "mm",
         width = 110, height = 110)
  
  plotSimVsRegTime <- ggplot(compare, aes(x = averageTimeInSystem/60, y = RTimeInSystem)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=8)) + labs(x="Simulated time in system (min)") +
    theme(axis.title.y = element_text(size=8)) + labs(y="Regression time in system (min)") +
    theme(text = element_text(size=8)) +
    theme(legend.position="bottom", legend.box = "horizontal") + geom_abline(intercept = 0, slope = 1)+
    scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    guides(colour = guide_legend(nrow = 1))
  ggsave(filename = paste(pdfFileName,"plotSimVsRegTime.pdf",collapse = ""),
         plot = plotSimVsRegTime,
         path = saveFolder,
         units = "mm",
         width = 110, height = 110)
  
  plotThrough <- plot_ly(compare, x = averageThroughput, y = RThrough, mode = "markers",
                         group = round(gcVehicleVelocity/277.67, digits = 0), symbol = as.factor(gcNoVehicleUsedForPikUp), opacity = 0.8, text = paste("scenarioNo = ",scenarioNo, "Demand = ", round(3600/IATProfileMean, digits = 0), "Vehicles = ", gcNoVehicleUsedForPikUp)) %>% 
    layout( xaxis = Xaxis_template, yaxis = Yaxis_template, title = paste(rule, "Compare simulated and regression average troughput"), legend = l)
  
  
  plotDemandVsThroughput <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageThroughput)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
    theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average \nthroughput (PAX)") +
    theme(text = element_text(size=12)) +
    theme(legend.position="bottom", legend.box = "horizontal")+
    scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    guides(colour = guide_legend(nrow = 1))+
    theme(aspect.ratio=1/3)
 
   plotDemandVsThroughputThreeToSiX <- plotDemandVsThroughput
  
  if (nrow(as.data.frame(table(plotsData$gcNoVehicleUsedForPikUp)))==3) {
    plotDemandVsThroughputThreeToSiX <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageThroughput)) + 
      geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                     size = 3600/IATProfileMean), alpha=0.7)  +
      labs(title= pdfFileName) +
      scale_color_continuous(name="Velocity",low = "red",high = "green")  +
      scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
      theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
      theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average \nthroughput (PAX)") +
      theme(text = element_text(size=12)) +
      theme(legend.position="bottom", legend.box = "horizontal")+
      scale_shape_manual(name ="Vehicle", values = c(52,53,54))+
      guides(colour = guide_legend(nrow = 1))+
      theme(aspect.ratio=1/3)
    
  }
   
   plotDemandVsThroughputTwo <- plotDemandVsThroughput
   
   if (nrow(as.data.frame(table(plotsData$gcNoVehicleUsedForPikUp)))==2) {
     plotDemandVsThroughputTwo <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageThroughput)) + 
       geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                      size = 3600/IATProfileMean), alpha=0.7)  +
       labs(title= pdfFileName) +
       scale_color_continuous(name="Velocity",low = "red",high = "green")  +
       scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
       theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
       theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average \nthroughput (PAX)") +
       theme(text = element_text(size=12)) +
       theme(legend.position="bottom", legend.box = "horizontal")+
       scale_shape_manual(name ="Vehicle", values = c(49,54))+
       guides(colour = guide_legend(nrow = 1))+
       theme(aspect.ratio=1/3)
     
   }
  
  
  ggsave(filename = paste(pdfFileName,"plotDemandVsThroughput.pdf",collapse = ""),
         plot = plotDemandVsThroughput,
         path = saveFolder,
         units = "mm",
         width = 150, height = 110)
  
  #########################
  # For paper test
  
  plot <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageThroughput)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
    theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average \n throughput (PAX)") +
    theme(text = element_text(size=12)) +
    theme(legend.position="bottom", legend.box = "horizontal")+
    scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    guides(colour = guide_legend(nrow = 2))
  
  ggsave(filename = paste(pdfFileName,"AAAplot.pdf",collapse = ""),
         plot = plot,
         path = saveFolder,
         units = "mm",
         width = 100, height = 70)
  
  #########################
  
  
  plotSimVsRegThroughput <- ggplot(compare, aes(x = averageThroughput, y = RThrough)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=8)) + labs(x="Simulated throughput (PAX)") +
    theme(axis.title.y = element_text(size=8)) + labs(y="Regression throughput (PAX)") +
    theme(text = element_text(size=8)) + scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    theme(legend.position="bottom", legend.box = "horizontal") + geom_abline(intercept = 0, slope = 1)+
    guides(colour = guide_legend(nrow = 1))
  ggsave(filename = paste(pdfFileName,"plotSimVsRegThroughput.pdf",collapse = ""),
         plot = plotSimVsRegThroughput,
         path = saveFolder,
         units = "mm",
         width = 110, height = 110)
  
  
  plotCost<- plot_ly(compare, x = totalTripTimes/60/60, y = RCost, mode = "markers",
                     group = round(gcVehicleVelocity/277.67, digits = 0), 
                     symbol = as.factor(gcNoVehicleUsedForPikUp), 
                     opacity = 0.8, text = paste("scenarioNo = ",scenarioNo, "Demand = ", round(3600/IATProfileMean, digits = 0), "Vehicles = ", gcNoVehicleUsedForPikUp)) %>% 
    layout( xaxis = Xaxis_template, yaxis = Yaxis_template, title = paste(rule,"Compare simulated and regression cost"), legend = l)
  
  
  plotDemandVsCost <- ggplot(compare, aes(x=3600/IATProfileMean, y=totalTripTimes/60/60)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
    theme(axis.title.y = element_text(size=12)) + labs(y="Regression cost \n(hours)") +
    theme(text = element_text(size=12)) +
    theme(legend.position="bottom", legend.box = "horizontal")+
    scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    guides(colour = guide_legend(nrow = 1))+
    theme(aspect.ratio=1/3)
  
  ggsave(filename = paste(pdfFileName,"plotDemandVsCost.pdf",collapse = ""),
         plot = plotDemandVsCost,
         path = saveFolder,
         units = "mm",
         width = 110, height = 110)
  
  
  plotSimVsRegCost <- ggplot(compare, aes(x = totalTripTimes/60/60, y = RCost)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=8)) + labs(x="Simulated cost (hours)") +
    theme(axis.title.y = element_text(size=8)) + labs(y="Regression cost (hours)") +
    theme(text = element_text(size=8)) +scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    theme(legend.position="bottom", legend.box = "horizontal") + 
    geom_abline(intercept = 0, slope = 1)+
    guides(colour = guide_legend(nrow = 1))
  ggsave(filename = paste(pdfFileName,"plotSimVsRegCost.pdf",collapse = ""),
         plot = plotSimVsRegCost,
         path = saveFolder,
         units = "mm",
         width = 110, height = 110)
  
  
  
  
  ##############################################################################################
  grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  }
  
  nice <-grid_arrange_shared_legend(plotDemandVsTime, plotDemandVsThroughput, plotDemandVsCost)
  ggsave(filename = paste(pdfFileName,"Compare.pdf",collapse = ""),
         plot = nice,
         path = saveFolder,
         units = "mm",
         width = 200, height = 250)
  
  
  
  #############################################################################################
  # SA and charts
  
  averageTimeInSystem.model.simple <- lm(averageTimeInSystem/60 ~ 
                                           (gcNoVehicleUsedForPikUp + 
                                              gcVehicleVelocity + 
                                              IATProfileMean)^2 
                                         , data = plotsData)
  
  averageThroughput.model.simple <- lm(averageThroughput ~ 
                                         (gcNoVehicleUsedForPikUp + 
                                            gcVehicleVelocity + 
                                            IATProfileMean)^2
                                       , data = plotsData)
  
  averageCost.model.simple <- lm(totalTripTimes/60/60 ~ 
                                   (gcNoVehicleUsedForPikUp + 
                                      gcVehicleVelocity + 
                                      IATProfileMean)^2
                                 , data = plotsData)
  
  LMGaverageTimeInSystem <- calc.relimp(averageTimeInSystem.model.simple,type=c("lmg"),rela=TRUE)
  
  LMGaverageThroughput <- calc.relimp(averageThroughput.model.simple,type=c("lmg"),rela=TRUE)
  
  averageCost <- calc.relimp(averageCost.model.simple,type=c("lmg"),rela=TRUE)
  
  LMGWait <- as.data.frame(LMGaverageTimeInSystem@lmg)
  #row.names(LMGWait) = c("Vehicles", "Velocity", "Arrivals", "Vehicles^2", "Velocity^2", "Arrivals^2", "Vehicles:Veocity", "Vehicles:Arrivals","Velocity:Arrivals")
  
  LMGThroughput <- as.data.frame(LMGaverageThroughput@lmg)
  #row.names(LMGThroughput) = c("Vehicles", "Velocity", "Arrivals", "Vehicles^2", "Velocity^2", "Arrivals^2", "Vehicles:Veocity", "Vehicles:Arrivals","Velocity:Arrivals")
  
  #LMGTrip <- as.data.frame(LMGaverageTripTimes@lmg)
  #row.names(LMGTrip) = c("Vehicles", "Velocity", "Arrivals", "Vehicles^2", "Velocity^2", "Arrivals^2", "Vehicles:Veocity", "Vehicles:Arrivals","Velocity:Arrivals")
  
  LLMGCost <- as.data.frame(averageCost@lmg)
  #row.names(LLMGCost) = c("Vehicles", "Velocity", "Arrivals", "Vehicles^2", "Velocity^2", "Arrivals^2", "Vehicles:Veocity", "Vehicles:Arrivals","Velocity:Arrivals")
  
  
  
  ImportanceData <- as.matrix(abs(cbind(LMGWait,
                                        LMGThroughput,
                                        LLMGCost)))
  
  heatcols<-heat.colors(ImportanceData)
  
  colnames(ImportanceData) <- c("Time","Throughput","Cost")
  ImportanceDataScale <- as.data.frame(apply(ImportanceData, 2, scale01))
  ImportanceData <- cbind(ImportanceDataScale,names = c(row.names(ImportanceData)))
  ImportanceData <- melt(ImportanceData, measure.vars = c("Time","Throughput","Cost"))
  colnames(ImportanceData) <- c("coef","metric","importance")
  
  LMGHeatMap <- ggplot(as.data.frame(ImportanceData[]), aes(x=metric, y=coef)) + 
    geom_tile(aes(fill = importance),colour = "white") + 
    scale_fill_gradient(name = "Importance", low = "white",high = "steelblue") +
    labs(title= rule) +
    theme(axis.title.x = element_text(size=8)) + labs(x="Performance Metric") +
    theme(axis.title.y = element_text(size=8, angle=90)) + labs(y="Input Variable") +
    theme(axis.text.x = element_text(size=8, angle=90)) +
    theme(axis.text.y = element_text(size=8, angle = 45)) +
    theme(text = element_text(size=8))+
    theme(legend.position="bottom", legend.box = "horizontal")
  
    # ggsave(filename = paste(pdfFileName,"LMG.pdf",collapse = ""),
    # plot = LMGHeatMap,
    # path = saveFolder,
    # units = "cm",
    # width = 30,
    # height = 20,
    # dpi = 600)
    
  ggsave(filename = paste(pdfFileName,"LMG.pdf",collapse = ""),
           plot = LMGHeatMap,
           path = saveFolder,
           units = "mm",
           width = 100, height = 160)
  
  
  ###################################################
  # SA and charts
  # Rename data columns to something eaisier to work with
  colnames(plotsData)[1]<-"rule"
  colnames(plotsData)[2]<-"scenario"
  colnames(plotsData)[3]<-"N"
  colnames(plotsData)[4]<-"V"
  colnames(plotsData)[5]<-"I"
  
  
  averageTimeInSystem.model.simple <- lm(averageTimeInSystem/60 ~ 
                                           (N + 
                                              V + 
                                              I)^2 
                                         , data = plotsData)
  
  averageThroughput.model.simple <- lm(averageThroughput ~ 
                                         (N + 
                                            V + 
                                            I)^2
                                       , data = plotsData)
  
  averageCost.model.simple <- lm(totalTripTimes/60/60 ~ 
                                   (N + 
                                      V + 
                                      I)^2
                                 , data = plotsData)
  
  LMGaverageTimeInSystem <- calc.relimp(averageTimeInSystem.model.simple,type=c("lmg"),rela=TRUE)
  LMGaverageTimeInSystemFalse <- calc.relimp(averageTimeInSystem.model.simple,type=c("lmg"),rela=FALSE)
  
  LMGaverageThroughput <- calc.relimp(averageThroughput.model.simple,type=c("lmg"),rela=TRUE)
  LMGaverageThroughputFalse  <- calc.relimp(averageThroughput.model.simple,type=c("lmg"),rela=FALSE)
  
  
  averageCost <- calc.relimp(averageCost.model.simple,type=c("lmg"),rela=TRUE)
  averageCostFalse <- calc.relimp(averageCost.model.simple,type=c("lmg"),rela=FALSE)
  
  LMGWait <- as.data.frame(LMGaverageTimeInSystem@lmg)
  LMGWaitFalse <- as.data.frame(LMGaverageTimeInSystemFalse@lmg)
  
  
  LMGThroughput <- as.data.frame(LMGaverageThroughput@lmg)
  LMGThroughputFalse <- as.data.frame(LMGaverageThroughputFalse@lmg)
  
  LLMGCost <- as.data.frame(averageCost@lmg)
  LLMGCostFalse <- as.data.frame(averageCostFalse@lmg)
  
  
  ImportanceDataMatrix <- as.matrix(abs(cbind(LMGWait,
                                              LMGThroughput,
                                              LLMGCost,
                                              LMGWaitFalse,
                                              LMGThroughputFalse,
                                              LLMGCostFalse
  )))
  
  
  
  colnames(ImportanceDataMatrix) <- c("Time","Throughput","Cost","TimeF","ThroughputF","CostF")
  ImportanceDataScale <- as.data.frame(apply(ImportanceDataMatrix[,1:3], 2, scale01))
  ImportanceData <- cbind(ImportanceDataScale,ImportanceDataMatrix[,4:6],names = c(row.names(ImportanceDataMatrix)))
  #ImportanceData <- as.data.frame(cbind(ImportanceDataMatrix,names = c(row.names(ImportanceDataMatrix))))
  
  fillMelt <- melt(ImportanceData, measure.vars = c("Time","Throughput","Cost"))
  valueMelt <- melt(ImportanceData, measure.vars = c("TimeF","ThroughputF","CostF"))
  ImportanceData <-cbind(fillMelt[,c("names","variable","value")], valueMelt[,"value"])
  
  #ImportanceData <- melt(ImportanceData, measure.vars = c("Time","Throughput","Cost"))
  colnames(ImportanceData) <- c("coef","metric","importance","value")
  levels(ImportanceData$metric)<-c("Time in \nsystem", "Average \n throughput\n", "Total \ncost")
  
  LMGCompareAll<-ggplot(as.data.frame(ImportanceData[]), aes(x=metric, y=coef)) + 
    geom_tile(aes(fill = importance),colour = "white") + 
    scale_fill_gradient(name = "Realative importance\nof each regressor", low = "white",high = "steelblue") +
    geom_text(aes(fill = importance, label = rlabel(value)), size=4) +
    labs(title =  bquote(R^{2}~"="~.(round(summary(averageTimeInSystem.model.simple)$r.squared,2))~
                           "                      "~ 
                           R^{2}~"="~.(round(summary(averageThroughput.model.simple)$r.squared,2))~
                           "                      "~
                           R^{2}~"="~.(round(summary(averageCost.model.simple)$r.squared,2))
    )) +
    theme(axis.title.x = element_text(size=12)) + labs(x=rule) +
    theme(axis.title.y = element_text(size=12, angle=90)) + labs(y="Regressor") +
    theme(axis.text.x = element_text(size=12, angle=0)) +
    theme(axis.text.y = element_text(size=12, angle = 0)) +
    theme(text = element_text(size=12))+
    theme(legend.position="bottom", legend.box = "horizontal")+
    theme(aspect.ratio=1/3)
  
  testLMG<-ggplot(as.data.frame(ImportanceData[]), aes(x=metric, y=coef)) + 
    geom_tile(aes(fill = importance),colour = "white") + 
    scale_fill_gradient(name = "Realative importance\nof each regressor", low = "white",high = "steelblue") +
    geom_text(aes(fill = importance, label = rlabel(value)), size=4) +
    labs(title =  bquote(R^{2}~"="~.(round(summary(averageTimeInSystem.model.simple)$r.squared,2))~
                           "                  "~ 
                           R^{2}~"="~.(round(summary(averageThroughput.model.simple)$r.squared,2))~
                           "                  "~ 
                           R^{2}~"="~.(round(summary(averageCost.model.simple)$r.squared,2))
    )) +
    theme(axis.title.x = element_text(size=8)) + labs(x="Performance Metric") +
    theme(axis.title.y = element_text(size=8, angle=90)) + labs(y="Regressor") +
    theme(axis.text.x = element_text(size=8, angle=0)) +
    theme(axis.text.y = element_text(size=8, angle = 0)) +
    theme(text = element_text(size=8))+
    theme(legend.position="bottom", legend.box = "horizontal")+
    theme(plot.margin = unit(c(3,1,1,1), "lines")) 
  
  ggsave(filename = paste(pdfFileName,"LMGWithNumbers.pdf",collapse = ""),
         plot = testLMG,
         path = "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\",
         units = "mm",
         width = 110, height = 110)
  

  # What to return
  
  
  newList <- list("plotTIS" = plotTIS, 
                  "plotThrough" = plotThrough, 
                  "plotCost" = plotCost,
                  "niceplot"=nice,"LMG" = LMGHeatMap, 
                  "data" = LMGaverageTimeInSystem,
                  "plotDemandVsTime" = plotDemandVsTime,
                  "plotDemandVsThroughput" = plotDemandVsThroughput,
                  "plotDemandVsCost" = plotDemandVsCost,
                  "plotSimVsRegTime" = plotSimVsRegTime,
                  "testLMG" = testLMG,
                  "LMGCompareAll"=LMGCompareAll,
                  "plotDemandVsThroughputThreeToSiX" =plotDemandVsThroughputThreeToSiX,
                  "plotDemandVsThroughputTwo"=plotDemandVsThroughputTwo,
                  "averageTimeInSystem.model.simple"=averageTimeInSystem.model.simple,
                  "averageCost.model.simple" =   averageCost.model.simple,
                  "averageThroughput.model.simple"= averageThroughput.model.simple
                  )
  
  return(newList)
  
}

rlabel <- function(x) {
  r<-as.character.numeric_version(round(x, 2))
  if (r=="0") {
    r <- "0.00"
  }
  return(r)
  
}

scale01 <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -min(x, na.rm=TRUE))}

compareLMG <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}


# Create gaphs
FCplotsFig <- function(plotsData, rule, pdfFileName, saveFolder) {
  
  averageTimeInSystem.model.simple <- lm(averageTimeInSystem/60 ~ 
                                           (gcNoVehicleUsedForPikUp + 
                                              gcVehicleVelocity + 
                                              IATProfileMean)^2 
                                         , data = plotsData)
  
  averageThroughput.model.simple <- lm(averageThroughput ~ 
                                         (gcNoVehicleUsedForPikUp + 
                                            gcVehicleVelocity + 
                                            IATProfileMean)^2
                                       , data = plotsData)
  
  averageCost.model.simple <- lm(totalTripTimes/60/60 ~ 
                                   (gcNoVehicleUsedForPikUp + 
                                      gcVehicleVelocity + 
                                      IATProfileMean)^2
                                 , data = plotsData)
  
  
  compare <-cbind(plotsData,
                  RTimeInSystem = averageTimeInSystem.model.simple$fitted.values,
                  RThrough = averageThroughput.model.simple$fitted.values,
                  RCost = averageCost.model.simple$fitted.values
  )
  
  l <- list(
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2,
    x = 1.2,
    y = 0.5,
    title = "test"
  )
  Xaxis_template <- list(title="Simuation result")
  Yaxis_template <- list(title="Regression result")
  # D3 plot
  plotTIS <- plot_ly(compare, x = averageTimeInSystem/60, y = RTimeInSystem, mode = "markers",
                     group = round(gcVehicleVelocity/277.67, digits = 0), symbol = as.factor(gcNoVehicleUsedForPikUp), opacity = 0.8, text = paste("scenarioNo = ",scenarioNo, "Demand = ", round(3600/IATProfileMean, digits = 0), "Vehicles = ", gcNoVehicleUsedForPikUp)) %>% 
    layout( xaxis = Xaxis_template, yaxis = Yaxis_template, title = paste(rule, " Compare simulated and regression average time in system"), legend = l)
  
  # Time in system Paper plot
  
  plotDemandVsTime <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageTimeInSystem/60)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
    theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
    theme(text = element_text(size=12)) +
    theme(legend.position="bottom", legend.box = "horizontal")+
    scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    guides(colour = guide_legend(nrow = 1))+
    theme(aspect.ratio=1/3)
  
  ggsave(filename = paste(pdfFileName,"plotDemandVsTime.pdf",collapse = ""),
         plot = plotDemandVsTime,
         path = saveFolder,
         units = "mm",
         width = 110, height = 110)
  
  plotSimVsRegTime <- ggplot(compare, aes(x = averageTimeInSystem/60, y = RTimeInSystem)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=8)) + labs(x="Simulated time in system (min)") +
    theme(axis.title.y = element_text(size=8)) + labs(y="Regression time in system (min)") +
    theme(text = element_text(size=8)) +
    theme(legend.position="bottom", legend.box = "horizontal") + geom_abline(intercept = 0, slope = 1)+
    scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    guides(colour = guide_legend(nrow = 1))
  ggsave(filename = paste(pdfFileName,"plotSimVsRegTime.pdf",collapse = ""),
         plot = plotSimVsRegTime,
         path = saveFolder,
         units = "mm",
         width = 110, height = 110)
  
  plotThrough <- plot_ly(compare, x = averageThroughput, y = RThrough, mode = "markers",
                         group = round(gcVehicleVelocity/277.67, digits = 0), symbol = as.factor(gcNoVehicleUsedForPikUp), opacity = 0.8, text = paste("scenarioNo = ",scenarioNo, "Demand = ", round(3600/IATProfileMean, digits = 0), "Vehicles = ", gcNoVehicleUsedForPikUp)) %>% 
    layout( xaxis = Xaxis_template, yaxis = Yaxis_template, title = paste(rule, "Compare simulated and regression average troughput"), legend = l)
  
  
  plotDemandVsThroughput <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageThroughput)) + 
    geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                   size = 3600/IATProfileMean), alpha=0.7)  +
    labs(title= pdfFileName) +
    scale_color_continuous(name="Velocity",low = "red",high = "green")  +
    scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
    theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
    theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average \nthroughput (PAX)") +
    theme(text = element_text(size=12)) +
    theme(legend.position="bottom", legend.box = "horizontal")+
    scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
    guides(colour = guide_legend(nrow = 1))+
    theme(aspect.ratio=1/3)
  
  plotDemandVsThroughputThreeToSiX <- plotDemandVsThroughput
  
  if (nrow(as.data.frame(table(plotsData$gcNoVehicleUsedForPikUp)))==3) {
    plotDemandVsThroughputThreeToSiX <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageThroughput)) + 
      geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                     size = 3600/IATProfileMean), alpha=0.7)  +
      labs(title= pdfFileName) +
      scale_color_continuous(name="Velocity",low = "red",high = "green")  +
      scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
      theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
      theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average \nthroughput (PAX)") +
      theme(text = element_text(size=12)) +
      theme(legend.position="bottom", legend.box = "horizontal")+
      scale_shape_manual(name ="Vehicle", values = c(52,53,54))+
      guides(colour = guide_legend(nrow = 1))+
      theme(aspect.ratio=1/3)
    
  }
  
  plotDemandVsThroughputTwo <- plotDemandVsThroughput
  
  if (nrow(as.data.frame(table(plotsData$gcNoVehicleUsedForPikUp)))==2) {
    plotDemandVsThroughputTwo <- ggplot(compare, aes(x=3600/IATProfileMean, y=averageThroughput)) + 
      geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                     size = 3600/IATProfileMean), alpha=0.7)  +
      labs(title= pdfFileName) +
      scale_color_continuous(name="Velocity",low = "red",high = "green")  +
      scale_shape(name ="Vehicle")+scale_size(name ="Demand")+
      theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
      theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average \nthroughput (PAX)") +
      theme(text = element_text(size=12)) +
      theme(legend.position="bottom", legend.box = "horizontal")+
      scale_shape_manual(name ="Vehicle", values = c(49,54))+
      guides(colour = guide_legend(nrow = 1))+
      theme(aspect.ratio=1/3)
    
  }
  
  
  
  
  
  newList <- list(
                  "plotDemandVsThroughputTwo"=plotDemandVsThroughputTwo
  )
  
  return(newList)
  
}

FIFOstatedata <- read.csv(file="C:\\projlib\\FallsCreek\\AUTORUNNEROUTPUT\\FIFO\\pruned\\State.csv",head=TRUE,sep=",")

stateChartsData <- subset( prunedSAData, ( X.Rule == "FIFO"))
FIFOstates <- as.data.frame(stable(FIFOstatedata))
AllAllFIFO <- cbind(stateChartsData,FIFOstates)

randomSample <- subset(AllAllFIFO, (V59<=10 & gcVehicleVelocity/277.6 <22))
randomSample <- randomSample[sample(1:nrow(randomSample), 10,
                                    replace=FALSE),]

randomSample2<-cbind(melt(randomSample[1,],measure.vars=c("X1", "X2","X3")),time=seq(1:3))
for(i in 1:nrow(randomSample)){
  FIFOstatesPlot<- melt(randomSample[i,],measure.vars=c("X1", "X2","X3"))
  FIFOstatesPlot$time <-seq(1:3)
  randomSample2 <- rbind(randomSample2,FIFOstatesPlot)
}

randomSample3 <- subset(AllAllFIFO, (V59>10 & gcVehicleVelocity/277.6 <22))
randomSample3 <- randomSample3[sample(1:nrow(randomSample3), 10,
                                    replace=FALSE),]

randomSample4<-cbind(melt(randomSample3[1,],measure.vars=c("X1", "X2","X3")),time=seq(1:3))
for(i in 1:nrow(randomSample3)){
  FIFOstatesPlot<- melt(randomSample3[i,],measure.vars=c("X1", "X2","X3"))
  FIFOstatesPlot$time <-seq(1:3)
  randomSample4 <- rbind(randomSample2,FIFOstatesPlot)
}



allPlotsData<-cbind(melt(AllAllFIFO[1,],measure.vars=c("X1", "X2","X3")),time=seq(1:3))
for(i in 1:nrow(AllAllFIFO)){
  FIFOstatesPlot<- melt(AllAllFIFO[i,],measure.vars=c("X1", "X2","X3"))
  FIFOstatesPlot$time <-seq(1:3)
  allPlotsData <- rbind(allPlotsData,FIFOstatesPlot)
}

                       FIFOstatesPlot<- melt(FIFOstates[500,],measure.vars=c("X1", "X2","X3"))
                       melt(FIFOstatesPlot,measure.vars=c("value"))
                       FIFOstatesPlot$time <-seq(1:3)






set.seed(20)
names(allPlotsData)
cols<-c(#"gcNoVehicleUsedForPikUp",
        #"gcVehicleVelocity","IATProfileMean",
        "X4","X5","X6",
        "averageWaitTimeATS","averageThroughput",
        "averageTripTimes","averagePAXPertrip")


train <- subset(allPlotsData, (gcVehicleVelocity/277.6 <22))[cols]
numClusters <- 4
FIFOCluster <- kmeans(train, numClusters, nstart = 20)

clustersData <- as.data.frame(cbind(subset(allPlotsData, (gcVehicleVelocity/277.6 <22)),FIFOCluster$cluster))

FIFOCluster$centers
table(clustersData$`FIFOCluster$cluster`, clustersData$gcNoVehicleUsedForPikUp)
table(clustersData$`FIFOCluster$cluster`, clustersData$state)

table(clustersData$`FIFOCluster$cluster`)
table(clustersData$state)

ggplot(clustersData, aes(x=time, y=value, group = scenarioNo)) + 
  geom_point(aes(colour = as.factor(`FIFOCluster$cluster`), shape = as.factor(gcNoVehicleUsedForPikUp),
                 size = 3600/IATProfileMean), alpha=0.7)  +
  labs(title= "Identify if system has capacity") +
  #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
  #scale_color_continuous(name="Velocity",low = "red",high = "green")  +
  scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
  theme(axis.title.x = element_text(size=12)) + labs(x="Simulation time (hours)") +
  theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
  theme(text = element_text(size=12)) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
  guides(colour = guide_legend(nrow = 1))+
  theme(aspect.ratio=1/3)+geom_line()+scale_y_continuous(limits = c(0, 150))


library(Rcmdr)
scatter3d(clustersData$averagePAXPertrip, 
          clustersData$averageTripTimes, 
          clustersData$averageThroughput)

                       
                       
      RValues<-subset(allPlotsData, (gcVehicleVelocity/277.6 <22))
 max(RValues$V59)
 summary(RValues$V59)
 plot+box(RValues$V59)
 
 
 ggplot(subset(allPlotsData, (gcVehicleVelocity/277.6 <22)), aes(x=factor(1), y=sdTimeInSystem/60))+geom_boxplot()
 summary(subset(allPlotsData, (gcVehicleVelocity/277.6 <22))$sdTimeInSystem/60)
 
 ggplot(subset(allPlotsData, (gcVehicleVelocity/277.6 <22)), aes(x=factor(1), y=V59))+geom_boxplot()
 
 ggplot(subset(allPlotsData, (gcVehicleVelocity/277.6 <22)), aes(x=V59))+stat_ecdf()
 
 
 ggplot(subset(allPlotsData, (V59<=0.6& gcVehicleVelocity/277.6 <22)), aes(x=time, y=value, group = scenarioNo)) + 
   geom_abline(intercept=0,
               slope=0,colour = "green")+
   geom_abline(intercept=0,
               slope=0.25, colour = "blue")+
   geom_abline(intercept=0,
               slope=0.5, colour = "blue")+
   geom_abline(intercept=0,
               slope=0.75, colour = "blue")+
   geom_abline(intercept=0,
               slope=1, colour = "red")+
   labs(title= "pdfFileName") +
   #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
   scale_color_continuous(name="Velocity",low = "red",high = "green")  +
   scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
   theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
   theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
   theme(text = element_text(size=12)) +
   theme(legend.position="bottom", legend.box = "horizontal")+
   scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
   guides(colour = guide_legend(nrow = 1))+
   theme(aspect.ratio=1/1)
 

stableWaitTime<-ggplot(randomSample2, aes(x=time, y=value, group = scenarioNo)) + 
  geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                 size = 3600/IATProfileMean), alpha=0.7)  +
  labs(title= "M<=10") +
  #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
  scale_color_continuous(name="Velocity",low = "red",high = "green")  +
  scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
  theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
  theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
  theme(text = element_text(size=12)) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
  guides(colour = guide_legend(nrow = 1))+
  theme(aspect.ratio=1/3)+geom_line()+scale_y_continuous(limits = c(0, 150))
randomSample <- subset(allPlotsData, (V59<=10 & gcVehicleVelocity/277.6 <22))
randomSample <- randomSample[sample(1:nrow(randomSample), 5,
                                    replace=FALSE),]
table(randomSample2$gcNoVehicleUsedForPikUp)

stableWaitTimeFinal<-ggplot(randomSample2, aes(x=time, y=value, group = scenarioNo)) + 
  geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                 size = 3600/IATProfileMean), alpha=0.7)  +
  labs(title= "M<=10") +
  #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
  scale_color_continuous(name="Velocity",low = "red",high = "green")  +
  scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
  theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
  theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
  theme(text = element_text(size=12)) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_shape_manual(name ="Vehicle", values = c(50,51,52,53,54))+
  guides(colour = guide_legend(nrow = 1))+
  theme(aspect.ratio=1/3)+geom_line()+scale_y_continuous(limits = c(0, 150))

ggplot(subset(allPlotsData, (V59<=20 & V59>10 &gcVehicleVelocity/277.6 <22)), aes(x=time, y=value, group = scenarioNo)) + 
  geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                 size = 3600/IATProfileMean), alpha=0.7)  +
  labs(title= "10<M<=20") +
  #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
  scale_color_continuous(name="Velocity",low = "red",high = "green")  +
  scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
  theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
  theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
  theme(text = element_text(size=12)) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
  guides(colour = guide_legend(nrow = 1))+
  theme(aspect.ratio=1/3)+geom_line()+scale_y_continuous(limits = c(0, 150))

unStableWaitTime<-ggplot(randomSample4, aes(x=time, y=value, group = scenarioNo)) + 
  geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                 size = 3600/IATProfileMean), alpha=0.7)  +
  labs(title= "20<M") +
  #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
  scale_color_continuous(name="Velocity",low = "red",high = "green")  +
  scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
  theme(axis.title.x = element_text(size=12)) + labs(x="Demand") +
  theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
  theme(text = element_text(size=12)) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
  guides(colour = guide_legend(nrow = 1))+
  theme(aspect.ratio=1/3)+geom_line()+scale_y_continuous(limits = c(0, 150))




ggplot(subset(allPlotsData, (gcVehicleVelocity/277.6 <22)), aes(x=time, y=value, group = scenarioNo)) + 
  geom_point(aes(colour = round(gcVehicleVelocity/277.67, digits = 0), shape = as.factor(gcNoVehicleUsedForPikUp),
                 size = 3600/IATProfileMean), alpha=0.7)  +
  labs(title= "Identify if system has capacity") +
  #scale_colour_distiller(name = "Diamond\nclarity", guide = "colourbar", type = "seq", palette = "Blues", direction = -1)+
  scale_color_continuous(name="Velocity",low = "red",high = "green")  +
  scale_shape(name ="Vehicle") + scale_size(name ="Demand")+
  theme(axis.title.x = element_text(size=12)) + labs(x="Simulation time (hours)") +
  theme(axis.title.y = element_text(size=12)) + labs(y="Simulated average time\n in system (min)") +
  theme(text = element_text(size=12)) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_shape_manual(name ="Vehicle", values = c(49,50,51,52,53,54))+
  guides(colour = guide_legend(nrow = 1))+
  theme(aspect.ratio=1/3)+geom_line(color = state)+scale_y_continuous(limits = c(0, 150))

scale1<-lm(averageTimeInSystem~(scale(gcNoVehicleUsedForPikUp)+scale(gcVehicleVelocity)+scale(IATProfileMean))^2, 
   data=subset(allPlotsData, (V59<=10& gcVehicleVelocity/277.67 <21)))

noscale<-lm(averageTimeInSystem~((gcNoVehicleUsedForPikUp)+(gcVehicleVelocity)+(IATProfileMean))^2, 
   data=subset(allPlotsData, (V59<=10& gcVehicleVelocity/277.67 <21)))

scale1<-as.data.frame(scale1$coefficients)
noscale<-as.data.frame(noscale$coefficients)
rownames(scale1)<-rownames(noscale)


as.data.frame(cbind(format((abs(scale1)/sum(abs(scale1))), scientific=FALSE),
                    format((abs(noscale)/sum(abs(noscale))), scientific=FALSE)))





stable <- FCplots(subset(allPlotsData, (V59<=10& gcVehicleVelocity/277.67 <21)), 
                  "stable", "stable", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")
stable$LMGCompareAll
stable$plotDemandVsThroughput
stable$plotDemandVsTime
test<-stable$averageTimeInSystem.model.simple
abs(as.numeric(test$coefficients*1000000))
stable$data
write(stargazer(stable$averageTimeInSystem.model.simple,
                title="FIFO Regression Results", align=TRUE), file = "C:\\projlib\\R\\FallsCreek\\LaTeX\\TestReg.txt")


changing <- FCplots(subset(allPlotsData, (V59<=20 & V59>10 &gcVehicleVelocity/277.6 <21)), 
                    "", "chaning", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")
changing$LMGCompareAll
changing$plotDemandVsThroughput
gone <- FCplots(subset(allPlotsData, (V59 >10 & gcVehicleVelocity/277.6 <21)), 
                "", "gone", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")
gone$LMGCompareAll
gone$plotDemandVsThroughput

names(FIFOstates)

write.csv(hello, file = "C:\\projlib\\R\\FallsCreek\\hello.csv")


stable <- function(XData) {
  temp <- as.data.frame(rep(1, nrow(XData)))
  
  colnames(temp) = "state"
  RData <- (cbind(XData,temp))
  for(i in 1:nrow(RData)) {
    WaitData <- t(RData[i,which( colnames(RData)== "X1" ):which( colnames(RData)== "X3" )])
    Time <- as.data.frame(rep(1:3, 1))
    StateData <- (cbind(Time,WaitData))
    colnames(StateData) = c("SimTime","WaitTime")
    #print(StateData)
    model.simple <- lm(WaitTime~ SimTime, data = StateData)
    state <- coef(model.simple)[2]
    RData[i,ncol(XData)+2] <- state
    if (state > 0.1) {
      RData[i,ncol(XData)+1] <- 0
      
    }
    # scenario <- t(RData[i,which( colnames(RData)== "X.Rule" ):which( colnames(RData)== "bagProfileMax" )])
    #  <- cbind(scenario,i, WaitData[1])
    # <- cbind(scenario,i, WaitData[2])
    # <- cbind(scenario,i, WaitData[2])
  }
  return(RData)
}

# Look at qartiles
#qartilesData <-  qartiles(TestGrouping)


#qartilesDataQ1 <- subset(qartilesData, (comb == 1))
#Q1 <- FCplots(qartilesDataQ1, "Q1", "Q1", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")

#qartilesDataQ2 <- subset(qartilesData, (comb == 2))
#Q2 <- FCplots(qartilesDataQ2, "Q2", "Q2", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")

#qartilesDataQ3 <- subset(qartilesData, (comb == 3))
#Q3 <- FCplots(qartilesDataQ3, "Q3", "Q3", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")

#qartilesDataQ4 <- subset(qartilesData, (comb == 4))
#Q4 <- FCplots(qartilesDataQ4, "Q4", "Q4", "C:\\Users\\CISR Research-04\\Documents\\Latex Documents\\FC Paper\\Fig\\R output\\")
corData<-subset(AllAllFIFO,gcVehicleVelocity/277.67<21)
corData <- as.data.frame(cbind("veh"=corData$gcNoVehicleUsedForPikUp,
                 corData$gcVehicleVelocity,
                 corData$IATProfileMean,
                 corData$averageTimeInSystem,
                 corData$averageThroughput,
                 corData$totalTripTimes))

cor(corData)



AllAllFIFO$scenarioNo
AllAllFIFO$newVar <-AllAllFIFO$gcVehicleVelocity/277.67
gp <- ggplot(subset(AllAllFIFO,((gcNoVehicleUsedForPikUp==5 | gcNoVehicleUsedForPikUp ==6) & newVar<21)), 
             aes(x=gcNoVehicleUsedForPikUp, y=averageThroughput, 
              colour=as.factor(newVar), 
              group=as.factor(newVar)))
gp + geom_line(aes(group=as.factor(newVar)), size=.6) + 
  geom_point(aes(shape=as.factor(newVar)), size=3) + 
  geom_errorbar(aes(ymax=x+se, ymin=x-se), width=.1)

library("plyr")
summ <- ddply(subset(AllAllFIFO,((gcNoVehicleUsedForPikUp==5 | gcNoVehicleUsedForPikUp ==6) & newVar<21)), 
              .(gcNoVehicleUsedForPikUp, gcVehicleVelocity), summarise, averageThroughput = mean(averageThroughput))

ggplot(subset(AllAllFIFO,((gcNoVehicleUsedForPikUp==5 | gcNoVehicleUsedForPikUp ==6) & newVar<21)), 
       aes(as.factor(gcNoVehicleUsedForPikUp), averageThroughput, colour=gcVehicleVelocity, group = gcVehicleVelocity)) +
  geom_boxplot() +
  geom_point(data = summ, aes(group=gcVehicleVelocity), colour="blue", 
             position = position_dodge(width=0.75)) +
  geom_line(data = summ, aes(group=gcVehicleVelocity), 
            position = position_dodge(width=0.75)) +
  scale_x_discrete("Dose") +
  scale_y_continuous("Response") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, hjust = 0.54, vjust = 0),
        axis.title.y = element_text(size = 12, angle = 90,  vjust = 0.25))

ggplot(subset(AllAllFIFO,(newVar<21)), 
       aes(x=as.factor(gcNoVehicleUsedForPikUp), averageThroughput, colour=gcVehicleVelocity)) +
  geom_boxplot()

