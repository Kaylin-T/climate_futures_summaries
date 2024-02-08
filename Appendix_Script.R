library(here); library(ggplot2);  library(data.table); library(lubridate); library(dplyr); library(openxlsx); library(tidyr)


#rm(list = ls())


#SiteID defined in main markdown  
#Hist.data <- "C:/Users/mjcarlson/OneDrive - DOI/Desktop/GRA Work/2023_BIBE/BIBE-Historical/"
#Future.data <- "C:/Users/mjcarlson/OneDrive - DOI/Desktop/GRA Work/2023_BIBE/"
#Output.directory <- "C:/Users/mjcarlson/OneDrive - DOI/Desktop/GRA Work/2023_BIBE/Output/"

#Hist.data <- paste0("multi-park-historical-v2/")
#Future.data <- paste0("data/FOMR/")

Hist.data <- paste0("multi-park-historical-v2/")
Future.data <- paste0("data/", params$name,"/")

#Output.directory <- paste0("data/", params$name, "/Output/")

##################
##################
#Load Data

load(paste0(Future.data,"input-data/Final_Environment.RData"))
OutDir <- Future.data
LongName <- park$UNIT_NAME
nps_boundary <- read.csv("nps_boundary.csv", header = TRUE)
ExpRegion <- read.csv("NPS_centroid_ExpRegion.csv", header=TRUE)
NPVuln <- read.csv("NPVuln_Park_Database.csv", header=TRUE)

if(nps_boundary$REGION[which(nps_boundary$UNIT_CODE == params$name)] == "Northeast"){
CF_selected <- "WarmDry_HotWet"
} else {
CF_selected <- "WarmWet_HotDry"
}

if(CF_selected == "WarmWet_HotDry") {
  FutureSubset <- CFs_all[c(1,5)]; CFs = FutureSubset  # Pick pair of climate futures.
  CF_abbreviation <- "WW-HD"
  CF1.Name <- "Warm Wet"
  CF2.Name <- "Hot Dry"
  colors2<- colors5[c(1,4)] # Select pair of climate futures - WarmWet/HotDry
  colors3<-c("white",colors2)
  col<- c("darkgray",colors2)  # WarmWet/HotDry
  CFDir = paste0(OutDir,"WarmWet_HotDry/") # for .csv's
} else{
  FutureSubset <- CFs_all[c(4,2)]; CFs = FutureSubset  # Pick pair of climate futures.
  CF_abbreviation <- "WD-HW"
  CF1.Name <- "Warm Dry"
  CF2.Name <- "Hot Wet"
  colors2<- colors5[c(3,2)] # Select pair of climate futures - HotWet/WarmDry
  colors3<-c("white",colors2)
  # col<- c("darkgray","#9A9EE5","#E10720")  # WarmWet/HotDry
  col<- c("darkgray", colors2)  # HotWet/WarmDry
  CFDir = paste0(OutDir,"WarmDry_HotWet/") # for .csv's
}
TableDir = paste0(CFDir,"tables/") # for .csv's
FigDir = paste0(CFDir,"figures/") # for .csv's

D_Annual <- openxlsx::read.xlsx(xlsxFile=paste0(TableDir,SiteID,"_",CF_abbreviation,"_Plot_data.xlsx"),sheet="D_Annual")
recurrence <- read.csv(paste0(TableDir, "precip_recurrence_interval.csv"))
AnnualWB <- read.csv(paste0(TableDir,"WB-Annual.csv")) %>% 
  left_join(WB_GCMs,by="GCM") %>% 
  mutate(sum_d.in = sum_d.mm/ 25.4,
         sum_aet.in = sum_aet.mm / 25.4)
AnnualWB <- AnnualWB %>%
  replace_na(list(CF = "Historical")) %>%
  mutate(CF = factor(CF, levels=c("Historical",CFs)))
Drought.char <- read.csv(paste0(TableDir, "Drought_characteristics.csv"))


#Historical Data
Historical.AnnualMeans <- read.csv("Annual-Averages.csv", header = TRUE)
Historical.AnnualMeans <- filter(Historical.AnnualMeans, ID == SiteID)
Historical.Regression <- read.csv(paste0("RegressionTables/", park$UNIT_CODE, "-Regression Table.csv"))
Historical.Anomalies <- read.csv(paste0(Hist.data, "ALL-Anomalies-table 2.csv"))

# Historical
Exposure.Data <- data.frame(SiteID = SiteID)
Exposure.Data$nClim.Tavg.min <- min(Historical.AnnualMeans$TavgF)
Exposure.Data$nClim.Tavg.mean <- mean(Historical.AnnualMeans$TavgF)
Exposure.Data$nClim.Tavg.max <- max(Historical.AnnualMeans$TavgF)
Exposure.Data$nClim.Prcp.min <- min(Historical.AnnualMeans$PptIn)
Exposure.Data$nClim.Prcp.mean <- mean(Historical.AnnualMeans$PptIn)
Exposure.Data$nClim.Prcp.max <- max(Historical.AnnualMeans$PptIn)
Exposure.Data$Prcp.max.hist <- max(Baseline_all$PrcpIn)
Exposure.Data$Tavg.trend <- ifelse(Historical.Regression$YrCoeff.degF.in..100yrs.[2] >0, "increased", "decreased")
Exposure.Data$Prcp.trend <- ifelse(Historical.Regression$YrCoeff.degF.in..100yrs.[4] >0, "increased", "decreased")
Exposure.Data$Tavg.rate.1900 <- Historical.Regression$YrCoeff.degF.in..100yrs.[1]
Exposure.Data$Tavg.rate.1970 <- Historical.Regression$YrCoeff.degF.in..100yrs.[2]
Exposure.Data$Prcp.rate.1900 <- Historical.Regression$YrCoeff.degF.in..100yrs.[3]
Exposure.Data$Prcp.rate.1970 <- Historical.Regression$YrCoeff.degF.in..100yrs.[4]

filtered_tavgRows <- Historical.Anomalies %>%
  group_by(SiteID) %>%
  filter(n() > 1)
selected_tavgRows <- filtered_tavgRows %>%
  slice(1)
selected_tavgRows2 <- filtered_tavgRows %>%
  slice(2)

Exposure.Data$Tavg.Anomalies.1 <- selected_tavgRows$hist.anomalies.tmean[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$Tavg.Anomalies.2 <- selected_tavgRows2$hist.anomalies.tmean[which(selected_tavgRows2$SiteID == park$UNIT_CODE)]
Exposure.Data$PrcpAbove.Anomalies.1 <- selected_tavgRows$hist.anomalies.above.prcp[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$PrcpAbove.Anomalies.2 <- selected_tavgRows2$hist.anomalies.above.prcp[which(selected_tavgRows2$SiteID == park$UNIT_CODE)]
Exposure.Data$PrcpBelow.Anomalies.1 <- selected_tavgRows$hist.anomalies.below.prcp[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$PrcpBelow.Anomalies.2 <- selected_tavgRows2$hist.anomalies.below.prcp[which(selected_tavgRows2$SiteID == park$UNIT_CODE)]
Exposure.Data$Tavg.Anomalies.recent.percent <- selected_tavgRows$recent.percent.tmean.anomaly[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$Tavg.Anomalies.years <- (Exposure.Data$Tavg.Anomalies.recent.percent/100)*22
Exposure.Data$PrcpAbove.Anomalies.recent.percent <- selected_tavgRows$recent.percent.above.prcp.anomaly[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$ObservedPrcp <- nps_boundary$ObservedPrcpChange[which(nps_boundary$UNIT_CODE == park$UNIT_CODE)]

# Future
Exposure.Data$Future.DeltaTavg.min <- min(Future_Means$DeltaTavg)
Exposure.Data$Future.DeltaTavg.max <- max(Future_Means$DeltaTavg)
Exposure.Data$Future.DeltaPr.min <- min(Future_Means$DeltaPr*365)
Exposure.Data$Future.DeltaPr.max <- max(Future_Means$DeltaPr*365)
Exposure.Data$Future.DeltaPr.min.percent <- (min(Future_Means$DeltaPr*365))/(BaseMeanPr*365)*100
Exposure.Data$Future.DeltaPr.max.percent <- (max(Future_Means$DeltaPr*365))/(BaseMeanPr*365)*100
Exposure.Data$Future.DeltaPr.99.CF1 <- D_Annual$OverPrecip99[2]
Exposure.Data$Future.DeltaPr.99.CF2 <- D_Annual$OverPrecip99[3]

# Selecting based on GCM & CF name
row_index <- match(CF1.Name, WB_GCMs$CF)
if (!is.na(row_index)){
  selected_GCM <- WB_GCMs$GCM[row_index]
  selected_rows <- which(Future_all$GCM %in% selected_GCM)

  if (length(selected_rows) > 0){
    Exposure.Data$Future.PrcpIn.CF1 <- max(Future_all$PrcpIn[selected_rows])
  }
}

row_index2 <- match(CF2.Name, WB_GCMs$CF)
if (!is.na(row_index2)){
  selected_GCM2 <- WB_GCMs$GCM[row_index2]
  selected_rows2 <- which(Future_all$GCM %in% selected_GCM2)
  
  if (length(selected_rows2) > 0){
    Exposure.Data$Future.PrcpIn.CF2 <- max(Future_all$PrcpIn[selected_rows2])
  }
}

# Selecting based on GCM & row number
# if(CF1.Name == "Warm Wet"){
#   Exposure.Data$Future.PrcpIn.CF1 <- max(Future_all$PrcpIn[which(Future_all$GCM %in% WB_GCMs$GCM[1])])
#   Exposure.Data$Future.PrcpIn.CF2 <- max(Future_all$PrcpIn[which(Future_all$GCM %in% WB_GCMs$GCM[2])])
# } else {
#   Exposure.Data$Future.PrcpIn.CF1 <- max(Future_all$PrcpIn[which(Future_all$GCM %in% WB_GCMs$GCM[3])])
#   Exposure.Data$Future.PrcpIn.CF2 <- max(Future_all$PrcpIn[which(Future_all$GCM %in% WB_GCMs$GCM[4])])
# }

Exposure.Data$DeltaTavg.CF1 <- D_Annual$TavgF[2]
Exposure.Data$DeltaTavg.CF2 <- D_Annual$TavgF[3]
Exposure.Data$DeltaPrcp.CF1 <- D_Annual$PrcpIn[2]
Exposure.Data$DeltaPrcp.CF2 <- D_Annual$PrcpIn[3]
Exposure.Data$HI.Dan.CF1 <- D_Annual$HI.Dan[2]
Exposure.Data$HI.Dan.CF2 <- D_Annual$HI.Dan[3]
Exposure.Data$TmaxHigh <- round(HistTmaxHigh, 1)
Exposure.Data$Tmax99 <- round(HistTmax99, 1)

Exposure.Data$Hist_return50 <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF=="Historical")],1)
Exposure.Data$CF1_return50  <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF==CFs[1])],1)
Exposure.Data$CF2_return50  <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF==CFs[2])],1)
Exposure.Data$Hist_return100 <- round(recurrence$GEV[which(recurrence$return == 100 & recurrence$CF=="Historical")],1)
Exposure.Data$CF1_return100  <- round(recurrence$GEV[which(recurrence$return == 100 & recurrence$CF==CFs[1])],1)
Exposure.Data$CF2_return100  <- round(recurrence$GEV[which(recurrence$return == 100 & recurrence$CF==CFs[2])],1)
toleranceP <- 0.5
toleranceN <- -0.5
CF1_GEV <- recurrence %>% filter(CF == CFs[1]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(GEV)
CF2_GEV <- recurrence %>% filter(CF == CFs[2]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(GEV)
returnRow1 <- as.integer(recurrence %>% filter(CF == CFs[1]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(return))
if (CF1_GEV - Exposure.Data$Hist_return50 <= toleranceP | toleranceN){
  Exposure.Data$CF1_return.year <- as.integer(returnRow1)
} else {
  Exposure.Data$CF1_return.year <- "N/A"
}
returnRow2 <- as.integer(recurrence %>% filter(CF == CFs[2]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(return))
if (CF2_GEV - Exposure.Data$Hist_return50 <= toleranceP | toleranceN){
  Exposure.Data$CF2_return.year <- as.integer(returnRow2)
} else {
  Exposure.Data$CF2_return.year <- "N/A"
}
# Exposure.Data$CF1_return.year <- as.integer(recurrence %>% filter(CF == CFs[1]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(return))
# Exposure.Data$CF2_return.year <- as.integer(recurrence %>% filter(CF == CFs[2]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(return))
Exposure.Data$HistPrecip99 <- HistPr99
Exposure.Data$Hist.meanWB<-mean(AnnualWB$sum_d.in[which(AnnualWB$year<=2012)])
Exposure.Data$CF1.WBdelta <- mean(AnnualWB$sum_d.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                            AnnualWB$CF == CFs[1])]) - Exposure.Data$Hist.meanWB
Exposure.Data$CF2.WBdelta <- mean(AnnualWB$sum_d.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                            AnnualWB$CF == CFs[2])]) - Exposure.Data$Hist.meanWB
Exposure.Data <- Exposure.Data %>% mutate_if(is.numeric, round, digits=1) #Rounding all variables
D_Annual <- D_Annual %>% mutate_if(is.numeric, round, digits=1)
Drought.char <- Drought.char %>% mutate_if(is.numeric, round, digits=1)
Exposure.Data$CF1_return.percent <- round((1/Exposure.Data$CF1_return.year)*100,1)
Exposure.Data$CF2_return.percent <- round((1/Exposure.Data$CF2_return.year)*100,1)


# ADDED EXPOSURE REPORT VARIABLES

# Historical trend for more/less frequent precip anomalies
Exposure.Data$PrcpAnomalies.Abovetrend <- if(Exposure.Data$PrcpAbove.Anomalies.recent.percent > 0) {
  "more frequently"
} else{
  if(Exposure.Data$PrcpAbove.Anomalies.recent.percent < 0){
  "less frequently"
  } else  {
    "as frequently as they did in the past"
  }
}

# Graph colors based on CF
Exposure.Data$Colors <- ifelse(CF1.Name == "Warm Wet", "blue, and the Hot Dry climate future in red", "orange, and the Hot Wet climate future in green")

# Precipitation changes either increasing for all models or just one
Exposure.Data$PrcpModels <- ifelse(Exposure.Data$Future.DeltaPr.min <0, paste0("Projected changes in precipitation are less clear, with some models projecting a decrease in average annual precipitation by ", Exposure.Data$Future.DeltaPr.min, " inches (",Exposure.Data$Future.DeltaPr.min.percent,"%) and others projecting an increase of ", Exposure.Data$Future.DeltaPr.max, " inches (",Exposure.Data$Future.DeltaPr.max.percent,"%)."), paste0("All climate models project increases in precipitation, ranging from ", Exposure.Data$Future.DeltaPr.min, " (",Exposure.Data$Future.DeltaPr.min.percent,"%) to ", Exposure.Data$Future.DeltaPr.max, " inches (",Exposure.Data$Future.DeltaPr.max.percent,"%)."))
Exposure.Data$WettestCF <- ifelse(Exposure.Data$DeltaPrcp.CF1 > Exposure.Data$DeltaPrcp.CF2, CF1.Name, CF2.Name)
Exposure.Data$DriestCF <- ifelse(Exposure.Data$DeltaPrcp.CF1 < Exposure.Data$DeltaPrcp.CF2, CF1.Name, CF2.Name)
Exposure.Data$PrecipFutures <- ifelse(Exposure.Data$DeltaPrcp.CF1 & Exposure.Data$DeltaPrcp.CF2 > 0, paste("even very dry years could still occur under both climate futures, despite a positive trend in precipitation"), paste("even very dry years could still occur under the", Exposure.Data$WettestCF, "future and very wet years could still be experienced under the", Exposure.Data$DriestCF, "future"))
Exposure.Data$PrcpSeasons <- if(D_Annual$PrcpIn[2] & D_Annual$PrcpIn[3] > 0){
  "increase"
} else {
    if(D_Annual$PrcpIn[2] & D_Annual$PrcpIn[3] < 0){
      "decrease"
    } else {
      "increase for one climate future"
  }
}

# Trend in extreme precipitation increasing for both CFs or just one
Exposure.Data$Prcp24HrTrend <- if(Exposure.Data$Future.PrcpIn.CF1 & Exposure.Data$Future.PrcpIn.CF2 > Exposure.Data$Prcp.max.hist){
  paste0("higher not only in the ", CF1.Name, " scenario but also in the ", CF2.Name, "scenario")
} else {
  if(Exposure.Data$Future.PrcpIn.CF1 > Exposure.Data$Prcp.max.hist){
    paste0("higher in the ", CF1.Name, " scenario but not in the ", CF2.Name, " scenario")
  } else {
    if(Exposure.Data$Future.PrcpIn.CF2 > Exposure.Data$Prcp.max.hist){
      paste0("higher in the ", CF2.Name, " scenario but not in the ", CF1.Name, " scenario")
    } else  {
      paste0("N/A")
    }
  }
}

#### Might be removed
# Exposure.Data$PrcpDrought <- if(Exposure.Data$DeltaPrcp.CF1 & Exposure.Data$DeltaPrcp.CF2 > 0){
#   "While extreme precipitation may be a concern in the future,"
# } else {
#   if(Exposure.Data$DeltaPrcp.CF1 & Exposure.Data$DeltaPrcp.CF2 < 0){
#     "Exacerbated by decreasing precipitation under both climate futures,"
#   } else {
#     "While extreme precipitation may be a concern in the future,"
#   }
# }

# Future Tavg exceeds historical highs for which CFs
All.Means <- openxlsx::read.xlsx(xlsxFile=paste0(TableDir,SiteID,"_",CF_abbreviation,"_Plot_data.xlsx"),sheet="Means")
Exposure.Data$TavgCompare <- ifelse(All.Means$TavgF[2] > All.Means$TavgF[1] & All.Means$TavgF[3] > All.Means$TavgF[1], paste("both climate futures"), paste("the", CF2.Name, "future"))

# WB increasing/decreasing trend for one/all CFs
Exposure.Data$WBtrend <- if(Exposure.Data$CF1.WBdelta > 0 & Exposure.Data$CF2.WBdelta > 0){
  "projected to increase in both climate futures relative to the historical period (1979-2020)"
  } else  {
    if(Exposure.Data$CF1.WBdelta < 0 & Exposure.Data$CF2.WBdelta < 0){
      "projected to decrease in both climate futures relative to the historical period (1979-2020)"
    } else {
      if(Exposure.Data$CF1.WBdelta == 0 & Exposure.Data$CF2.WBdelta == 0){
        "projected to be the same in both climate futures relative to the historical period (1979-2020)"
      } else  {
      "projected to increase in one climate future relative to the historical period (1979-2020)"
    }
  }
}

Exposure.Data$WBExtremes <- if(Exposure.Data$CF1.WBdelta > 0 & Exposure.Data$CF2.WBdelta > 0){
  "more years that are drier than in the past, some notably wet years, and fewer years that would have historically been considered 'average.'"
} else  {
  if(Exposure.Data$CF1.WBdelta < 0 & Exposure.Data$CF2.WBdelta < 0){
    "more years that are wetter than in the past, and fewer years that would have historically been considered 'average.'"
  } else {
    if(Exposure.Data$CF1.WBdelta == 0 & Exposure.Data$CF2.WBdelta == 0){
      "similar conditions to the past."
    } else  {
      "more years that are wetter than in the past, some notably dry years, and fewer years that would have historically been considered 'average.'"
    }
  }
}

Exposure.Data$wbCF2 <- if(Exposure.Data$CF2.WBdelta > 0){
  "the average year will have a water deficit comparable to years that currently would be considered dry. Under this climate future, managers can expect most years to have reduced plant growth, lower stream flow, and increased fire risk and plant stress."
} else  {
    "the average year will have a water deficit comparable to what the park experiences historically."
}

Exposure.Data$ExPrcTrend <- if(D_Annual$OverPrecip99[2] > 0 & D_Annual$OverPrecip99[3] > 0){paste(
  "Extreme precipitation is projected to increase under both climate futures")
} else  {
  if(D_Annual$OverPrecip99[2] < 0 & D_Annual$OverPrecip99[3] < 0){paste(
    "Extreme precipitation is projected to decrease under both climate futures")
  } else {
    if (D_Annual$OverPrecip99[2] > 0 & D_Annual$OverPrecip99[3] < 0){paste(
      "Extreme precipitation is projected to increase under the", CF1.Name, "climate future but decrease under the", CF2.Name, "climate future")
    } else {
      if (D_Annual$OverPrecip99[2] < 0 & D_Annual$OverPrecip99[3] > 0)
      {paste(
        "Extreme precipitation is projected to increase under the", CF2.Name, "climate future but decrease under the", CF1.Name, "climate future")
      } else  {
        paste("Extreme precipitation is projected to remain similar under both climate futures")
      }
    }
  }
}

filtered_rows <- Historical.Anomalies %>%
  group_by(SiteID) %>%
  filter(n() > 1)
selected_rows <- filtered_rows %>%
  slice(1)

#### Recently removed text
# Exposure.Data$HistPrcpTrend <- if(selected_rows$recent.percent.above.prcp.anomaly[which(selected_rows$SiteID == park$UNIT_CODE)] > 0){
#   paste0("Years with historically high precipitation totals (e.g., ", Exposure.Data$PrcpAbove.Anomalies.1, " and ", Exposure.Data$PrcpAbove.Anomalies.2, ") occur more frequently than they did in the past")
# } else {
#   paste0("Years with historically low precipitation totals (e.g., ", Exposure.Data$PrcpBelow.Anomalies.1, " and ", Exposure.Data$PrcpBelow.Anomalies.2, ") occur less frequently than they did in the past")
# }

##### Old return interval data
# Exposure.Data$CF1PrcpTrend <- ifelse(Exposure.Data$CF1_return50 > Exposure.Data$Hist_return50, "increase to", "decrease to")
# Exposure.Data$CF2PrcpTrend <- ifelse(Exposure.Data$CF2_return50 > Exposure.Data$Hist_return50, "increase", "decrease")
# Exposure.Data$FloodingTrend <- ifelse(Exposure.Data$CF1PrcpTrend == "increase to" & Exposure.Data$CF2PrcpTrend == "increase to", "worse than", "similar to")

# Extreme precip increasing/decreasing trend
# Exposure.Data$ExPrcpTrend <- if(Exposure.Data$CF1_return100 > Exposure.Data$Hist_return100 & Exposure.Data$CF2_return100 > Exposure.Data$Hist_return100){paste(
#   "Extreme precipitation is expected to increase under both climate futures")
# } else  {
#   if(Exposure.Data$CF1_return100 < Exposure.Data$Hist_return100 & Exposure.Data$CF2_return100 > Exposure.Data$Hist_return100){paste(
#     "Extreme precipitation is expected to increase under the", CF2.Name, "climate future")
#   } else {
#     if(Exposure.Data$CF1_return100 > Exposure.Data$Hist_return100 & Exposure.Data$CF2_return100 < Exposure.Data$Hist_return100){paste(
#       "Extreme precipitation is expected to increase under the", CF1.Name, "climate future")
#     } else  {paste(
#       "Extreme precipitation is not expected to increase under either climate future")
#     }
#   }
# }


# Naming convention explanation
Exposure.Data$Conven <- ifelse(CF1.Name == "Warm Wet", "climate future might be more arid than what the park experienced historically but the name denotes that it projects wetter conditions than the", "climate future might be wetter than what the park experienced historically but the name denotes that it projects drier conditions than the")

  
# Drought
Exposure.Data$Severity1 <- ifelse(Drought.char$Severity[which(Drought.char$CF == CF1.Name)] > Drought.char$Severity[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Severity <- ifelse(Drought.char$Severity[which(Drought.char$CF == CF2.Name)] > Drought.char$Severity[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Duration1 <- ifelse(Drought.char$Duration[which(Drought.char$CF == CF1.Name)] > Drought.char$Duration[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Duration <- ifelse(Drought.char$Duration[which(Drought.char$CF == CF2.Name)] > Drought.char$Duration[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$DrtFree1 <- ifelse(Drought.char$Drt.Free[which(Drought.char$CF == CF1.Name)] < Drought.char$Drt.Free[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$DrtFree <- ifelse(Drought.char$Drt.Free[which(Drought.char$CF == CF2.Name)] < Drought.char$Drt.Free[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Freq1 <- ifelse(Drought.char$Frequency[which(Drought.char$CF == CF1.Name)] > Drought.char$Frequency[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Freq <- ifelse(Drought.char$Frequency[which(Drought.char$CF == CF2.Name)] > Drought.char$Frequency[which(Drought.char$CF == "Historical")], TRUE, FALSE)

drought2_combinations <- expand.grid(
  Exposure.Data$Severity == c(TRUE, FALSE),
  Exposure.Data$Duration == c(TRUE, FALSE),
  Exposure.Data$DrtFree == c(TRUE, FALSE),
  Exposure.Data$Freq == c(TRUE, FALSE)
)
combinations2_df <- as.data.frame(drought2_combinations)
col_names <- c("Severity", "Duration", "Drt.Free", "Frequency")
colnames(combinations2_df) <- col_names

Exposure.Data$MostSevere <- ifelse(Drought.char$Severity[which(Drought.char$CF == CF1.Name)] > Drought.char$Severity[which(Drought.char$CF == CF2.Name)], CF1.Name, CF2.Name)

Exposure.Data$DrtCF1Duration <- ifelse(Exposure.Data$Duration1 == TRUE, "longer", "shorter")
Exposure.Data$DrtCF1Severity <- ifelse(Exposure.Data$Severity1 == TRUE, "more", "less")
Exposure.Data$DrtCF1Freq <- ifelse(Exposure.Data$Freq1 == TRUE, "more", "less")
Exposure.Data$DrtCF2Duration <- ifelse(Exposure.Data$Duration == TRUE, "longer", "shorter")
Exposure.Data$DrtCF2Severity <- ifelse(Exposure.Data$Severity == TRUE, "increasingly worsen", "remain similar or improve")
Exposure.Data$DrtCF2DrtFree <- ifelse(Exposure.Data$DrtFree == FALSE, "increase", "decrease")
Exposure.Data$DrtCF2Freq <- if(Exposure.Data$DrtFree == TRUE){
  paste("most years are expected to have below average SPEI values. Years with below average SPEI values will be categorized by surface water deficits and will likely experience drought conditions.Managers should prepare for shorter drought-free intervals with more prolonged drought than has been experienced in the past and consider adaptations for surface water and drought intolerant plant species.")
} else {
  if (Exposure.Data$Severity == TRUE){
    paste("fewer years are expected to have below average SPEI values, and will likely experience longer drought-free intervals but more intense drought conditions. Years with below-average SPEI values, such as the ones projected in Figure 7 (upper plot), would be categorized by surface water deficits and would likely experience drought conditions. Managers should prepare for longer drought-free intervals but more severe drought than has been experienced in the past and consider adaptations for surface water and drought intolerant plant species.")
  } else {
  paste("fewer years are expected to have below average SPEI values, and will likely experience longer drought-free intervals. Years with below-average SPEI values, such as the ones projected in Figure 7 (upper plot), would be categorized by surface water deficits and would likely experience drought conditions. For", CF2.Name, ", managers can expect to see fewer years with surface water deficits.")
  }
}

### Old drought text
# Exposure.Data$DrtTrend <- if(Exposure.Data$Severity1 == TRUE & Exposure.Data$Severity == TRUE){
#   paste("Drought is expected to worsen under both climate futures, relative to the past")
# } else {
#   if (Exposure.Data$Severity1 == TRUE | Exposure.Data$Severity == TRUE){
#     paste("Drought is expected to worsen under the", Exposure.Data$MostSevere, "climate future, relative to the past")
#   } else {
#     paste("Drought is expected to remain similar to past conditions under both climate futures")
#   }
# }

Exposure.Data$DrtCFsTrend <- if(Exposure.Data$DrtFree == TRUE & Exposure.Data$Severity == TRUE & Exposure.Data$Duration == TRUE & Exposure.Data$DrtFree1 == TRUE & Exposure.Data$Severity1 == TRUE & Exposure.Data$Duration1 == TRUE){
  paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought duration and severity are projected to increase and the drought-free interval is projected to decrease, relative to the past. These projections are aligned with research showing that climate change may lead to droughts that are longer and more severe than what has occurred historically, with shorter periods between drought events for resources to recover")
} else {
  if (Exposure.Data$DrtFree == FALSE & Exposure.Data$Severity == TRUE & Exposure.Data$Duration == TRUE & Exposure.Data$DrtFree1 == FALSE & Exposure.Data$Severity1 == TRUE & Exposure.Data$Duration1 == TRUE){
    paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought duration and severity are projected to increase, but the drought-free interval is also projected to increase, relative to the past. These projections are aligned with research showing that climate change may lead to droughts that are longer and more severe than what has occurred historically")
  } else {
      if (Exposure.Data$DrtFree == FALSE & Exposure.Data$Severity == FALSE & Exposure.Data$Duration == TRUE & Exposure.Data$DrtFree1 == FALSE & Exposure.Data$Severity1 == FALSE & Exposure.Data$Duration1 == TRUE){
        paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought duration is projected to increase, but severity is projected to decrease and the drought-free interval is projected to increase, relative to the past. These projections are aligned with research showing that climate change may lead to droughts that are longer than what has occurred historically")
      } else {
        if (Exposure.Data$DrtFree == FALSE & Exposure.Data$Severity == TRUE & Exposure.Data$Duration == FALSE & Exposure.Data$DrtFree1 == FALSE & Exposure.Data$Severity1 == TRUE & Exposure.Data$Duration1 == FALSE){
          paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought severity is projected to increase, but duration is projected to decrease and the drought-free interval is projected to increase, relative to the past. These projections are aligned with research showing that climate change may lead to droughts that are more severe than what has occurred historically")
        } else {
          if (Exposure.Data$DrtFree == TRUE & Exposure.Data$Severity == FALSE & Exposure.Data$Duration == TRUE & Exposure.Data$DrtFree1 == TRUE & Exposure.Data$Severity1 == FALSE & Exposure.Data$Duration1 == TRUE){
            paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought duration is projected to increase and the drought-free interval is projected to decrease, but severity is also projected to decrease, relative to the past. These projections are aligned with research showing that climate change may lead to droughts that are longer and more frequent than what has occurred historically")
          } else {
            if (Exposure.Data$DrtFree == TRUE & Exposure.Data$Severity == TRUE & Exposure.Data$Duration == FALSE & Exposure.Data$DrtFree1 == TRUE & Exposure.Data$Severity1 == TRUE & Exposure.Data$Duration1 == FALSE){
              paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought severity is projected to increase considerably and the drought-free interval is projected to decrease, but the duration is also projected to decrease, relative to the past. These projections are aligned with research showing that climate change may lead to droughts that are longer and more severe than what has occurred historically")
            } else {
              paste0(" ")
            }
          }
        }
      }
  }
}


Exposure.Data$Drt2Trend <- if(Exposure.Data$Severity == TRUE & Exposure.Data$DrtFree == FALSE){
  paste("increasingly severe compared to historical droughts, represented by the black bars in Figure 7. However, as we near mid-century (gray shaded area in Figure 7), drought-free intervals are also expected to increase. This means that managers should prepare for less frequent but more severe drought than has been experienced in the past and consider adaptations for surface water and drought intolerant plant species")
} else {
  if (Exposure.Data$Severity == TRUE & Exposure.Data$DrtFree == TRUE){
    paste("increasingly severe compared to historical droughts, represented by the black bars in Figure 7. Additionally, as we near mid-century (gray shaded area in Figure 7), drought-free intervals are expected to decrease. This means that managers should prepare for more frequent and severe drought than has been experienced in the past and consider adaptations for surface water and drought intolerant plant species")
  } else {
    if (Exposure.Data$Severity == FALSE & Exposure.Data$DrtFree == TRUE) {
    paste("less severe or remain similar to historical droughts, represented by the black bars in Figure 7. However, as we near mid-century (gray shaded area in Figure 7), drought-free intervals are also expected to decrease. This means that managers should prepare for more frequent but less severe drought than has been experienced in the past and consider adaptations for surface water and drought intolerant plant species")
  } else {
    if (Exposure.Data$Severity == FALSE & Exposure.Data$DrtFree == FALSE) {
      paste("less severe or remain similar to historical droughts, represented by the black bars in Figure 7. Additionally, as we near mid-century (gray shaded area in Figure 7), drought-free intervals are expected to increase. This means that parks may experience less frequent drought with similar severity to what has been experienced in the past")
    } else {
      paste("N/A")
    }
  }
  }
}

Exposure.Data$Drt1Condition <- ifelse(Exposure.Data$Severity1 == Exposure.Data$DrtFree1, paste("with"), paste("but"))


Exposure.Data$Drt1Trend <- if(Exposure.Data$Severity1 == TRUE & Exposure.Data$DrtFree1 == FALSE){
  paste0(params$name, " is also projected to experience longer drought-free intervals between drought events")
} else {
  if (Exposure.Data$Severity1 == TRUE & Exposure.Data$DrtFree1 == TRUE){
    paste0("shorter drought-free intervals between drought events for ", params$name, " resources to recover")
  } else {
    if (Exposure.Data$Severity1 == FALSE & Exposure.Data$DrtFree1 == TRUE) {
      paste0(params$name, " is also projected to experience shorter drought-free intervals to recover between drought events")
  } else {
      if (Exposure.Data$Severity1 == FALSE & Exposure.Data$DrtFree1 == FALSE){
        paste0("longer drought-free intervals between drought events for ", params$name, " resources to recover")
      } else {
        paste0("N/A")
      }
    }
  }
}

Exposure.Data$DrtRsrch <- if((Exposure.Data$DrtFree == TRUE | Exposure.Data$DrtFree1) & (Exposure.Data$Severity == TRUE | Exposure.Data$Severity1 == TRUE) & (Exposure.Data$Duration == TRUE | Exposure.Data$Duration1 == TRUE)){
  paste0("These projections are aligned with research showing that climate change may lead to droughts that are longer and more severe than what has occurred historically, with shorter periods between drought events for resources to recover")
} else {
  if ((Exposure.Data$DrtFree == FALSE | Exposure.Data$DrtFree1 == FALSE) & (Exposure.Data$Severity == TRUE | Exposure.Data$Severity1 == TRUE) & (Exposure.Data$Duration == TRUE | Exposure.Data$Duration1 == TRUE)){
    paste0("These projections are aligned with research showing that climate change may lead to droughts that are longer and more severe than what has occurred historically")
  } else {
    if ((Exposure.Data$DrtFree == FALSE | Exposure.Data$DrtFree1 == FALSE) & (Exposure.Data$Severity == FALSE | Exposure.Data$Severity1 == FALSE) & (Exposure.Data$Duration == TRUE | Exposure.Data$Duration1 == TRUE)){
      paste0("These projections are aligned with research showing that climate change may lead to droughts that are longer than what has occurred historically")
    } else {
      if ((Exposure.Data$DrtFree == FALSE | Exposure.Data$DrtFree1 == FALSE) & (Exposure.Data$Severity == TRUE | Exposure.Data$Severity1 == TRUE) & (Exposure.Data$Duration == FALSE | Exposure.Data$Duration1 == FALSE)){
        paste0("These projections are aligned with research showing that climate change may lead to droughts that are more severe than what has occurred historically")
      } else {
        if ((Exposure.Data$DrtFree == TRUE | Exposure.Data$DrtFree1 == TRUE) & (Exposure.Data$Severity == FALSE | Exposure.Data$Severity1 == FALSE) & (Exposure.Data$Duration == TRUE | Exposure.Data$Duration1 == TRUE)){
          paste0("These projections are aligned with research showing that climate change may lead to droughts that are longer and more frequent than what has occurred historically")
        } else {
          if ((Exposure.Data$DrtFree == TRUE | Exposure.Data$DrtFree1 == TRUE) & (Exposure.Data$Severity == TRUE | Exposure.Data$Severity1 == TRUE) & (Exposure.Data$Duration == FALSE | Exposure.Data$Duration1 == FALSE)){
            paste0("These projections are aligned with research showing that climate change may lead to droughts that are more frequent and severe than what has occurred historically")
          } else {
            paste0(" ")
          }
        }
      }
    }
  }
}


# Figure 5 edits
Exposure.Data$ExtremeTemps <- ifelse(Exposure.Data$HI.Dan.CF2 > 4, paste("Extreme temperatures are expected to increase at", params$name, "under both climate futures (Figure 5), with an additional", Exposure.Data$HI.Dan.CF1, "days each year exceeding the dangerous heat index threshold under the", CF1.Name, "climate future and a more pronounced increase of", Exposure.Data$HI.Dan.CF2, "days each year under the", CF2.Name, "future.

Dangerously hot days can pose health risks to park employees and visitors, particularly affecting vulnerable groups such as children, the elderly, and individuals with preexisting health conditions. The Occupational Safety and Health Administration (OSHA) has established guidelines associated with heat index classifications and protective measures that should be taken for ranges of heat index values. Dangerous heat index days are days that exceed a heat index (a combination of heat and humidity) of 105 °F. In 2004, the NPS Risk Management Office issued guidance that general heat stress controls should be applied when the heat index exceeds 105 °F, which is within the “dangerous” heat index range (NPS 2004)."),
paste0("Extreme temperatures are expected to increase at ", params$name, " under both climate futures (Figure 5), with an additional ", D_Annual$Tmax99[2], " days each year exceeding the historical 99th percentile (",Exposure.Data$Tmax99," °F) under the ", CF1.Name, " climate future and a more pronounced increase of ", D_Annual$Tmax99[3], " days each year under the ", CF2.Name, " future."))

Exposure.Data$TempsFigures <- ifelse(Exposure.Data$HI.Dan.CF2 > 4, paste0("The upper bar graph represents the average number of days annually with temperatures greater than the historical 99th percentile (",Exposure.Data$Tmax99," °F) historically (1979-2012) and for the two climate futures (2050). The bottom bar graph represents the average annual number of dangerous heat index days historically and for the two climate futures. Dangerous heat index days are days that exceed 105 °F."),
paste0("Metrics of extreme temperature at ", params$name,". The bar graph represents the average number of days annually with temperatures greater than the historical 99th percentile (",Exposure.Data$Tmax99," °F) historically (1979-2012) and for the two climate futures (2050)."))
  
fig5code <- ifelse(Exposure.Data$HI.Dan.CF2 > 4, "OverTmax99-HI.Dan-Panel.jpg", "Tmax99-Annual-bar.png")

# Observed precipitation change based on region - Not using this?
Exposure.Data$PrcpRegion <- nps_boundary$ObservedPrcpChange[which(nps_boundary$UNIT_CODE == park$UNIT_CODE)]
# Observed precipitation change increase or decrease - Not using this?
Exposure.Data$RegionPrcpDI <- ifelse(Exposure.Data$PrcpRegion >0, "increase", "decrease")

# Appendix: Calculations for change
Exposure.Data$PrcpChangeCF1 <- Exposure.Data$Future.PrcpIn.CF1 - Exposure.Data$Prcp.max.hist
Exposure.Data$PrcpChangeCF2 <- Exposure.Data$Future.PrcpIn.CF2 - Exposure.Data$Prcp.max.hist
Exposure.Data$DrtDurChangeCF1 <- Drought.char$Duration[2] - Drought.char$Duration[1]
Exposure.Data$DrtDurChangeCF2 <- Drought.char$Duration[3] - Drought.char$Duration[1]
Exposure.Data$DrtFreeChangeCF1 <- Drought.char$Drt.Free[2] - Drought.char$Drt.Free[1]
Exposure.Data$DrtFreeChangeCF2 <- Drought.char$Drt.Free[3] - Drought.char$Drt.Free[1]
Exposure.Data$DrtSevChangeCF1 <- Drought.char$Severity[2] - Drought.char$Severity[1]
Exposure.Data$DrtSevChangeCF2 <- Drought.char$Severity[3] - Drought.char$Severity[1]

# Appendix: Seasonal changes
Exposure.Data$TempWinterHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Winter")]
Exposure.Data$DTempWinterCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Winter")]
Exposure.Data$DTempWinterCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Winter")]
Exposure.Data$TempSpringHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Spring")]
Exposure.Data$DTempSpringCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Spring")]
Exposure.Data$DTempSpringCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Spring")]
Exposure.Data$TempSummerHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Summer")]
Exposure.Data$DTempSummerCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Summer")]
Exposure.Data$DTempSummerCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Summer")]
Exposure.Data$TempFallHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Fall")]
Exposure.Data$DTempFallCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Fall")]
Exposure.Data$DTempFallCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Fall")]

Exposure.Data$PrcpWinterHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Winter")]
Exposure.Data$DPrcpWinterCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Winter")]
Exposure.Data$DPrcpWinterCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Winter")]
Exposure.Data$PrcpSpringHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Spring")]
Exposure.Data$DPrcpSpringCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Spring")]
Exposure.Data$DPrcpSpringCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Spring")]
Exposure.Data$PrcpSummerHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Summer")]
Exposure.Data$DPrcpSummerCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Summer")]
Exposure.Data$DPrcpSummerCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Summer")]
Exposure.Data$PrcpFallHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Fall")]
Exposure.Data$DPrcpFallCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Fall")]
Exposure.Data$DPrcpFallCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Fall")]

Exposure.Data <- Exposure.Data %>% mutate_if(is.numeric, round, digits=1) #Rounding all variables
      
#write.csv(Exposure.Data, paste0(Output.directory,"-",SiteID,"-ExposureData.csv"))


# Table for Appendix 1
Appendix1 <- data.frame(matrix(nrow=12, ncol=3))
row.names(Appendix1) <- c("ANN Tmean", "ANN Prcp", "Tmax over 95th", "HI Dangerous","50-yr return", "50-yr event",
                          "Prcp over 95th", "Drt Dur", "Drt return", "Drt severity", "CWD", "AET")
colnames(Appendix1) <- c(CFs, "Historical")

Appendix1[1] <- c(Exposure.Data$DeltaTavg.CF1,Exposure.Data$DeltaPrcp.CF1,D_Annual$Tmax99[2],Exposure.Data$HI.Dan.CF1,Exposure.Data$CF1_return.year,
                  Exposure.Data$CF1_return50,D_Annual$OverPrecip99[2],Drought.char$Duration[2],
                  Drought.char$Drt.Free[2],Drought.char$Severity[2],Exposure.Data$CF1.WBdelta,
                  mean(AnnualWB$sum_aet.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                   AnnualWB$CF == CFs[1])]) - mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)]))
round(Appendix1, digits = 1)

Appendix1[2] <- c(Exposure.Data$DeltaTavg.CF2,Exposure.Data$DeltaPrcp.CF2,D_Annual$Tmax99[3],Exposure.Data$HI.Dan.CF2,Exposure.Data$CF2_return.year,
                  Exposure.Data$CF2_return50,D_Annual$OverPrecip99[3],Drought.char$Duration[3],
                  Drought.char$Drt.Free[3],Drought.char$Severity[3],Exposure.Data$CF2.WBdelta,
                  mean(AnnualWB$sum_aet.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                   AnnualWB$CF == CFs[2])]) - mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)]))

Appendix1[3] <- c(D_Annual$TavgF[1],D_Annual$PrcpIn[1],D_Annual$Tmax99[1],D_Annual$HI.Dan[1],50,Exposure.Data$Hist_return50,
                  D_Annual$OverPrecip99[1],Drought.char$Duration[1],Drought.char$Drt.Free[3],Drought.char$Severity[1],Exposure.Data$Hist.meanWB,
                  mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)]))

#write.csv(Appendix1, paste0(Output.directory,"-",SiteID,"-Appendix1.csv"))


Exposure.Data$AET1 <- round(mean(AnnualWB$sum_aet.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                       AnnualWB$CF == CFs[1])]) - mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)]), digits=1)
Exposure.Data$AET2 <- round(mean(AnnualWB$sum_aet.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                       AnnualWB$CF == CFs[2])]) - mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)]), digits=1)
Exposure.Data$AET3 <- round(mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)]), digits=1)

