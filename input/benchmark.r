#############################################################
##  STEO Benchmarking Code
##  This is a testing program that runs both Options 1 and 2.
#############################################################

## We load the zoo package, a contributed package that must be installed before it can be loaded.
## Installation seems to be unzipping the .zip file into R/<version>/library/zoo
library(zoo)

options (warn=1)               ## print warnings as they occur
############################################################
## We set default parameter values for benchmarking.
############################################################
SerNameDefault <- 'Series'
OptionDefault <- 1
ChTolFacDefault <- 1
HW_alphaDefault <- NULL
HW_betaDefault <- NULL


############################################################
## We read in STEO and NEMS series from comma-delimited
##  files produced by the NEMS Fortran code.
############################################################

STEOData <- read.csv('STEO_TO_R.csv')
NEMSData <- read.csv('NEMS_TO_R.csv')

## We convert the series to zoo objects
SeriesName <- colnames(NEMSData)[2]
STEOSeries <- zoo(STEOData[,2], STEOData$Year)
NEMSSeries <- zoo(NEMSData[,2], NEMSData$Year)

## We identify the first and last years of the series.
FirstSTEOYear <- min(index(STEOSeries))
LastSTEOYear <- max(index(STEOSeries))
LastNEMSYear <- max(index(NEMSSeries))
FirstNEMSYear <- LastSTEOYear + 1

## We truncate the NEMS series to remove the STEO years.
## NEMSSeries is the series starting the year after STEO stops
## FullNEMSSeries is the full, untruncated series
FullNEMSSeries <- NEMSSeries
NEMSSeries <- window(NEMSSeries, start = FirstNEMSYear, end=LastNEMSYear)

## We find the number of years to extend the STEO trend line.
STEOExtend <- LastNEMSYear - LastSTEOYear

## We find the minimum value of the STEO and NEMS series.
##  This will be used as the default series minimum.
STEOmin <- min(coredata(STEOSeries))
NEMSmin <- min(coredata(FullNEMSSeries))
Defaultmin <- min(STEOmin, NEMSmin)

## We find the maximum value of the STEO and NEMS series.
##  This will be used as the default series maximum.
STEOmax <- max(coredata(STEOSeries))
NEMSmax <- max(coredata(FullNEMSSeries))
Defaultmax <- max(STEOmax, NEMSmax)

#############################################################
## We read in the parameter file.  This is a comma-delimited
##  file that must be produced by the NEMS Fortran code.
##  It can contain missing values for some or all parameters.
#############################################################
BenchParam <- read.csv('RBM_PARAM.csv')
BenchParam <- BenchParam[which(BenchParam$SeriesName == SeriesName),]

## We create R objects to hold the parameters.
Option <- BenchParam$Option[1]
ChangeToleranceFactor <- BenchParam$ChMult[1]
HW_alpha <- BenchParam$alpha[1]
HW_beta <- BenchParam$beta[1]
SeriesMin <- BenchParam$SeriesMin[1]
SeriesMax <- BenchParam$SeriesMax[1]


##############################################################
## We set the parameters to default values if missing.
##############################################################
SeriesName <- ifelse(is.na(SeriesName), SerNameDefault, SeriesName) 
Option <- ifelse(is.na(Option), OptionDefault, Option)
ChangeToleranceFactor <- ifelse(is.na(ChangeToleranceFactor), ChTolFacDefault, ChangeToleranceFactor)

####################################################################
##  We set the HW paramters to NULL if not specified.
##  R will optimize the values, based on squared prediction errors.
####################################################################
if (is.na(HW_alpha))
   {HW_alpha <- NULL
     }
   
if (is.na(HW_beta))
   {HW_beta <- NULL
   }

############################################################
## The default minimum (or max) for SeriesMin (or max) is
##   the actual STEO and NEMS minimum (or max). 
############################################################
SeriesMin <- ifelse(is.na(SeriesMin), Defaultmin, SeriesMin)
SeriesMax <- ifelse(is.na(SeriesMax), Defaultmax, SeriesMax)


###########################################################################
## We create a user-defined function for splicing two zoo objects.
###########################################################################

ZooSplice <- function(X, Y) {
  Xdata <- coredata(X)
  Ydata <- coredata(Y)
  Xind <- index(X)
  Yind <- index(Y)
  XYdata <- c(Xdata, Ydata)
  XYind <- c(Xind, Yind)
  XY <- zoo(XYdata, XYind)
  
  return(XY)
}

###########################################################################
## We create a user-defined function for linear splicing.
## The function takes two zoo objects and a numeric change tolerance.
###########################################################################

LinearSplice <- function(X, Y, ChangeTolerance) {
  
  YearsX <- index(X)
  YearsY <- index(Y)
  DataX <- coredata(X)
  DataY <- coredata(Y)
  LenX <- length(X)
  LenY <- length(Y)
  
  NEMStart <- YearsY[1]
  NEMSEnd <- max(YearsY)
  STEOEnd <- max(YearsX)
  STEOStart <- min(YearsX)
  NEMSDatStart <- DataY[1]
  
  SpYearStart_num <- STEOEnd-STEOStart+1
  STEODatEnd <- DataX[SpYearStart_num]
  
  STEOGap <- NEMSDatStart - STEODatEnd
  STEOGapSign <- STEOGap/abs(STEOGap)
  
  ## If the difference between the end of the X series and the beginning of the
  ##  Y series is within the change tolerance, we simply splice the two series.
  
  if (abs(STEOGap) <= ChangeTolerance * abs(STEODatEnd)) {
    
    SpliceYear1 <- NEMStart
    SplicedSeries <- ZooSplice(X, Y)
    
  } else  {
    
  ## If the difference between the end of the X series and the beginning
  ##  of the Y series is NOT within the change tolerance, we perform a
  ##  linear splicing or ramping of the two series.
  
  ##  We first create a linear series that meets the 
  ##   change tolerance limits.
  
  LineSeries <- rep(0, LenY)
  
  for (t in 1:LenY) {
    LineSeries[t] <- STEODatEnd +
      (STEODatEnd * ChangeTolerance * t * STEOGapSign)
  }  ## end for loop
  
  ##  We create a data frame to hold the linear and Y series.
  TimeMat <- cbind((1:LenY), YearsY, DataY, LineSeries)
  colnames(TimeMat) <- c('YearNum', 'YearsY', 'DataY', 'LineSeries')
  TimeData <- as.data.frame(TimeMat)
  
  ##  We compute the differences between the linear and Y series.
  TimeData$LineDiff <- abs(TimeData$DataY - TimeData$LineSeries)
  
  ##  We find the first year for which the linear series value is within the change
  ##   tolerance.  This is the splice year.
  AbsDiff <- abs(STEODatEnd) * ChangeTolerance
  WithinTol <- TimeData[which(TimeData$LineDiff < AbsDiff),]
  SpliceYear1 <- WithinTol[1,]$YearsY
  
  ##  We take the needed section of the linear series up to the splice year.
  LineSeries2 <- zoo(TimeData$LineSeries, TimeData$YearsY)
  LineSeries3 <- window(LineSeries2, start = NEMStart, end = SpliceYear1)
  
  ##  We truncate the Y series to remove the section up to the splice year.
  Y_trunc <- window(Y, start = SpliceYear1 + 1, end = NEMSEnd)
  
  ##  We splice the linear series to the truncated Y series.
  SplicedSeries0 <- ZooSplice(LineSeries3, Y_trunc)
  
  ##  We splice the ramped series to the X series.
  SplicedSeries <- ZooSplice(X, SplicedSeries0)
  
   } ## end else
  
  SeriesData <- list(SplicedSeries, SpliceYear1)
  
  return(SeriesData)
  
}  # end LinearSplice function code


###########################################################################
## We create a user-defined function for splicing and plotting zoo objects.
##  This option will be used when the STEO-to-NEMS gap is 
##  within the change tolerance limit.
###########################################################################

SimpleSplice <- function(X, Y, Z, SeriesName) {

SplicedSeries <- ZooSplice(X, Y)

## We create ts objects for plotting.
SplicedSeries.ts <- as.ts(SplicedSeries)
BenchSeries_STEO.ts <- as.ts(X)
BenchSeries_NEMS.ts <- as.ts(Y)
FullNEMSSeries.ts <- as.ts(Z)

## We plot the original and spliced series.
pdf(file=paste('STEOSimpleSplice', SeriesName, '.pdf', sep = ""))
ts.plot(SplicedSeries.ts, BenchSeries_STEO.ts, FullNEMSSeries.ts, main = paste("Simple Splice for STEO Benchmarking of ", SeriesName),
        sub = 'Simple Splice; Change Within Tolerance', xlab = "Year",
        gpars = list(col = c('red', 'blue', 'darkgreen'), lty = c(1, 2, 1)))
legend("bottomright", c("Spliced Series", "STEO Series", "NEMS Series"), lty = c(1, 21), col = c('red', 'blue', 'darkgreen'))

dev.off()

return(SplicedSeries)

} ## end SimpleSplice function code


###########################################################################
## We create a user-defined function to perform benchmarking Option 1.
###########################################################################

BenchOption_1 <- function(STEOSeries, ExtSTEO, NEMSSeries, ChangeTolerance)
  {

  ## For Option 1, we test to see which gap is smaller:  the gap between the STEO
  ##  and its extended trend (use Option 1A) or the gap between the STEO
  ##  and the NEMS (use Option1B). Options 1A and 1B are documented in the memo.

  if (TrendGap < NEMSGap) {
  ## linearly splice the STEO to its trend
  SpSeriesData <- LinearSplice(STEOSeries, ExtSTEO, ChangeTolerance)
  SpSeries1 <- SpSeriesData[[1]]
  SpliceYear1 <- SpSeriesData[[2]]
  Option1A <- TRUE
  
  ## linearly splice the extended STEO to the NEMS
  SpSeries_trunc <- window(SpSeries1, start=FirstSTEOYear, end=SpliceYear1)
  NEMS_trunc <- window(NEMSSeries, start=(SpliceYear1 + 1), end=LastNEMSYear)
  SpSeriesData2 <- LinearSplice(SpSeries_trunc, NEMS_trunc, ChangeTolerance)
  SpSeries <- SpSeriesData2[[1]]
  SpliceYear <- SpSeriesData2[[2]]
  
  BenchOutput <- list(SpSeries, SpliceYear, Option1A)
  return(BenchOutput)
  
  } ## end if
  
  else {
  
  ## linearly splice the STEO directly to the NEMS
  SpSeriesData <- LinearSplice(STEOSeries, NEMSSeries, ChangeTolerance)
  SpSeries2 <- SpSeriesData[[1]]
  SpliceYear <- SpSeriesData[[2]]
  Option1A <- FALSE
  
  BenchOutput <- list(SpSeries2, SpliceYear, Option1A)
  return(BenchOutput)

  } ## end else
 } ## end function BenchOption_1


###########################################################################
## We create a user-defined function to perform benchmarking Option 2.
###########################################################################

BenchOption_2 <- function(STEOSeries, ExtSTEO, NEMSSeries, ChangeTolerance)
{
  
## For option 2, we linearly splice the STEO to its trend series
##  and then adjust the NEMS to agree with the STEO trend.

SpSeriesData1 <- LinearSplice(STEOSeries, ExtSTEO, ChangeTolerance)
SpSeries1 <- SpSeriesData1[[1]]
SpliceYear1 <- SpSeriesData1[[2]]

## We truncate both series prior to splicing.
SpSeries2 <- window(SpSeries1, start = FirstSTEOYear, end = SpliceYear1)
NEMS_trunc2 <- window(NEMSSeries, start=(SpliceYear1), end=LastNEMSYear)
AdjFacNumer <- coredata(window(SpSeries1, start = SpliceYear1, end=SpliceYear1))
AdjFacDenom <- NEMS_trunc2[1]
AdjFac <- as.numeric(AdjFacNumer/AdjFacDenom)

NEMS_trunc3 <- window(NEMS_trunc2, start = (SpliceYear1 + 1), end = LastNEMSYear)
tempvec <- as.vector(coredata(NEMS_trunc3))
tempvec2 <- tempvec * AdjFac

NEMS_trunc4 <- zoo(tempvec2, index(NEMS_trunc3))

## We define the years in the NEMS series that will be adjusted.
NEMSInd <- (SpliceYear1 + 1):LastNEMSYear

## We create a zoo object to contain the NEMS series.
NEMS_trunc5 <- zoo(NEMS_trunc4, NEMSInd)

SplicedSeries_Op2 <- ZooSplice(SpSeries2, NEMS_trunc5)

Option2 <- TRUE

BenchOutput <- list(SplicedSeries_Op2, SpliceYear1, Option2)
return(BenchOutput)

}  ## end BenchOption_2 function code


######################################################
######################################################
##  THE MAIN BODY OF THE PROGRAM BEGINS HERE.
######################################################
######################################################

######################################################
#  We first find the change tolerance.
######################################################

## We compute the median percentage change for the STEO series.
BenchDiff <- diff(STEOSeries, lag = 1, differences = 1, arithmetic = TRUE)
BenchSeriesTrunc <- window(STEOSeries, start = (FirstSTEOYear + 1), end = LastSTEOYear)
BenchSeriesTruncData <- coredata(BenchSeriesTrunc)
BenchSeriesDiffData <- coredata(BenchDiff)

## To prevent division by 0, we remove any 0's from the truncated seres
BenchSeriesDiffData <- BenchSeriesDiffData[which(BenchSeriesTruncData != 0)]
BenchSeriesTruncData <- BenchSeriesTruncData[which(BenchSeriesTruncData != 0)]
BenchSeriesPropChange <- abs(BenchSeriesDiffData/BenchSeriesTruncData)

## We use the median percentage change to set the change tolerance.
ChangeTolerance1 <- median(BenchSeriesPropChange)
ChangeTolerance <- ChangeTolerance1 * ChangeToleranceFactor

#####################################################
## We find the STEO-to-NEMS change.
#####################################################

LastSTEO <- window(STEOSeries, start = LastSTEOYear, end = LastSTEOYear)
LastSTEO_data <- coredata(LastSTEO)
FirstNEMS_data <- coredata(NEMSSeries[1])
NEMSChange <- abs((LastSTEO_data - FirstNEMS_data)/LastSTEO_data)

## We compare the STEO-to-NEMS change to the change tolerance
##  and perform a simple splice if the change is within tolerance.

if (NEMSChange <= ChangeTolerance)
   {
    SplicedSeries <- SimpleSplice(STEOSeries, NEMSSeries, FullNEMSSeries, SeriesName)
    SimpleSpliceUsed <- TRUE
    }  else {

      SimpleSpiceUsed <- FALSE}

#############################################################
## We compute the smoothed STEO series using
##  Holt-Winters smoothing with the specified
##  parameters (no seasonal (gamma) parameter).
#############################################################

SmoothSTEO <- HoltWinters(STEOSeries, alpha = HW_alpha, beta = HW_beta, gamma = FALSE)

## We extract the rounded alpha and beta parameters.
##  These will appear in the subtitle of the diagnostic plot.
HW_alpha <- round(SmoothSTEO$alpha, digits = 4)
HW_beta <- round(SmoothSTEO$beta, digits = 4)

## We extend the STEO trend series out to the end of the NEMS series to ensure 
##  an adequate length series for possible splicing.
predSTEO <- predict(SmoothSTEO, STEOExtend)
SmoothSTEOseries <- SmoothSTEO$fitted
SmoothSTEO1 <- SmoothSTEOseries[1:(LastSTEOYear -FirstSTEOYear - 1), 1]
STEOIndex <- (FirstSTEOYear + 2):LastSTEOYear
STEOTrend1 <- zoo(SmoothSTEO1, STEOIndex)

## We convert the extended STEO trend to a zoo object.
ExtSTEO <- as.zoo(predSTEO)

## We ensure that the extended STEO stays within the series extrema.
MinVector <- rep(SeriesMin, length(ExtSTEO))
MaxVector <- rep(SeriesMax, length(ExtSTEO))

ExtSTEOind <- index(ExtSTEO)
ExtSTEOdata <- coredata(ExtSTEO)

MyIndMin <- which(MinVector > ExtSTEOdata)
MyIndMax <- which(MaxVector < ExtSTEOdata)

ExtSTEOdata[MyIndMin] <- SeriesMin
ExtSTEOdata[MyIndMax] <- SeriesMax

ExtSTEO <- zoo(ExtSTEOdata, ExtSTEOind)

ExtSTEO_full <- ZooSplice(STEOTrend1, ExtSTEO)

##  We convert the extended STEO trend to a ts object for plotting
STEOTrend.ts <- as.ts(ExtSTEO)

## We compute the gap between the STEO series and the extended STEO series.
TrendGap <- abs(coredata(STEOSeries[max(index(STEOSeries))-FirstSTEOYear +1]) - coredata(ExtSTEO[1]))

## We compute the gap between the STEO series and the NEMS series.
NEMSGap <- abs(coredata(STEOSeries[max(index(STEOSeries))-FirstSTEOYear +1]) - coredata(NEMSSeries[1]))

###############################################################
## This code runs options 1 and 2, extracts the results, plots
##  them in a pdf, and outputs them to comma-delimited files.
###############################################################

Option1Data <- BenchOption_1(STEOSeries, ExtSTEO, NEMSSeries, ChangeTolerance)
SpSeries_Op1 <- Option1Data[[1]]
SpliceYear_Op1 <- Option1Data[[2]]
Option1A <- Option1Data[[3]]

Option2Data <- BenchOption_2(STEOSeries, ExtSTEO, NEMSSeries, ChangeTolerance)
SpSeries_Op2 <- Option2Data[[1]]
SpliceYear_Op2 <- Option2Data[[2]]
Option_2 <- Option1Data[[3]]

## We convert the series to ts objects for plotting.
STEOSeries.ts <- as.ts(STEOSeries)
FullNEMSSeries.ts <- as.ts(FullNEMSSeries)
SpSeries_Op1.ts <- as.ts(SpSeries_Op1)
SpSeries_Op2.ts <- as.ts(SpSeries_Op2)

## We plot the series in a pdf file.
pdf(file=paste('STEOBench', SeriesName, '.pdf', sep = ""))
ts.plot(SpSeries_Op1.ts, SpSeries_Op2.ts, FullNEMSSeries.ts, STEOTrend.ts, STEOSeries.ts,
        main = paste("STEO Benchmarking of ", SeriesName, sep = ""),
        sub = paste('alpha = ', HW_alpha, ',  beta = ', HW_beta, sep=''), xlab = "Year",
        gpars = list(col = c('red', 'blue', 'darkgreen', 'orange', 'purple'), lty = c(1, 2, 2, 1, 2)))
legend("bottomright", c("Benchmarked Series, Option1", "Benchmarked Series, Option2", "NEMS Series", "STEO Trend Series", "STEO Series"), lty = c(1, 2, 2, 1, 2), col = c('red', 'blue', 'darkgreen', 'orange', 'purple'))

dev.off()

## We output the benchmarked series to comma-delimited files.

## We output the Option 1 series.
Option1Series <- coredata(SpSeries_Op1)
Year <- index(SpSeries_Op1)
Option1Data <- as.data.frame(cbind(Year, Option1Series))
write.csv(Option1Data, file = 'R_TO_NEMS1.csv', row.names = FALSE)

## We output the Option 2 series.
Option2Series <- coredata(SpSeries_Op2)
Year <- index(SpSeries_Op2)
Option2Data <- as.data.frame(cbind(Year, Option2Series))
write.csv(Option2Data, file = 'R_TO_NEMS2.csv', row.names = FALSE)
