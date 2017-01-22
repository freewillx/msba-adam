library(stringr)
library(zoo)

####### Load Data #######
#' Load raw data
#' @return time series data: "date" "us.price" "us.rig" "us.prod" "tool.a" "tool.b" "tool.c"
LoadMultivariateTimeSeries <- function(fileName){
  
  tool.demand.data <- read.csv(fileName)
  tool.demand.data$date <- as.Date(str_replace_all(paste(tool.demand.data$Year, "-", tool.demand.data$Month, "-1"), " ", ""))
  
  tool.demand.data <- na.omit(tool.demand.data[,c(9,3:8)])
  
  return(tool.demand.data)
}

monthlyFeaturesTbl <- tbl_df(LoadMultivariateTimeSeries("./Data/predictors.csv"))
monthlyDataA = monthlyFeaturesTbl %>% select(-starts_with("tool.b")) %>% select(-starts_with("tool.c")) %>% rename(demand = tool.a)
monthlyDataB = monthlyFeaturesTbl %>% select(-starts_with("tool.a")) %>% select(-starts_with("tool.c")) %>% rename(demand = tool.b)
monthlyDataC = monthlyFeaturesTbl %>% select(-starts_with("tool.a")) %>% select(-starts_with("tool.b")) %>% rename(demand = tool.c)

#######  Feature Engineering ####### 
library(dplyr)

#' A Function to create additional data features to express time series quarterly changes 
#' @param time series data: date, us.price, us.rig, us.prod, demand
#' @return time series data: date, us.price, us.rig, us.prod, us.price.ma, us.rig.ma, us.prod.ma, demand.ma, us.price.sd, us.rig.sd, us.prod.sd, demand.sd, demand, demand.lead
CreateTimeDataFeatures <- function(monthlyData, histTimeWindow = 2, forecastTimeRange = 1, std = TRUE){
  if (histTimeWindow > 1){
    monthly.data.features <- monthlyData %>% 
      mutate(us.price.ma = rollmean(x = us.price, histTimeWindow, align = "right", fill = NA)) %>% 
      mutate(us.rig.ma = rollmean(x = us.rig, histTimeWindow, align = "right", fill = NA)) %>% 
      mutate(us.prod.ma = rollmean(x = us.prod, histTimeWindow, align = "right", fill = NA)) %>% 
      mutate(demand.ma = rollmean(x = demand, histTimeWindow, align = "right", fill = NA))
    if (std == TRUE){
      monthly.data.features <- monthly.data.features %>% 
        mutate(us.price.sd = rollapply(data = us.price, width = histTimeWindow, FUN = sd, align = "right", fill = NA)) %>%
        mutate(us.rig.sd = rollapply(data = us.rig, width = histTimeWindow, FUN = sd, align = "right", fill = NA)) %>%
        mutate(us.prod.sd = rollapply(data = us.prod, width = histTimeWindow, FUN = sd, align = "right", fill = NA)) %>%
        mutate(demand.sd = rollapply(data = demand, width = histTimeWindow, FUN = sd, align = "right", fill = NA))
    }
    
    monthly.data.features <- monthly.data.features[, c(1:4, 6:ncol(monthly.data.features), 5)]
    monthly.data.features <- monthly.data.features %>% mutate(demand.lead = lead(demand, n = forecastTimeRange))
  }
  
  return(monthly.data.features)
}


####### MARS Prediction with sliding window - default 24 months #######
library(earth)
MarsPredictRowSlidingWindow <- function(rowEntry, fullDataSet, timeWindow=24, f){
  tool.train <- tail(fullDataSet %>% filter(date<rowEntry$date), timeWindow)
  tool.train <- na.omit(tool.train)
  mars.model <- earth(f, data = tool.train)
  mars.pred <- predict(mars.model, newdata = rowEntry)
  return(mars.pred)
}

####### SVM Prediction with sliding window - default 24 months #######
library(e1071)
SvmPredictRowSlidingWindow <- function(rowEntry, fullDataSet, timeWindow=24, f, svmParam = NULL){
  tool.train <- tail(fullDataSet %>% filter(date<rowEntry$date), timeWindow)
  tool.train <- na.omit(tool.train)
  
  # Find the best paramters for SVM model using all data
  if (!is.null(svmParam)){
    c <- svmParam$best.model$cost
    g <- svmParam$best.model$gamma
    svm.model <- svm(f, data = tool.train, cost = c, gamma = g)
  } else {
    svm.model <- svm(f, data = tool.train)
  }
  
  svm.pred <- predict(svm.model, newdata = rowEntry)
  return(svm.pred)
}

####### Ensemble Modeling #######
EnsembleDemandForecast <- function(monthlyData, forecastTimeRange = 1, modelMethod = "MARS"){
  
  # Create ensemble model data features
  monthlyDataFeatures = CreateTimeDataFeatures(monthlyData, histTimeWindow = 2, forecastTimeRange, std = FALSE)
  quarterlyDataFeatures = CreateTimeDataFeatures(monthlyData, histTimeWindow = 3, forecastTimeRange)
  halfAnnualDataFeatures = CreateTimeDataFeatures(monthlyData, histTimeWindow = 6, forecastTimeRange)
  annualDataFeatures = CreateTimeDataFeatures(monthlyData, histTimeWindow = 12, forecastTimeRange)
  
  # Prediction Data for Ensemble - preserve first 24 months for MARS trainning
  ensembleData = select(monthlyDataFeatures[25:nrow(monthlyDataFeatures),], date, demand, demand.lead)
  
  
  # Monthly Model 
  monthly.formula <- as.formula(paste("demand.lead", "~", 
                                         paste(names(monthlyDataFeatures)[!names(monthlyDataFeatures) %in% c("date", "demand.lead")], collapse = "+")))
  monthly.mars.pred <- monthlyDataFeatures[25:nrow(monthlyDataFeatures),]
  
  # Quarterly Model 
  quarterly.formula <- as.formula(paste("demand.lead", "~", 
                                        paste(names(quarterlyDataFeatures)[!names(quarterlyDataFeatures) %in% c("date", "demand.lead")], collapse = "+")))
  quarterly.mars.pred <- quarterlyDataFeatures[25:nrow(quarterlyDataFeatures),]
  
  # Half Annual Model 
  halfAnnual.formula <- as.formula(paste("demand.lead", "~", 
                                         paste(names(halfAnnualDataFeatures)[!names(halfAnnualDataFeatures) %in% c("date", "demand.lead")], collapse = "+")))
  halfAnnual.mars.pred <- halfAnnualDataFeatures[25:nrow(halfAnnualDataFeatures),]
  
  # Annual Model 
  annual.formula <- as.formula(paste("demand.lead", "~", 
                                     paste(names(annualDataFeatures)[!names(annualDataFeatures) %in% c("date", "demand.lead")], collapse = "+")))
  annual.mars.pred <- annualDataFeatures[25:nrow(annualDataFeatures),]
  
  if (modelMethod == "MARS"){
    ensembleData$monthly.pred = as.numeric(
      by(monthly.mars.pred, 1:nrow(monthly.mars.pred), 
         FUN = MarsPredictRowSlidingWindow, 
         fullDataSet=monthlyDataFeatures, 
         f = monthly.formula))
    
    ensembleData$quarterly.pred = as.numeric(
      by(quarterly.mars.pred, 1:nrow(quarterly.mars.pred), 
         FUN = MarsPredictRowSlidingWindow, 
         fullDataSet=quarterlyDataFeatures, 
         f = quarterly.formula))
    
    ensembleData$half.annual.pred = as.numeric(
      by(halfAnnual.mars.pred, 1:nrow(halfAnnual.mars.pred), 
         FUN = MarsPredictRowSlidingWindow, 
         fullDataSet=halfAnnualDataFeatures, 
         f = halfAnnual.formula))
    
    ensembleData$annual.pred = as.numeric(
      by(annual.mars.pred, 1:nrow(annual.mars.pred), 
         FUN = MarsPredictRowSlidingWindow, 
         fullDataSet=annualDataFeatures, 
         f = annual.formula))
    
    ensembleData$ensemble = rowMeans(ensembleData[3:ncol(ensembleData)])
    
  } else if (modelMethod == "SVM"){
    
    best.svm.param.month <- tune.svm(monthly.formula, data = na.omit(monthlyDataFeatures), gamma = seq(0.5,3.0, by=0.1), cost = seq(100,1000, by = 100))
    ensembleData$monthly.pred = as.numeric(
      by(monthly.mars.pred, 1:nrow(monthly.mars.pred), 
         FUN = SvmPredictRowSlidingWindow, 
         fullDataSet=monthlyDataFeatures, 
         f = monthly.formula,
         svmParam = best.svm.param.month
         ))
    
    best.svm.param.quarter <- tune.svm(quarterly.formula, data = na.omit(quarterlyDataFeatures), gamma = seq(0.5,3.0, by=0.1), cost = seq(100,1000, by = 100))
    ensembleData$quarterly.pred = as.numeric(
      by(quarterly.mars.pred, 1:nrow(quarterly.mars.pred), 
         FUN = SvmPredictRowSlidingWindow, 
         fullDataSet=quarterlyDataFeatures, 
         f = quarterly.formula,
         svmParam = best.svm.param.quarter))
    
    best.svm.param.halfannual <- tune.svm(halfAnnual.formula, data = na.omit(halfAnnualDataFeatures), gamma = seq(0.5,3.0, by=0.1), cost = seq(100,1000, by = 100))
    ensembleData$half.annual.pred = as.numeric(
      by(halfAnnual.mars.pred, 1:nrow(halfAnnual.mars.pred), 
         FUN = SvmPredictRowSlidingWindow, 
         fullDataSet=halfAnnualDataFeatures, 
         f = halfAnnual.formula,
         svmParam = best.svm.param.halfannual))
    
    best.svm.param.annual <- tune.svm(annual.formula, data = na.omit(annualDataFeatures), gamma = seq(0.5,3.0, by=0.1), cost = seq(100,1000, by = 100))
    ensembleData$annual.pred = as.numeric(
      by(annual.mars.pred, 1:nrow(annual.mars.pred), 
         FUN = SvmPredictRowSlidingWindow, 
         fullDataSet=annualDataFeatures, 
         f = annual.formula,
         svmParam = best.svm.param.annual))
    
    ensembleData$ensemble = rowMeans(ensembleData[3:ncol(ensembleData)])
    
  }
  
  return(ensembleData)
}


###### Calculate RMSE #####
CalculateRMSE <- function(predictedData){
  predictedData=na.omit(predictedData)
  sqrt( sum( (predictedData$demand.lead - predictedData$monthly.pred)^2 , na.rm = TRUE ) / nrow(predictedData) )
  sqrt( sum( (predictedData$demand.lead - predictedData$quarterly.pred)^2 , na.rm = TRUE ) / nrow(predictedData) )
  sqrt( sum( (predictedData$demand.lead - predictedData$half.annual.pred)^2 , na.rm = TRUE ) / nrow(predictedData) )
  sqrt( sum( (predictedData$demand.lead - predictedData$annual.pred)^2 , na.rm = TRUE ) / nrow(predictedData) )
  sqrt( sum( (predictedData$demand.lead - predictedData$ensemble)^2 , na.rm = TRUE ) / nrow(predictedData) )
}


###### Forecast Demand #####


aN1EnsembleData = EnsembleDemandForecast(monthlyDataA, forecastTimeRange = 1)
aN2EnsembleData = EnsembleDemandForecast(monthlyDataA, forecastTimeRange = 2)
aN3EnsembleData = EnsembleDemandForecast(monthlyDataA, forecastTimeRange = 3)

bN1EnsembleData = EnsembleDemandForecast(monthlyDataB, forecastTimeRange = 1)
bN2EnsembleData = EnsembleDemandForecast(monthlyDataB, forecastTimeRange = 2)
bN3EnsembleData = EnsembleDemandForecast(monthlyDataB, forecastTimeRange = 3)

cN1EnsembleData = EnsembleDemandForecast(monthlyDataC, forecastTimeRange = 1)
cN2EnsembleData = EnsembleDemandForecast(monthlyDataC, forecastTimeRange = 2)
cN3EnsembleData = EnsembleDemandForecast(monthlyDataC, forecastTimeRange = 3)

# aN1EnsembleData = EnsembleDemandForecast(monthlyDataA, forecastTimeRange = 1, modelMethod = "SVM")
# bN1EnsembleData = EnsembleDemandForecast(monthlyDataB, forecastTimeRange = 1, modelMethod = "SVM")
# cN1EnsembleData = EnsembleDemandForecast(monthlyDataC, forecastTimeRange = 1, modelMethod = "SVM")

CalculateRMSE(aN1EnsembleData)
CalculateRMSE(aN2EnsembleData)
CalculateRMSE(aN3EnsembleData)
CalculateRMSE(bN1EnsembleData)
CalculateRMSE(bN2EnsembleData)
CalculateRMSE(bN3EnsembleData)
CalculateRMSE(cN1EnsembleData)
CalculateRMSE(cN2EnsembleData)
CalculateRMSE(cN3EnsembleData)



###### Generate Data For Phase 3 Simulations #####
aN1Sim = aN1EnsembleData[,1]
aN1Sim$diff = aN1EnsembleData$demand.lead - aN1EnsembleData$ensemble
mean(aN1Sim$diff, na.rm = TRUE)
sd(aN1Sim$diff, na.rm = TRUE)

aN2Sim = aN2EnsembleData[,1]
aN2Sim$diff = aN2EnsembleData$demand.lead - aN2EnsembleData$ensemble
mean(aN2Sim$diff, na.rm = TRUE)
sd(aN2Sim$diff, na.rm = TRUE)

aN3Sim = aN3EnsembleData[,1]
aN3Sim$diff = aN3EnsembleData$demand.lead - aN3EnsembleData$ensemble
mean(aN3Sim$diff, na.rm = TRUE)
sd(aN3Sim$diff, na.rm = TRUE)

bN1Sim = bN1EnsembleData[,1]
bN1Sim$diff = bN1EnsembleData$demand.lead - bN1EnsembleData$ensemble
mean(bN1Sim$diff, na.rm = TRUE)
sd(bN1Sim$diff, na.rm = TRUE)

bN2Sim = bN2EnsembleData[,1]
bN2Sim$diff = bN2EnsembleData$demand.lead - bN2EnsembleData$ensemble
mean(bN2Sim$diff, na.rm = TRUE)
sd(bN2Sim$diff, na.rm = TRUE)

bN3Sim = bN3EnsembleData[,1]
bN3Sim$diff = bN3EnsembleData$demand.lead - bN3EnsembleData$ensemble
mean(bN3Sim$diff, na.rm = TRUE)
sd(bN3Sim$diff, na.rm = TRUE)

cN1Sim = cN1EnsembleData[,1]
cN1Sim$diff = cN1EnsembleData$demand.lead - cN1EnsembleData$ensemble
mean(cN1Sim$diff, na.rm = TRUE)
sd(cN1Sim$diff, na.rm = TRUE)

cN2Sim = cN2EnsembleData[,1]
cN2Sim$diff = cN2EnsembleData$demand.lead - cN2EnsembleData$ensemble
mean(cN2Sim$diff, na.rm = TRUE)
sd(cN2Sim$diff, na.rm = TRUE)

cN3Sim = cN3EnsembleData[,1]
cN3Sim$diff = cN3EnsembleData$demand.lead - cN3EnsembleData$ensemble
mean(cN3Sim$diff, na.rm = TRUE)
sd(cN3Sim$diff, na.rm = TRUE)
















# Two possible ensemble approach - ensemble various models / ensemble various window length or grid search window length

# If daily data available, use monte-carlo sim to find time point to retrieve train/test data
## 1.Train ensemble models with various algorithms such as SVM / glm / MARS etc
## 2.For each ensemble models, Bagging ensemble and take average or Stacking ensemble base on window length on full train data set 
## 3.Apply performance estimation using monte-carlo sim to find time point to retrieve train/test data

# Once best model choosen, in production, train new ensemble model with sliding window data (step 1 and 2)

# Otherwise, stacking ensemble using old data set

# Need a slide to explain the concept - we ditch ARIMA / we could use ARIMA or human forecast as baseline
