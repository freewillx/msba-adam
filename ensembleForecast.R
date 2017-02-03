library(stringr)
library(zoo)
library(dplyr)
###### Load Data ######
#' Load raw data
#' @return time series data: "date" "us.price" "us.rig" "us.prod" "tool.a" "tool.b" "tool.c"
LoadMultivariateTimeSeries <- function(fileName){
  
  tool.demand.data <- read.csv(fileName)
  
  if("Day" %in% colnames(tool.demand.data)){
    tool.demand.data$date <- as.Date(str_replace_all(paste(tool.demand.data$Year, "-", tool.demand.data$Month, "-", tool.demand.data$Day), " ", ""))
    tool.demand.data <- na.omit(tool.demand.data[,c(ncol(tool.demand.data),4:(ncol(tool.demand.data)-1))])
  } else {
    tool.demand.data$date <- as.Date(str_replace_all(paste(tool.demand.data$Year, "-", tool.demand.data$Month, "-1"), " ", ""))
    tool.demand.data <- na.omit(tool.demand.data[,c(ncol(tool.demand.data),3:(ncol(tool.demand.data)-1))])
  }
  
  
  return(tool.demand.data)
}

monthlyFeaturesTbl <- tbl_df(LoadMultivariateTimeSeries("./Data/predictors.csv"))
monthlyDataA = monthlyFeaturesTbl %>% select(-starts_with("tool.b")) %>% select(-starts_with("tool.c")) %>% rename(demand = tool.a)
monthlyDataB = monthlyFeaturesTbl %>% select(-starts_with("tool.a")) %>% select(-starts_with("tool.c")) %>% rename(demand = tool.b)
monthlyDataC = monthlyFeaturesTbl %>% select(-starts_with("tool.a")) %>% select(-starts_with("tool.b")) %>% rename(demand = tool.c)

write.csv(monthlyDataA, "./Results/Forecast/A-Monthly-Raw.csv")
write.csv(monthlyDataB, "./Results/Forecast/B-Monthly-Raw.csv")
write.csv(monthlyDataC, "./Results/Forecast/C-Monthly-Raw.csv")

###### Feature Engineering ######

#' A Function to create additional data features to express time series quarterly changes 
#' @param monthlyData: date, us.price, us.rig, us.prod, demand
#' @return time series data: date, us.price, us.rig, us.prod, us.price.ma, us.rig.ma, us.prod.ma, demand.ma, us.price.sd, us.rig.sd, us.prod.sd, demand.sd, demand, demand.actual
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


###### MARS Prediction with sliding window - default 24 months ######
library(earth)

#' A prediction function using MARS applied to each row of a time series dataframe to calculate forecast 
#' @param rowEntry: an entry of data with multiple features, should be a subset of fullDataSet, a minimum of trainWindow data points should be preserved and excluded from the rowEntries
#' @param fullDataSet: dataframe contains all the time series features with date and the dependent variable
#' @param trainWindow: the length of sliding time window for trainning to calculate the prediction
#' @param f: the formula for prediction
#' @return the predicted value
MarsPredictRowSlidingWindow <- function(rowEntry, fullDataSet, trainWindow=24, f){
  tool.train <- tail(fullDataSet %>% filter(date<rowEntry$date), trainWindow)
  tool.train <- na.omit(tool.train)
  mars.model <- earth(f, data = tool.train)

  mars.pred <- predict(mars.model, newdata = rowEntry)
  mars.pred <- ifelse(mars.pred >= 0, mars.pred, 0)
  return(mars.pred)
}

###### SVM Prediction with sliding window - default 24 months ######
library(e1071)

#' A prediction function using SVM applied to each row of a time series dataframe to calculate forecast 
#' @param rowEntry: an entry of data with multiple features, should be a subset of fullDataSet, a minimum of trainWindow data points should be preserved and excluded from the rowEntries
#' @param fullDataSet: dataframe contains all the time series features with date and the dependent variable
#' @param trainWindow: the length of sliding time window for trainning to calculate the prediction
#' @param f: the formula for prediction
#' @return the predicted value
SvmPredictRowSlidingWindow <- function(rowEntry, fullDataSet, trainWindow=24, f, svmParam = NULL){
  tool.train <- tail(fullDataSet %>% filter(date<rowEntry$date), trainWindow)
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
  svm.pred <- ifelse(svm.pred >= 0, svm.pred, 0)
  return(svm.pred)
}

###### Ensemble Modeling ######
library(lubridate)

#' The bagging ensemble prediction function using monthly time series data to forcast demand
#' The ensemble function will first generate data features for different base models with monthly data summerized at different length of historical time window:
#' - monthly/quarterly/half annual/annual
#' With the generated data features, we train 4 models to produce 4 forcast values, for example:
#' - use last month summerized data to forecast next month
#' - use last 3 months summerized data to forecast next month
#' - use last 6 months summerized data to forecast next month
#' - use last 12 month summerized data to forecast next month
#' The average of the 4 forecast values is the ensemble forecast
#' @param monthlyData: the input monthly data
#' @param forecastTimeRange: the unit of time to forecast in the future, by default forecast next month
#' @param modelMethod: the modeling algorithm to be used
#' @param trainSize: the number of datapoint of the sliding window. i.e. the length of holdout data for training
#' @return a dataframe with actual demand of the month, actual future demand, the forecasted demand from all base models and ensemble model:
#' date, demand, demand.lead, monthly.pred, quarterly.pred, half.annual.pred, annual.pred, ensemble
EnsembleDemandForecast <- function(monthlyData, forecastTimeRange = 1, modelMethod = "MARS", trainSize = 24){
  
  # Create base model data features
  monthlyDataFeatures = CreateTimeDataFeatures(monthlyData, histTimeWindow = 2, forecastTimeRange, std = FALSE)
  quarterlyDataFeatures = CreateTimeDataFeatures(monthlyData, histTimeWindow = 3, forecastTimeRange)
  halfAnnualDataFeatures = CreateTimeDataFeatures(monthlyData, histTimeWindow = 6, forecastTimeRange)
  annualDataFeatures = CreateTimeDataFeatures(monthlyData, histTimeWindow = 12, forecastTimeRange)
  
  # Store predictions of each base model as input data for Ensemble - preserve first 24 months for MARS trainning
  ensembleData = select(monthlyDataFeatures[(trainSize+1):nrow(monthlyDataFeatures),], date, demand.lead) %>%
    rename(demand.actual = demand.lead) %>%
    mutate(date = lead(date, n = forecastTimeRange))
  
  # Add n more rows to store predictions, n=forecastTimeRange
  lastDateWithData = ensembleData$date[nrow(ensembleData)-forecastTimeRange]
  for (i in 1:forecastTimeRange) {
    month(lastDateWithData) <- month(lastDateWithData) + 1
    ensembleData$date[nrow(ensembleData)-forecastTimeRange+i] <- lastDateWithData
  }
  
  
  # Monthly Base Model 
  monthly.formula <- as.formula(paste("demand.lead", "~", 
                                         paste(names(monthlyDataFeatures)[!names(monthlyDataFeatures) %in% c("date", "demand.lead")], collapse = "+")))
  monthly.mars.pred <- monthlyDataFeatures[(trainSize+1):nrow(monthlyDataFeatures),]
  
  # Quarterly Base Model 
  quarterly.formula <- as.formula(paste("demand.lead", "~", 
                                        paste(names(quarterlyDataFeatures)[!names(quarterlyDataFeatures) %in% c("date", "demand.lead")], collapse = "+")))
  quarterly.mars.pred <- quarterlyDataFeatures[(trainSize+1):nrow(quarterlyDataFeatures),]
  
  # Half Annual Base Model 
  halfAnnual.formula <- as.formula(paste("demand.lead", "~", 
                                         paste(names(halfAnnualDataFeatures)[!names(halfAnnualDataFeatures) %in% c("date", "demand.lead")], collapse = "+")))
  halfAnnual.mars.pred <- halfAnnualDataFeatures[(trainSize+1):nrow(halfAnnualDataFeatures),]
  
  # Annual Base Model 
  annual.formula <- as.formula(paste("demand.lead", "~", 
                                     paste(names(annualDataFeatures)[!names(annualDataFeatures) %in% c("date", "demand.lead")], collapse = "+")))
  annual.mars.pred <- annualDataFeatures[(trainSize+1):nrow(annualDataFeatures),]
  
  if (modelMethod == "MARS"){
    ensembleData$monthly.pred = as.numeric(
      by(monthly.mars.pred, 1:nrow(monthly.mars.pred), 
         FUN = MarsPredictRowSlidingWindow, 
         fullDataSet=monthlyDataFeatures, 
         trainWindow=trainSize,
         f = monthly.formula))
    
    ensembleData$quarterly.pred = as.numeric(
      by(quarterly.mars.pred, 1:nrow(quarterly.mars.pred), 
         FUN = MarsPredictRowSlidingWindow, 
         fullDataSet=quarterlyDataFeatures, 
         trainWindow=trainSize,
         f = quarterly.formula))
    
    ensembleData$half.annual.pred = as.numeric(
      by(halfAnnual.mars.pred, 1:nrow(halfAnnual.mars.pred), 
         FUN = MarsPredictRowSlidingWindow, 
         fullDataSet=halfAnnualDataFeatures, 
         trainWindow=trainSize,
         f = halfAnnual.formula))
    
    ensembleData$annual.pred = as.numeric(
      by(annual.mars.pred, 1:nrow(annual.mars.pred), 
         FUN = MarsPredictRowSlidingWindow, 
         fullDataSet=annualDataFeatures, 
         trainWindow=trainSize,
         f = annual.formula))
    
    ensembleData$ensemble.pred = rowMeans(ensembleData[3:ncol(ensembleData)])
    
  } else if (modelMethod == "SVM"){
    
    best.svm.param.month <- tune.svm(monthly.formula, data = na.omit(monthlyDataFeatures), gamma = seq(0.5,3.0, by=0.1), cost = seq(100,1000, by = 100))
    ensembleData$monthly.pred = as.numeric(
      by(monthly.mars.pred, 1:nrow(monthly.mars.pred), 
         FUN = SvmPredictRowSlidingWindow, 
         fullDataSet=monthlyDataFeatures, 
         trainWindow=trainSize,
         f = monthly.formula,
         svmParam = best.svm.param.month
         ))
    
    best.svm.param.quarter <- tune.svm(quarterly.formula, data = na.omit(quarterlyDataFeatures), gamma = seq(0.5,3.0, by=0.1), cost = seq(100,1000, by = 100))
    ensembleData$quarterly.pred = as.numeric(
      by(quarterly.mars.pred, 1:nrow(quarterly.mars.pred), 
         FUN = SvmPredictRowSlidingWindow, 
         fullDataSet=quarterlyDataFeatures, 
         trainWindow=trainSize,
         f = quarterly.formula,
         svmParam = best.svm.param.quarter))
    
    best.svm.param.halfannual <- tune.svm(halfAnnual.formula, data = na.omit(halfAnnualDataFeatures), gamma = seq(0.5,3.0, by=0.1), cost = seq(100,1000, by = 100))
    ensembleData$half.annual.pred = as.numeric(
      by(halfAnnual.mars.pred, 1:nrow(halfAnnual.mars.pred), 
         FUN = SvmPredictRowSlidingWindow, 
         fullDataSet=halfAnnualDataFeatures, 
         trainWindow=trainSize,
         f = halfAnnual.formula,
         svmParam = best.svm.param.halfannual))
    
    best.svm.param.annual <- tune.svm(annual.formula, data = na.omit(annualDataFeatures), gamma = seq(0.5,3.0, by=0.1), cost = seq(100,1000, by = 100))
    ensembleData$annual.pred = as.numeric(
      by(annual.mars.pred, 1:nrow(annual.mars.pred), 
         FUN = SvmPredictRowSlidingWindow, 
         fullDataSet=annualDataFeatures, 
         trainWindow=trainSize,
         f = annual.formula,
         svmParam = best.svm.param.annual))
    
    ensembleData$ensemble.pred = rowMeans(ensembleData[3:ncol(ensembleData)])
    
  }
  
  return(ensembleData)
}


###### Forecast Demand ######
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
# aN2EnsembleData = EnsembleDemandForecast(monthlyDataA, forecastTimeRange = 2, modelMethod = "SVM")
# aN3EnsembleData = EnsembleDemandForecast(monthlyDataA, forecastTimeRange = 3, modelMethod = "SVM")
# 
# bN1EnsembleData = EnsembleDemandForecast(monthlyDataB, forecastTimeRange = 1, modelMethod = "SVM")
# bN2EnsembleData = EnsembleDemandForecast(monthlyDataB, forecastTimeRange = 2, modelMethod = "SVM")
# bN3EnsembleData = EnsembleDemandForecast(monthlyDataB, forecastTimeRange = 3, modelMethod = "SVM")
# 
# cN1EnsembleData = EnsembleDemandForecast(monthlyDataC, forecastTimeRange = 1, modelMethod = "SVM")
# cN2EnsembleData = EnsembleDemandForecast(monthlyDataC, forecastTimeRange = 2, modelMethod = "SVM")
# cN3EnsembleData = EnsembleDemandForecast(monthlyDataC, forecastTimeRange = 3, modelMethod = "SVM")


write.csv(aN1EnsembleData, file = "./Results/Forecast/A-1M-Forecast.csv")
write.csv(aN2EnsembleData, file = "./Results/Forecast/A-2M-Forecast.csv")
write.csv(aN3EnsembleData, file = "./Results/Forecast/A-3M-Forecast.csv")


write.csv(bN1EnsembleData, file = "./Results/Forecast/B-1M-Forecast.csv")
write.csv(bN2EnsembleData, file = "./Results/Forecast/B-2M-Forecast.csv")
write.csv(bN3EnsembleData, file = "./Results/Forecast/B-3M-Forecast.csv")


write.csv(cN1EnsembleData, file = "./Results/Forecast/C-1M-Forecast.csv")
write.csv(cN2EnsembleData, file = "./Results/Forecast/C-2M-Forecast.csv")
write.csv(cN3EnsembleData, file = "./Results/Forecast/C-3M-Forecast.csv")


# Calculate Difference
aN1EnsembleData$monthly.diff = aN1EnsembleData$ensemble.pred - aN1EnsembleData$demand.actual
aN2EnsembleData$monthly.diff = aN2EnsembleData$ensemble.pred - aN2EnsembleData$demand.actual
aN3EnsembleData$monthly.diff = aN3EnsembleData$ensemble.pred - aN3EnsembleData$demand.actual

bN1EnsembleData$monthly.diff = bN1EnsembleData$ensemble.pred - bN1EnsembleData$demand.actual
bN2EnsembleData$monthly.diff = bN2EnsembleData$ensemble.pred - bN2EnsembleData$demand.actual
bN3EnsembleData$monthly.diff = bN3EnsembleData$ensemble.pred - bN3EnsembleData$demand.actual

cN1EnsembleData$monthly.diff = cN1EnsembleData$ensemble.pred - cN1EnsembleData$demand.actual
cN2EnsembleData$monthly.diff = cN2EnsembleData$ensemble.pred - cN2EnsembleData$demand.actual
cN3EnsembleData$monthly.diff = cN3EnsembleData$ensemble.pred - cN3EnsembleData$demand.actual


###### Calculate RMSE ######
CalculateRMSE <- function(predictedData){
  predictedData=na.omit(predictedData)
  rmseCompare <- data.frame(
    monthlyModel = sqrt( sum( (predictedData$demand.actual - predictedData$monthly.pred)^2 , na.rm = TRUE ) / nrow(predictedData)),
    quarterlyModel = sqrt( sum( (predictedData$demand.actual - predictedData$quarterly.pred)^2 , na.rm = TRUE ) / nrow(predictedData) ),
    halfAnnualModel = sqrt( sum( (predictedData$demand.actual - predictedData$half.annual.pred)^2 , na.rm = TRUE ) / nrow(predictedData) ),
    annualModel = sqrt( sum( (predictedData$demand.actual - predictedData$annual.pred)^2 , na.rm = TRUE ) / nrow(predictedData) ),
    ensembleModel = sqrt( sum( (predictedData$demand.actual - predictedData$ensemble.pred)^2 , na.rm = TRUE ) / nrow(predictedData) )
  )
  return(rmseCompare)
}

rmseResults <- cbind(data.frame(tool="A", forecastMonth=1), CalculateRMSE(aN1EnsembleData))
rmseResults <- rbind(rmseResults, cbind(data.frame(tool="A", forecastMonth=2), CalculateRMSE(aN2EnsembleData)))
rmseResults <- rbind(rmseResults, cbind(data.frame(tool="A", forecastMonth=3), CalculateRMSE(aN3EnsembleData)))

rmseResults <- rbind(rmseResults, cbind(data.frame(tool="B", forecastMonth=1), CalculateRMSE(bN1EnsembleData)))
rmseResults <- rbind(rmseResults, cbind(data.frame(tool="B", forecastMonth=2), CalculateRMSE(bN2EnsembleData)))
rmseResults <- rbind(rmseResults, cbind(data.frame(tool="B", forecastMonth=3), CalculateRMSE(bN3EnsembleData)))

rmseResults <- rbind(rmseResults, cbind(data.frame(tool="C", forecastMonth=1), CalculateRMSE(cN1EnsembleData)))
rmseResults <- rbind(rmseResults, cbind(data.frame(tool="C", forecastMonth=2), CalculateRMSE(cN2EnsembleData)))
rmseResults <- rbind(rmseResults, cbind(data.frame(tool="C", forecastMonth=3), CalculateRMSE(cN3EnsembleData)))

write.csv(rmseResults, file = "./Results/RMSE.csv")
