library(stringr)
library(zoo)
library(dplyr)

############ Load Data ############
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


# Daily data clean up
library(lubridate)

dailyDataA <- tbl_df(LoadMultivariateTimeSeries("./Data/toola_dd.csv")) %>% rename(daily.demand = toola.dd)
dailyDataA$month=floor_date(dailyDataA$date, unit = "month")
# Exclude 2016-03 and other missing data
dailyDataA <- dailyDataA %>% filter(month != as.Date("2016-03-01")) %>% select(-month) %>% na.omit()

dailyDataB <- tbl_df(LoadMultivariateTimeSeries("./Data/toolb_dd.csv")) %>% rename(daily.demand = toolb.dd)
dailyDataB$month=floor_date(dailyDataB$date, unit = "month")
# Exclude 2016-03 and other missing data
dailyDataB <- dailyDataB %>% filter(month != as.Date("2016-03-01")) %>% select(-month) %>% na.omit()

dailyDataC <- tbl_df(LoadMultivariateTimeSeries("./Data/toolc_dd.csv")) %>% rename(daily.demand = toolc.dd)
dailyDataC$month=floor_date(dailyDataC$date, unit = "month")
# Exclude 2016-03 and other missing data
dailyDataC <- dailyDataC %>% filter(month != as.Date("2016-03-01")) %>% select(-month) %>% na.omit()

write.csv(dailyDataA, "./Results/Processed/A-Daily-Raw.csv", row.names=FALSE)
write.csv(dailyDataB, "./Results/Processed/B-Daily-Raw.csv", row.names=FALSE)
write.csv(dailyDataC, "./Results/Processed/C-Daily-Raw.csv", row.names=FALSE)


############ Model Evaluation Functions ############

RandomSampleRMSE <- function(forecastDf, sample.size = 300){

  # Random sample 300 days but holdoutlast 30 days from random sampling range
  samples = forecastDf[sample((nrow(forecastDf)-30), sample.size),]
  
  rmseResult = samples %>% select(date)
  rmseResult$rmse = NA
  
  for (i in 1:nrow(samples)){
    entry = samples[i,]
    # Actual 30 days demand starting from the sampled date
    compare = filter(forecastDf, date >= entry$date)[1:30,1:2]
    # Transpose the forecasted 30 days demand data in columns, starting from the sampled date
    compare$forecast = as.data.frame(t(entry[,3:ncol(entry)]))[,1]
    rmseResult$rmse[i] = sqrt( mean( (compare$daily.demand - compare$forecast)^2 , na.rm = TRUE ) )
  }
  
  return(rmseResult)
}


############ Baseline Model ############
# 90 days moving avearge as the forecast of next 30 days
SlbBaselineForecast <- function(dailyData){
  forecastDF <- dailyData %>% select(date, daily.demand) %>%
    mutate(n1.demand = rollmean(x = daily.demand, 90, align = "right", fill = NA)) %>%
    filter(date >= as.Date("2015-01-01"))
  
  # Set the same value for 30 days forecast
  for (i in 2:30){
    col.name = paste("n",i,".demand", sep="")
    forecastDF[[col.name]] <- forecastDF$n1.demand
  }
  
  return(forecastDF)
}

baselineForecastA <- SlbBaselineForecast(dailyDataA)
baselineForecastB <- SlbBaselineForecast(dailyDataB)
baselineForecastC <- SlbBaselineForecast(dailyDataC)

write.csv(baselineForecastA, "./Results/DailyForecast/baselineToolA.csv", row.names=FALSE)
write.csv(baselineForecastB, "./Results/DailyForecast/baselineToolB.csv", row.names=FALSE)
write.csv(baselineForecastC, "./Results/DailyForecast/baselineToolC.csv", row.names=FALSE)

# RMSE model performance
baselineRmseA <- RandomSampleRMSE(baselineForecastA)
baselineRmseB <- RandomSampleRMSE(baselineForecastB)
baselineRmseC <- RandomSampleRMSE(baselineForecastC)

write.csv(baselineRmseA, "./Results/Evaluation/baselineRmseA.csv", row.names=FALSE)
write.csv(baselineRmseB, "./Results/Evaluation/baselineRmseB.csv", row.names=FALSE)
write.csv(baselineRmseC, "./Results/Evaluation/baselineRmseC.csv", row.names=FALSE)


############ Best Possible Model - Future 30 days average ############
# Use 30 days future moving average as forecast as the best possible model, as if we can see the future for performance comparison
BestAvgForecast <- function(dailyData){
  forecastDF <- dailyData %>% select(date, daily.demand) %>%
    mutate(n1.demand = rollmean(x = daily.demand, 31, align = "left", fill = NA)) %>%
    filter(date >= as.Date("2015-01-01"))
  
  # Set the same value for 30 days forecast
  for (i in 2:30){
    col.name = paste("n",i,".demand", sep="")
    forecastDF[[col.name]] <- forecastDF$n1.demand
  }
  
  return(forecastDF)
}

bestAvgForecastA <- na.omit(BestAvgForecast(dailyDataA))
bestAvgForecastB <- na.omit(BestAvgForecast(dailyDataB))
bestAvgForecastC <- na.omit(BestAvgForecast(dailyDataC))

# RMSE model performance
bestAvgRmseA <- RandomSampleRMSE(bestAvgForecastA)
bestAvgRmseB <- RandomSampleRMSE(bestAvgForecastB)
bestAvgRmseC <- RandomSampleRMSE(bestAvgForecastC)

write.csv(bestAvgRmseA, "./Results/Evaluation/bestAvgRmseA.csv", row.names=FALSE)
write.csv(bestAvgRmseB, "./Results/Evaluation/bestAvgRmseB.csv", row.names=FALSE)
write.csv(bestAvgRmseC, "./Results/Evaluation/bestAvgRmseC.csv", row.names=FALSE)

############ ARIMA Model ############
# Forecast 30 days with Auto ARIMA
library(forecast)

ArimaForecast <- function(dailyData){
  
  forecastDF <- dailyData %>% select(date, daily.demand) %>%
    filter(date >= as.Date("2015-01-01"))
  
  # Add forecast value columns
  for (i in 1:30){
    col.name = paste("n",i,".demand", sep="")
    forecastDF[[col.name]] <- NA
  }
  
  for (i in 1:nrow(forecastDF)){
    forecastDate = forecastDF$date[i]
    
    # Retrieve 365 days of data from the specific forecastDate
    trainData = dailyData %>% filter(date < forecastDate) %>% top_n(n = 365, date) %>% select(daily.demand)
    train_TS <- ts(trainData, frequency = 365)
    ts_model <- auto.arima(train_TS)
    
    # Forecast 30 days
    ts_forecast <- forecast.Arima(ts_model, h=30)
    
    # Fill datafrme with forecast values
    for (j in 1:30){
      col.name = paste("n",i,".demand", sep="")
      forecastDF[[i,(j+2)]] <- ts_forecast$mean[j]
    }
  }
  
  return(forecastDF)
}

arimaForecastA <- ArimaForecast(dailyDataA)
arimaForecastB <- ArimaForecast(dailyDataB)
arimaForecastC <- ArimaForecast(dailyDataC)

write.csv(arimaForecastA, "./Results/DailyForecast/arimaForecastA.csv", row.names=FALSE)
write.csv(arimaForecastB, "./Results/DailyForecast/arimaForecastB.csv", row.names=FALSE)
write.csv(arimaForecastC, "./Results/DailyForecast/arimaForecastC.csv", row.names=FALSE)

# RMSE model performance
arimaRmseA <- RandomSampleRMSE(arimaForecastA)
arimaRmseB <- RandomSampleRMSE(arimaForecastB)
arimaRmseC <- RandomSampleRMSE(arimaForecastC)

write.csv(arimaRmseA, "./Results/Evaluation/arimaRmseA.csv", row.names=FALSE)
write.csv(arimaRmseB, "./Results/Evaluation/arimaRmseB.csv", row.names=FALSE)
write.csv(arimaRmseC, "./Results/Evaluation/arimaRmseC.csv", row.names=FALSE)



############ Multivariate Model - MARS and Ensemble ############

# Create target variable - Use 30 days future moving average as trainning data target variable
dailyEmDataA <- dailyDataA %>% mutate(target.demand = rollmean(x = daily.demand, 31, align = "left", fill = NA))
dailyEmDataB <- dailyDataB %>% mutate(target.demand = rollmean(x = daily.demand, 31, align = "left", fill = NA))
dailyEmDataC <- dailyDataC %>% mutate(target.demand = rollmean(x = daily.demand, 31, align = "left", fill = NA))

dailyEmDataA <- dailyEmDataA[c(1, 3, 2)]
dailyEmDataB <- dailyEmDataB[c(1, 3, 2)]
dailyEmDataC <- dailyEmDataC[c(1, 3, 2)]

monthlyFeaturesTbl <- tbl_df(LoadMultivariateTimeSeries("./Data/predictors.csv"))
monthlyDataA = monthlyFeaturesTbl %>% select(-starts_with("tool.b")) %>% select(-starts_with("tool.c")) %>% rename(demand = tool.a)
monthlyDataB = monthlyFeaturesTbl %>% select(-starts_with("tool.a")) %>% select(-starts_with("tool.c")) %>% rename(demand = tool.b)
monthlyDataC = monthlyFeaturesTbl %>% select(-starts_with("tool.a")) %>% select(-starts_with("tool.b")) %>% rename(demand = tool.c)

remove(monthlyFeaturesTbl)

write.csv(monthlyDataA, "./Results/Processed/A-Monthly-Raw.csv")
write.csv(monthlyDataB, "./Results/Processed/B-Monthly-Raw.csv")
write.csv(monthlyDataC, "./Results/Processed/C-Monthly-Raw.csv")

# Feature Engineering
#' A Function to create additional data features to express variations of the time series
#' @param dailyData: date, target.demand, daily.demand, us.price, us.rig, us.prod
#' @return time series data: target.demand, daily.demand, dd.min, dd.max, dd.ma, dd.sd, us.price, us.price.ma, us.price.sd, us.rig, us.rig.ma, us.rig.sd, us.prod, us.prod.ma, us.prod.sd
CreateTimeDataFeatures <- function(dailyData, monthlyData, histTimeWindowMth = 1){
  
  if (histTimeWindowMth == 1){
    dailyData.feature <- dailyData %>%
      #daily.demand
      mutate(dd.min = rollapply(data = daily.demand, width = 30, FUN = min, align = "right", fill = NA)) %>%
      mutate(dd.max = rollmax(x = daily.demand, 30, align = "right", fill = NA)) %>%
      mutate(dd.ma = rollmean(x = daily.demand, 30, align = "right", fill = NA)) %>%
      mutate(dd.sd = rollapply(data = daily.demand, width = 30, FUN = sd, align = "right", fill = NA))
    
    dailyData.feature$month=floor_date(dailyData.feature$date, unit = "month")
    dailyData.feature <- full_join(dailyData.feature, monthlyData, by = c("month" = "date")) %>% select(-month, -demand)
     
  } else {
    
    monthlyData.feature <- monthlyData %>%
      #us.price
      mutate(us.price.ma = rollmean(x = us.price, histTimeWindowMth, align = "right", fill = NA)) %>% 
      mutate(us.price.sd = rollapply(data = us.price, width = histTimeWindowMth, FUN = sd, align = "right", fill = NA)) %>%
      #us.rig
      mutate(us.rig.ma = rollmean(x = us.rig, histTimeWindowMth, align = "right", fill = NA)) %>% 
      mutate(us.rig.sd = rollapply(data = us.rig, width = histTimeWindowMth, FUN = sd, align = "right", fill = NA)) %>%
      #us.prod
      mutate(us.prod.ma = rollmean(x = us.prod, histTimeWindowMth, align = "right", fill = NA)) %>% 
      mutate(us.prod.sd = rollapply(data = us.prod, width = histTimeWindowMth, FUN = sd, align = "right", fill = NA))
    
    dailyData.feature <- dailyData
    dailyData.feature$month=floor_date(dailyData.feature$date, unit = "month")
    dailyData.feature <- full_join(dailyData.feature, monthlyData.feature, by = c("month" = "date")) %>% select(-month, -demand)
    
  }
  
    
  return(na.omit(dailyData.feature))
}

dailyEmDataA.1m.features <- CreateTimeDataFeatures(dailyEmDataA, monthlyDataA, histTimeWindowMth = 1)
dailyEmDataA.3m.features <- CreateTimeDataFeatures(dailyEmDataA, monthlyDataA, histTimeWindowMth = 3)
dailyEmDataA.6m.features <- CreateTimeDataFeatures(dailyEmDataA, monthlyDataA, histTimeWindowMth = 6)

dailyEmDataB.1m.features <- CreateTimeDataFeatures(dailyEmDataB, monthlyDataB, histTimeWindowMth = 1)
dailyEmDataB.3m.features <- CreateTimeDataFeatures(dailyEmDataB, monthlyDataB, histTimeWindowMth = 3)
dailyEmDataB.6m.features <- CreateTimeDataFeatures(dailyEmDataB, monthlyDataB, histTimeWindowMth = 6)

dailyEmDataC.1m.features <- CreateTimeDataFeatures(dailyEmDataC, monthlyDataC, histTimeWindowMth = 1)
dailyEmDataC.3m.features <- CreateTimeDataFeatures(dailyEmDataC, monthlyDataC, histTimeWindowMth = 3)
dailyEmDataC.6m.features <- CreateTimeDataFeatures(dailyEmDataC, monthlyDataC, histTimeWindowMth = 6)

remove(monthlyDataA)
remove(monthlyDataB)
remove(monthlyDataC)


############ MARS Modeling - 1 Month Historical Data Analysis ############
# Forecast 30 days with MARS
library(earth)

MarsForecast <- function(dailyEmData){
  
  # Reserve data before 2015 as trainning data rolling window
  forecastDF <- dailyEmData %>% select(date, daily.demand) %>%
    filter(date >= as.Date("2015-01-01"))
  
  # Add forecast value columns
  for (i in 1:30){
    col.name = paste("n",i,".demand", sep="")
    forecastDF[[col.name]] <- NA
  }
  
  for (i in 1:nrow(forecastDF)){
    forecastDate = forecastDF$date[i]
    
    
    
    # Retrieve 2 years of data from the specific forecastDate
    train.data = na.omit(dailyEmData %>% filter(date < forecastDate) %>% top_n(n = 730, date))
    
    f <- as.formula(paste("target.demand", "~", 
                                        paste(names(dailyEmData)[!names(dailyEmData) %in% c("date", "target.demand")], collapse = "+")))
    
    mars.model <- earth(f, data = train.data)
    
    pred.data = dailyEmData %>% filter(date == forecastDate) %>% select(-date, -target.demand)
    mars.pred <- predict(mars.model, newdata = pred.data)
    mars.pred <- ifelse(mars.pred >= 0, mars.pred, 0)
    
    # Fill datafrme with forecast values
    for (j in 1:30){
      forecastDF[[i,(j+2)]] <- mars.pred
    }
  }
  
  return(forecastDF)
}

mars1M.ForecastA <- MarsForecast(dailyEmDataA.1m.features)
mars1M.ForecastB <- MarsForecast(dailyEmDataB.1m.features)
mars1M.ForecastC <- MarsForecast(dailyEmDataC.1m.features)

write.csv(mars1M.ForecastA, "./Results/DailyForecast/mars1M.ForecastA.csv", row.names=FALSE)
write.csv(mars1M.ForecastB, "./Results/DailyForecast/mars1M.ForecastB.csv", row.names=FALSE)
write.csv(mars1M.ForecastC, "./Results/DailyForecast/mars1M.ForecastC.csv", row.names=FALSE)

# RMSE model performance
mars1M.RMSE.A <- RandomSampleRMSE(mars1M.ForecastA)
mars1M.RMSE.B <- RandomSampleRMSE(mars1M.ForecastB)
mars1M.RMSE.C <- RandomSampleRMSE(mars1M.ForecastC)

write.csv(mars1M.RMSE.A, "./Results/Evaluation/mars1M.RMSE.A.csv", row.names=FALSE)
write.csv(mars1M.RMSE.B, "./Results/Evaluation/mars1M.RMSE.B.csv", row.names=FALSE)
write.csv(mars1M.RMSE.C, "./Results/Evaluation/mars1M.RMSE.C.csv", row.names=FALSE)


############ MARS Ensemble Modeling - combining 1/3/6 Months Historical Data Analysis ############
# Forecast 30 days with MARS

MarsEnsembleForecast <- function(dailyEmData.1M, dailyEmData.3M, dailyEmData.6M){
  
  # Reserve data before 2015 as trainning data rolling window
  forecastDF <- dailyEmData.1M %>% select(date, daily.demand) %>%
    filter(date >= as.Date("2015-01-01"))
  
  # Add forecast value columns
  for (i in 1:30){
    col.name = paste("n",i,".demand", sep="")
    forecastDF[[col.name]] <- NA
  }
  
  for (i in 1:nrow(forecastDF)){
    forecastDate = forecastDF$date[i]
    
    # 1 Month Model
    # Retrieve 2 years of data from the specific forecastDate
    train.data.1m = na.omit(dailyEmData.1M %>% filter(date < forecastDate) %>% top_n(n = 730, date))
    
    f.1m <- as.formula(paste("target.demand", "~", 
                          paste(names(train.data.1m)[!names(train.data.1m) %in% c("date", "target.demand")], collapse = "+")))
    
    mars.model.1m <- earth(f.1m, data = train.data.1m)
    
    pred.1m = dailyEmData.1M %>% filter(date == forecastDate) %>% select(-date, -target.demand)
    mars.pred.1m <- predict(mars.model.1m, newdata = pred.1m)
    mars.pred.1m <- ifelse(mars.pred.1m >= 0, mars.pred.1m, 0)
    
    # 3 Month Model
    # Retrieve 2 years of data from the specific forecastDate
    train.data.3m = na.omit(dailyEmData.3M %>% filter(date < forecastDate) %>% top_n(n = 730, date))
    
    f.3m <- as.formula(paste("target.demand", "~", 
                             paste(names(train.data.3m)[!names(train.data.3m) %in% c("date", "target.demand")], collapse = "+")))
    
    mars.model.3m <- earth(f.3m, data = train.data.3m)
    
    pred.3m = dailyEmData.3M %>% filter(date == forecastDate) %>% select(-date, -target.demand)
    mars.pred.3m <- predict(mars.model.3m, newdata = pred.3m)
    mars.pred.3m <- ifelse(mars.pred.3m >= 0, mars.pred.3m, 0)
    
    # 6 Month Model
    # Retrieve 2 years of data from the specific forecastDate
    train.data.6m = na.omit(dailyEmData.6M %>% filter(date < forecastDate) %>% top_n(n = 730, date))
    
    f.6m <- as.formula(paste("target.demand", "~", 
                             paste(names(train.data.6m)[!names(train.data.6m) %in% c("date", "target.demand")], collapse = "+")))
    
    mars.model.6m <- earth(f.6m, data = train.data.6m)
    
    pred.6m = dailyEmData.6M %>% filter(date == forecastDate) %>% select(-date, -target.demand)
    mars.pred.6m <- predict(mars.model.6m, newdata = pred.6m)
    mars.pred.6m <- ifelse(mars.pred.6m >= 0, mars.pred.6m, 0)
    
    # Fill datafrme with forecast values
    for (j in 1:30){
      forecastDF[[i,(j+2)]] <- mean(c(mars.pred.1m, mars.pred.3m, mars.pred.6m))
    }
  }
  
  return(forecastDF)
}


marsEnsembleForecastA <- MarsEnsembleForecast(dailyEmDataA.1m.features, dailyEmDataA.3m.features, dailyEmDataA.6m.features)
marsEnsembleForecastB <- MarsEnsembleForecast(dailyEmDataB.1m.features, dailyEmDataB.3m.features, dailyEmDataB.6m.features)
marsEnsembleForecastC <- MarsEnsembleForecast(dailyEmDataC.1m.features, dailyEmDataC.3m.features, dailyEmDataC.6m.features)

write.csv(marsEnsembleForecastA, "./Results/DailyForecast/marsEnsembleForecastA.csv", row.names=FALSE)
write.csv(marsEnsembleForecastB, "./Results/DailyForecast/marsEnsembleForecastB.csv", row.names=FALSE)
write.csv(marsEnsembleForecastC, "./Results/DailyForecast/marsEnsembleForecastC.csv", row.names=FALSE)

# RMSE model performance
marsEnsembleRMSE.A <- RandomSampleRMSE(marsEnsembleForecastA)
marsEnsembleRMSE.B <- RandomSampleRMSE(marsEnsembleForecastB)
marsEnsembleRMSE.C <- RandomSampleRMSE(marsEnsembleForecastC)

write.csv(marsEnsembleRMSE.A, "./Results/Evaluation/marsEnsembleRMSE.A.csv", row.names=FALSE)
write.csv(marsEnsembleRMSE.B, "./Results/Evaluation/marsEnsembleRMSE.B.csv", row.names=FALSE)
write.csv(marsEnsembleRMSE.C, "./Results/Evaluation/marsEnsembleRMSE.C.csv", row.names=FALSE)


############ Plot Model Performance ############
library(reshape2)
library(ggplot2)
library(ggthemes)

plotRmseA = baselineRmseA %>% select(date, rmse) %>% rename(Baseline = rmse)
plotRmseA$ARIMA = arimaRmseA$rmse
plotRmseA$MARS.1M = mars1M.RMSE.A$rmse
plotRmseA$MARS.EN = marsEnsembleRMSE.A$rmse
plotRmseA$Future.AVG = bestAvgRmseA$rmse
plotRmseA = melt(plotRmseA, id.vars = "date") %>% rename(Models = variable, RMSE = value)

plotRmseB = baselineRmseB %>% select(date, rmse) %>% rename(Baseline = rmse)
plotRmseB$ARIMA = arimaRmseB$rmse
plotRmseB$MARS.1M = mars1M.RMSE.B$rmse
plotRmseB$MARS.EN = marsEnsembleRMSE.B$rmse
plotRmseB$Future.AVG = bestAvgRmseB$rmse
plotRmseB = melt(plotRmseB, id.vars = "date") %>% rename(Models = variable, RMSE = value)

plotRmseC = baselineRmseC %>% select(date, rmse) %>% rename(Baseline = rmse)
plotRmseC$ARIMA = arimaRmseC$rmse
plotRmseC$MARS.1M = mars1M.RMSE.C$rmse
plotRmseC$MARS.EN = marsEnsembleRMSE.C$rmse
plotRmseC$Future.AVG = bestAvgRmseC$rmse
plotRmseC = melt(plotRmseC, id.vars = "date") %>% rename(Models = variable, RMSE = value)

ggplot(plotRmseA, aes(x=Models, y=RMSE, fill=Models)) + geom_boxplot() + 
  scale_fill_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  scale_x_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  ggtitle("Asset A - RMSE") + theme_classic()
ggplot(plotRmseA, aes(x=RMSE, fill=Models)) + geom_density(alpha=.3) + 
  scale_fill_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  xlab("Asset A - RMSE") + ylab("Density") + theme_classic()

# Exclude outliers of tool B MARS.1M model for better plot visibility
ggplot(plotRmseB, aes(x=Models, y=RMSE, fill=Models)) + geom_boxplot() + 
  scale_fill_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  scale_x_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  ggtitle("Asset B - RMSE") + coord_cartesian(ylim = c(0,25)) + theme_classic()
ggplot(plotRmseB, aes(x=RMSE, fill=Models)) + geom_density(alpha=.3) + 
  scale_fill_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  xlab("Asset B - RMSE") + ylab("Density") + coord_cartesian(xlim = c(0,30)) + theme_classic()

ggplot(plotRmseC, aes(x=Models, y=RMSE, fill=Models)) + geom_boxplot() + 
  scale_fill_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  scale_x_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  ggtitle("Asset C - RMSE") + theme_classic()
ggplot(plotRmseC, aes(x=RMSE, fill=Models)) + geom_density(alpha=.3) + 
  scale_fill_discrete(labels=c("Baseline","ARIMA","MARS","Ensemble","Future 30-days avg")) +
  xlab("Asset C - RMSE") + ylab("Density") + theme_classic()


############  Average RMSE comparison ############
mean(baselineRmseA$rmse)
mean(arimaRmseA$rmse)
mean(mars1M.RMSE.A$rmse)
mean(marsEnsembleRMSE.A$rmse)
mean(bestAvgRmseA$rmse)

mean(baselineRmseB$rmse)
mean(arimaRmseB$rmse)
mean(mars1M.RMSE.B$rmse)
mean(marsEnsembleRMSE.B$rmse)
mean(bestAvgRmseB$rmse)

mean(baselineRmseC$rmse)
mean(arimaRmseC$rmse)
mean(mars1M.RMSE.C$rmse)
mean(marsEnsembleRMSE.C$rmse)
mean(bestAvgRmseC$rmse)





############ Plot Prediction Comparison Functions ############
PlotPredictionComparison <- function(compareStartDate, comparingHistDays, baseline, arimaForecast, ensembleForecast, bestAvg){
  
  # Actual demand starting from the comparingDays
  actual = filter(baseline, date >= compareStartDate-comparingHistDays & date < compareStartDate)[,1:2]
  actual$Baseline = NA
  actual$ARIMA = NA
  actual$Ensemble = NA
  actual$Future.AVG = NA
  
  # 30 days demand starting from the sampled date
  predictions = filter(baseline, date >= compareStartDate)[1:30,1:2]
  
  # Transpose the forecasted 30 days demand data in columns, starting from the sampled date
  predictions$Baseline = as.data.frame(t(filter(baseline, date == compareStartDate)[,3:ncol(baseline)]))[,1]
  predictions$ARIMA = as.data.frame(t(filter(arimaForecast, date == compareStartDate)[,3:ncol(arimaForecast)]))[,1]
  predictions$Ensemble = as.data.frame(t(filter(ensembleForecast, date == compareStartDate)[,3:ncol(ensembleForecast)]))[,1]
  predictions$Future.AVG = as.data.frame(t(filter(bestAvg, date == compareStartDate)[,3:ncol(bestAvg)]))[,1]
  
  plot_data <- rbind(actual, predictions)
  
  # TODO Plot adjust colors

    ggplot(plot_data) + 
    geom_line(aes(x=date, y=daily.demand, colour = "1. Actual Demand", color="black"), size=0.8) + 
    geom_line(aes(x=date, y=Baseline, colour = "2. Baseline Prediction"), linetype="twodash", size=1) + 
    geom_line(aes(x=date, y=ARIMA, colour = "3. ARIMA Prediction"), linetype="dotdash", size=1) +
    geom_line(aes(x=date, y=Ensemble, colour = "4. Ensemble Prediction"), linetype="twodash", size=1.5) +
    geom_line(aes(x=date, y=Future.AVG, colour = "5. Future 30 days average demand"), linetype="twodash", size=1) +
    xlab ("") + ylab ("Daily Demand") +
    scale_colour_manual(values = c("black", "red", "orange", "deepskyblue", "yellowgreen")) +
    theme_classic()
  
}

# Plot Asset A
compareStartDate = marsEnsembleForecastA[nrow(marsEnsembleForecastA)-30,]$date

PlotPredictionComparison(compareStartDate, 60, baselineForecastA, arimaForecastA, marsEnsembleForecastA, bestAvgForecastA)

compareStartDate = as.Date("2015-12-01")
PlotPredictionComparison(compareStartDate, 60, baselineForecastB, arimaForecastB, marsEnsembleForecastB, bestAvgForecastB)

compareStartDate = as.Date("2016-01-01")
compareStartDate = marsEnsembleForecastA[nrow(marsEnsembleForecastA)-30,]$date
PlotPredictionComparison(compareStartDate, 60, baselineForecastC, arimaForecastC, marsEnsembleForecastC, bestAvgForecastC)
