library(lubridate)
library(stringr)
library(dplyr)
library(zoo)
library(plotly)
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

a1MForecast <- read.csv("./Results/Forecast/A-1M-Forecast.csv") %>% select(date, ensemble.pred) %>% rename(forecast = ensemble.pred)
a1MForecast$date <- as.Date(a1MForecast$date)

b1MForecast <- read.csv("./Results/Forecast/B-1M-Forecast.csv") %>% select(date, ensemble.pred) %>% rename(forecast = ensemble.pred)
b1MForecast$date <- as.Date(b1MForecast$date)

c1MForecast <- read.csv("./Results/Forecast/C-1M-Forecast.csv") %>% select(date, ensemble.pred) %>% rename(forecast = ensemble.pred)
c1MForecast$date <- as.Date(c1MForecast$date)

dailyDemandA <- tbl_df(LoadMultivariateTimeSeries("./Data/toola_dd.csv")) %>% rename(daily.demand = toola.dd) %>% filter(date >= as.Date("2013-01-01") & date < as.Date("2016-07-01"))
dailyDemandB <- tbl_df(LoadMultivariateTimeSeries("./Data/toolb_dd.csv")) %>% rename(daily.demand = toolb.dd) %>% filter(date >= as.Date("2013-01-01") & date < as.Date("2016-07-01"))
dailyDemandC <- tbl_df(LoadMultivariateTimeSeries("./Data/toolc_dd.csv")) %>% rename(daily.demand = toolc.dd) %>% filter(date >= as.Date("2013-01-01") & date < as.Date("2016-07-01"))


###### Data Processing - Calculate demand difference and std of difference ######

# Tool A
dailyDemandA$month=floor_date(dailyDemandA$date, unit = "month")

# Monthly average
dailyDemandA <- full_join(dailyDemandA,
                          summarise(group_by(dailyDemandA, month), monthly.avg=mean(daily.demand)),
                          by = c("month" = "month"))
# Daily difference from monthly average
dailyDemandA$avg.diff <- dailyDemandA$monthly.avg - dailyDemandA$daily.demand

# Exclude March 2016 Data due to data quality issue
dailyDemandA <- dailyDemandA %>% filter(month != as.Date("2016-03-01"))

# Standard deviation of daily difference from monthly average
dailyDemandA <- full_join(dailyDemandA,
                          summarise(group_by(dailyDemandA, month), avg.diff.sd=sd(avg.diff)) %>%
                            mutate(prev.diff.sd = lag(avg.diff.sd, n = 1)),
                                     by = c("month" = "month"))

# Monthly average forecast
dailyDemandA <- full_join(dailyDemandA, select(a1MForecast, date, forecast), by = c("month" = "date"))




# Tool B
dailyDemandB$month=floor_date(dailyDemandB$date, unit = "month")

# Monthly average
dailyDemandB <- full_join(dailyDemandB,
                          summarise(group_by(dailyDemandB, month), monthly.avg=mean(daily.demand)),
                          by = c("month" = "month"))
# Daily difference from monthly average
dailyDemandB$avg.diff <- dailyDemandB$monthly.avg - dailyDemandB$daily.demand

# Exclude March 2016 Data due to data quality issue
dailyDemandB <- dailyDemandB %>% filter(month != as.Date("2016-03-01"))

# Standard deviation of daily difference from monthly average
dailyDemandB <- full_join(dailyDemandB,
                          summarise(group_by(dailyDemandB, month), avg.diff.sd=sd(avg.diff)) %>%
                            mutate(prev.diff.sd = lag(avg.diff.sd, n = 1)),
                          by = c("month" = "month"))

# Monthly average forecast
dailyDemandB <- full_join(dailyDemandB, select(b1MForecast, date, forecast), by = c("month" = "date"))




# Tool C
dailyDemandC$month=floor_date(dailyDemandC$date, unit = "month")

# Monthly average
dailyDemandC <- full_join(dailyDemandC,
                          summarise(group_by(dailyDemandC, month), monthly.avg=mean(daily.demand)),
                          by = c("month" = "month"))
# Daily difference from monthly average
dailyDemandC$avg.diff <- dailyDemandC$monthly.avg - dailyDemandC$daily.demand

# Exclude March 2016 Data due to data quality issue
dailyDemandC <- dailyDemandC %>% filter(month != as.Date("2016-03-01"))

# Standard deviation of daily difference from monthly average
dailyDemandC <- full_join(dailyDemandC,
                          summarise(group_by(dailyDemandC, month), avg.diff.sd=sd(avg.diff)) %>%
                            mutate(prev.diff.sd = lag(avg.diff.sd, n = 1)),
                          by = c("month" = "month"))

# Monthly average forecast
dailyDemandC <- full_join(dailyDemandC, select(c1MForecast, date, forecast), by = c("month" = "date"))




######  Normality check ######
hist(dailyDemandA$avg.diff)
qqnorm(dailyDemandA$avg.diff)
shapiro.test(dailyDemandA$avg.diff)
sd(na.omit(dailyDemandA$avg.diff))

hist(dailyDemandB$avg.diff)
qqnorm(dailyDemandB$avg.diff)
shapiro.test(dailyDemandB$avg.diff)
sd(na.omit(dailyDemandB$avg.diff))

hist(dailyDemandC$avg.diff)
qqnorm(dailyDemandC$avg.diff)
shapiro.test(dailyDemandC$avg.diff)
sd(na.omit(dailyDemandC$avg.diff))


######  TODO Heatmap for choice of factor ######
factorA1 = 1
factorA2 = 1.8
factorA3 = 2.5

factorB1 = 1
factorB2 = 1.8
factorB3 = 2.5

factorC1 = 1
factorC2 = 1.8
factorC3 = 2.5



######  Inventory Planning ######

# Tool A
dailyDemandA$monthly.stock.f1 = dailyDemandA$monthly.avg + dailyDemandA$prev.diff.sd * factorA1
dailyDemandA <- dailyDemandA %>% mutate(monthly.stock.f1 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f1))
dailyDemandA$stock.fc.f1 = dailyDemandA$forecast + dailyDemandA$prev.diff.sd * factorA1

dailyDemandA$monthly.stock.f2 = dailyDemandA$monthly.avg + dailyDemandA$prev.diff.sd * factorA2
dailyDemandA <- dailyDemandA %>% mutate(monthly.stock.f2 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f2))
dailyDemandA$stock.fc.f2 = dailyDemandA$forecast + dailyDemandA$prev.diff.sd * factorA2

dailyDemandA$monthly.stock.f3 = dailyDemandA$monthly.avg + dailyDemandA$prev.diff.sd * factorA3
dailyDemandA <- dailyDemandA %>% mutate(monthly.stock.f3 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f3))
dailyDemandA$stock.fc.f3 = dailyDemandA$forecast + dailyDemandA$prev.diff.sd * factorA3

plotToolAPlan <- plot_ly(data = dailyDemandA, x=~date) %>%
  add_lines(y = ~daily.demand, name = 'daily demand', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'solid')) %>%
  add_lines(y = ~forecast, name = 'forecast', line = list(color = "rgb(0, 0, 0)", width = 3, dash = 'dashdot')) %>%
  # add_lines(y = ~monthly.stock.f1, name = 'factor 1', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  add_lines(y = ~monthly.stock.f2, name = 'factor 2', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  # add_lines(y = ~monthly.stock.f3, name = 'factor 3', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  add_lines(y = ~stock.fc.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~stock.fc.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~stock.fc.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  layout(
    xaxis = list(
      title = "",
      hoverformat = "%Y-%m-%d",
      showgrid = FALSE,
      tickmode = "auto",
      ticks = "inside",
      rangeselector = list(
        buttons = list(
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "backward"),
          list(
            count = 2,
            label = "2 yr",
            step = "year",
            stepmode = "backward"),
          list(step = "all"))),
      rangeslider = list(type = "date")
    ),
    yaxis = list(
      title = "daily demand",
      showgrid = FALSE,
      tickmode = "auto",
      ticks = "outside",
      rangemode = "tozero"
    ),
    shapes = list(
      list(type = "rect",
           fillcolor = "grey", line = list(color = "grey"), opacity = 0.1,
           x0 = as.Date("2013-01-01"), x1 = as.Date("2015-01-01"), xref = "x",
           y0 = 0, y1 = max(dailyDemandA$daily.demand, na.rm = TRUE) + 2, yref = "y"))
  )
plotToolAPlan


# Tool B
dailyDemandB$monthly.stock.f1 = dailyDemandB$monthly.avg + dailyDemandB$prev.diff.sd * factorA1
dailyDemandB <- dailyDemandB %>% mutate(monthly.stock.f1 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f1))
dailyDemandB$stock.fc.f1 = dailyDemandB$forecast + dailyDemandB$prev.diff.sd * factorA1

dailyDemandB$monthly.stock.f2 = dailyDemandB$monthly.avg + dailyDemandB$prev.diff.sd * factorA2
dailyDemandB <- dailyDemandB %>% mutate(monthly.stock.f2 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f2))
dailyDemandB$stock.fc.f2 = dailyDemandB$forecast + dailyDemandB$prev.diff.sd * factorA2

dailyDemandB$monthly.stock.f3 = dailyDemandB$monthly.avg + dailyDemandB$prev.diff.sd * factorA3
dailyDemandB <- dailyDemandB %>% mutate(monthly.stock.f3 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f3))
dailyDemandB$stock.fc.f3 = dailyDemandB$forecast + dailyDemandB$prev.diff.sd * factorA3

plotToolBPlan <- plot_ly(data = dailyDemandB, x=~date) %>%
  add_lines(y = ~daily.demand, name = 'daily demand', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'solid')) %>%
  add_lines(y = ~forecast, name = 'forecast', line = list(color = "rgb(0, 0, 0)", width = 3, dash = 'dashdot')) %>%
  # add_lines(y = ~monthly.stock.f1, name = 'factor 1', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  add_lines(y = ~monthly.stock.f2, name = 'factor 2', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  # add_lines(y = ~monthly.stock.f3, name = 'factor 3', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  add_lines(y = ~stock.fc.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~stock.fc.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~stock.fc.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  layout(
    xaxis = list(
      title = "",
      hoverformat = "%Y-%m-%d",
      showgrid = FALSE,
      tickmode = "auto",
      ticks = "inside",
      rangeselector = list(
        buttons = list(
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "backward"),
          list(
            count = 2,
            label = "2 yr",
            step = "year",
            stepmode = "backward"),
          list(step = "all"))),
      rangeslider = list(type = "date")
    ),
    yaxis = list(
      title = "daily demand",
      showgrid = FALSE,
      tickmode = "auto",
      ticks = "outside",
      rangemode = "tozero"
    ),
    shapes = list(
      list(type = "rect",
           fillcolor = "grey", line = list(color = "grey"), opacity = 0.1,
           x0 = as.Date("2013-01-01"), x1 = as.Date("2015-01-01"), xref = "x",
           y0 = 0, y1 = max(dailyDemandB$daily.demand, na.rm = TRUE) + 2, yref = "y"))
  )
plotToolBPlan



# Tool C
dailyDemandC$monthly.stock.f1 = dailyDemandC$monthly.avg + dailyDemandC$prev.diff.sd * factorA1
dailyDemandC <- dailyDemandC %>% mutate(monthly.stock.f1 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f1))
dailyDemandC$stock.fc.f1 = dailyDemandC$forecast + dailyDemandC$prev.diff.sd * factorA1

dailyDemandC$monthly.stock.f2 = dailyDemandC$monthly.avg + dailyDemandC$prev.diff.sd * factorA2
dailyDemandC <- dailyDemandC %>% mutate(monthly.stock.f2 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f2))
dailyDemandC$stock.fc.f2 = dailyDemandC$forecast + dailyDemandC$prev.diff.sd * factorA2

dailyDemandC$monthly.stock.f3 = dailyDemandC$monthly.avg + dailyDemandC$prev.diff.sd * factorA3
dailyDemandC <- dailyDemandC %>% mutate(monthly.stock.f3 = ifelse(month > as.Date("2015-01-01"), NA, monthly.stock.f3))
dailyDemandC$stock.fc.f3 = dailyDemandC$forecast + dailyDemandC$prev.diff.sd * factorA3

plotToolCPlan <- plot_ly(data = dailyDemandC, x=~date) %>%
  add_lines(y = ~daily.demand, name = 'daily demand', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'solid')) %>%
  add_lines(y = ~forecast, name = 'forecast', line = list(color = "rgb(0, 0, 0)", width = 3, dash = 'dashdot')) %>%
  # add_lines(y = ~monthly.stock.f1, name = 'factor 1', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  add_lines(y = ~monthly.stock.f2, name = 'factor 2', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  # add_lines(y = ~monthly.stock.f3, name = 'factor 3', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'longdashdot')) %>%
  # add_lines(y = ~stock.fc.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~stock.fc.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~stock.fc.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  layout(
    xaxis = list(
      title = "",
      hoverformat = "%Y-%m-%d",
      showgrid = FALSE,
      tickmode = "auto",
      ticks = "inside",
      rangeselector = list(
        buttons = list(
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "backward"),
          list(
            count = 2,
            label = "2 yr",
            step = "year",
            stepmode = "backward"),
          list(step = "all"))),
      rangeslider = list(type = "date")
    ),
    yaxis = list(
      title = "daily demand",
      showgrid = FALSE,
      tickmode = "auto",
      ticks = "outside",
      rangemode = "tozero"
    ),
    shapes = list(
      list(type = "rect",
           fillcolor = "grey", line = list(color = "grey"), opacity = 0.1,
           x0 = as.Date("2013-01-01"), x1 = as.Date("2015-01-01"), xref = "x",
           y0 = 0, y1 = max(dailyDemandC$daily.demand, na.rm = TRUE) + 2, yref = "y"))
  )
plotToolCPlan


# TODO:
# Show the story of model selection
# Build function for random forest, could work for tool B
# Calculate forecast differences againt daily demand of each tool and compute the mean and standard deviation
# Box plot forecast difference
# Check normal distribution of daily diff
# Create Heat map of means and std of forecast differences
# Plot forecast vs daily demand and differences


# Sys.setenv("plotly_username"="wyhzhang")
# Sys.setenv("plotly_api_key"="sRqGpBFlPtzcoJ764EqD")
# plotly_POST(plotToolAPlan, filename = "toolAPlan")
# plotly_POST(plotToolBPlan, filename = "toolBPlan")
# plotly_POST(plotToolCPlan, filename = "toolCPlan")