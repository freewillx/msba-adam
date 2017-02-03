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
histTimeWindow = 30

# Tool A
dailyDemandA$month=floor_date(dailyDemandA$date, unit = "month")
dailyDemandA <- full_join(dailyDemandA, select(a1MForecast, date, forecast), by = c("month" = "date"))
dailyDemandA$diff = dailyDemandA$forecast - dailyDemandA$daily.demand

# Exclude March 2016 Data due to data quality issue
dailyDemandA <- dailyDemandA %>% filter(month != as.Date("2016-03-01"))

# Calculate monthly standard deviation
a1MDiffStd <- summarise(group_by(na.omit(dailyDemandA), month), sd=sd(diff)) %>% mutate(monthly.diff.sd = lag(sd, n = 1))
dailyDemandA <- full_join(dailyDemandA, 
                          select(a1MDiffStd, month, monthly.diff.sd), 
                          by = c("month" = "month"))

# Calculate 30 days rolling standard deviation
dailyDemandA <- dailyDemandA %>%
  mutate(rolling.diff.sd = rollapply(data = diff, width = histTimeWindow, FUN = sd, align = "right", fill = NA))


# Tool B
dailyDemandB$month=floor_date(dailyDemandB$date, unit = "month")
dailyDemandB <- full_join(dailyDemandB, select(b1MForecast, date, forecast), by = c("month" = "date"))
dailyDemandB$diff = dailyDemandB$forecast - dailyDemandB$daily.demand

# Exclude March 2016 Data due to data quality issue
dailyDemandB <- dailyDemandB %>% filter(month != as.Date("2016-03-01"))

# Calculate monthly standard deviation
b1MDiffStd <- summarise(group_by(na.omit(dailyDemandB), month), sd=sd(diff)) %>% mutate(monthly.diff.sd = lag(sd, n = 1))
dailyDemandB <- full_join(dailyDemandB, 
                          select(b1MDiffStd, month, monthly.diff.sd), 
                          by = c("month" = "month"))

# Calculate 30 days rolling standard deviation
dailyDemandB <- dailyDemandB %>%
  mutate(rolling.diff.sd = rollapply(data = diff, width = histTimeWindow, FUN = sd, align = "right", fill = NA))


# Tool C
dailyDemandC$month=floor_date(dailyDemandC$date, unit = "month")
dailyDemandC <- full_join(dailyDemandC, select(c1MForecast, date, forecast), by = c("month" = "date"))
dailyDemandC$diff = dailyDemandC$forecast - dailyDemandC$daily.demand

# Exclude March 2016 Data due to data quality issue
dailyDemandC <- dailyDemandC %>% filter(month != as.Date("2016-03-01"))

# Calculate monthly standard deviation
c1MDiffStd <- summarise(group_by(na.omit(dailyDemandC), month), sd=sd(diff)) %>% mutate(monthly.diff.sd = lag(sd, n = 1))
dailyDemandC <- full_join(dailyDemandC, 
                          select(c1MDiffStd, month, monthly.diff.sd), 
                          by = c("month" = "month"))

# Calculate 30 days rolling standard deviation
dailyDemandC <- dailyDemandC %>%
  mutate(rolling.diff.sd = rollapply(data = diff, width = histTimeWindow, FUN = sd, align = "right", fill = NA))


######  Normality check ######
hist(dailyDemandA$diff)
qqnorm(dailyDemandA$diff)
shapiro.test(dailyDemandA$diff)
sd(na.omit(dailyDemandA$diff))

hist(dailyDemandB$diff)
qqnorm(dailyDemandB$diff)
shapiro.test(dailyDemandB$diff)
sd(na.omit(dailyDemandB$diff))

hist(dailyDemandC$diff)
qqnorm(dailyDemandC$diff)
shapiro.test(dailyDemandC$diff)
sd(na.omit(dailyDemandC$diff))


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
histTimeWindow = 30

# Tool A
dailyDemandA$daily.stock.f1 = dailyDemandA$forecast + dailyDemandA$rolling.diff.sd * factorA1
dailyDemandA$daily.stock.f2 = dailyDemandA$forecast + dailyDemandA$rolling.diff.sd * factorA2
dailyDemandA$daily.stock.f3 = dailyDemandA$forecast + dailyDemandA$rolling.diff.sd * factorA3

dailyDemandA$monthly.stock.f1 = dailyDemandA$forecast + dailyDemandA$monthly.diff.sd * factorA1
dailyDemandA$monthly.stock.f2 = dailyDemandA$forecast + dailyDemandA$monthly.diff.sd * factorA2
dailyDemandA$monthly.stock.f3 = dailyDemandA$forecast + dailyDemandA$monthly.diff.sd * factorA3

plotToolAPlan <- plot_ly(data = dailyDemandA, x=~date) %>%
  add_lines(y = ~daily.demand, name = 'daily demand', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'solid')) %>%
  add_lines(y = ~forecast, name = 'forecast', line = list(color = "rgb(148, 103, 189)", width = 2, dash = 'dashdot')) %>%
  add_lines(y = ~daily.stock.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~daily.stock.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~daily.stock.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
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
    )
  )
plotToolAPlan


# Tool B
dailyDemandB$daily.stock.f1 = dailyDemandB$forecast + dailyDemandB$rolling.diff.sd * factorB1
dailyDemandB$daily.stock.f2 = dailyDemandB$forecast + dailyDemandB$rolling.diff.sd * factorB2
dailyDemandB$daily.stock.f3 = dailyDemandB$forecast + dailyDemandB$rolling.diff.sd * factorB3

dailyDemandB$monthly.stock.f1 = dailyDemandB$forecast + dailyDemandB$monthly.diff.sd * factorB1
dailyDemandB$monthly.stock.f2 = dailyDemandB$forecast + dailyDemandB$monthly.diff.sd * factorB2
dailyDemandB$monthly.stock.f3 = dailyDemandB$forecast + dailyDemandB$monthly.diff.sd * factorB3

plotToolBPlan <- plot_ly(data = dailyDemandB, x=~date) %>%
  add_lines(y = ~daily.demand, name = 'daily demand', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'solid')) %>%
  add_lines(y = ~forecast, name = 'forecast', line = list(color = "rgb(148, 103, 189)", width = 2, dash = 'dashdot')) %>%
  add_lines(y = ~daily.stock.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~daily.stock.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~daily.stock.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
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
    )
  )
plotToolBPlan



# Tool C
dailyDemandC$daily.stock.f1 = dailyDemandC$forecast + dailyDemandC$rolling.diff.sd * factorC1
dailyDemandC$daily.stock.f2 = dailyDemandC$forecast + dailyDemandC$rolling.diff.sd * factorC2
dailyDemandC$daily.stock.f3 = dailyDemandC$forecast + dailyDemandC$rolling.diff.sd * factorC3

dailyDemandC$monthly.stock.f1 = dailyDemandC$forecast + dailyDemandC$monthly.diff.sd * factorC1
dailyDemandC$monthly.stock.f2 = dailyDemandC$forecast + dailyDemandC$monthly.diff.sd * factorC2
dailyDemandC$monthly.stock.f3 = dailyDemandC$forecast + dailyDemandC$monthly.diff.sd * factorC3

plotToolCPlan <- plot_ly(data = dailyDemandC, x=~date) %>%
  add_lines(y = ~daily.demand, name = 'daily demand', line = list(color = "rgb(31, 119, 180)", width = 2, dash = 'solid')) %>%
  add_lines(y = ~forecast, name = 'forecast', line = list(color = "rgb(148, 103, 189)", width = 2, dash = 'dashdot')) %>%
  add_lines(y = ~daily.stock.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~daily.stock.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~daily.stock.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f1, name = 'factor 1', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f2, name = 'factor 2', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  # add_lines(y = ~monthly.stock.f3, name = 'factor 3', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
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
    )
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