######  Time series plot of 1 months/2 months/3 months forecast ###### 
library(plotly)
library(dplyr)

###### Load Data ######
aRaw <- read.csv("./Results/Processed/A-Daily-Raw.csv")
aRaw$date = as.Date(aRaw$date)
bRaw <- read.csv("./Results/Processed/B-Daily-Raw.csv")
bRaw$date = as.Date(bRaw$date)
cRaw <- read.csv("./Results/Processed/C-Daily-Raw.csv")
cRaw$date = as.Date(cRaw$date)


# Plot daily demand daata
rawDailyDemand = data.frame(Date = aRaw$date, Tool_A = aRaw$daily.demand)
rawDailyDemand <- full_join(rawDailyDemand, bRaw, by = c("Date" = "date")) %>% rename(Tool_B = daily.demand) %>%
  full_join(cRaw, by = c("Date" = "date")) %>% rename(Tool_C = daily.demand)

rawDailyDemand = filter(rawDailyDemand, Date >= as.Date("2013-01-01"))


plotDailyDemand <- plot_ly(data = rawDailyDemand, x=~Date) %>%
  add_lines(y = ~Tool_A, name = 'Asset A Demand', line = list(color = "rgb(248, 118, 109)")) %>%
  add_lines(y = ~Tool_B, name = 'Asset B Demand', line = list(color = "rgb(0, 186, 56)")) %>%
  add_lines(y = ~Tool_C, name = 'Asset C Demand', line = list(color = "rgb(95, 155, 255)")) %>%
  layout(xaxis = list(
    title = "",
    hoverformat = "%Y-%m",
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
    title = "Daily Demand",
    showgrid = FALSE,
    tickmode = "auto",
    ticks = "outside",
    rangemode = "tozero"
  )
  # ,
  # shapes = list(
  #   list(type = "rect",
  #        fillcolor = "grey", line = list(color = "grey"), opacity = 0.1,
  #        x0 = as.Date("2013-01-01"), x1 = as.Date("2015-01-01"), xref = "x",
  #        y0 = 0, y1 = max(na.omit(rawDailyDemand$Tool_B)) + 2, yref = "y")
  #   )
  )
plotDailyDemand

















aRaw <- aRaw[,c("date", "demand")]
aRaw$date <- as.Date(aRaw$date)

a1MForecast <- read.csv("./Results/Forecast/A-1M-Forecast.csv")
a1MForecast <- a1MForecast[,2:ncol(a1MForecast)]
a1MForecast$date <- as.Date(a1MForecast$date)

a2MForecast <- read.csv("./Results/Forecast/A-2M-Forecast.csv")
a2MForecast <- a2MForecast[,2:ncol(a2MForecast)]
a2MForecast$date <- as.Date(a2MForecast$date)

a3MForecast <- read.csv("./Results/Forecast/A-3M-Forecast.csv")
a3MForecast <- a3MForecast[,2:ncol(a3MForecast)]
a3MForecast$date <- as.Date(a3MForecast$date)

bRaw <- read.csv("./Results/Forecast/B-Monthly-Raw.csv")
bRaw <- bRaw[,c("date", "demand")]
bRaw$date <- as.Date(bRaw$date)

b1MForecast <- read.csv("./Results/Forecast/B-1M-Forecast.csv")
b1MForecast <- b1MForecast[,2:ncol(b1MForecast)]
b1MForecast$date <- as.Date(b1MForecast$date)

b2MForecast <- read.csv("./Results/Forecast/B-2M-Forecast.csv")
b2MForecast <- b2MForecast[,2:ncol(b2MForecast)]
b2MForecast$date <- as.Date(b2MForecast$date)

b3MForecast <- read.csv("./Results/Forecast/B-3M-Forecast.csv")
b3MForecast <- b3MForecast[,2:ncol(b3MForecast)]
b3MForecast$date <- as.Date(b3MForecast$date)

cRaw <- read.csv("./Results/Forecast/C-Monthly-Raw.csv")
cRaw <- cRaw[,c("date", "demand")]
cRaw$date <- as.Date(cRaw$date)

c1MForecast <- read.csv("./Results/Forecast/C-1M-Forecast.csv")
c1MForecast <- c1MForecast[,2:ncol(c1MForecast)]
c1MForecast$date <- as.Date(c1MForecast$date)

c2MForecast <- read.csv("./Results/Forecast/C-2M-Forecast.csv")
c2MForecast <- c2MForecast[,2:ncol(c2MForecast)]
c2MForecast$date <- as.Date(c2MForecast$date)

c3MForecast <- read.csv("./Results/Forecast/C-3M-Forecast.csv")
c3MForecast <- c3MForecast[,2:ncol(c3MForecast)]
c3MForecast$date <- as.Date(c3MForecast$date)


###### Plot demand forecast ######
# Tool A
forecastDataA = head(aRaw, n = 25) %>% rename(train = demand) %>% 
  full_join(aRaw[25:nrow(aRaw),] %>% rename(actual = demand), by = c("date" = "date")) %>%
  full_join(select(a1MForecast, date, ensemble.pred) %>% rename(pred1m = ensemble.pred), by = c("date" = "date")) %>%
  full_join(select(a2MForecast, date, ensemble.pred) %>% rename(pred2m = ensemble.pred), by = c("date" = "date")) %>%
  full_join(select(a3MForecast, date, ensemble.pred) %>% rename(pred3m = ensemble.pred), by = c("date" = "date"))

# Interpolate points for smooth ploting
forecastDataA[25,4] <- forecastDataA[25,3]
forecastDataA[26,5] <- forecastDataA[26,3]
forecastDataA[27,6] <- forecastDataA[27,3]

plotToolAForecast <- plot_ly(data = forecastDataA, x=~date) %>%
  add_lines(y = ~train, name = 'training window', line = list(color = "rgb(31, 119, 180)", width = 4, dash = 'dashdot')) %>%
  add_lines(y = ~actual, name = 'actual demand', line = list(color = "rgb(31, 119, 180)", width = 4, dash = 'solid')) %>%
  add_lines(y = ~pred3m, name = '3m forecast', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~pred2m, name = '2m forecast', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~pred1m, name = '1m forecast', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  layout(xaxis = list(
           title = "",
           hoverformat = "%Y-%m",
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
           title = "demand",
           showgrid = FALSE,
           tickmode = "auto",
           ticks = "outside",
           rangemode = "tozero"
         ),
         shapes = list(
           list(type = "rect",
                fillcolor = "grey", line = list(color = "grey"), opacity = 0.1,
                x0 = forecastDataA$date[1], x1 = forecastDataA$date[25], xref = "x",
                y0 = 0, y1 = max(na.omit(aRaw$demand)) + 2, yref = "y"))
  )
plotToolAForecast


# Tool B
forecastDataB = head(bRaw, n = 25) %>% rename(train = demand) %>% 
  full_join(bRaw[25:nrow(bRaw),] %>% rename(actual = demand), by = c("date" = "date")) %>%
  full_join(select(b1MForecast, date, ensemble.pred) %>% rename(pred1m = ensemble.pred), by = c("date" = "date")) %>%
  full_join(select(b2MForecast, date, ensemble.pred) %>% rename(pred2m = ensemble.pred), by = c("date" = "date")) %>%
  full_join(select(b3MForecast, date, ensemble.pred) %>% rename(pred3m = ensemble.pred), by = c("date" = "date"))

# Interpolate points for smooth ploting
forecastDataB[25,4] <- forecastDataB[25,3]
forecastDataB[26,5] <- forecastDataB[26,3]
forecastDataB[27,6] <- forecastDataB[27,3]

plotToolBForecast <- plot_ly(data = forecastDataB, x=~date) %>%
  add_lines(y = ~train, name = 'training window', line = list(color = "rgb(31, 119, 180)", width = 4, dash = 'dashdot')) %>%
  add_lines(y = ~actual, name = 'actual demand', line = list(color = "rgb(31, 119, 180)", width = 4, dash = 'solid')) %>%
  add_lines(y = ~pred3m, name = '3m forecast', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~pred2m, name = '2m forecast', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~pred1m, name = '1m forecast', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  layout(xaxis = list(
    title = "",
    hoverformat = "%Y-%m",
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
    title = "demand",
    showgrid = FALSE,
    tickmode = "auto",
    ticks = "outside",
    rangemode = "tozero"
  ),
  shapes = list(
    list(type = "rect",
         fillcolor = "grey", line = list(color = "grey"), opacity = 0.1,
         x0 = forecastDataB$date[1], x1 = forecastDataB$date[25], xref = "x",
         y0 = 0, y1 = max(na.omit(bRaw$demand)) + 2, yref = "y"))
  )
plotToolBForecast


# Tool C
forecastDataC = head(cRaw, n = 25) %>% rename(train = demand) %>% 
  full_join(cRaw[25:nrow(cRaw),] %>% rename(actual = demand), by = c("date" = "date")) %>%
  full_join(select(c1MForecast, date, ensemble.pred) %>% rename(pred1m = ensemble.pred), by = c("date" = "date")) %>%
  full_join(select(c2MForecast, date, ensemble.pred) %>% rename(pred2m = ensemble.pred), by = c("date" = "date")) %>%
  full_join(select(c3MForecast, date, ensemble.pred) %>% rename(pred3m = ensemble.pred), by = c("date" = "date"))

# Interpolate points for smooth ploting
forecastDataC[25,4] <- forecastDataC[25,3]
forecastDataC[26,5] <- forecastDataC[26,3]
forecastDataC[27,6] <- forecastDataC[27,3]

plotToolCForecast <- plot_ly(data = forecastDataC, x=~date) %>%
  add_lines(y = ~train, name = 'training window', line = list(color = "rgb(31, 119, 180)", width = 4, dash = 'dashdot')) %>%
  add_lines(y = ~actual, name = 'actual demand', line = list(color = "rgb(31, 119, 180)", width = 4, dash = 'solid')) %>%
  add_lines(y = ~pred3m, name = '3m forecast', line = list(color = "rgb(214, 39, 40)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~pred2m, name = '2m forecast', line = list(color = "rgb(255, 127, 14)", width = 2, dash = 'dot')) %>%
  add_lines(y = ~pred1m, name = '1m forecast', line = list(color = "rgb(44, 160, 44)", width = 2, dash = 'dot')) %>%
  layout(xaxis = list(
    title = "",
    hoverformat = "%Y-%m",
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
    title = "demand",
    showgrid = FALSE,
    tickmode = "auto",
    ticks = "outside",
    rangemode = "tozero"
  ),
  shapes = list(
    list(type = "rect",
         fillcolor = "grey", line = list(color = "grey"), opacity = 0.1,
         x0 = forecastDataC$date[1], x1 = forecastDataC$date[25], xref = "x",
         y0 = 0, y1 = max(na.omit(cRaw$demand)) + 2, yref = "y"))
  )
plotToolCForecast

# plotly_POST(plotToolAForecast, filename = "toolAForecast")
# plotly_POST(plotToolBForecast, filename = "toolBForecast")
# plotly_POST(plotToolCForecast, filename = "toolCForecast")