library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(shiny)
library(rsconnect)
library(DT)
library(devtools)
library(lattice)
library(latticeExtra)
library(TSA)
library(tseries)
library(pander)
library(zoo)
library(xts)
library(caret)
library(forecast) # forecasting time series ARIMA 
library(prophet) # forecasting time series
library(highcharter) #Interactive Plot

economics

data <- economics %>% dplyr::select(date, unemploy)
data

extended_data <- data %>% 
  rbind(tibble::tibble(date = seq.Date(from = as.Date("2015-05-01"),
                                  by = "month", length.out = 12), 
                       unemploy = rep(NA, 12)))
tail(extended_data)




