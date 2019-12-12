#' ## Assignment Set 3: Lesson 10
#'
#' Add R Code under the questions.
#' If you are done, 'knit' the document, commit and push to GitHub.
#' You can use GitHub Pages if you like, add links to README.md
#' to make it easier to find the pretty version.
#'
#' 1. Estimate an automated ARIMA model of Swiss GDP.
#'
library(tidyverse)
library(rdbnomics)
library(forecast)
library(tsbox)
library(seasonal)
library(quantmod)
library(prophet)
library(prophet)

swiss_gdp <-
  rdb(ids = c(
    "Eurostat/namq_10_gdp/Q.CLV05_MEUR.NSA.B1G.CH"
  )) %>%
  select(period, value) %>%
  ts_ts()

m_arima <- auto.arima(swiss_gdp)

#' 2. Inspect the model: What is the degree of seasonal and non-seasonal
#' differentiation, what are the seasonal and non-seasonal AR and MA orders?
#'
m_arima
seas(swiss_gdp)


#' 3. Re-estimate 1, using R base `arima()` function
#'
base_arima <- arima(swiss_gdp)
base_arima


#' 4. Seasonally adjust Swiss GDP. Compare the ARIMA model with the one from
#' `auto.arima`.
#'
m_arima_adj <- auto.arima(swiss_gdp, seasonal = F)


#' 5. Download your stock title of choice. Use prophet to forecast the series.
#'
nestle <-
  getSymbols("NESN.SW", src = "yahoo", auto.assign = FALSE) %>%
  ts_tbl() %>%
  filter(id == "NESN.SW.Close")

ts_plot(nestle)

nestle <- nestle[,-1]
colnames(nestle) <- c("ds","y")

forecast_nestle <- prophet(nestle)

future <- make_future_dataframe(forecast_nestle, periods = 365)
forecast <- predict(forecast_nestle, future)

plot(forecast_nestle, forecast)





