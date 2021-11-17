options(scipen = 999)

library(openxlsx)
library(data.table)
library(lubridate)
library(ggplot2)

DT <- read.xlsx("Data/MortalityDataEngandWales_2011_2021.xlsx", sheet = "TotalDeaths2011to2020") |> as.data.table()
setnames(DT, 1:2, c("WE","Deaths"))

DT[, WE := as.Date(WE, origin = as.Date("1899-12-30"))]

TS <- ts(DT$Deaths, freq=365.25/7, start = decimal_date(ymd("2011-01-07"))) #https://stackoverflow.com/questions/22188660/r-time-series-modeling-on-weekly-data-using-ts-object


# The first confirmed cases of coronavirus in the UK were on January 29 [2020],
# when two Chinese nationals fell ill at the Staycity Aparthotel in York

plot(TS)

ggplot(DT, aes(WE, Deaths)) + geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = \(x) x/1e3) +
  geom_vline(xintercept = ymd("2020-01-29"), color="red", lty=2) +
  ylab("Deaths (thousands)") +
  theme(axis.title.x = element_blank())

#2/3 of observations of the covid-free period
TR <- round(DT[WE < ymd("2020-01-29"), .N] * 2/3)

TR <- DT[1:TR]
