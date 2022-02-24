#Import Library
library(lubridate)
library(tidyverse)
library(astsa)
library(forecast)
library(readxl)
library(urca)
library(ggfortify)
library(tsutils)
library(highcharter)

#import data
durhamData <- read_excel(
  "Y:/Official Stats/WeatherDataFormatted.xlsx",
  sheet = "Durham1880to2021"
) %>%
  data.frame() %>%
  filter(yyyy > 1970)

#Converting To Time Series
durham_min_temp_ts <- ts(data = durhamData$tmin.in.degC, frequency = 12, start = c(1970,1))
#Selecting Data 
durham_min_temp_ts

#Plot Time Series Data
autoplot(durham_min_temp_ts) + ylab("Max temp (degC)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', breaks = '1 year', minor_breaks = '2 month') +
  theme_bw() + ggtitle("Durham min Temperatures 1970 - 2021")

#Decomposition using stl()
decomp <- stl(durham_min_temp_ts, s.window = 'periodic')
#Plot decomposition
autoplot(decomp) + theme_bw() + scale_x_date(date_labels = '%b - %Y', breaks = '1 year', minor_breaks = '2 month') +
  ggtitle("Remainder")


#seasonal plot
seasonplot(durham_min_temp_ts, year.labels = TRUE, col = 1:13, 
           main =  "Seasonal Plot", ylab= "Min Temperature (DegC)")

#Seasonal Sub-Series Plot
seasplot(durham_min_temp_ts, outplot = 3, trend = FALSE, 
         main = "Seasonal Subseries Plot", ylab= "Min Temperature (DegC)")

#Seasonal Boxplot
seasplot(durham_min_temp_ts, outplot = 2, trend = FALSE, 
         main = "Seasonal Box Plot", ylab= "Min Temperature (DegC)")

#Create Train Set
durham_train <- window(durham_min_temp_ts, end = c(2015,12))
#Create Test Set 
durham_test <- window(durham_min_temp_ts, start = c(2016,1))


acf2(durham_train)

## test some fits
fit1 <- Arima(durham_train, order = c(2,0,2), seasonal = c(2,0,2))
fit2 <- Arima(durham_train, order = c(0,0,2), seasonal = c(0,0,2))
fit3 <- Arima(durham_train, order = c(2,0,2), seasonal = c(1,0,2))
fit4 <- Arima(durham_train, order = c(0,0,1), seasonal = c(1,0,2))

#choose which model to use - lowest AICc indicates best fit
data.frame('Model-1' = fit1$aicc, 
           'Model-2' = fit2$aicc, 
           'Model-3' =  fit3$aicc,
           'Model-4' =  fit4$aicc,
           row.names =   "AICc Value")

#fit1 wins!
checkresiduals(fit1)

#Modifying Data For ggplot
model_1 <- forecast(fit1, h=34) 

model_1 <- data.frame(x=as.matrix(model_1$mean))
durham_train_df <- data.frame(x=as.matrix(durham_train))

model_1_plot <- rbind(durham_train_df,model_1)

model_1_plot <- model_1_plot %>% 
  mutate('Date' = seq(from = as.Date("1970-01-01", '%Y-%m-%d'), to = as.Date("2020-10-31",'%Y-%m-%d'),by = 'month'))

durham_ts_df <- data.frame(x=as.matrix(durham_min_temp_ts))

durham_ts_df <- durham_ts_df %>% 
  mutate('Date' = seq(from = as.Date("1970-01-01", '%Y-%m-%d'), to = as.Date("2020-10-31",'%Y-%m-%d'),by = 'month'))

durham_train_df <- durham_train_df %>% 
  mutate('Date' = seq(from = as.Date("1970-01-01", '%Y-%m-%d'), to = as.Date("2017-12-31",'%Y-%m-%d'),by = 'month'))

colors <- c("ARIMA Model Forecast 2018" = "blue", "Actual Data" = "black")


#Creating Plot
ggplot() + geom_line(model_1_plot,
                     mapping = aes(x=Date, y=x, 
                                   color= "ARIMA Model Forecast 2018"),lty = 2) +
  geom_line(durham_ts_df,mapping = aes(x=Date, y=x, 
                                      color= "Actual Data"), lty = 1, show.legend = TRUE) +
  ylab("Max Temp (degC)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', breaks = '1 year', 
               minor_breaks = '2 month') +
  theme_bw() + ggtitle("Durham Rainfall 1970 - 2020") + 
  scale_color_manual(values=colors)


#check model accuracy
accuracy(forecast(fit1, h=34), durham_test)

### apply models to full data 

#Create Model
ARIMA_Model <- Arima(durham_min_temp_ts, order = c(1,0,2), seasonal = c(1,0,2))

#ARIMA Model Forecast
autoplot(forecast(ARIMA_Model, h=12)) + theme_bw() + 
  ylab("Max temp (degC)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', 
               breaks = '1 year', minor_breaks = '2 month') +
  theme_bw() + ggtitle("Durham Rainfall Forecast Nov 2020 - Oct 2022 
  ARIMA Model")


####create highchart

#create date frame for latest 24 months
chart_data_actual <- durhamData %>%
  select(
  yyyy,
  mm,
  tmin.in.degC
  ) %>%
  tail(24) %>%
  mutate(
    month = make_date(
      yyyy,
      mm,
      "1"
    )
  )

forecast <- forecast(ARIMA_Model, h=12, level = c(95, 99))

chart_data_forecast <- data.frame(
  mean = as.matrix(forecast$mean),
  min = as.matrix(forecast$lower),
  max = as.matrix(forecast$upper),
  month = seq(from = as.Date("2021-11-01", '%Y-%m-%d'), to = as.Date("2022-10-31",'%Y-%m-%d'),by = 'month')
)

#create mock row to add to forecast so chart joins up
mock_row <- data.frame(
  mean = chart_data_actual$tmin.in.degC[24],
  min.95. = chart_data_actual$tmin.in.degC[24],
  min.99. = chart_data_actual$tmin.in.degC[24],
  max.95. = chart_data_actual$tmin.in.degC[24],
  max.99. = chart_data_actual$tmin.in.degC[24],
  month = as.Date("2021-10-01", '%Y-%m-%d')
)

chart_data_forecast <- chart_data_forecast %>%
  bind_rows(mock_row) %>%
  arrange(desc(month))

highchart() %>% 
  hc_chart(style = list(fontFamily = "Arial")) %>% 
  hc_add_series(data = chart_data_forecast,
                name = "99% prediction interval",
                type = "arearange",
                lineWidth = 0,
                color = rgb(66,85,99, alpha = 50, maxColorValue = 255),
                marker = list(enabled = FALSE),
                dataLabels = list(enabled = FALSE),
                # enableMouseTracking = FALSE,
                hcaes(x = month,
                      high = signif(max.99.,3),
                      low = signif(min.99.,3))) %>% 
  hc_add_series(data = chart_data_forecast,
                name = "95% prediction interval",
                type = "arearange",
                lineWidth = 0,
                color = rgb(66,85,99, alpha = 90, maxColorValue = 255),
                marker = list(enabled = FALSE),
                dataLabels = list(enabled = FALSE),
                hcaes(x = month,
                      high = signif(max.95.,3),
                      low = signif(min.95.,3))) %>% 
  hc_add_series(data = chart_data_forecast,
                name = "Expected min temp (deg C)",
                type = "line",
                dashStyle = "Dash",
                color = "#231f20",
                marker = list(enabled = FALSE),
                dataLabels = list(enabled = FALSE),
                hcaes(x = month,
                      y = signif(mean,3))) %>%
  hc_add_series(data = chart_data_actual,
                name = "Min temp (deg C)",
                type = "line",
                lineWidth = 3,
                color = "#005EB8",
                marker = list(enabled = FALSE),
                dataLabels = list(enabled = FALSE),
                hcaes(x = month,
                      y = signif(tmin.in.degC,3))) %>% 
  hc_xAxis(type = "datetime",
           dateTimeLabelFormats = list(month = "%b %y"),
           title = list(text = "Month")) %>% 
  hc_yAxis(title = list(text = "Temperatue (deg C)")) %>% 
  hc_title(text = "Predicted min temp (deg C) Nov 2021 to Oct 2022",
           style = list(fontSize = "16px",
                        fontWeight = "bold")) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_tooltip(enabled = TRUE,
             shared = TRUE,
             sort = TRUE) %>% 
  hc_credits(enabled = TRUE) %>% 
  hc_plotOptions(arearange = list(states = list(hover = list(enabled = FALSE))))

