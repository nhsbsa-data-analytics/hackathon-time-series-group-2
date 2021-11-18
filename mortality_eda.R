options(scipen = 999)

library(openxlsx)
library(data.table)
library(lubridate)
library(ggplot2)
library(forecast)

DT <- read.xlsx("Data/MortalityDataEngandWales_2011_2021.xlsx", sheet = "TotalDeaths2011to2020") |> as.data.table()
setnames(DT, 1:2, c("WE","Deaths"))

DT[, WE := as.Date(WE, origin = as.Date("1899-12-30"))]

 
# The first confirmed cases of coronavirus in the UK were on January 29 [2020],
# when two Chinese nationals fell ill at the Staycity Aparthotel in York.

plot(TS)

ggplot(DT, aes(WE, Deaths)) + geom_line() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = \(x) x/1e3) +
  geom_vline(xintercept = ymd("2020-01-29"), color="firebrick", lty=2) +
  geom_vline(xintercept = ymd("2017-01-20"), color="royalblue", lty=2) + 
  ylab("Deaths (thousands)") +
  theme(axis.title.x = element_blank())

#2/3 of observations of the covid-free period
tr <- round(DT[WE < ymd("2020-01-29"), .N] * 2/3)
TR <- DT[1:tr]
TS <- DT[(tr+1):DT[WE < ymd("2020-01-29"), .N]]
rm(tr)

TR.ts <- ts(TR$Deaths, freq=365.25/7, start = decimal_date(ymd("2011-01-07")))
TS.ts <- ts(TS$Deaths, freq=365.25/7, start = decimal_date(ymd("2017-01-20")))

m1 <- auto.arima(TR.ts)

ggtsdisplay(TR.ts)
diff(TR.ts, lag=52) |> ggtsdisplay()
diff(TR.ts, lag=52) |> ndiffs()



R <- data.table()
start <- Sys.time()
max_level = 5

for (s_ma in 0:max_level) {
  
  for (s_ar in 0:max_level) {
    
    for (ns_ma in 0:max_level) {
      
      for (ns_ar in 0:max_level) {
        
        m <- arima(TR.ts, order = c(ns_ar,0,ns_ma), seasonal = c(s_ar,1,s_ma)) 
        
        RSS = sum((TS[, Deaths] - forecast(m, h=158)$mean)^2)
        
        R <- rbind(R, data.table(Model = paste0("ARIMA(",ns_ar,",0,",ns_ma,")(",s_ar,",1,",s_ma,")" ),
                                   RSS = RSS))  
        rm(m)
        
      }
      
    }
    
  }
  
}

Sys.time() - start #2 min

R[order(RSS)]

m <- arima(TR.ts, order = c(4,0,2), seasonal = c(0,1,0)) 

fit <- forecast(m, h=158)

TS[, `:=`(F1 = fit$mean,
          F1_Lo = fit$lower[,2],
          F1_Hi = fit$upper[,2])]


ggplot(DT, aes(WE, Deaths)) + geom_line() +
  geom_line(aes(y=F1), data = TS, color="royalblue") + 
  geom_ribbon(aes(ymin = F1_Lo, ymax=F1_Hi), data=TS, alpha=.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = \(x) x/1e3) +
  geom_vline(xintercept = ymd("2020-01-29"), color="firebrick", lty=2) +
  geom_vline(xintercept = ymd("2017-01-20"), color="blue", lty=2) + 
  ylab("Deaths (thousands)") +
  theme(axis.title.x = element_blank())


TRTS <- ts(DT[WE < ymd("2020-01-29"), Deaths], freq=365.25/7, start = decimal_date(ymd("2011-01-07")))   #Potentially useful: https://stackoverflow.com/questions/22188660/r-time-series-modeling-on-weekly-data-using-ts-object
m <- arima(TRTS, order = c(4,0,2), seasonal = c(0,1,0)) 

fit <- forecast(m, h=89)


COV <- copy(DT[WE >= ymd("2020-01-29")])

COV[, `:=`(F2 = fit$mean,
           F2_Lo = fit$lower[,2],
           F2_Hi = fit$upper[,2])]


ggplot(DT, aes(WE, Deaths)) + geom_line() +
  #geom_line(aes(y=F1), data = TS, color="royalblue") + 
  #geom_ribbon(aes(ymin = F1_Lo, ymax=F1_Hi), data=TS, alpha=.2) +
  geom_line(aes(y=F2), data = COV, color="firebrick") + 
  geom_ribbon(aes(ymin = F2_Lo, ymax=F2_Hi), data=COV, alpha=.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = \(x) x/1e3) +
  geom_vline(xintercept = ymd("2020-01-29"), color="firebrick", lty=2) +
  geom_vline(xintercept = ymd("2017-01-20"), color="blue", lty=2) + 
  ylab("Deaths (thousands)") +
  theme(axis.title.x = element_blank())

#Excess deaths
COV[, sum(Deaths-F2)]

