options(scipen = 999)

library(openxlsx)
library(data.table)
library(lubridate)
library(ggplot2)
library(forecast)
library(stringr)
library(RColorBrewer)
library(gridExtra)
library(TSA)



DT <- data.table()

for (i in 2011:2019) {
  
  t <- read.xlsx("Data/MortalityDataEngandWales_2011_2021.xlsx", sheet = paste0("TransposedData",i), cols = c(2, 17:32)) |> as.data.table()
  
  DT <- rbind(DT, t)
  
  rm(t)
  
}

setnames(DT, 1, "WE")
DT[, WE2 := dmy(WE)]
DT[is.na(WE2), WE2 := as.Date(as.numeric(WE), origin = as.Date("1899-12-30"))]

DT[, c(1,9,10) := NULL]

setnames(DT, 15, "WE")

setnames(DT, 1:7, paste0("M", names(DT)[1:7]))
setnames(DT, 8:14, paste0("F", names(DT)[8:14]))

DT <- melt(DT, id=15)

setnames(DT, 2, "AG")

DT[, Gender := str_extract(AG, "^.")]
DT[, AG := str_remove(AG, "^.")]

DT[, Gender := factor(Gender, levels = c("M","F"))]

DT[, AG := factor(AG, levels = c("Under.1.year", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"),
                                  labels = c("<01", "01-14", "15-44", "45-64", "65-74", "75-84", "85+"))]

colm <- colorRampPalette(brewer.pal(9, "Blues"))(10)[4:10]
colf <- colorRampPalette(brewer.pal(9, "OrRd"))(10)[4:10]


ggplot(DT, aes(WE, value)) +
  geom_line(aes(color=paste(Gender, AG)))  +
  facet_wrap(~Gender) + 
  scale_color_manual("Gender/AG", values = c(colf, colm)) +
  theme(axis.title.x = element_blank())

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

DT[, value_norm := normalize(value), by=.(Gender,AG)]

ggplot(DT, aes(WE, value_norm)) +
  geom_line(aes(color=paste(Gender, AG)))  +
  facet_wrap(~Gender) + 
  scale_color_manual("Gender/AG", values = c(colf, colm)) +
  theme(axis.title.x = element_blank())

ggplot(DT, aes(WE, value_norm)) +
  geom_line(aes(color=paste(Gender, AG)))  +
  facet_grid(AG~Gender) + 
  scale_color_manual("Gender/AG", values = c(colf, colm)) +
  theme(axis.title.x = element_blank(),
        legend.position = 'none')


DTm <- DT[Gender=="M"] |> dcast(WE~AG, value.var = "value_norm")


Rm <- data.table()

for (j in names(DTm)[2:8]) {
  
  for (i in names(DTm)[2:8]) {
    
    r = cor.test(DTm[, get(i)], DTm[, get(j)])$estimate
    
    Rm <- rbind(Rm, data.table(x = i, y = j, r = r))
    
  }
  
}

Rm <- Rm[x > y]

ggplot(Rm, aes(x, y, fill = r)) +
  geom_tile() +
  geom_text(aes(label = broman::myround(r,2)), color = "white", size = 4) +
  coord_fixed() +
  scale_fill_gradient(low = colm[1], high = colm[7]) +
  theme(axis.title = element_blank(),
        panel.background = element_blank()) -> gm


DTf <- DT[Gender=="F"] |> dcast(WE~AG, value.var = "value_norm")


Rf <- data.table()

for (j in names(DTf)[2:8]) {
  
  for (i in names(DTf)[2:8]) {
    
    r = cor.test(DTf[, get(i)], DTf[, get(j)])$estimate
    
    Rf <- rbind(Rf, data.table(x = i, y = j, r = r))
    
  }
  
}

Rf <- Rf[x > y]

ggplot(Rf, aes(x, y, fill = r)) +
  geom_tile() +
  geom_text(aes(label = broman::myround(r,2)), color = "white", size = 4) +
  coord_fixed() +
  scale_fill_gradient(low = colf[1], high = colf[7]) +
  theme(axis.title = element_blank(),
        panel.background = element_blank()) -> gf

grid.arrange(gm, gf, nrow = 1)



# CCF coefs before and after differencing/pre-whitening

ccf(DTm[, `75-84`], DTm[, `85+`], main="M:75-84 vs M:85+", ylab="CCF")
prewhiten(diff(diff(DTm[, `75-84`],52)), diff(diff(DTm[, `85+`],52)), main="M:75-84 vs M:85+", ylab="CCF")

#t$ccf$acf
#t$ccf$acf[23] #correlation at lag 0

Rm <- data.table()

for (j in names(DTm)[2:8]) {
  
  for (i in names(DTm)[2:8]) {
    
    ccf <- prewhiten(diff(diff(DTm[, get(i)],52)), diff(diff(DTm[, get(j)],52)))$ccf
    
    dt <- data.table(ccf = as.numeric(ccf$acf),lag = as.numeric(ccf$lag))
    
    r_0 = dt[lag==0, ccf]
    
    r_max = prewhiten(diff(diff(DTm[, get(i)],52)), diff(diff(DTm[, get(j)],52)))$ccf$acf |> max()
    
    Rm <- rbind(Rm, data.table(x = i, y = j, r_0 = r_0, r_max = r_max))
    
  }
  
}

ggplot(Rm, aes(x, y, fill = r_0)) +
  geom_tile() +
  geom_text(aes(label = broman::myround(r_0,2)), color = "white", size = 4) +
  coord_fixed() +
  scale_fill_gradient(low = colm[1], high = colm[7]) +
  theme(axis.title = element_blank(),
        panel.background = element_blank()) -> m1

ggplot(Rm, aes(x, y, fill = r_max)) +
  geom_tile() +
  geom_text(aes(label = broman::myround(r_max,2)), color = "white", size = 4) +
  coord_fixed() +
  scale_fill_gradient(low = colm[1], high = colm[7]) +
  theme(axis.title = element_blank(),
        panel.background = element_blank()) -> m2

grid.arrange(m1, m2, nrow = 1)



Rf <- data.table()

for (j in names(DTf)[2:8]) {
  
  for (i in names(DTf)[2:8]) {
    
    ccf <- prewhiten(diff(diff(DTf[, get(i)],52)), diff(diff(DTf[, get(j)],52)))$ccf
    
    dt <- data.table(ccf = as.numeric(ccf$acf),lag = as.numeric(ccf$lag))
    
    r_0 = dt[lag==0, ccf]
    
    r_max = prewhiten(diff(diff(DTf[, get(i)],52)), diff(diff(DTf[, get(j)],52)))$ccf$acf |> max()
    
    Rf <- rbind(Rf, data.table(x = i, y = j, r_0 = r_0, r_max = r_max))
    
  }
  
}

ggplot(Rf, aes(x, y, fill = r_0)) +
  geom_tile() +
  geom_text(aes(label = broman::myround(r_0,2)), color = "white", size = 4) +
  coord_fixed() +
  scale_fill_gradient(low = colf[1], high = colf[7]) +
  theme(axis.title = element_blank(),
        panel.background = element_blank()) -> f1

ggplot(Rf, aes(x, y, fill = r_max)) +
  geom_tile() +
  geom_text(aes(label = broman::myround(r_max,2)), color = "white", size = 4) +
  coord_fixed() +
  scale_fill_gradient(low = colf[1], high = colf[7]) +
  theme(axis.title = element_blank(),
        panel.background = element_blank()) -> f2

grid.arrange(f1, f2, nrow = 1)

# 2 points:
# matrices are not symmetrical because prewhitening fits a ts model to one series (so it does matters to which) and applies it to both series
# 2nd matrix shows max correlation at any lag: here, the only differences are in low-r regions and are negligible
