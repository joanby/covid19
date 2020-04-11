covid19=read.csv("covid_19_clean_complete.csv")
covid19$Date=as.Date(as.character(covid19$Date),"%m/%d/%Y")
infectados.totales.por.dia = aggregate(covid19$Confirmed ~ 
                                         covid19$Date,FUN=sum)
fallecidos.totales.por.dia = aggregate(covid19$Deaths ~ 
                                         covid19$Date,FUN=sum)
recuperados.totales.por.dia = aggregate(covid19$Recovered ~ 
                                          covid19$Date,FUN=sum)
tabla.totales = data.frame(infectados.totales.por.dia[,1],infectados.totales.por.dia[,2],
                           fallecidos.totales.por.dia[,2],recuperados.totales.por.dia[,2])
names(tabla.totales) = c("Fecha", "Infectados", "Fallecidos", "Recuperados")


library(ggplot2)
x=tabla.totales[,1]
ggplot(tabla.totales, aes(x)) +      
  geom_line(aes(y=tabla.totales$Infectados, colour="Infectados")) + 
  geom_line(aes(y=tabla.totales$Fallecidos, colour="Fallecidos")) + 
  geom_line(aes(y=tabla.totales$Recuperados, colour="Recuperados")) + 
  xlab("Fecha") + ylab("Frecuencias") +
  scale_color_manual(values=c("red", "blue", "green"))

serie.temporal.infectados = ts(tabla.totales$Infectados,frequency = 1,start=c(1,22))
serie.temporal.infectados
plot.ts(serie.temporal.infectados)
?ts


# Decomposing Time Series

# Suposing Non-Seasonal Data
# Trend component suposing addicional model
library(TTR)
componente.trend = SMA(serie.temporal.infectados,n=4) # moving averages of n=4
plot.ts(componente.trend)


# Forecasts
# Simple Exponential Smoothing
forecast.infectados = HoltWinters(serie.temporal.infectados,gamma=FALSE)
forecast.infectados$fitted
plot(forecast.infectados)
forecast.infectados$SSE

# further time point
library(forecast)
forecast.infectados2 = forecast:::forecast.HoltWinters(forecast.infectados,h=10)
forecast:::plot.forecast(forecast.infectados2)

acf(forecast.infectados2$residuals[3:75],lag.max=20)
Box.test(forecast.infectados2$residuals,lag=20,type="Ljung-Box") # no evidence of non-zero autocorrelation
plot.ts(forecast.infectados2$residuals)
shapiro.test(forecast.infectados2$residuals)
