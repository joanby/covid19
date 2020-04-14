covid19=read.csv("covid_19_clean_complete.csv")
covid19$Date=as.Date(as.character(covid19$Date),"%m/%d/%Y")
covid19=covid19[covid19$Country.Region=="China",]
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

serie.temporal.infectados = ts(tabla.totales$Infectados,frequency = 7,start=c(1,22))
serie.temporal.infectados
plot.ts(serie.temporal.infectados)
?ts


# Decomposing Time Series

# Suposing Non-Seasonal Data
# Trend component suposing addicional model
library(TTR)
componente.trend = SMA(serie.temporal.infectados,n=7) # moving averages of n=7
plot.ts(componente.trend)

# Suposing Seasonal Data

components = decompose(serie.temporal.infectados)
components$seasonal
plot(components)
serie.temporal.infectados.adjusted = serie.temporal.infectados-components$seasonal
plot.ts(serie.temporal.infectados.adjusted)

# Forecasts
# Simple Exponential Smoothing
forecast.infectados = HoltWinters(serie.temporal.infectados.adjusted,gamma=FALSE)
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


#------------ESTUDIO FALLECIDOS---------------------
serie.temporal.fallecidos = ts(tabla.totales$Fallecidos,frequency = 7,start=c(1,22))
serie.temporal.fallecidos
plot.ts(serie.temporal.fallecidos)
?ts


# Decomposing Time Series

# Suposing Non-Seasonal Data
# Trend component suposing addicional model
library(TTR)
componente.trend = SMA(serie.temporal.fallecidos,n=7) # moving averages of n=7
plot.ts(componente.trend)

# Suposing Seasonal Data

components = decompose(serie.temporal.fallecidos)
components$seasonal
plot(components)
serie.temporal.fallecidos.adjusted = serie.temporal.fallecidos-components$seasonal
plot.ts(serie.temporal.infectados.adjusted)

# Forecasts
# Simple Exponential Smoothing
(forecast.fallecidos = HoltWinters(serie.temporal.fallecidos.adjusted,gamma=FALSE))
forecast.fallecidos$fitted
plot(forecast.fallecidos)
forecast.fallecidos$SSE

# further time point
library(forecast)
forecast.fallecidos2 = forecast:::forecast.HoltWinters(forecast.fallecidos,h=10)
forecast:::plot.forecast(forecast.fallecidos2)

acf(forecast.fallecidos2$residuals[3:75],lag.max=20)
Box.test(forecast.fallecidos2$residuals,lag=20,type="Ljung-Box") # no evidence of non-zero autocorrelation
plot.ts(forecast.fallecidos2$residuals)
shapiro.test(forecast.fallecidos2$residuals)

#-------------Estudio modelo SIR-------------------------
# Fijamos un pais: España
library(wbstats)
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2019)
covid19.España = covid19[covid19$Country.Region=="Spain",]
infectados.por.dia = aggregate(covid19.España$Confirmed ~ 
                                         covid19.España$Date,FUN=sum)
fallecidos.por.dia = aggregate(covid19.España$Deaths ~ 
                                         covid19.España$Date,FUN=sum)

infectados.por.dia2 = infectados.por.dia[,2] + fallecidos.por.dia[,2]
recuperados.por.dia = aggregate(covid19.España$Recovered ~ 
                                          covid19.España$Date,FUN=sum)
habitantes=pop_data$value[pop_data$country=="Spain"]
susceptibles.por.dia = habitantes-infectados.por.dia2-recuperados.por.dia[,2]
tabla.totales = data.frame(infectados.totales.por.dia[,1],susceptibles.por.dia,infectados.por.dia2,recuperados.por.dia[,2])
names(tabla.totales) = c("Fecha", "Susceptibles","Infectados", "Recuperados")
library(ggplot2)
x=tabla.totales[,1]
ggplot(tabla.totales, aes(x)) +      
  geom_line(aes(y=tabla.totales$Susceptibles/1000, colour="Susceptibles")) + 
  geom_line(aes(y=tabla.totales$Infectados, colour="Infectados")) + 
  geom_line(aes(y=tabla.totales$Recuperados, colour="Recuperados")) + 
  xlab("Fecha") + ylab("Frecuencias") +
  scale_color_manual(values=c("red", "blue", "green"))

plot(-tabla.totales$Recuperados,habitantes*log(tabla.totales$Susceptibles/habitantes))
x=-tabla.totales$Recuperados
y=habitantes*log(tabla.totales$Susceptibles)
summary(lm( y ~ x))

#--------------------------------China
# Fijamos un pais: China
library(wbstats)
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2019)
covid19.China = covid19[covid19$Country.Region=="China",]
infectados.por.dia = aggregate(covid19.China$Confirmed ~ 
                                 covid19.China$Date,FUN=sum)
fallecidos.por.dia = aggregate(covid19.China$Deaths ~ 
                                 covid19.China$Date,FUN=sum)

infectados.por.dia2 = infectados.por.dia[,2] + fallecidos.por.dia[,2]
recuperados.por.dia = aggregate(covid19.China$Recovered ~ 
                                  covid19.China$Date,FUN=sum)
habitantes=pop_data$value[pop_data$country=="China"]
susceptibles.por.dia = habitantes-infectados.por.dia2-recuperados.por.dia[,2]
tabla.totales = data.frame(infectados.totales.por.dia[,1],susceptibles.por.dia,infectados.por.dia2,recuperados.por.dia[,2])
names(tabla.totales) = c("Fecha", "Susceptibles","Infectados", "Recuperados")
library(ggplot2)
x=tabla.totales[,1]
ggplot(tabla.totales, aes(x)) +      
  geom_line(aes(y=tabla.totales$Susceptibles/1000, colour="Susceptibles")) + 
  geom_line(aes(y=tabla.totales$Infectados, colour="Infectados")) + 
  geom_line(aes(y=tabla.totales$Recuperados, colour="Recuperados")) + 
  xlab("Fecha") + ylab("Frecuencias") +
  scale_color_manual(values=c("red", "blue", "green"))

plot(-tabla.totales$Recuperados,habitantes*log(tabla.totales$Susceptibles/habitantes))
x=tabla.totales$Recuperados
y=habitantes*log(tabla.totales$Susceptibles)
summary(lm( y ~ x))
R0=-summary(lm( y ~ x))$coefficients[2]
exp(-R0*tabla.totales$Recuperados[75]/habitantes)
R0
