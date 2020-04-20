## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
reticulate::use_python("/Users/juangabriel/anaconda3/bin/python")
#reticulate::py_install("sympy")


## ------------------------------------------------------------------------
covid19=read.csv("covid_19_clean_complete.csv")
covid19$Date=as.Date(as.character(covid19$Date),"%m/%d/%Y")


## ------------------------------------------------------------------------
str(covid19)


## ------------------------------------------------------------------------
covid19.España = covid19[covid19$Country.Region=="Spain",]


## ------------------------------------------------------------------------
infectados.por.dia = aggregate(covid19.España$Confirmed ~ covid19.España$Date,FUN=sum)
fallecidos.por.dia = aggregate(covid19.España$Deaths ~ covid19.España$Date,FUN=sum)
recuperados.por.dia = aggregate(covid19.España$Recovered ~ covid19.España$Date,FUN=sum)


## ------------------------------------------------------------------------
tabla.España = data.frame(unique(covid19.España$Date),infectados.por.dia[,2],
                          fallecidos.por.dia[,2],recuperados.por.dia[,2])
names(tabla.España) = c("Fecha", "Infectados","Fallecidos", "Recuperados")


## ------------------------------------------------------------------------
head(tabla.España,10)


## ------------------------------------------------------------------------
infectados = ts(tabla.España$Infectados,frequency = 7,start=c(1,3))
infectados


## ----eval=FALSE----------------------------------------------------------
## plot.ts(infectados)


## ----echo=FALSE----------------------------------------------------------
plot.ts(infectados)
autoplot(infectados)


## ------------------------------------------------------------------------
components = decompose(infectados,type="multiplicative")


## ------------------------------------------------------------------------
components$seasonal


## ----eval=FALSE----------------------------------------------------------
## plot(components)


## ----echo=FALSE----------------------------------------------------------
plot(components)
autoplot(components)

## ----eval=FALSE----------------------------------------------------------
## infectados.ajustados = infectados-components$seasonal
## plot(infectados.ajustados)


## ----echo=FALSE----------------------------------------------------------
infectados.ajustados = infectados-components$seasonal
autoplot(infectados.ajustados)


## ----eval=FALSE----------------------------------------------------------
## (predicción.infectados=HoltWinters(infectados.ajustados,gamma=FALSE))


## ----echo=FALSE----------------------------------------------------------
(predicción.infectados=HoltWinters(infectados.ajustados,gamma=FALSE))


## ------------------------------------------------------------------------
predicción.infectados$SSE
sqrt(predicción.infectados$SSE)


## ----eval=FALSE----------------------------------------------------------
## plot(predicción.infectados)


## ----echo=FALSE----------------------------------------------------------
plot(predicción.infectados)


## ----message=FALSE-------------------------------------------------------
library(forecast)
(predicción.infectados.semana = forecast:::forecast.HoltWinters(predicción.infectados, 
                                                                h=7))


## ------------------------------------------------------------------------
forecast::autoplot(predicción.infectados.semana)


## ----eval=FALSE----------------------------------------------------------
## acf(predicción.infectados.semana$residuals[3:75], lag.max=7)


## ----echo=FALSE----------------------------------------------------------
ggAcf(predicción.infectados.semana$residuals[3:75], lag.max=7)


## ------------------------------------------------------------------------
Box.test(predicción.infectados.semana$residuals,lag=7,type="Ljung-Box")


## ------------------------------------------------------------------------
shapiro.test(predicción.infectados.semana$residuals)


## ------------------------------------------------------------------------
fallecidos = ts(tabla.España$Fallecidos,frequency = 7,start=c(1,3))
autoplot(fallecidos)


## ------------------------------------------------------------------------
components = decompose(fallecidos,type="additive")
components$seasonal


## ------------------------------------------------------------------------
autoplot(components)


## ------------------------------------------------------------------------
fallecidos.ajustados = fallecidos-components$seasonal
autoplot(fallecidos.ajustados)


## ------------------------------------------------------------------------
(predicción.fallecidos=HoltWinters(fallecidos.ajustados,gamma=FALSE))


## ------------------------------------------------------------------------
predicción.fallecidos$SSE
sqrt(predicción.fallecidos$SSE)


## ------------------------------------------------------------------------
plot(predicción.fallecidos)


## ----message=FALSE-------------------------------------------------------
(predicción.fallecidos.semana = forecast:::forecast.HoltWinters(predicción.fallecidos, 
                                                                h=7))


## ------------------------------------------------------------------------
forecast::autoplot(predicción.fallecidos.semana)


## ------------------------------------------------------------------------
ggAcf(predicción.fallecidos.semana$residuals[3:75], lag.max=7)


## ------------------------------------------------------------------------
Box.test(predicción.fallecidos.semana$residuals,lag=7,type="Ljung-Box")


## ------------------------------------------------------------------------
shapiro.test(predicción.fallecidos.semana$residuals)

