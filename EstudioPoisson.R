## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
reticulate::use_python("/Users/juangabriel/anaconda3/bin/python")
#reticulate::py_install("sympy")


## ------------------------------------------------------------------------
covid19=read.csv("covid_19_clean_complete.csv")


## ------------------------------------------------------------------------
str(covid19)


## ------------------------------------------------------------------------
library(wbstats)
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2019)


## ------------------------------------------------------------------------
pop_data[pop_data$country=="Spain",]
pop_data[pop_data$country=="Italy",]
pop_data[pop_data$country=="France",]


## ------------------------------------------------------------------------
paises=unique(covid19$Country.Region)
covid19.limpio =c()
for (i in 1:length(paises)){
  if(length(which(paises[i] %in% pop_data$country))>0){
    covid19.limpio = rbind(covid19.limpio,covid19[covid19$Country.Region==paises[i],])
  }
}



## ------------------------------------------------------------------------
covid19.limpio$Date=as.Date(as.character(covid19.limpio$Date),"%m/%d/%Y")
infectados.totales.por.dia = aggregate(covid19.limpio$Confirmed ~ 
                    covid19.limpio$Date,FUN=sum)
fallecidos.totales.por.dia = aggregate(covid19.limpio$Deaths ~ 
                    covid19.limpio$Date,FUN=sum)
recuperados.totales.por.dia = aggregate(covid19.limpio$Recovered ~ 
                    covid19.limpio$Date,FUN=sum)
tabla.totales = data.frame(infectados.totales.por.dia[,1],infectados.totales.por.dia[,2],
              fallecidos.totales.por.dia[,2],recuperados.totales.por.dia[,2])
names(tabla.totales) = c("Fecha", "Infectados", "Fallecidos", "Recuperados")


## ------------------------------------------------------------------------
head(tabla.totales,10)


## ----eval=FALSE----------------------------------------------------------
## library(ggplot2)
## x=tabla.totales[,1]
## ggplot(tabla.totales, aes(x)) +
##   geom_line(aes(y=tabla.totales$Infectados, colour="Infectados")) +
##   geom_line(aes(y=tabla.totales$Fallecidos, colour="Fallecidos")) +
##   geom_line(aes(y=tabla.totales$Recuperados, colour="Recuperados")) +
##   xlab("Fecha") + ylab("Frecuencias") +
##   scale_color_manual(values=c("red", "blue", "green"))


## ----echo=FALSE----------------------------------------------------------
library(ggplot2)
x=tabla.totales[,1]
ggplot(tabla.totales, aes(x)) +      
  geom_line(aes(y=tabla.totales$Recuperados, colour="Recuperados")) + 
  geom_line(aes(y=tabla.totales$Fallecidos, colour="Fallecidos")) + 
  geom_line(aes(y=tabla.totales$Infectados, colour="Infectados")) + 
  xlab("Fecha") + ylab("Frecuencias") +
  scale_color_manual(values=c("red", "blue", "green"))


## ------------------------------------------------------------------------
fecha="0020-03-15"


## ------------------------------------------------------------------------
confirmados.por.pais = aggregate(covid19.limpio$Confirmed[covid19.limpio$Date==fecha] ~ 
                    covid19.limpio$Country.Region[covid19.limpio$Date==fecha],FUN=sum)
names(confirmados.por.pais)=c("Pais","Confirmados")


## ------------------------------------------------------------------------
head(confirmados.por.pais,10)


## ------------------------------------------------------------------------
paises=unique(covid19.limpio$Country.Region)
suma.total.habitantes=sum(pop_data[pop_data$country %in% paises,]$value)
número.total.infectados = sum(confirmados.por.pais$Confirmados)


## ------------------------------------------------------------------------
tabla.infectados.paises =c()

for (i in 1:length(paises)){
    habitantes=pop_data[pop_data$country==paises[i],]$value
    confirmados = confirmados.por.pais$Confirmados[confirmados.por.pais$Pais==paises[i]]
    confirmados.estimados = número.total.infectados*habitantes/suma.total.habitantes
    tabla.infectados.paises=rbind(tabla.infectados.paises,
                                  c(confirmados,confirmados.estimados))
}
tabla.infectados.paises=as.data.frame(tabla.infectados.paises)
tabla.infectados.paises = data.frame(paises,tabla.infectados.paises)
names(tabla.infectados.paises)=c("pais","infectados","infectados.estimados")


## ------------------------------------------------------------------------
chisq.test(tabla.infectados.paises$infectados,
  p=tabla.infectados.paises$infectados.estimados/sum(tabla.infectados.paises$infectados))


## ------------------------------------------------------------------------
paises.con.problemas = which(tabla.infectados.paises$infectados.estimados < 5)
paises[paises.con.problemas]


## ------------------------------------------------------------------------
tabla.infectados.paises2 = tabla.infectados.paises[-paises.con.problemas,]


## ------------------------------------------------------------------------
pais.añadir = data.frame("problemas",sum(tabla.infectados.paises[
  tabla.infectados.paises$pais%in% paises[paises.con.problemas],]
  $infectados),sum(tabla.infectados.paises[tabla.infectados.paises$
  pais %in% paises[paises.con.problemas],]$infectados.estimados))
names(pais.añadir)=names(tabla.infectados.paises2)
pais.añadir


## ------------------------------------------------------------------------
tabla.infectados.paises2 = rbind(tabla.infectados.paises2,pais.añadir)

chisq.test(tabla.infectados.paises2$infectados,
  p=tabla.infectados.paises2$infectados.estimados/sum(tabla.infectados.paises2$infectados))



## ------------------------------------------------------------------------
fecha="0020-03-30"
confirmados.por.pais = aggregate(covid19.limpio$Confirmed[covid19.limpio$Date==fecha] ~ 
                    covid19.limpio$Country.Region[covid19.limpio$Date==fecha],FUN=sum)
names(confirmados.por.pais)=c("Pais","Confirmados")
número.total.infectados = sum(covid19.limpio[covid19.limpio$Date==fecha,]$Confirmed)


## ------------------------------------------------------------------------
tabla.infectados.paises =c()

for (i in 1:length(paises)){
    habitantes=pop_data[pop_data$country==paises[i],]$value
    confirmados = confirmados.por.pais$Confirmados[confirmados.por.pais$Pais==paises[i]]
    confirmados.estimados = número.total.infectados*habitantes/suma.total.habitantes
    tabla.infectados.paises=rbind(tabla.infectados.paises,
                                  c(confirmados,confirmados.estimados))
}
tabla.infectados.paises=as.data.frame(tabla.infectados.paises)
tabla.infectados.paises = data.frame(paises,tabla.infectados.paises)
names(tabla.infectados.paises)=c("pais","infectados","infectados.estimados")




## ------------------------------------------------------------------------
paises.con.problemas = which(tabla.infectados.paises$infectados.estimados < 5)
tabla.infectados.paises2 = tabla.infectados.paises[-paises.con.problemas,]
pais.añadir = data.frame("problemas",sum(tabla.infectados.paises[
  tabla.infectados.paises$pais%in% paises[paises.con.problemas],]
  $infectados),sum(tabla.infectados.paises[tabla.infectados.paises$
  pais %in% paises[paises.con.problemas],]$infectados.estimados))
names(pais.añadir)=names(tabla.infectados.paises2)
tabla.infectados.paises2 = rbind(tabla.infectados.paises2,pais.añadir)

chisq.test(tabla.infectados.paises2$infectados,
  p=tabla.infectados.paises2$infectados.estimados/sum(tabla.infectados.paises2$infectados))

#-----------------------------------
