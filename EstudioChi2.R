covid19=read.csv("covid_19_clean_complete.csv")
covid19$Country.Region


paises=unique(covid19$Country.Region)


library(wbstats)
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2019)
pop_data


# llevam els paisos que no surten al wbstats
covid19.netejat =c()
for (i in 1:length(paises)){
  if(length(which(paises[i] %in% pop_data$country))>0){
    covid19.netejat = rbind(covid19.netejat,covid19[covid19$Country.Region==paises[i],])
  }
}






fecha="4/5/20"
confirmados.por.pais = aggregate(covid19.netejat$Confirmed[covid19.netejat$Date==fecha] ~ covid19.netejat$Country.Region[covid19.netejat$Date==fecha],
                                 FUN=sum)
names(confirmados.por.pais)=c("Pais","Confirmados")



paises=unique(covid19.netejat$Country.Region)
suma.total.habitantes=sum(pop_data[pop_data$country %in% paises,]$value)
infectados.totales = sum(covid19.netejat[covid19.netejat$Date==fecha,]$Confirmed)
estimación.lambda = infectados.totales/suma.total.habitantes 
# lambda es el número de infectados por cada 1000 habitantes

# Sea X la variable aleatoria que nos da el número de infectados por cada 1000 habitantes.
# Suponemos que X sigue la distribución de Poisson.
# Así si un país tiene n habitantes i el número de infectados sigue una distribución de Poisson, el 
# número de infectados estimado del pais seria lambda*n/1000, esperanza o valor medio de infectados del pais.

tabla.infectados.paises =c()

for (i in 1:length(paises)){
    habitantes=pop_data[pop_data$country==paises[i],]$value
    confirmados = confirmados.por.pais$Confirmados[confirmados.por.pais$Pais==paises[i]]
    confirmados.estimados = estimación.lambda*habitantes
    tabla.infectados.paises=rbind(tabla.infectados.paises,c(confirmados,confirmados.estimados))
}
tabla.infectados.paises=as.data.frame(tabla.infectados.paises)
tabla.infectados.paises = data.frame(paises,tabla.infectados.paises)
names(tabla.infectados.paises)=c("pais","infectados","infectados.estimados")
tabla.infectados.paises$infectados.estimados=tabla.infectados.paises$infectados.estimados/
  sum(tabla.infectados.paises$infectados)

paises.con.problemas = which(tabla.infectados.paises$infectados.estimados < 5)

tabla.infectados.paises2 = tabla.infectados.paises[-paises.con.problemas,]

fila.añadir = data.frame("problemas",as.numeric(as.character(sum(tabla.infectados.paises[tabla.infectados.paises$pais 
                        %in% paises[paises.con.problemas],]$infectados))),
                        as.numeric(as.character(sum(tabla.infectados.paises[tabla.infectados.paises$pais %in% paises[paises.con.problemas],]
                            $infectados.estimados))))

names(fila.añadir)=names(tabla.infectados.paises2)

tabla.infectados.paises2 = rbind(tabla.infectados.paises2,fila.añadir)

chisq.test(as.numeric(as.character(tabla.infectados.paises$infectados)),p=as.numeric(as.character(
  tabla.infectados.paises$infectados.estimados)))
