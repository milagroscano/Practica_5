
# ggplot - Series temporales

# Vamos a utilizar los datos de temperatura del aire analizados en la parte anterior.
# Cargo las librerias que voy a necesitar
library(ggplot2)
library(lubridate)
##
## Attaching package: 'lubridate'
## The following objects are masked from 'package:base':
##
## date, intersect, setdiff, union
library(metR)
#Quedemosnos unicamente con el punto mas cercano a la estacion OCBA
# (-34,-58)
archivo <- "/home/clinux01/LaboAtm/Practica_4/air.mon.mean.nc" #donde esta el archivo
datos_OCBA<- ReadNetCDF(archivo, vars = "air",
                        subset = list(lat =-34,
                                      lon = 360-58))
# Me quedo con el periodo 1990-2010
datos_OCBA_periodo<- datos_OCBA[which(year(datos_OCBA$time) %in% 1990:2010),]

# Miro los datos
head(datos_OCBA_periodo)

# Primera capa
grafico <- ggplot(data = datos_OCBA_periodo, mapping = aes(x= time, y= air))
grafico

# Ahora agrego la geometria que quiero, en este caso como es una serie temporal puedo
#usar lineas. Uso geom_line
grafico<- grafico + geom_line()
grafico

# Supongamos que queremos que la linea sea de color azul, esto lo
# agregamos en geom_line(color= )
grafico <- grafico + geom_line(color= "blue")
grafico

# Ahora podemos agregar los titulos, subtitulos, ejes.
grafico <- grafico + labs(title = "Temperatura OCBA", subtitle = "Periodo 1990-2010",
                          x = "mes",
                          y = "Temperatura(°C)" )
grafico

######################################### HACER EN CASA 
# Ejercicio:
# 
# Calcular el promedio de temperatura anual (promedio de los 12 meses del año) y 
# graficar la serie resultante con lineas y puntos. Además agregar la
# linea de tendencia lineal.

# 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
