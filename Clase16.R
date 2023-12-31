
# Clase 16 - aggregate/graf barras-hist

rm(list = ls())
# Aggregate in R (funciona solo para data frame)
# La funci�n Aggregate divide los datos de un dataframe en subconjuntos, calcula estad�sticas para cada
# subconjunto y devuelve el resultado en un nuevo data.frame.
# Es similar al apply de los array/matrices.
# La sintaxis de la funci�n aggregate depender� de los datos de entrada. Nosotros vamos a ver ejemplos para
# un dataframe.
# Sintaxis:
#   # Data frame
#   aggregate(x, # Objeto de R
#             by, # Lista de variables (elementos que forman los grupos)
#             FUN, # Funci�n a ser aplicada para crear el resumen estad�stico
#             ..., # Argumentos adicionales a ser pasados a FUN
#             simplify = TRUE, # Simplificar el resultado lo m�ximo posible (TRUE) o no (FALSE)
#             drop = TRUE) # Deshechar las combinaciones no usadas de grupos (TRUE) o no (FALSE).

# Veamos ejemplos de como agregar la media o realizar recuentos (Adaptados de: https:// r-coder.com/ aggregate-en-r/ )

# Usando la funcion �mean�
# Usamos siguiente conjunto de datos de R
df <- chickwts
head(df)
## weight feed
## 1 179 horsebean
## 2 160 horsebean
## 3 136 horsebean
## 4 227 horsebean
## 5 217 horsebean
## 6 168 horsebean

# Para utilizar la funci�n aggregate para calcular la media por grupos , hay que especificarla variable num�rica
# en el primer argumento, la categ�rica (como una lista) en el segundo y la funci�n que se aplicar� (en este caso mean) en el tercero.
group_mean <- aggregate(df$weight, list(df$feed), mean)
# para cada subgrupo le calcula la media, generando un data frame
group_mean <- aggregate(weight ~ feed, data = df, mean) # Equivalente
group_mean
class(group_mean)

# Usando la funcion �length�
# Contar los elementos de una variable categ�rica
aggregate(chickwts$feed, by = list(chickwts$feed), FUN = length)
## Group.1 x
## 1 casein 12
## 2 horsebean 10
## 3 linseed 12
## 4 meatmeal 11
## 5 soybean 14
## 6 sunflower 12

# Por ultimo notar que �Aggregate� tambien deja aplicar cualquier funci�n que se quiera, incluso una funci�n
# (FUN ) personalizada.

#################################################################

# Graficos de barras, histogramas y boxplots

# BARRAS  
rm(list = ls())

setwd("/home/clinux01/LaboAtm/Practica_5/")
# Cargo las librerias a utilizar
library(ggplot2)
library(lubridate)

gualeguaychu <- read.csv("gualeguaychu.csv")
head(gualeguaychu)

# C�mo esta serie de tiempo es muy larga nos quedamos con un periodo m�s
# corto (2010-2018) y un a�o (2018) que guardo en respectivos data.frames
gualeguaychu <- gualeguaychu[year(gualeguaychu$Fecha) >= 2010 & year(gualeguaychu$Fecha) <= 2018,]
gualeguaychu_2018 <- gualeguaychu[year(gualeguaychu$Fecha) == 2018,]

# Ahora vamos a graficar los datos de precipitaci�n del a�o 2018 con la
# funcion geom_col() de ggplot que directamente genera columnas y la
# altura nos indicara la cantidad de precipitaci�n.
p <- ggplot(data = gualeguaychu_2018, mapping = aes(x=Fecha, y=pre))
p <- p + geom_col()
p

# Para que el eje x se lea mejor, voy a usar month(Fecha), es decir de la
# variable fecha selecciono solo los meses
p <- ggplot(data = gualeguaychu_2018, mapping = aes(x= month(Fecha), y=pre))
p <- p + geom_col()
p
# Ggplot calculo la suma de los valores de precipitaci�n para cada mes para
# poder graficar una barra para cada mes
# El eje de x esta raro, pues los valores de meses estan cada 2.5

# Podemos agregarle los t�tulos, nombres de los ejes al gr�fico. Tambien
# defino el eje x como continuo y los breaks para que tenga el valor de cada mes
p <- ggplot(data = gualeguaychu_2018, mapping = aes(x= month(Fecha), y=pre))
p <- p + geom_col()
p <- p + labs(title = "Precipitaci�n mensual acumulada 2018",
              subtitle = "Estaci�n: Gualeguaychu",
              x = "Mes",
              y = "Precipitaci�n (mm)")
p <- p + scale_x_continuous(breaks = c(1:12)) # no llega hasta el 12, porque no tiene datos de nov y dic
p

# Podria agregar color a las barras con los argumentos color=� � y fill =� � dentro de geom_col()

# Ahora supongamos que queremos ver la informaci�n de varios a�os.
# Usemos los datos del periodo 2010-2018 y calculemos la suma de la
# precipitaci�n para cada mes de ese periodo. Usamos aggregate.

# Vamos a usar una nueva funci�n del paquete lubridate para manipular
# fechas. Fecha lo paso a una variable tipo Date (con as.Date),y lo que hace
# floor_date(Fecha, �month�) es �redondear para abajo� la fecha hasta el
# mes. Entonces por ejemplo todas las fechas del mes de enero de 2000
# pasan a ser �2000-01-01� sin importar el d�a.

gualeguaychu$year <- year(gualeguaychu$Fecha) # me quedo con los a�os de las fechas
gualeguaychu$month <- month(gualeguaychu$Fecha) # me quedo con los meses de las fechas
suma_PP_mensual <- aggregate(x=gualeguaychu$pre,by=list(gualeguaychu$year,gualeguaychu$month),FUN="sum")
# by me agrega columnas, con $ le asigno el nombre previamente y luego lo agrego a la lista de by

# Cambio los nombres de las columnas
colnames(suma_PP_mensual) <- c("Year", "Month", "Precip")
head(suma_PP_mensual)

# Ahora miremos la suma (acumulado) precipitaci�n para cada mes y a�o y
# juguemos con el argumento position.

# debo armar una fecha donde junta el a�o con su mes, para poder graficar bien con un eje correcto
suma_PP_mensual$Fecha <- paste(suma_PP_mensual$Year,suma_PP_mensual$Month,sep="-")
#suma_PP_mensual$Fecha2<- seq.Date(from = as.Date("2010-01-01"),to=as.Date("2018-10-01"),by="month")
p <- ggplot(suma_PP_mensual, aes(x=Fecha, y=Precip)) +
  geom_col(aes(fill = factor(Year)), position = "dodge") + # fill = factor(Year) me llena por cada a�o con distinto color
  labs(title = "Precipitaci�n mensual en Gualeguaychu", x = "Mes", y = "Precipitaci�n [mm]", fill = "Year")
p

# Si bien nos da algo de informacion, puede ser muy confuso. Veamos otra
# opcion que nos permite generar un panel para cada a�o usando facet_wrap().
p <- ggplot(suma_PP_mensual, aes(Month, Precip)) +
     geom_col() +
     facet_wrap(~Year, ncol =4) +
     labs(title = "Precipitaci�n mensual en Gualeguaychu", x = "Mes", y = "Precipitaci�n [mm]") + 
     scale_x_continuous(breaks =seq(1,12,1))
p
# los titulos de los paneles ggplot los pone solos

# HISTOGRAMAS

# Vamos a construir un histograma con todos los datos de precipitaci�n del periodo 2010-2018.
p <- ggplot(gualeguaychu, mapping = aes(x=pre))+ # mapping me crea un hist, solo debo especificar que dato quiero convertirlo en hist
     geom_histogram()
p

# Tenemos muchos valores que son 0, esto suele ocurrir con la precipitaci�n.
# Podemos no considerarlos, y asi ver que valores predominan. Entonces en
# un nuevo data.frame guardo los dias donde la pp fue mayor a 0 mm
precip_gualeguaychu <- gualeguaychu[gualeguaychu$pre>0, ]
p <- ggplot(data = precip_gualeguaychu ,aes(pre))
p <- p + geom_histogram(breaks= seq(0,200,10))
p

# Vemos que la mayoria de los datos estan entre 0 y 10 mm. Podemos
# agregar color a las barras, los titulos, etc.
p <- p + geom_histogram(breaks= seq(0,200,10), fill= "blue", col= "black") # col = bordes, fill = relleno
p <- p + labs(title = "Histograma de precipitaci�n diaria en Gualeguaychu",
              subtitle = "Periodo 2000-2010",
              x = "Precipitaci�n (mm)",
              y = "Frecuencia")
p

# BOXPLOT 

# Tambien podriamos querer ver la distribuci�n de una variable a partir de
# un boxplot. Grafiquemos el boxplot de la precipitaci�n sin ceros
box <-ggplot(data = precip_gualeguaychu ,aes(y= pre))
box <- box + geom_boxplot()
box
# los puntitos por fuera del box son datos particulares que no siguen la mediana general

# Podemos retocar algunos argumentos para que el boxplot quede m�s lindo
box <- box + geom_boxplot(outlier.colour = "black", outlier.size = 1, notch = TRUE,fill = "#E69F00")
box <- box + labs(title = "Boxplot de precipitaci�n diaria en Gualeguaychu",
                  subtitle = "Periodo 2000-2010",
                  y = "Precipitaci�n (mm)")
box <- box + stat_boxplot(geom = "errorbar") +xlim(c(-2, 2))
box

# hacer practica 5 para ejercitar ggplot