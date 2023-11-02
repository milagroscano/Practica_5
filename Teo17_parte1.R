
# Teorica 17 - Graficado - PARTE 1

rm(list = ls())

# Es la función genérica para graficar objetos de R en X-Y
# plot(x, y, ...)
# x: la coordenada x de los puntos en el grafico. Alternativamente, una
# función o un objeto de R con un método de ploteo puede ser provisto
# y: la coordenada y de puntos en el grafico, y es opcional en el caso que x
# sea una estructura adecuada.

# grafico sobre datos cargados en R como cars
cars
plot(cars) # grafica con puntos

# grafico la funcion seno
plot(sin, -pi, 2*pi) # grafica con linea

# type el tipo de plot que debe dibujarse. 
# Posibilidades:
# "p" para puntos,
# "l" para lineas,
# "b" ambos (punto+linea),
# "c" grafica solo las lineas de la opcion "b",
# "o" ambos pero sobrepuestos,
# "h" del tipo ‘histograma’ (o ‘alta-densidad’) de lineas verticales,
# "s" tipo escalera,
# "S" otro tipo de pasos ver ‘Details’ de plot,
# "n" no graficar.

# pruebo varios tipos de plot
plot(cars,type="p") 
plot(cars,type="l") 
plot(cars,type="b")
plot(cars,type="h")
plot(cars,type="c")
plot(cars,type="s")
plot(cars,type="S")

# TITULOS
# main: El titulo del grafico
# sub: subtitulo del grafico
# xlab: titulo para el eje x
# ylab: titulo para el eje y
# asp: la razon y/x para el aspecto del grafico, see plot.window.
plot(cars,type="o",main="Grafico de la variable",sub="cars",xlab="velocidad",ylab="distancia")

############################ EJEMPLO

# Defino dos variables
x <- (0:65)/10
y <- sin(x)

# Primer gráfico, que es?
plot(x)  # me genera un grafico de una recta en puntitos

# Y ahora que vemos?
plot(x, y) # me genera la funcion seno pero con puntitos en vez de una linea como antes

# Incorporamos titulos
plot(x, y, main="Función Seno")

# Cambiamos de Función
z <- cos(x)
plot(x, z, type="l", main="Función Coseno") # determino que tenga una linea en vez de puntitos

# ALGUNOS PARAMETROS

# col permite modificar colores
# Axes  opciones para los ejes
# frame.plot  trazar un recuadro al grafico
# lty  tipos de lineas.
# lwd  anchos de lineas.
# par  establecer o preguntar parámetros gráficos
# legend  agregar una leyenda al grafico
# layout  divide espacio a graficar en filas y columnas
# mtext  titulo para eje derecho
# xlim, ylim  cambia el rango de valores de los ejes

# Iremos viendo estos parametros con algunos ejemplos
plot(x, y, main="Seno", type="l")
plot(x, z, main="Coseno", lty=3, col="red", type="l")
plot(x, z, main="Coseno", lty=4, col="blue", type="l", xlim=c(0, 2), ylab="cos(x)")

# ALGUNAS FUNCIONES

# Hay una serie de funciones que permiten dibujar sobre un gráfico ya creado:
# windows() Crea una ventana nueva
# points(x, y, ...) Dibuja una nube de puntos
# lines(x, y, ...) Dibuja una línea que une todos los puntos
# ablines() Dibuja una línea recta dada la interc. y pendiente
# polygons(x, y, ...)  Dibuja un polígono cerrado
# text(x, y, labels, ...)  Escribe texto en unas coordenadas

plot(x, y, main="Funciones seno y coseno", type="l")
# me agrega el grafico de lineas sobre el ya hecho
lines(x, z, col="blue", lty=2) # col=4 es equivalente
# me agrega textos a las lineas para identificar cada una, le debo dar coordenadas
text(x=c(0.5, 0.5), y=c(0, 1), labels=c("sin(x)", "cos(x)"),col=c("black", "blue"))

# Cambiar las posiciones del text
text(x=c(2.5, 2.4), y=c(0.9, -1.0), labels=c("sin(x)", "cos(x)"),col=c("black", "blue"))

# FUNCIONES GRÁFICAS INTERACTIVAS

# En R existen una serie de funciones que permiten completar los gráficos de
# manera interactiva por parte del usuario.

# locator() devuelve las coordenadas de los puntos.
plot(x, y, main="Funciones seno y coseno", type="l")
lines(x, z, col=2, lty=2)
legend(locator(1),legend=c("sin(x)","cos(x)"),lty=c(1,2),col=c(1,2))
# Grafica y se “queda esperando”

# identify(x, y, labels)
# identifica los puntos con el mouse y escribe la correspondiente etiqueta.

x <- 1:10; y <- sample(1:10)
plot(x, y); identify(x, y) #usar ESC para terminar
# Y ahora le indicamos que aparezca una etiqueta particular
nombres <- paste("punto", x, ".", y, sep ="")
plot(x, y); identify(x, y, labels=nombres) #usar ESC para terminar

# ESCALAS DE COLORES
Demo("colors")
# R definedcolors: ?rainbow
pie(rep(1, 12), col = rainbow(12))
pie(rep(1, 12), col = heat.colors(12))
pie(rep(1, 12), col = terrain.colors(12))
pie(rep(1, 12), col = topo.colors(12))
pie(rep(1, 12), col = cm.colors(12))

# Buscar tonalidades de colores
colors()[grep("red",colors())] # todos los tonos de rojo
# Paletas de colores definidas por el usuario:
# Códigos de colores en Guia de Colores.pdf (en campus – Material Adicional)
# ver script Colours.r (en campus)

####################################################################################################
# SCRIPT COLOURS  
rm(list = ls())
#*******************************************************************************
# Gustavo Naumann
# UBA-CONICET, Buenos Aires, 24.08.2015
#
# Purpose: Introduction to different definitions of colours and colour palettes in R 
#*******************************************************************************

############################################
### Vector con colores ##################### 
############################################

X<-1:5 
colors <- c("honeydew","dodgerblue","firebrick","forestgreen","gold")

par(fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
    omi=c(0.05,0.15,0,0.05), # global margins in inches (bottom, left, top, right)
    mai=c(0.6,0.6,0.6,0.5), bg="pink", fg="brown") # subplot margins in inches (bottom, left, top, right)

barplot(X, col= colors)
legend("topleft", colors  , text.col = "black", horiz = FALSE,
       col=colors, cex=0.8, lty=1, y.intersp = 1, lwd = 3 )

#########################################
#### Paletas de colores Mapas ###########
#########################################
require(ncdf4)
require(fields)
require(mapdata)

nc <- nc_open(paste("/home/clinux01/LaboAtm/","netcdf_SLP_SA_1979_2014.nc",sep=""))
lon <- ncvar_get(nc,"longitude")
lats <- ncvar_get(nc,"latitude")
time <- ncvar_get(nc,"time")
MSLP <- ncvar_get(nc,"msl") ## MSLP * 100

P <- MSLP[,,1]/100
mn <- min(P); mx <- max(P)
brk <- seq(mn,mx,length.out = 13)
levs <- brk

RGB<-matrix(c(0,  0.167,	1,
              0.1,	0.4,	1,
              0.2,	0.6,	1,
              0.4,	0.8,	1,
              0.6,	0.933,	1,
              0.8,	1,	1,
              1,	1,	0.8,
              1,	0.933,	0.6,
              1,	0.8,	0.4,
              1,	0.6,	0.2,
              1,	0.4,	0.1,
              1,	0.167,	0), nrow=12, byrow=TRUE)

cols<-rgb(RGB[,1],RGB[,2],RGB[,3]) #convierte color a rgb

image.plot(lon-360,rev(lats[1:55]),P[,55:1], breaks=brk, lab.breaks=names(brk), col=cols)
# necesito las longitudes en lado este por eso deben ser negativas y les resto 360
# luego necesito que las latitudes esten al reves, por eso rev 
contour(lon-360,rev(lats[1:55]),P[,55:1], add=TRUE, col=c("gray12","gray50","gray89"), levels = c(1010,1012,1016))
# add = T significa para superponer sobre el grafico ya creado
map(database="worldHires", add=TRUE, col="black", interior=TRUE)

#################################################
#### Generar una escala de grises  ##############
#################################################

for (i in 1:12) {
  k <- 6*i
  cols[i] <- paste("gray",k,sep ="")
}

image.plot(lon-360,rev(lats[1:55]),P[,55:1], breaks=brk, lab.breaks=names(brk), col=cols)
contour(lon-360,rev(lats[1:55]),P[,55:1], add=TRUE, col=c("gray12","gray50","gray89"), levels = c(1010,1012,1016))
map(database="worldHires", add=TRUE, col="black", interior=TRUE)

#####################################################
#### Generar una escala de cian RGB  ################
#####################################################
rgbs <- cbind(12, seq(32,255, length=12), seq(32,255, length=12)) / 255
cols <- rgb(rgbs[,1], rgbs[,2], rgbs[,3]) 

image.plot(lon-360,rev(lats[1:55]),P[,55:1], breaks=brk, lab.breaks=names(brk), col=cols)
contour(lon-360,rev(lats[1:55]),P[,55:1], add=TRUE, col=c("gray12","gray50","gray89"), levels = c(1010,1012,1016))
map(database="worldHires", add=TRUE, col="black", interior=TRUE)

# FIN
#######################################################################################################################

# ALGUNOS PARAMETROS GRAFICOS
# axes: un valor logico indicando si ambos ejes deben ser dibujados. Usar parametro grafico
# “xaxt” o “yaxt” para eliminar uno de los ejes.
# frame.plot: logical, indicando si se debe trazar un recuadro al grafico. Default TRUE
rm(list = ls())
x <- -10:10
plot(x)
plot(x,frame.plot=F)

# lty: un vector con tipos de lineas.
# lwd: un vector con anchos de lineas. Numeros positivos, default 1.
# 0=blank, 1=solid (default), 2=dashed, 3=dotted,
# 4=dotdash, 5=longdash, 6=twodash

plot(x, frame.plot = F, lty=2, lwd= 50)

# par puede ser usado para establecer o preguntar por parámetros gráficos
# Escribir en la consola par() devuelve los valores de los parámetros
par()

plot(x, frame.plot = F, lty=2, lwd= 50)

# Tomar la primer parte del script Colours.R e ir agregando/modificando los
# siguientes parámetros
# bg: El color de fondo del área de graficado
# fg: El color del “foreground” de los gráficos. Es el color que se usa por
# default en los ejes, cajas alrededor de los gráficos, etc.
# fig: Un vector numérico del tipo c(x1, x2, y1, y2) que da las coordenadas
# (NDC) de la figura en la región de display. Si se establecen estos valores
# se inicia un nuevo grafico, para agregar al grafico existente usar
# también new = TRUE.
# mai: Un vector numérico del tipo c(inferior, izquierdo, superior, derecho)
# que especifica el tamaño de los márgenes en pulgadas.
# mar: idem mai pero en lugar de pulgadas numero de líneas.
# Default c(5, 4, 4, 2) + 0.1.

# layout divide el dispositivo en tantas filas y columnas como aparecen en la matriz mat, con
# los anchos de las columnas y las alturas de las filas especificadas en sus respectivos argumentos.
# 
# layout(mat, widths = rep.int(1, ncol(mat)), heights = rep.int(1, nrow(mat)), respect = FALSE)
# layout.show(n = 1)  grafica (parte de) la disposicion actual, esquema de las n figuras.
# lcm(x)  tomar x como el numero de centimetros

# EJEMPLO 1
layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
layout.show(2)
x <- array(rnorm(100),c(100,4)) #datos a graficar.
titulo <- c("Figura1","Figura 2","Figura 3","Figura 4")

#Titulo de cada grafico individual
for(k in 1:4) {
  plot(x[,k], type="l", col="darkblue", lwd=2, ylab="variable",xlab="tiempo",xlim=c(0,100),ylim =c(-3,3))
  title(main=(titulo[k]), line=1, cex.main=1.2)
}
# Son 4 graficos, lo va a hacer en 2 paginas

# EJEMPLO 2
nf <- layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE), respect = TRUE) #igual a la anterior salvo respect, relaciones entre altos y anchos
layout.show(nf)

# EJEMPLO 3
## crea una sola figura de 5cm x 5cm
nf <- layout(matrix(1), widths = lcm(5), heights = lcm(5)) #le especifico el alto y el ancho de la figura
layout.show(nf)

