
# Teorica 17 - parte 2 (ultima clase)

rm(list = ls())
# FIGURAS EN VARIOS PANELES
# VOY A GRAFICAR 4 SERIES EN UNA HOJA (DOS FILAS Y DOS COLUMNAS)

x<-array(rnorm(100),c(100,4)) #creo los datos a graficar. Replico los 100 datos en 4 columnas
titulo<-c("Figura1","Figura 2","Figura 3","Figura 4") #Titulo de cada grafico individual
par(fig=c(0,1,0,1), # Region de la Figura dentro de la region display (x1,x2,y1,y2)
    omi=c(0.05,0.15,0,0.05), # margenes globales en pulgadas (inferior, izquierdo,
    superior, derecho)
mai=c(0.6,0.6,0.6,0.5) # margenes del subplot en pulgadas (inf izq, superior, derecho)
layout(matrix(1:4, 2, 2, byrow= TRUE))
for(k in 1:4){
  plot(x[,k], type="l", col="darkblue", lwd=2, ylab="variable",xlab="tiempo",xlim=c(0,100),ylim=c(-3,3) )
  title(main=(titulo[k]), line=1, cex.main=1.2)  # cex.main tamaño de letra
  abline(v=20,lty=2,col="grey"); abline(v=40,lty=2,col="grey");
  abline(v=60,lty=2,col="grey") # para que aparezcan 4 lineas punteadas
}

# PLOTEO DE UNA SERIE DE TIEMPO - CURVE
# En el caso del grafico de la función seno considera plot.function y utilizo
# curve(expr, from = NULL, to = NULL, n = 101, add = FALSE, type = "l", xname
#       = "x", xlab = xname, ylab = NULL, log = NULL, xlim = NULL, ...)
# Traza una curva correspondiente a la función sobre el intervalo [desde, hasta].
# curve también puede graficar una expresión en la variable xname, default x.

curve(sin, -2*pi, 2*pi, xname = "t")
curve(tan, xname = "t", add = NA, main = "curve(tan) --> same x-scale as previous plot")
# add = NA me toma el mismo rango de valores que el grafico anterior, ya que no lo defino

# BARPLOT
# Crea un grafico de barras con barras horizontales o verticales. (Ver las opciones en help)

require(grDevices) # librería para colores
tN <- table(Ni <- stats::rpois(100, lambda = 5))
#genera datos al azar usando la funcion de poisson media 5
r <- barplot(tN, col = rainbow(20))
#- type = "h" plotting *is* 'bar'plot
lines(r, tN, type = "h", col = "red", lwd = 2)
# grafica una linea desde los valores de r hasta los valores de tN, lwd es el tamaño de la linea

# No especifica colores, usa gris
barplot(tN, space = 1.5, axisnames = FALSE, sub = "barplot(..., space= 1.5, axisnames = FALSE)")
# space genera espacio entre las barras, al no especificar color toma gris, y sub es un subtitulo

mp <- barplot(VADeaths) # default
tot <- colMeans(VADeaths)
# tot se define como el valor medio
text(mp, tot + 3, format(tot), xpd = TRUE, col = "blue")
# le agrega el valor medio a cada colummnas de mp que es el barplot 

barplot(VADeaths, beside = TRUE, col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"), 
        legend = rownames(VADeaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)
# le agrego un titulo, font.main = 4 es un tipo de letra que se utiliza

# GRAFICOS EN DOS EJES
x<-array(rnorm(100),c(100,4))
par(fig=c(0,1,0,1), # Region de la figura en el display device (x1,x2,y1,y2)
    omi=c(0.05,0.9,0,0.3)) # margenes globales en pulgadas (abajo, izq, sup, derecha)
plot(x[,1], type = "l", col = "darkblue", lwd = 3, ylab = "variable 1",xlab="Tiempo", xlim = c(0,100), ylim = c(-3,3))
title(main=("Grafico en dos ejes"), font=2, line=1, cex.main=1.2)
par(new=TRUE) #para graficar sobre la figura anterior, nueva configuracion de graficado
barplot(x[,3]*x[,1], col="darkred",lwd=2, yaxt="n", ylab="",xaxt="n", xlab="",cex.axis=1.0 )
# en la multiplicacion cambio los datos de la variable
axis(4, cex.axis=1.0) ## Valores en el eje derecho, 1 es abajo, 2 es la izq, 3 es arriba y 4 la derecha
mtext("variable 2", side=4, line=2, col="darkred", cex=1.0) ## Texto para el eje derecho

# BOXPLOT
x<-rnorm(100)
boxplot(x)

# también es posible dibujar boxplots para matrices, uno por cada columna (variable) de la matriz.
x<-array(rnorm(100),c(100,5)) # a cada una de esas 100 filas le calcula un box plot para cada columna
boxplot(x)

#OTRO EJEMPLO
boxplot(x,notch=TRUE) # notch ajusta la mediana del boxplot
title(main="notch=TRUE")

# PLOTEAR UNA MATRIZ - contour
# Crear un grafico de contornos o agregar contornos a un grafico ya existente

# EJEMPLO 1
x <- -6:16
op <- par(mfrow = c(2, 2),mai=c(0.4,0.4,0.4,0.4))
# mfrow me define los espacios para los graficos, en este caso seria como una matriz de 2x2, es decir me da 4 espacios
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z) # image grafica en colores
contour(x, x, z, col = "pink", add = TRUE, method = "edge", vfont = c("sans serif", "plain"))
# add = t me genera un grafico sobre el ya graficado anteriormente
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1, xlab = quote(x[1]), ylab = quote(x[2]))
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple", main = "20 levels; \"simple\"
labelling method",cex.main=0.8)
# nlev e sla cantidad de lineas, y el method es el tipo de linea

# EJEMPLO 2 - Correrlo linea por linea
rx <- range(x <- 10*1:nrow(volcano))
ry <- range(y <- 10*1:ncol(volcano))
ry <- ry + c(-1, 1) * (diff(rx) - diff(ry))/2
# las tres lineas anteriores son para definir los valores de los ejes x e y
tcol <- terrain.colors(12) # define colores del grafico
par(opar); opar <- par(pty = "s", bg = "lightcyan")
plot(x = 0, y = 0, type = "n", xlim = rx, ylim = ry, xlab = "", ylab = "")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = tcol[8], border = "red")
contour(x, y, volcano, col = tcol[2], lty = "solid", add = TRUE, vfont = c("sans serif", "plain"))
title("A Topographic Map of Maunga Whau", font = 4)
abline(h = 200*0:4, v = 200*0:4, col = "lightgray", lty = 2, lwd = 0.1)
# abline pone lineas vert y horiz sobre los ejes

#  filled.contour
# Esta función produce un grafico de contornos con las áreas entre los contornos rellenas con un
# color solido. Se muestra la escala de colores a la derecha del grafico.

# EJEMPLO 1
require(grDevices) # for colours
filled.contour(volcano, color = terrain.colors, asp = 1)
# filled.contour ya viene con una leyenda, es decir con una escala de colores

# EJEMPLO 2
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
filled.contour(x, y, volcano, color = terrain.colors,
               plot.title = title(main = "The Topography of Maunga Whau",
                                  xlab = "Meters North", ylab = "Meters West"),
               plot.axes = { axis(1, seq(100, 800, by = 100))
                 axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "Height\n(meters)"), # key.title titulo a la leyenda, \ es para seguir debajo
               key.axes = axis(4, seq(90, 190, by = 10)))
mtext(paste("filled.contour(.) from", R.version.string), side = 1, line = 4, adj = 1, cex = .66)
# le agrego un titulo principal al grafico, a los ejes y a la escala

# PLOTEAR UNA SUPERFICIE EN 3D - SURF3D
require(plot3D)

X=matrix(seq(-8,8,by=0.5),ncol=33,nrow=33)
Y=matrix(seq(-8,8,by=0.5),ncol=33,nrow=33,byrow=T)

#otra forma de generar los dos renglones anteriores es usando la funcion mesh
MM<-mesh(seq(-8,8,by=0.5),seq(-8,8,by=0.5))
class(MM)
X<-MM$x ; Y<-MM$y

R = sqrt(X^2 + Y^2) + .Machine$double.eps

#La matriz R contiene la distancia desde el centro de  la matriz, 
#que es el origen. Sumar eps impide la divisi�n por cero 
#(en la etapa siguiente) que genera valores Inf en los datos.

Z = sin(R)/R

surf3D(X,Y,Z,facets=F)
surf3D(X,Y,Z,facets=NA)
surf3D(X,Y,Z,facets=F,border = "black",colkey=F)
surf3D(X,Y,Z)
surf3D(X,Y,Z,phi=40,theta = 20)
surf3D(X,Y,Z,phi=40,theta = 60)
surf3D(X,Y,Z,colkey=F)
surf3D(X,Y,Z,colvar=Y,colkey=F,ltheta=20,bty="b2",phi=0)
surf3D(X,Y,Z,colvar=X,colkey=F,bty="b")
surf3D(X,Y,Z,colvar=X,colkey=F,box=F,shade=0.5)
surf3D(X,Y,Z,colvar=X,colkey=F,box=F,border = "black", xlim = range(X)*0.8, 
       ylim = range(Y)*0.8, zlim = range(Z)*0.8)
surf3D(X,Y,Z,colvar=X,colkey=F,box=F,border = "black", xlim = range(X)*0.8, 
       ylim = range(Y)*0.8, zlim = range(Z)*1.2)
surf3D(X,Y,Z,box=FALSE,theta=60,col = "lightblue", shade = 0.9)
for (angle in seq(0, 360, by = 10)) plotdev(theta = angle)
#Con la flecha en plots ir hacia atras para ver los graficos 
#generados con el for

# GENERACIÓN DE ARCHIVOS DE GRÁFICO
# ✓tiff(“mygraph.tiff”) Guarda como archivo tiff
# ✓png("mygraph.png") Guarda como archivo png
# ✓jpeg("mygraph.jpg") Guarda como archivo jpeg
# ✓bmp("mygraph.bmp") Guarda como archivo bmp
# ✓pdf("mygraph.pdf") Guarda como archivo pdf
# ✓postscript("mygraph.ps") Guarda como archivo postscript
# Al guardar los gráficos en cualquiera de estos formatos no se genera la
# figura en la pantalla de Rstudio sino que hay que abrir el archivo una vez
# que se completo el grafico se cerro el dispositivo con Dev.off()

x <- seq(1, 10, by =2)
y <- seq(0.5, 2.5, by = 0.5)
jpeg("Grafico.jpg") #toma todas las opciones default
plot(x,y,type="l",xlab="x",ylab="y", main="Parabola y=x^2", col="red")
dev.off()

#### Generar los gráficos de Ejemplos de Rectángulos en RStudio
example(rect) # genera dos graficos

#### Generar los gráficos de Ejemplos de Rectángulos como JPEG
jpeg(file = "myplot.jpeg") # me guarda solo la ultima imagen generada por example
example(rect)
dev.off()
###### Abrir el archivo y ver lo generado

######## Repetir lo anterior pero cambiando el nombre del archivo como se ve abajo
jpeg(file = "myplot%02d.jpeg") # me guarda la primer imagen en myplot01.jpeg y la segunda imagen en otro archivo
example(rect)
dev.off()
###### Cuantos archivos de gráficos genero??????


# PAQUETE FIELDS
# arrow.plot (VECTORES DE VIENTO)
# arrow.plot es una funcion del paquete fields que agrega flechas en puntos especificados
# donde la longitud de las flechas se escalan para entrar en el area de dibujo.
require(fields)
require(ncdf4)
require(mapdata)

#setwd("F:/Moira/materias/SeminarioComputacion/Teoricas 2016/Ejemplos Teorica")
setwd("D:/Moira/PCFacultad/materias/SeminarioComputacion/Teoricas 2016/Ejemplos Teorica")
# tengo que poner mi setwd para abrir los archivos, y debo descrgarlos
nc_u<-nc_open("u925_globo.nc")
nc_v<-nc_open("v925_globo.nc")
u_925<-get.var.ncdf(nc_u,"uwnd") # nc_get_var
v_925<-get.var.ncdf(nc_v,"vwnd")
longitud<-get.var.ncdf(nc_u,"lon")
latitud<-get.var.ncdf(nc_u,"lat")

u=c(u_925[,,1])
v=c(v_925[,,1])
uv=cbind(u,v)

X<-rep(longitud,73)
Y<-rep(latitud,each=144)
XY<-cbind(X,Y)

jpeg("ejemplo17.jpg",width = 720, height = 480, units = "px")
plot(0, type = "n", xlim = range(longitud), ylim =range(latitud),xlab ="",ylab="", axes=TRUE,
     main = "Viento en 925hPa")
arrow.plot(XY,uv,arrow.ex = 0.01,true.angle = FALSE,xpd=FALSE,col="red",length=0.1,arrowfun = arrows,par(xaxp=c(0,350,10)))
map(database="world2", myborder=1.5,add=TRUE, col="black", interior=TRUE,ylim=range(latitud),xlim = range(longitud))

dev.off()


###### Genero una segunda figura con las flechas mas espaciadas entre si

l=1
XY_10<-matrix(NA,nrow=1052,ncol=2)
uv_10<-matrix(NA,nrow=1052,ncol=2)

for (i in seq(1,length(XY),by=10)){
  XY_10[l,]=XY[i,]
  uv_10[l,]=uv[i,]
  l=l+1
}

plot.new()
plot(0, type = "n", xlim = range(longitud), ylim =range(latitud),xlab ="",ylab="", axes=TRUE,
     main = "Viento en 925hPa")
arrow.plot(XY_10,uv_10,arrow.ex = 0.1,true.angle = FALSE,xpd=FALSE,col="cyan",length=0.1,arrowfun = arrows,par(xaxp=c(0,350,10)))
map(database="world2", myborder=1.5,add=TRUE, col="black", interior=TRUE,ylim=range(latitud),xlim = range(longitud))


##### Superpongo el campo U (ZONAL) del Viento a los Vectores viento
plot.new()
plot(0, type = "n", xlim = range(longitud), ylim =range(latitud),xlab ="",ylab="", axes=TRUE,
     main = "Viento en 925hPa y Componente U")
arrow.plot(XY_10,uv_10,arrow.ex = 0.1,true.angle = FALSE,xpd=FALSE,col="cyan",length=0.1,arrowfun = arrows,par(xaxp=c(0,350,10)))
map(database="world2", myborder=1.5,add=TRUE, col="grey65", interior=TRUE,ylim=range(latitud),xlim = range(longitud))

##### Superpongo el campo U (ZONAL) del Viento indicando valores positivos y Negativos a los Vectores viento
plot.new()
plot(0, type = "n", xlim = range(longitud), ylim =range(latitud),xlab ="",ylab="", axes=TRUE,
     main = "V 925hPa y U positivo y negativo")
arrow.plot(XY_10,uv_10,arrow.ex = 0.1,true.angle = FALSE,xpd=FALSE,col="cyan",length=0.1,arrowfun = arrows,par(xaxp=c(0,350,10)))
map(database="world2", myborder=1.5,add=TRUE, col="grey65", interior=TRUE,ylim=range(latitud),xlim = range(longitud))


contour(longitud,rev(latitud[1:73]),u_925[,73:1,1], col="chocolate", add=TRUE, ltw=1.4, lty=2,levels=seq(-20,0,5))
contour(longitud,rev(latitud[1:73]),u_925[,73:1,1], col="firebrick", add=TRUE, ltw=1.4, lty=1,levels=seq(0,20,5))

# GGPLOT - ver diaposss