# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(readxl)){install.packages("readxl")}

# La dimensión de una pieza se distribuye de manera normal con
# media=82 mm y desviación estándar=0.5

# 1. Calcule el porcentaje de piezas que cumple con las especificaciones 82+-1
# es decir 81 a 83 -----

P83<- pnorm(c(83), mean(82), sd=0.5, lower.tail=T)
P81<-pnorm(81,82,0.5)
P81a83<-P83-P81; P81a83

# 2. Gráfica de la distribución de probabilidad -----

.x<-seq(80.355, 83.645, length.out=1000)
plot.new()
dev.new(10,10)
plotDistr(.x, dnorm(.x,82,0.5), cdf=F, 
          xlab="dimensión de la pieza",
          ylab="Densidad",
          main="Distribución normal",
          regions=list(c(81,83)), col="red",
          legend.pos = "topleft", cex=0.1          
          )
# 3. Probabilidad de que las piezas presenten más de 83.5 mm -----
Pmas835<-pnorm(83.5,82,0.5, lower.tail = F);Pmas835

# 4. Gráfica de la distribución de probabilidad
.x1<-seq(80.355, 83.645, length.out=1000)
plot.new()
dev.new(10,10)
plotDistr(.x1, dnorm(.x1,82,0.5), cdf=F, 
          xlab="dimensión de la pieza",
          ylab="Densidad",
          main="Distribución normal",
          regions=list(c(83.5,+Inf)), col="red",
          legend.pos = "topleft", cex=0.1          
)

# Segundo procedimiento para graficar la distribución de probabilidad
# Crear la siguiente función: normal_area
# mean: media de la variable normal
# sd: desviación típica de la variable normal
# lb: límite inferior del área
# ub: límite superior del área
# acolor: color del área
# ...: argumentos adicionales para ser pasados a la función lines

normal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dnorm(x, mean, sd), type = "n", ylab = "")
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
}

# Área entre 81 a 83
normal_area(mean = 82, sd = 0.5, lb = 81, ub = 83, lwd = 2, acolor = "red")
text(82, 0.25, "81 a 83")

## Verificar el ajuste de la distribución normal de probabilidad
sacarosa<-read_excel("sacarosa.xlsx")
head(sacarosa)
sac1<-subset(sacarosa, muestra=="Lodo") 
head(sac1)  
hist(sac1$ph)

h1<- graph.freq(sac1$ph, col="yellow", frequency =1, xlab="pH", ylab="Número de muestras", main="frecuencia absoluta")
h2<- graph.freq(sac1$ph, frequency =2 , main="Polígono de frecuencia")
polygon.freq(h2, col="blue", lwd=2, frequency =2)
h3<- graph.freq(sac1$ph, col="brown", frequency =3 , main="Gráfico de densidad")
h4<- graph.freq(sac1$ph, col="blue", frequency =3 , main="Densidad normal", density=4)
normal.freq(h4, col="red", lty=4,lwd=2, frequency=3)
dens1<-density(sac1$ph)
plot(dens1)

# Gráfico de cuantil-cuantil
qqnorm(sac1$ph)
qqline(sac1$ph)
qqPlot(sac1$ph, xlab = "Cuantiles teóricos", ylab = "Cuantiles observados")

# Prueba de Shapiro & Wilk
shapiro.test(sac1$ph)
