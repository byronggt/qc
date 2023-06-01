# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

## Fundamentos de distribución binomial de probabilidad -----
# En un examen de selección múltiple, con 10 preguntas 
# con 4 posibles respuestas
# y una sola de ellas como correcta

# 1. Valores de la distribución binomial para n=10 p=0.25 q=0.75
tabla.p=data.frame(Probability=dbinom(0:10, size=10, prob=0.25))
rownames(tabla.p) <- 0:10 
print(tabla.p)

# 2. ¿Cuál es la probabilidad de responder ninguna correcta?
# P(X=0)
P0<-dbinom(0,10,0.25); P0 # Usar dbinom para valores puntuales de probabilidad

# 3. ¿Cuál es la probabilidad de responder de forma correcta 6 preguntas o menos?
# P(X<=6)
# Usar pbinom para probabilidades acumuladas
# Si es cola izquierda es desde el punto fijado hacia abajo
# Si es cola derecha debe ser un valor abajo del punto de interés
P6omenos<-pbinom(c(6), size=10, prob=0.25, lower.tail=TRUE); P6omenos

# 4. ¿Cuál es la probabilidad de responder de forma correcta 7 preguntas o más?
# P(X<=6)
# Si es cola derecha debe ser un valor abajo del punto de interés
# En este caso, como es 7 el interés, debe solicitarse para 6 y cola derecha
P7omas<-pbinom(6,10,0.25, lower.tail = F); P7omas

# 5. Gráfica de la distribución
ensayos<-0:10
plot(ensayos, dbinom(ensayos,size=10, prob=0.25), type = "h", xlab = "Número de éxitos", ylab="Probabilidad")
# Probabilidad para x=0,n=15, p=0.05
probf<-dbinom(0,15,0.05); probf

## Fundamentos de la distribución de Poisson ------
# dpois para valores puntuales
# ppois para valores acumulados
# qpois para cuantiles

# Se realizó un muestreo de bolsas de arroz para exportación
# Se encontró una media de 17.4 granos quebrados y
# se tolera no más de 50 granos quebrados por Kg.

ppois(c(50), lambda=17.4, lower.tail = T)
tabla.ps=data.frame(Probability=dpois(0:50, lambda = 17.4))
rownames(tabla.ps) <- 0:50 
print(tabla.ps)

# Gráfica de la distribución
x <- 0:50
lambda <- 17.4
plot(dpois(x, lambda), type = "h", lwd = 2,
     main = "Gráfica de la distribución de probabilidad",
     ylab = "P(X = x)", xlab = "Número de granos quebrados")

# Calcular la probabilidad de encontrar 20 granos quebrados o menos
P20menos<-ppois(c(20), lambda=17.4, lower.tail = T); P20menos

# Calcular la probabilidad de encontrar más de 20 granos quebrados
Pmas20<-ppois(c(20), lambda = 17.4, lower.tail = F); Pmas20
Pmas20a<-1-P20menos; Pmas20a

# Calcular la probabilidad de encontrar entre 20 a 30 granos quebrados
P20a30<-ppois(30, lambda=17.4)-ppois(19, lambda=17.4); P20a30

# Gráfica del segmento de probabilidad que corresponde de 20 a 30

# Antes agregar la siguiente función
# lambda: media
# lb: límite inferior de la suma
# ub: límite superior de la suma
# col: color
# lwd: ancho de línea

pois_sum <- function(lambda, lb, ub, col = 4, lwd = 1, ...) {
  x <- 0:(lambda + lambda * 2)
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  plot(dpois(x, lambda = lambda), type = "h", lwd = lwd, ...)
  
  if(lb == min(x) & ub == max(x)) {
    color <- col
  } else {
    color <- rep(1, length(x))
    color[(lb + 1):ub ] <- col
  }
  
  lines(dpois(x, lambda = lambda), type = "h",
        col =  color, lwd = lwd, ...)
}

# Gráfico de probabilidad cuando hay entre 20 a 30 granos quebrados
pois_sum(lambda = 17.4, lb = 19, ub = 30, lwd = 2,
         ylab = "P(X = x)", xlab = "Número de granos quebrados")