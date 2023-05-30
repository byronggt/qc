# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Fundamentos de distribución binomial de probabilidad
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

