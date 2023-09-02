# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(AcceptanceSampling)){install.packages("AcceptanceSampling")}
if(!require(readxl)){install.packages("readxl")}

# Riesgo del consumidor (Beta)
# Probabilidad de aceptar el lote con n=15 y c=0
p.aceptar<-dbinom(0,15,0.05); p.aceptar

# Riesgo del productor (Alfa)
p.rechazar<-1-p.aceptar; p.rechazar

# Curva de operación n=15 y c=0 -----

x <- OC2c(15, 0, type="b"); x

summary(x, full=TRUE)
plot(x, xlim = c(0, 0.3), col="red")
grid()
abline(h = 0.46, v = 0.05, col = "blue", lty = 4)

# Curva de operación n=30 y c=2 p=0.08 ------

y <- OC2c(30, 2, type="b"); y

summary(y, full=TRUE)
plot(y, xlim = c(0, 0.40), col="red")
grid()
abline(h = 0.57, v = 0.08, col = "blue", lty = 4)

# Riesgo del productor (alfa) y
# Riesgo del consumidor (beta) para n=30 y c=2
# con "p" de 4% y 16% 
# p(aceptar con 4%) = 0.88 ==> p(rechazo) = 0.12
# p(aceptar con 16%) = 0.12 ==> p(rechazo) = 0.88

plot(y, xlim = c(0, 0.40), col="red")
grid()
abline(h = 0.88, v = 0.04, col = "blue", lty = 4)
abline(h = 0.12, v = 0.16, col = "green", lty = 4)



# Curvas de operación para A y B -------

A <- OC2c(50, 2, pd=seq(0,.30,.01), type="b")
B <- OC2c(20, 1, pd=seq(0,.30,.01),type="b")

# Establecer AQL para un alfa=0.20, es decir Beta=0.8
# Graficar ambas curvas en el mismo gráfico --------

pd<-seq(0,.30,.01)
plot(pd,A@paccept, type="l", col=1, xlab="Porcentaje de defectuosos", ylab="Probabilidad de aceptar el lote" )
lines(pd,B@paccept, type="l", col=2)
legend(.15,.95,c("A","B"),lty=c(1,2,4), col=c(1,2,4))
grid()


# Plan de muestreo -------

find.plan(PRP=c(0.05,0.95),
          CRP=c(0.15,0.20),type="b",N=500)


# Curva OC para el plan de muestreo -------

plan<-OC2c(52,5,type="b")
plot(plan, type='l')
grid()

# Evaluar el plan de muestreo -------

assess(OC2c(52,5), PRP=c(0.05, 0.95), CRP=c(0.15,0.20))
