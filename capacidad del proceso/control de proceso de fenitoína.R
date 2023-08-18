# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt
# IMPORTANTE: Emplear desde el inicio 
# la versión de R 3.6.3 para el paquete qualityTools

if(!require(rriskDistributions)){install.packages("rriskDistributions")}
if(!require(car)){install.packages("car")}
if(!require(nortest)){install.packages("nortest")}
if(!require(readxl)){install.packages("readxl")}
if(!require(qcc)){install.packages("qcc")}

# Importar la tabla de datos "fenpos" para datos simulados
# Importar la tabla de datos "fenpos100" para datos reales

fenit<-read_excel("fenpos.xlsx")
fenit1<-read_excel("fenpos100.xlsx", sheet = "data")

# Calcular el resumen de los 5 números + la media
summary(fenit) 
attach(fenit)
head(fenit)

# Diagramas de caja para la 20 muestras
boxplot(pesos ~ muestra, col="orange")

# Gráfico de dispersión para las 20 muestras
plot(muestra, pesos, cex=0.7)  
lines(tapply(pesos,muestra,mean, color="blue")) # Agregar una línea sobre la media de fenitoina
dim(fenit)  # Dimensionalidad de la tabla de datos
fenit1 <- qcc.groups(pesos,muestra) # Agrupación de muestras

# Gráficos de control
obj <- qcc(fenit1, type="xbar") # Gráfico de media
obj1 <- qcc(fenit1, type="R") # Gráfico de rangos
obj2 <- qcc(fenit1, type="S") # Gráfico de desviación estándar 

# Capacidad del proceso
process.capability(obj, spec.limits=c(423,468))  

# Crear un objeto nuevo, para probar ajuste de otras distribuciones
pesos1<-pesos 

# Solicitar los gráficos de QQplot e hipótesis
# Notar que hay 4 posibles distribuciones para ajustar
# Elegir aquella con el menor valor de AIC
res11<-fit.cont(data2fit=pesos1) 

## Instalar la biblioteca qualityTools
# Debido a que es antigua, se requiere atención
if(!require(remotes)){install.packages("remotes")}
install_version("qualityTools", version = "1.55")
library(qualityTools)
str(fenit)

## Gráficos de control de procesos para las 4 distribuciones posibles
# Mejor ajuste para la distribución normal
# Distribución normal
cp(pesos,lsl=423, usl=468)

# Gráficos de control de procesos para otras distribuciones
cp(pesos, "gamma", lsl=423, usl=468)
cp(pesos, "logistic", lsl=423, usl=468)
cp(pesos, "lognormal", lsl=423, usl=468)
cp(pesos, "exponential", lsl=423, usl=468)
cp(pesos, "weibull", lsl=423, usl=468)

# Ajuste al considerar los grupos
cp(pesos,grouping=muestra, lsl=423, usl=468)
