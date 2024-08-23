# Dr. Byron González
# http://byrong.cc 

if(!require(readxl)){install.packages("readxl")}
if(!require(qcc)){install.packages("qcc")}

# Importar la tabla de datos "fenpos" 

fenit<-read_excel("fenpos.xlsx")
attach(fenit)
fenit1 <- qcc.groups(pesos,muestra)

# Gráficos de control
obj <- qcc(fenit1, type="xbar") # Gráfico de media
obj1 <- qcc(fenit1, type="R") # Gráfico de rangos
obj2 <- qcc(fenit1, type="S") # Gráfico de desviación estándar 

# Capacidad del proceso
process.capability(obj, spec.limits=c(423,468))  
