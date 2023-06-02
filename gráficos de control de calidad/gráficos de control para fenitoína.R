# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(car)){install.packages("car")}
if(!require(qcc)){install.packages("qcc")}
if(!require(readxl)){install.packages("readxl")}

# Importar la tabla de datos "fenpos10.xlsx"
fenpos10<-read_excel("fenpos10.xlsx")
head(fenpos10)

attach(fenpos10) # Colocar en memoria los campos de la tabla
qqPlot(fenpos10$pesos)
shapiro.test(fenpos10$pesos) # Verificar la normalidad 
summary(fenpos10) 

# Estadísticos descriptivos de cada campo
boxplot(pesos ~ muestra, col="orange") # Diagramas de caja 
plot(muestra, pesos, cex=2, pch=16, col="green") # Gráfico de dispersión 
lines(tapply(pesos,muestra,mean), lwd=3) # Agrega una línea sobre la media de diámetro
dim(fenpos10)  # Dimensionalidad de la tabla de datos

# Solicitar los gráficos de control

pesos1 <- qcc.groups(pesos,muestra) # Crea un objeto con el agrupamiento de diámetros
#en función del tamaño de la muestra

X <- qcc(pesos1, type="xbar") # Solicitar un gráfico de media
R <- qcc(pesos1, type="R") # Solicitar un gráfico de rangos
S <- qcc(pesos1, type="S") # Solicitar un gráfico de desviación
summary(X)
summary(R)
summary(S)
