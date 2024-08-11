# Dr. Byron González
# http://byrong.cc

if(!require(car)){install.packages("car")}
if(!require(qcc)){install.packages("qcc")}
if(!require(readxl)){install.packages("readxl")}

# Importar la tabla de datos sobre "coliformes_graficosqc.xlsx"
coliformes<-read_excel("coliformes_graficosqc.xlsx")
head(coliformes)

# Gráficos de control para “manos” ----

attach(coliformes) # Colocar en memoria los campos de la tabla
qqPlot(coliformes$manos)
shapiro.test(coliformes$manos) #Verificar la normalidad 
summary(coliformes) 

# Estadísticos descriptivos de cada campo

boxplot(manos ~ mes, col="orange") # Diagramas de caja 
plot(mes, manos, cex=2, pch=16, col="green") # Gráfico de dispersión 
lines(tapply(manos,mes,mean), lwd=3) # Agrega una línea sobre la media de UFC
abline(h=100, col="blue", lwd=3)
dim(coliformes)  # Dimensionalidad de la tabla de datos

# Solicitar los gráficos de control

manos1 <- qcc.groups(manos,mes) # Crea un objeto con el agrupamiento de UFC por mes
# en función del tamaño de la muestra
R <- qcc(manos1, type="R") # Solicitar un gráfico de rangos
X <- qcc(manos1, type="xbar", nsigmas=3) # Solicitar un gráfico de media
S <- qcc(manos1, type="S") #Solicitar un gráfico de desviación
summary(R)
summary(X)
summary(S)

## Gráficos de control para “superficies” ----

qqPlot(coliformes$superficies)
shapiro.test(coliformes$superficies) #Verificar la normalidad 

# Estadísticos descriptivos de cada campo

boxplot(superficies ~ mes, col="orange") # Diagramas de caja 
plot(mes, superficies, cex=2, pch=16, col="green") # Gráfico de dispersión 
lines(tapply(superficies,mes,mean), lwd=3) # Agrega una línea sobre la media de UFC
abline(h=100, col="blue", lwd=3)
superf<- qcc.groups(superficies,mes) # Crea un objeto con el agrupamiento de UFC por mes
# en función del tamaño de la muestra

Rs <- qcc(superf, type="R") # Solicitar un gráfico de rangos
Xs <- qcc(superf, type="xbar", nsigmas=3) # Solicitar un gráfico de media
Ss <- qcc(superf, type="S") # Solicitar un gráfico de desviación

## Gráficos de control para “leche” ------

qqPlot(coliformes$leche)
shapiro.test(coliformes$leche) # Verificar la normalidad 

# Estadísticos descriptivos de cada campo

boxplot(leche ~ mes, col="orange") # Diagramas de caja 
plot(mes, leche, cex=2, pch=16, col="green") # Gráfico de dispersión 
lines(tapply(leche,mes,mean), lwd=3) # Agrega una línea sobre la media de UFC
abline(h=10, col="blue", lwd=3)
leche1<- qcc.groups(leche,mes) # Crea un objeto con el agrupamiento de UFC por mes
# en función del tamaño de la muestra

Rl <- qcc(leche1, type="R") #Solicitar un gráfico de rangos
Xl <- qcc(leche1, type="xbar", nsigmas=3) #Solicitar un gráfico de media
Sl <- qcc(leche1, type="S") #Solicitar un gráfico de desviación



