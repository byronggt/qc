# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Solicitar las bibliotecas a emplear
if(!require(rriskDistributions)){install.packages("rriskDistributions")}
if(!require(car)){install.packages("car")}
if(!require(univariateML)){install.packages("univariateML")}
if(!require(nortest)){install.packages("nortest")}
if(!require(readxl)){install.packages("readxl")}
if(!require(dplyr)){install.packages("dplyr")}
# Abrir la tabla "alimentos150"
alimentos<-read_excel("alimentos150.xls")
attach(alimentos)
alim2<-alimentos %>%
  select(grupo,kcal) %>% 
  filter(grupo=="fruta")
alim2
detach(alimentos)
attach(alim2)
hist(kcal, freq=F)
lines(density(kcal))
densityPlot(kcal)
curve(dnorm(x, mean(kcal), sd(kcal)), lwd = 2, col = "blue", add = T)
qqPlot(kcal)  #Verificar la normalidad 

# Ho: La distribución de las kcal es normal
# Ha: La distribución de las kcal no es normal

shapiro.test(kcal) #Verificar la normalidad
kcal1<-kcal # Crear un objeto nuevo, para probar ajuste de otras distrib.
res11<-fit.cont(data2fit=kcal1) # Solicitar los gráficos de QQplot e hipótesis
# Seleccionar la distribución con el menor AIC. Es decir, el mejor ajuste

# Ho: La distribución de kcal es igual a la lognormal
# Ha: La distribución de kcal no es igual a la lognormal

summary(kcal) # Resumen de los 5 números más la media aritmética
boxplot(kcal, col="green") # Diagrama de cajas de dispersión 
points(mean(kcal), col = 1, pch = 15)

# Gráfica para visualizar el ajuste
hist(kcal,
     freq = FALSE)
lines(mllnorm(kcal), lwd = 2, lty = 1, col = "blue")
legend(x = 200, y = 0.014, legend = c("lnorm"),
       col = c("blue"), lty = 1:2)
rug(kcal)

# Ajuste de una distribución log-normal 
dist1 <- mllnorm(x = kcal)
summary(dist1)

# Intervalo de confianza del 95% estimados por bootstrapping
bootstrapml(dist1, probs = c(0.05, 0.95), reps = 1000)
detach(alim2)

# Ajuste de distribución normal para glúcidos
qqPlot(alimentos$glucidos)
shapiro.test(alimentos$glucidos)

# Prueba del signo acerca de la mediana de indice < 50
# Ho: mediana signo >= 50
# Ha: mediana signo < 50
alim3<-alimentos %>%
  select(grupo,indice) %>% 
  filter(grupo=="cereal")
alim3
if(!require(BSDA)){install.packages("BSDA")}
SIGN.test(alim3$indice, md=50, alternative = "less", conf.level = 0.90)

# Prueba del signo acerca de la mediana de micronutrientes < 7
# Ho: mediana micronutrientes >= 7
# Ha: mediana micronutrientes < 7
alim4<-alimentos %>%
  select(grupo,micronutrientes) %>% 
  filter(grupo=="cereal")
alim4
if(!require(BSDA)){install.packages("BSDA")}
SIGN.test(alim4$micronutrientes, md=7, alternative = "l", conf.level = 0.98)

