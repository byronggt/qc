# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(performance)){install.packages("performance")}
if(!require(jtools)){install.packages("jtools")}
if(!require(readxl)){install.packages("readxl")}

salinidad<-read_excel("salinidad1.xlsx")
names(salinidad)
head(salinidad)
cor(salinidad)

# Modelo para explicar el magnesio
attach(salinidad)
plot(ce,mg)
model1<-lm(mg~ce, data=salinidad)
abline(model1, col="red", lwd=3)
salinidad$pred_mg<-model1$fitted.values
salinidad$res_mg<-model1$residuals
head(salinidad)
summary(model1)
windows(10,10)
check_model(model1)
check_normality(model1)

plot(log(ce),log(mg))
model2<-lm(log(mg)~log(ce))
abline(model2, pred = mg, col="red", lwd=2)
summary(model2)
windows(10,10)
check_model(model2)
check_normality(model2)
plot(ce,mg)

# Convertir el modelo lineal transformado
# a un modelo de potencia de la forma mg=a*ce^b
# El modelo original es Log(mg)=-6.9385+1.1773*Log(ce)
# Antilogaritmo a -6.9385
a<-exp(-6.9385); a

# Modelo potencial: mg=0.0009697231*ce^1.1773
# Graficar el modelo potencial
plot(ce,mg)
curve(0.0009697231*x^1.1773, add=TRUE, col="blue", lwd=2)

# Modelo para explicar el sulfato

model3<-lm(so~ce+pH, data=salinidad)
summary(model3)
windows(10,10)
check_model(model3)
check_normality(model3)

# Eliminar pH del modelo
model4<-lm(so~ce, data=salinidad)
summary(model4)

# Eliminar el intercepto
model5<-lm(so~ce-1, data=salinidad)
summary(model5)
windows(10,10)
check_model(model5)
check_normality(model5)
plot(ce,so)

# Ajustar también un modelo potencial
# Gráfico de ajuste
effect_plot(model5, pred = ce, interval = TRUE, plot.points = TRUE, x.lab="Conductividad eléctrica", y.lab="Sulfatos")
