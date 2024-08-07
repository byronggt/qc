# Dr. Byron González
# http://byrong.cc

if(!require(readxl)){install.packages("readxl")}
if(!require(qcc)){install.packages("qcc")}

## Diagrama de Pareto -----

causa<-c("manos","superficies","fresas","leche","agua")
suma<-c(28,8,0,6,0)
coliformes<-data.frame(causa,suma)
defectos <- coliformes$suma
names(defectos) <- coliformes$causa 
windows(10,10)
pareto.chart(defectos, ylab = "Frecuencia de contaminación por coliformes", 
             col=heat.colors(length(defectos)))

## Diagrama de espina de pescado -----

windows(10,7)
cause.and.effect(
  cause=list(Personal = c("vestimenta", "pre Lavado de manos"),
             Materia_prima = c("frutas","verduras","leche"),
             Superficies = c("cuchillos","recipientes","accesorios eléctricos","tablas de picado"),
             Agua = c("agua purificada", "agua de grifo")),
  effect= "contaminación microbiana", 
  title= "Diagrama de causa y efecto"
)

