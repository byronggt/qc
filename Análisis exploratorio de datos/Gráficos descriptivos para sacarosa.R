# Dr. Byron González
# http://cete.fausac.gt

if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(skimr)){install.packages("skimr")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(esquisse)){install.packages("esquisse")}
if(!require(ggstatsplot)){install.packages("ggstatsplot")}
if(!require(readxl)){install.packages("readxl")}
#Lectura de la tabla de datos de sacarosa
sacarosa<-read_excel("sacarosa.xlsx")
ggplot(sacarosa, aes(ph,brix, color=muestra))+
  geom_point(size=4)
ggplot(sacarosa, aes(ph,brix))+
  geom_point(size=4, col="blue", shape=2)+
  facet_wrap(~muestra)
ggplot(sacarosa, aes(brix))+
  geom_histogram(col="yellow")+
  facet_wrap(~muestra)
ggplot(sacarosa, aes(pol))+
  geom_density(col="red")+
  facet_wrap(~muestra)
ggplot(sacarosa, aes(muestra,brix))+
  geom_boxplot(col="blue")
# Análisis exploratorio general de la tabla de datos
sacarosa %>% skim()
sacarosa %>% group_by(muestra) %>% skim()
# Prueba de medias de t independiente (t de Welch)
sac1<- sacarosa %>% 
  select(muestra,ph) %>% 
  filter(muestra=="Jugo Claro" | muestra=="Lodo")
# Usa el comando `ggbetweenstats()` para visualizar los datos
ggbetweenstats(sac1, x = muestra, y = ph, plot.type = "box",
               type="parametric", var.equal = T)
attach(sac1)
t.test(ph~muestra)
dettach(sac1)

# Comparación para tres o más grupos: Andeva de una vía
attach(sacarosa)
ggbetweenstats(sacarosa, x=muestra, y=ph, 
               var.equal = T, plot.type = "box", type = "parametric")

# Análisis de varianza entre muestra
# Anotar que se trata del mismo valor de F
# Además se asume que las varianzas son iguales
if(!require(agricolae)){install.packages("agricolae")}
anova.ph<-lm(ph~muestra)
summary(anova.ph)
r<-factor(seq(1:18))
anova.ph<-cbind(sacarosa,r)
anova.ph
attach(anova.ph)
result.ph<-aov(ph~muestra)
anova(result.ph)
pr.medias <-LSD.test(result.ph, "muestra",console=TRUE)