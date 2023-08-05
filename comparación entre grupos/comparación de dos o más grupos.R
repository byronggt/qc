# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(readxl)){install.packages("readxl")}
if(!require(ggstatsplot)){install.packages("ggstatsplot")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(performance)){install.packages("performance")}
if(!require(ggplot2)){install.packages("ggplot2")}

# Importar la tabla de datos
sacarosa<-read_excel("sacarosa.xlsx")

# Prueba de hipótesis acerca de la media 
# Segmentar para Jugo Filtrado Banda
jfb<-subset(sacarosa, muestra=="Jugo Filtrado Banda")
t.test(jfb$ph, alternative="g", mu=7, conf.level=0.95)

# Prueba de hipótesis entre dos medias independientes

jugof<-subset(sacarosa,
              muestra=="Jugo Filtrado Banda"|
              muestra=="Jugo Filtrado Rotativo")
head(jugof)
tail(jugof)

windows(10,10)
ggplot(jugof, aes(ph)) +
  geom_dotplot(aes(fill=muestra), binwidth = 0.5) +
  labs(x = "Valores de ph", y = "")


boxplot(jugof$ph~jugof$muestra, col="orange",
        xlab="Categoría de jugo",
        ylab="Valor de pH")

# Se asume que las varianzas son iguales
t.test(jugof$ph~jugof$muestra, 
       alternative="t", 
       var.equal=T,
       conf.level=0.95)

jfb<-subset(sacarosa,muestra=="Jugo Filtrado Banda")
jfr<-subset(sacarosa,muestra=="Jugo Filtrado Rotativo")
var.test(jfb$ph,jfr$ph)

# Usar el comando `ggbetweenstats()` para visualizar los datos
ggbetweenstats(jugof, x = muestra, y = ph, 
               plot.type = "box",
               type="parametric", var.equal = T)

## Comparación de tres o más grupos ---

# Diagramas de caja
boxplot(sacarosa$ph~sacarosa$muestra
        , col="orange"
        , xlab = "Muestra"
        , ylab = "Valor de ph")

# Comparación gráfica
ggbetweenstats(sacarosa, x=muestra, y=ph, 
               var.equal = T, plot.type = "box", type = "parametric")

# Comparación mediante prueba de F
# Análisis de varianza entre muestras
# Anotar que se trata del mismo valor de F que el obtenido antes
# Además se asume que las varianzas son iguales

# anova.ph<-lm(ph~muestra, data = sacarosa)
# summary(anova.ph)
# windows(10,10)
# check_model(anova.ph)

result.ph<-aov(ph~muestra, data = sacarosa)
anova(result.ph)
pr.medias <-LSD.test(result.ph, "muestra",console=TRUE)



