# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(readxl)){install.packages("readxl")}
if(!require(ggstatsplot)){install.packages("ggstatsplot")}
if(!require(agricolae)){install.packages("agricolae")}

# Importar la tabla de datos
sacarosa<-read_excel("sacarosa.xlsx")

# Prueba de hipótesis acerca de la media 
# Segmentar para Jugo Filtrado Banda
jfb<-subset(sacarosa, muestra=="Jugo Filtrado Banda")
t.test(jfb$ph, alternative="g", conf.level=0.95)

# Prueba de hipótesis entre dos medias independientes

jugof<-subset(sacarosa,
              muestra=="Jugo Filtrado Banda"|
              muestra=="Jugo Filtrado Rotativo")

boxplot(jugof$ph~jugof$muestra, col="orange",
        xlab="Categoría de jugo",
        ylab="Valor de pH")

# Se asume que las varianzas son iguales
t.test(jugof$ph~jugof$muestra, 
       alternative="t", 
       var.equal=T,
       conf.level=0.95)

# Usar el comando `ggbetweenstats()` para visualizar los datos
ggbetweenstats(jugof, x = muestra, y = ph, 
               plot.type = "box",
               type="parametric", var.equal = T)

## Comparación de tres o más grupos ---

# Comparación gráfica
ggbetweenstats(sacarosa, x=muestra, y=ph, 
               var.equal = T, plot.type = "box", type = "parametric")

# Comparación mediante prueba de F
# Análisis de varianza entre muestra
# Anotar que se trata del mismo valor de F que el obtenido antes
# Además se asume que las varianzas son iguales

anova.ph<-lm(sacarosa$ph~sacarosa$muestra)
summary(anova.ph)

result.ph<-aov(sacarosa$ph~sacarosa$muestra)
anova(result.ph)
pr.medias <-LSD.test(result.ph, "sacarosa$muestra",console=TRUE)



