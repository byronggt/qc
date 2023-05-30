# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

rm(list=ls())

if(!require(readxl)){install.packages("readxl")}
if(!require(gtsummary)){install.packages("gtsummary")}
if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(flextable)){install.packages("flextable")}
if(!require(ggplot2)){install.packages("ggplot2")}


# 1. Importar la tabla de datos -----
sacarosa<-read_excel("sacarosa.xlsx")
head(sacarosa)

# 2. Resumen de los 5 números + la media ------
summary(sacarosa)

# 3. Medidas de resumen estadístico por categoría de muestra

resumen<-numSummary(sacarosa[,c("brix"), drop=F], groups=sacarosa$muestra,
                    statistics=c("mean","sd", "IQR", "skewness"))

res1<-data.frame(resumen$table)
tablaT <- flextable(res1)
tablaT <- set_caption(tablaT, "Estadísticos de resumen") %>% 
  theme_vanilla() %>% 
  save_as_docx(path = "mitabla.docx")

# 4. Box plot por categoría de muestra para pol
attach(sacarosa)
boxplot(pol~muestra, col="orange")


# 5. Histograma por categoría de muestra para pol
ggplot(sacarosa, aes(pol))+
  geom_histogram(col="blue")+
  facet_wrap(~muestra)+
  labs(x="Contenido de pol en %", y="Número de muestras")

