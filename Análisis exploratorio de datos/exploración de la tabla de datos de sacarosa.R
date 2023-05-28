# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

rm(list=ls())

if(!require(readxl)){install.packages("readxl")}
if(!require(gtsummary)){install.packages("gtsummary")}
if(!require(DataExplorer)){install.packages("DataExplorer")}
if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(carData)){install.packages("carData")}
if(!require(flextable)){install.packages("flextable")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(skimr)){install.packages("skimr")}

# Importar la tabla de datos
sacarosa<-read_excel("sacarosa.xlsx")
head(sacarosa)

# Análisis exploratorio general de la tabla de datos
sacarosa %>% skim()
sacarosa %>% group_by(muestra) %>% skim()

# Características de la tabla de datos
introduce(sacarosa)

# Resumen gráfico porcentual de variables y datos perdidos
plot_intro(sacarosa)

# Resumen porcentual de datos perdidos por variable
plot_missing(sacarosa)

# Histogramas de variables cuantitativas
plot_histogram(sacarosa)

# Diagramas de caja para variables cuantitativas por muestra
box1<-sacarosa[c("muestra","brix", "pol")]
plot_boxplot(box1, by="muestra")

# Medidas de resumen estadístico por categoría de muestra

resumen<-numSummary(sacarosa[,c("brix"), drop=F], groups=sacarosa$muestra,
statistics=c("mean","sd", "IQR", "skewness"))

res1<-data.frame(resumen$table)
tablaT <- flextable(res1)
tablaT <- set_caption(tablaT, "Estadísticos de resumen") %>% 
  theme_vanilla() %>% 
  save_as_docx(path = "mitabla.docx")

# Construir tabla de resumen (ejemplo de mediana e IQR)
sac1<- sacarosa %>% 
    select(muestra,ph,pol) %>% 
    tbl_summary(muestra) %>% 
    add_n()
