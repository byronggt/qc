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
if(!require(explore)){install.packages("explore")}
# Ejecutar las siguientes 2 líneas una sola vez
install.packages("devtools")
devtools::install_github("agstn/dataxray")

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

# Otra descripción de la tabla de datos
sacarosa %>% explore()

sacarosa %>% select(muestra) %>% explore()

# * Descripción ----

sacarosa %>% describe_all()

sacarosa %>% describe_cat(muestra)

# * Exploración de todas las variables ----

sacarosa %>%
  explore_all(
    target = muestra,
    ncol   = 3
  )

# * Exploración con gráfica bivariada ----
sacarosa %>%
  explore(
    target = ph,
    var    = ph,
    var2   = muestra,
    col="green"
  )

# * Reporte ----
sacarosa %>%
  report(
    target      = ph,
    output_dir  = "reporte/",
    output_file = "explore_plots.html"
  )

# Data X-Ray ----

# 1. Exploración sin grupos ----
sacarosa %>%
  make_xray() %>%
  view_xray()
