# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

rm(list=ls())

if(!require(readxl)){install.packages("readxl")}
if(!require(gtsummary)){install.packages("gtsummary")}
if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(flextable)){install.packages("flextable")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(lattice)){install.packages("lattice")}
if(!require(rempsyc)){install.packages("rempsyc")}
if(!require(dplyr)){install.packages("dplyr")}


# 1. Importar la tabla de datos -----
sacarosa<-read_excel("sacarosa.xlsx")
head(sacarosa)

# 2. Resumen de los 5 números + la media ------
summary(sacarosa)

# 3. Medidas de resumen estadístico por categoría de muestra

res1<-numSummary(sacarosa[,c("brix"), drop=F], groups=sacarosa$muestra,
                    statistics=c("mean","sd", "IQR", "skewness"))
res1

resumen <- sacarosa %>%
  group_by(muestra) %>%
  summarise(Media = mean(pol),
            Mediana = median(pol),
            Desviación = sd(pol),
            Minimo = min(pol),
            Asimetría = skewness(pol),
            IQR = IQR(pol))
resumen %>% 
  flextable() %>% 
  autofit () %>% 
  save_as_docx(path = "res_med_descriptivas.docx")


# 4. Box plot por categoría de muestra para pol ------
attach(sacarosa)
boxplot(pol~muestra, col="orange")


# 5. Histograma por categoría de muestra para pol ------
ggplot(sacarosa, aes(pol))+
  geom_histogram(col="blue")+
  facet_wrap(~muestra)+
  labs(x="Contenido de pol en %", y="Número de muestras")

# 5a. Alternativa para obtener histogramas ------
m1<-as.factor(muestra)
histogram(~pol|m1, col="green", type = "count",
          xlab = "Contenido de pol en %",
          ylab = "Número de muestras")

histogram(~pol|m1, col="green", type = "percent",
          xlab = "Contenido de pol en %",
          ylab = "Porcentaje de muestras")

# Categorization de variables

sacarosa$ph_categ<- cut(sacarosa$ph, breaks = c(-Inf, 5.5, 7.5, Inf), labels = c("bajo", "medio", "alto"))
sacarosa$ph_categ

## Construir una tabla de contingencia
# Tabla con frecuencias
myp<-table(sacarosa$muestra,sacarosa$ph_categ, dnn=c("muestra","categoría de ph")); myp
addmargins(myp)

#tabf<-as.data.frame.matrix(myp); tabf
#nice_table(tabf)
#tablaF <- flextable(tabf, col_keys = names(tabf)); tablaF # Faltan nombres
#tablaT <- set_caption(tablaF, "Tabla de contingencia") %>% 
#  theme_vanilla() %>% 
#  save_as_docx(path = "mitabla1.docx")

# Tabla con proporciones
tablap<-prop.table(myp); tablap


