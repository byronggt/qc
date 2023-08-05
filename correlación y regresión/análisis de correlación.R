# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

# Colocar en memoria las bibliotecas a necesitar

if(!require(corrplot)){install.packages("corrplot")}
if(!require(PerformanceAnalytics)){install.packages("PerformanceAnalytics")}
if(!require(ggcorrplot)){install.packages("ggcorrplot")}
if(!require(psych)){install.packages("psych")}
if(!require(GGally)){install.packages("GGally")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(Hmisc)){install.packages("Hmisc")} 
if(!require(plotrix)){install.packages("plotrix")}
if(!require(ellipse)){install.packages("ellipse")}
if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(readxl)){install.packages("readxl")}

# Lectura de la tabla de datos de salinidad
salinidad<-read_excel("salinidad.xlsx")
plot(salinidad)
head(salinidad)

cor(salinidad[,c("pH","CE","Ca2+","Mg2+","Na+","K+","SO4 2-","STD")], use="complete")
rcorr.adjust(salinidad[,c("pH","CE","Ca2+","Mg2+","Na+","K+","SO4 2-","STD")], type="pearson", use="complete")

corrplot(cor(salinidad), method="circle")
corrplot(cor(salinidad), method="ellipse")
corrplot(cor(salinidad), method="number")
corrplot(cor(salinidad), type="upper")
corrplot(cor(salinidad), type="lower")
chart.Correlation(salinidad, histogram=T, pch=15)
sal<-cor(salinidad)
ggcorrplot(sal)
ggcorrplot(sal, type="lower", lab=T)
pairs.panels(salinidad)
ggpairs(salinidad)

# Lectura de la tabla de datos de "salinidad1.xlsx"

df<-read_excel("salinidad1.xlsx")
attach(df)

ggplot(df, aes(x = Ca, y = Mg)) +
  geom_point() +
  stat_ellipse(type = "norm", linetype = 2) # Para asumir una distribución normal multivariante

ggplot(df, aes(x =ce, y = Ca)) +
  geom_point() +
  stat_ellipse(type = "norm", linetype = 3)

# Emplear cor.test

cor.test(Ca,Mg, method = "pearson")
cor.test(ce,Ca, method = "pearson")

# matriz de correlación
mcor<-cor(df)

p.mat <- cor_pmat(mcor) # Calcular los valores de p

ggcorrplot(mcor, hc.order = TRUE,
           type = "lower", p.mat = p.mat)

# Dejar en blanco cuando no hay significancia
ggcorrplot(mcor, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank")

# Construyendo la matriz de varianzas y covarianzas
# En la diagonal principal se ubican las varianzas y fuera de ella las covarianzas 
# entre pares de variables

m<-round(cov(df),2);m

# Debido a cuestiones de unidades de medida, difícilmente identificamos cuando una 
# covarianza es alta o baja

# Construyendo la matriz de correlaciones

round(cor(df),3)

# En R, la función cov2cor() permite convertir una matriz de covarianzas en una matriz
# de correlaciones. A modo de ilustración, considere lo siguiente:

round(cov2cor(m),3)

# Por otra parte, si apenas conocemos la matriz de correlación, no es posible retornar a la
# matriz de covarianzas, a menos que tengamos la información de las varianzas o desviaciones estándar

# Valor de p para todos los pares de correlaciones

rcorr(as.matrix(df))