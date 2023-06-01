# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(RcmdrMisc)){install.packages("RcmdrMisc")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(ggcorrplot)){install.packages("ggcorrplot")}
if(!require(readxl)){install.packages("readxl")}

salinidad<-read_excel("salinidad.xlsx")
head(salinidad)
str(salinidad)
pairs(salinidad)

# Matriz de correlaciones

cor(salinidad[,c("pH","CE","Ca2+","Mg2+","Na+","K+","SO4 2-","STD")], use="complete")
rcorr.adjust(salinidad[,c("pH","CE","Ca2+","Mg2+","Na+","K+","SO4 2-","STD")], type="pearson", use="complete")

# Correlograma de tipo círculo
corrplot(cor(salinidad), method="circle")

# Correlograma de tipo elipse
corrplot(cor(salinidad), method="ellipse")

# Correlograma de tipo número
corrplot(cor(salinidad), method="number")

# Correlograma de tipo matriz superior
corrplot(cor(salinidad), type="upper")

# Correlograma de tipo matriz inferior
corrplot(cor(salinidad), type="lower")

# Correlograma de tipo mapa de calor
sal<-cor(salinidad)
ggcorrplot(sal)
ggcorrplot(sal, type="lower", lab=T)

