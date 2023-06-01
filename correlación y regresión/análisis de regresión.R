# Dr. Byron González
# Dr. Ezequiel López
# http://cete.fausac.gt

if(!require(performance)){install.packages("performance")}
if(!require(readxl)){install.packages("readxl")}

salinidad<-read_excel("salinidad1.xlsx")
names(salinidad)
cor(salinidad)

# Modelo para explicar el magnesio

model1<-lm(mg~ce, data=salinidad)
summary(model1)
windows(10,10)
check_model(model1)


# Modelo para explicar el sulfato

model3<-lm(so~ce+pH, data=salinidad)
summary(model3)
windows(10,10)
check_model(model3)
