#Linear Regression
#Logistic Regression
#Polynomial Regression
#OLS Ordinary Least Squared

#CRIM - per capita crime rate by town
#ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
#INDUS - proportion of non-retail business acres per town.
#CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
#NOX - nitric oxides concentration (parts per 10 million)
#RM - average number of rooms per dwelling
#AGE - proportion of owner-occupied units built prior to 1940
#DIS - weighted distances to five Boston employment centres
#RAD - index of accessibility to radial highways
#TAX - full-value property-tax rate per $10,000
#PTRATIO - pupil-teacher ratio by town
#Black - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#LSTAT - % lower status of the population
#MEDV - Median value of owner-occupied homes in $1000's

#Borrar todas las variables cargadas.
rm(list=ls()) 


#Cargar el dataset

#install.packages("MASS")
library(MASS) #Functions and datasets to support Venables and Ripley 2002.
data(Boston)  #Service concerning housing in the area of Boston Mass.
View(Boston)
sapply(Boston, class)
str(Boston)
sapply(Boston, typeof)
summary(Boston)

bostcrim<-Boston$crim #per capita crime rate by town
set.seed(2)

#Separar la informacion

#install.packages("caTools")
library(caTools) #Tools: moving window statistics, GIF
split<-sample.split(Boston$medv,SplitRatio = 0.7) #Split Data into Test and Train Set, random. 
split

training_data<-subset(Boston,split==TRUE) # Datos a emplear en el entrenamiento del modelo.
testing_data<-subset(Boston,split==FALSE) # Datos para probar el modelo.

#Definir la correlacion entre las variables.
# CRIM - per capita crime rate by town
# MEDV - Median value of owner-occupied homes in $1000's
plot(Boston$crim,Boston$medv,cex=0.5,xlab="Tasa de criminalidad.",ylab="Precio en miles de USD.")
cr<-cor(Boston)  # Compute the correlation.
cr

#Relation among the variables through scatter plot matrix
attach(Boston)
library(lattice) # Data visualization system inspired by Trellis graphics.

head(Boston)

#Scatter Plot Matrices
splom(~Boston[c(1:6,14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)
splom(~Boston[c(7:14)],groups=NULL,data=Boston,axis.line.tck=0,axis.text.alpha=0)


plot(rm,medv,cex=1,xlab="Habitaciones por vivienda.",ylab="Precio en miles de USD.")
abline(lm(medv~rm),col="red")

plot(medv,rm,cex=1,xlab="Precio en miles de USD.",ylab="Habitaciones por vivienda.")
abline(lm(rm~medv),col="blue")

#Visualizar la correlacion entre las variables.

#install.packages("corrplot")
library(corrplot) #The corrplot package is a graphical display of a correlation matrix.
corrplot(cr,type="lower")
corrplot(cr,method="number")

#Finding multicollinearity
#install.packages("caret")
library(ggplot2)
library(caret) #Classification and Regression Training

Boston_a=subset(Boston,select=-c(medv)) # Remueve la variable valor promedio. 

#Asigna las variables numericas a un nuevo dataframe.
numericData<-Boston_a[sapply(Boston_a,is.numeric)] 
head(numericData)
descrCor<-cor(numericData) # correlation
corrplot(descrCor,method="number")


#Multicorrecionalidad (MuLTICOLLINEARITY)
#Variance inflation factor (VIF) para detectar la multicorrecionalidad.
#install.packages("car")
library(car) #Functions and Datasets to Applied Regression, 2011.
model<-lm(medv~.,data=training_data)
model
summary(model)
vif(model) #Variance Inflation Factors
plot(vif(model))

#VIF de 1 significa que no existe correlacion entre las variables.

#installed.packages("viridisLite")
#install.packages("corrgram")
#library(viridisLite)
#library(viridis)
#library(corrgram)
#corrgram(Boston)

modeleq<-lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+ptratio+black+lstat,data=training_data)
vif(modeleq)
summary(modeleq)

#Eliminar las componentes poco significativas
modelred<-lm(medv~crim+zn+chas+nox+rm+dis+rad+ptratio+black+lstat,data=training_data)
vif(modelred)
summary(modelred)

#Predecir los valores de salida usando el modelo.
predic<-predict(modelred,testing_data) # Function for predictions
predictesting_data$medv
plot(testing_data$medv,type="l",lty=1.8,col="green")
lines(predic,type="l",col="blue")

setwd("C:/Users/lr_29/Desktop/Big Data Goal/R/Proyectos R Studio")
getwd()

sample_data1<-read.csv("sample_data.csv",header=T,sep=",")
sample_data1

#Revisar esto. 
predic<-predict(modelred,sample_data1)
predic
