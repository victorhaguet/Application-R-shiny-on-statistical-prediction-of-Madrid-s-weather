# library

library(tidyverse)
library(leaps)
library(caret)
library(dplyr)
library(nnet)

# Préparation des données
data=read.csv("data/data.csv", stringsAsFactors = TRUE)

data$CET=as.Date(data$CET)
data = data[,-c(14,15,16,19)]
for (i in c(2:17,19)){
  data[i] = replace_na(data[,i], as.integer((mean(data[,i][which(!is.na(data[,i]))]))))
}

data.reg=data[-c(1,18)]

# Coorélation

cor(data.reg)

#============ Régression MeanTemp =============#

#Choix des variables
dataframe=as.data.frame(data.reg)

models=regsubsets(Mean.TemperatureC~. ,data=dataframe)

summary(models)
plot(models,scale ="bic")
plot(models,scale ="r2")
plot(models,scale ="Cp")

# modélisation

reg2=lm(Mean.TemperatureC~lag(Mean.TemperatureC,1)+lag(Max.TemperatureC,1)+lag(Min.TemperatureC,1)+
          lag(Dew.PointC,1)+lag(MeanDew.PointC,1)+lag(Max.Humidity,1)+
          lag(Min.Humidity,1)+lag(Max.Sea.Level.PressurehPa,1)+lag(Min.Sea.Level.PressurehPa,1),data=dataframe)

summary(reg2)
r2=summary(reg2)$r.squared

# prédiction

predMeanTemp=predict(reg2,newdata =dataframe )

plot(predMeanTemp)
lines(dataframe[,3],col="red")
mean(predMeanTemp[-1]-dataframe[-1,3])

data.new=cbind(data,predMeanTemp)

#============ Régression MinTemp =============#

#Choix des variables

models=regsubsets(Min.TemperatureC~. ,data=dataframe)

summary(models)
plot(models,scale ="bic")
plot(models,scale ="r2")
plot(models,scale ="Cp")

# modélisation

regMin=lm(Min.TemperatureC~lag(Max.TemperatureC,1)+
            lag(Mean.TemperatureC,1)+lag(Dew.PointC,1)+lag(MeanDew.PointC,1)+
            lag(Max.Humidity,1)+lag(Min.Humidity,1)+lag(Max.Sea.Level.PressurehPa,1),data=dataframe)

summary(regMin)

# prédiction

predMintemp=predict(regMin,newdata =dataframe )

plot(predMintemp)
lines(dataframe[,4],col="red")
mean(predMintemp[-1]-dataframe[-1,4])
data.new=cbind(data.new,predMintemp)

#============ Régression MaxTemp =============#

#Choix des variables

models=regsubsets(Max.TemperatureC~. ,data=dataframe)
names(data)

summary(models)
plot(models,scale ="bic")
plot(models,scale ="r2")
plot(models,scale ="Cp")

# modélisation

regMax=lm(Max.TemperatureC~lag(Min.TemperatureC,1)+lag(Mean.TemperatureC,1)+lag(MeanDew.PointC,1)+
            lag(Min.Humidity,1)+lag(Mean.Humidity,1)+lag(Min.Sea.Level.PressurehPa,1)+
            lag(Mean.Wind.SpeedKm.h,1)+lag(WindDirDegrees,1),data=dataframe)

summary(regMax)

# prédiction

predMaxtemp=predict(regMax,newdata =dataframe )

plot(predMaxtemp)
lines(dataframe[,2],col="red")
mean(predMaxtemp[-1]-dataframe[-1,2])

data.new=cbind(data.new,predMaxtemp)

#============ Régression taux d'humidité =============#

#Choix des variables

models=regsubsets(Mean.Humidity~. ,data=dataframe)

summary(models)
plot(models,scale ="bic")
plot(models,scale ="r2")
plot(models,scale ="Cp")

# modélisation

regHumi=lm(Mean.Humidity~lag(Max.TemperatureC,1)+lag(Min.TemperatureC,1)+lag(MeanDew.PointC,1)
           +lag(Min.DewpointC,1)+lag(Max.Humidity,1)+lag(Min.Humidity,1)
           +lag(Max.Sea.Level.PressurehPa,1)+lag(Mean.Wind.SpeedKm.h,1),data=dataframe)

summary(regHumi)

# prédiction

predHumi=predict(regHumi,newdata =dataframe )

plot(predHumi)
lines(dataframe[,9],col="red")
mean(predHumi-dataframe[,9])

data.new=cbind(data.new,predHumi)

#============ Régression vitesse du vent =============#

#Choix des variables

models=regsubsets(Mean.Wind.SpeedKm.h~. ,data=dataframe)

summary(models)
plot(models,scale ="bic")
plot(models,scale ="r2")
plot(models,scale ="Cp")

# modélisation

regVent=lm(Mean.Wind.SpeedKm.h~lag(Max.TemperatureC,1)+lag(Min.TemperatureC,1)+lag(MeanDew.PointC,1)
           +lag(MeanDew.PointC,1)+lag(Max.Sea.Level.PressurehPa,1)
           +lag(Mean.Humidity,1)+lag(Min.Sea.Level.PressurehPa,1)+lag(Max.Wind.SpeedKm.h,1)+lag(CloudCover,1),data=dataframe)

summary(regVent)

# prédiction

predVent=predict(regVent,newdata =dataframe )

plot(predVent)
lines(dataframe[,14],col="red")
mean(predVent-dataframe[,14])

data.new=cbind(data.new,predVent)

# préparation des données Events

levels(data$Events) <- c("Pas d'évènement","Brouillard","Pluie","Pluie,grêle et orage","Brouillard pluie et neige","Brouillard et Pluie","Brouillard et orage","Brouillard et neige","Pluie et neige","Pluie et orage","Tornade","Pluie, neige et orage","Neige","Orage","Brouillard,pluie et orage","Pluie et grêle" )

data.reg2=data[-1]

# regression Events

regEvent=multinom(Events~. ,data=data.reg2)


predEvent=lag(predict(regEvent,newdata =data.reg2 ),1)


data.new=cbind(data.new,predEvent)

## Modéle page 2


#Regressions pour le second panel
reg=lm(Mean.TemperatureC~. ,data=data.reg)

summary(reg)


#Choix des variables

regManuelMean=lm(Mean.TemperatureC~lag(Mean.TemperatureC,1)+lag(Mean.TemperatureC,2)+lag(Mean.TemperatureC,3),data=dataframe)

regManuelMin=lm(Min.TemperatureC~lag(Min.TemperatureC,1)+lag(Min.TemperatureC,2)+lag(Min.TemperatureC,3),data=dataframe)

regManuelMax=lm(Max.TemperatureC~lag(Max.TemperatureC,1)+lag(Max.TemperatureC,2)+lag(Max.TemperatureC,3),data=dataframe)

summary(regManuel)
r2Manuel=summary(regManuel)$r.squared

summary(regManuel)$coef[c(1,2,3,4)]

# save a .R file

save(r2, file = "models.Rdata")
