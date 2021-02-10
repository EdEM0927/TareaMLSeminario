#Cargar las bibliotecas
library(faraway)#linealidad 
library(tseries)#acf 
library(nortest) #test de normalidad 
library(car) #para qqPlot y levene
library(ggplot2)
library(dplyr)
library(MASS)
library(leaps)
setwd("C:/Users/PROPIETARIO.ACER/Downloads")
datos <- read.csv("covid.csv",stringsAsFactors = TRUE)
names (datos) = c("X","Sexo","Obesidad","Diabetes","Epoc","Asma","Cardiovascular","RenalCronica","Inmunusupresion","Hipertension","TipoPaciente","Edad")
View(datos)
attach(datos)
class(datos)
datos1<-datos %>% mutate(SEXO=ifelse(SEXO=="HOMBRE",1,0))
levels(SEXO)[levels(SEXO)=="HOMBRE"]<-1
levels(SEXO)[levels(SEXO)=="MUJER"]<-0
levels(OBESIDAD)[levels(OBESIDAD)=="SI"]<-1
levels(OBESIDAD)[levels(OBESIDAD)=="NO"]<-0
levels(DIABETES)[levels(DIABETES)=="SI"]<-1
levels(DIABETES)[levels(DIABETES)=="NO"]<-0
levels(EPOC)[levels(EPOC)=="SI"]<-1
levels(EPOC)[levels(EPOC)=="NO"]<-0
levels(CARDIOVASCULAR)[levels(CARDIOVASCULAR)=="SI"]<-1
levels(CARDIOVASCULAR)[levels(CARDIOVASCULAR)=="NO"]<-0
levels(ASMA)[levels(ASMA)=="SI"]<-1
levels(ASMA)[levels(ASMA)=="NO"]<-0
levels(RENAL.CRONICA)[levels(RENAL.CRONICA)=="SI"]<-1
levels(RENAL.CRONICA)[levels(RENAL.CRONICA)=="NO"]<-0
levels(HIPERTENSION)[levels(HIPERTENSION)=="SI"]<-1
levels(HIPERTENSION)[levels(HIPERTENSION)=="NO"]<-0
levels(TIPO.PACIENTE)[levels(TIPO.PACIENTE)=="HOSPITALIZADO"]<-1
levels(TIPO.PACIENTE)[levels(TIPO.PACIENTE)=="AMBULATORIO"]<-0
levels(HIPERTENSION)[levels(HIPERTENSION)=="NO"]<-0
mutate(datos=ifelse(TIPO.PACIENTE=="HOSPITALIZADO",1,0))

datos2<-data.frame(SEXO,HIPERTENSION,RENAL.CRONICA,ASMA,EPOC,DIABETES,OBESIDAD)
str(datos)
###Hacemos nuestra regresion logistica##################
model1<-glm(TIPO.PACIENTE ~ 1, data = datos2, family = "binomial")
model2<-glm(TIPO.PACIENTE ~ ., data = datos2, family = "binomial")
model2<-glm(TIPO.PACIENTE ~ EDAD+factor(OBESIDAD)+factor(DIABETES)+EPOC+ASMA+factor(SEXO),data=datos2)
summary(model1)
summary(model2)
####CalculAmos el stepAICE####################
stepAIC(model1,scope=list(lower=model1,upper=model2),data=datos2,direction = 'forward')
stepAIC(model2,scope=list(lower=model1,upper=model2),data=datos2,direction = 'backward')
stepAIC(model1,scope=list(lower=model1,upper=model2),data=datos2,direction = 'forward')
################Calculamos el criterio del  BIC y Mallowp######## 
models_cp=regsubsets(TIPO.PACIENTE~., data=datos2, nbest=1,nvmax=9)
info=summary(models_cp)
cbind(info$which,round(cbind(rsq=info$rsq, djr2=info$adjr2, bic=info$bic, rss=info$rss),3))
