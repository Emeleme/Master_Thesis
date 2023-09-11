library(tidyverse)
library(bpnreg)
library(data.table)
library(circular)

Emlen_IndData<-read.table("E:/Users/mlmah/OneDrive/Documentos/MLME/Maestria/Animal Ecology 2022-2024/Tesis/Database/EmlenData_Spring2023.txt", h=T)

data<-(Emlen_IndData)

setnames(Emlen_IndData, old=c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24"), 
         new = c("0","15","30","45","60","75","90","105","120","135","150","165","180","195","210","225","240","255","270","285","300","315","330","345"), skip_absent=TRUE)
deg2rad <- function(deg) {(deg * pi) / (180)}

datalong_EmlenInd<-pivot_longer(
  data,
  cols=c("0","15","30","45","60","75","90","105","120","135","150","165","180","195","210","225","240","255","270","285","300","315","330","345"),
  names_to="sector",
  values_to="count")

datalong_rep1<-data.frame(Ring=rep(datalong_EmlenInd$Ring, times=datalong_EmlenInd$count), NumericID=rep(datalong_EmlenInd$NumericID, times=datalong_EmlenInd$count),Treatment=rep(datalong_EmlenInd$Treatment, times=datalong_EmlenInd$count), BreedingDistribution=rep(datalong_EmlenInd$BreedingDistribution, times=datalong_EmlenInd$count), direction=rep(datalong_EmlenInd$sector, times=datalong_EmlenInd$count))
datalong_rep1$direction<-as.numeric(datalong_rep1$direction)
datalong_rep1$direction<-as.vector(datalong_rep1$direction)
datalong_rep1$direction<-deg2rad(datalong_rep1$direction)
datalong_rep1$direction<-as.circular(datalong_rep1$direction, units = "radians", type = "angles")
datalong_rep1$NumericID<-as.numeric(datalong_rep1$NumericID)
datalong_rep1$Treatment<-as.factor(datalong_rep1$Treatment)

str(datalong_rep1)

#aaa<-bpnme(data=datalong_rep1, pred.I = datalong_rep1$direction + (1|datalong_rep1$NumericID))

aaa <- bpnme(pred.I=direction ~ Treatment + (1 | NumericID), data = datalong_rep1, its = 1)

#aaa<-bpnme(pred.I = datalong_rep1$direction + (1|datalong_rep1$NumericID), 
#           data = datalong_rep1, 
#           its=1)
#aa<- bpnme(degree~Treatment+(1|NumericID), data=datalong_rep1, its=1)
#aa
summary(aaa)