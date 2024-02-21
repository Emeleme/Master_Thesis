install.packages("tidyr")
install.packages("dplyr")
install.packages("readr")
install.packages("cli")
install.packages("devtools")
install.packages("rlang")

library(tidyr)
library(dplyr)
library(readr)
library(circular)

data<-(EmlenFunnel)
colnames(data) <- c('Ring','0','15','30','45','60','75','90','105','120','135','150','165','180','195','210','225','240','255','270','285','300','315','330','345','TotalScratches')
datalong<-pivot_longer(
  data,
  cols=c("0","15","30","45","60","75","90","105","120","135","150","165","180","195","210","225","240","255","270","285","300","315","330","345"),
  names_to="sector",
  values_to="count")

#-----
library(tidyverse)
library(bpnreg)
library(data.table)
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

datalong_rep1<-data.frame(Ring=rep(datalong_EmlenInd$Ring, times=datalong_EmlenInd$count), NumericID=rep(datalong_EmlenInd$NumericID, times=datalong_EmlenInd$count),Treatment=rep(datalong_EmlenInd$Treatment, times=datalong_EmlenInd$count), BreedingDistribution=rep(datalong_EmlenInd$BreedingDistribution, times=datalong_EmlenInd$count), degree=rep(datalong_EmlenInd$sector, times=datalong_EmlenInd$count))
datalong_rep1$degree<-as.circular(datalong_rep1$degree)
datalong_rep1$degree<-deg2rad(datalong_rep1$degree)
datalong_rep1$degree<-as.circular(datalong_rep1$degree, type="radians")
datalong_rep1$NumericID<-as.numeric(datalong_rep1$NumericID)

str(datalong_rep1)

aa<- bpnme(degree~Treatment+(1|NumericID), data=datalong_rep1, its=100, burn=10, n.lag=3, seed=11)

#-----
  

datalong_rep<-data.frame(ring=rep(datalong$Ring, times=datalong$count), degree=rep(datalong$sector, times=datalong$count))
datalong_degree<-as.data.frame(as.numeric(unlist(datalong)))
datalong_degree<-as.circular(datalong_degree, type="angles", units="degrees", template="geographics", modulo="2pi", zero=0, rotation="counter")
plot(datalong_degree, cex=1.5, bin=720, stack=FALSE, sep=0.035, shrink=1.3)

control<-subset(datalong_rep, )
data_rep_1<-subset(datalong_rep, ring=="1EA76002")
datalong_degree_1<-as.data.frame(as.numeric(unlist(data_rep_1)))
datalong_degree_1<-as.circular(datalong_degree_1, type="angles", units="degrees", template="geographics", modulo="2pi", zero=0, rotation="counter")
plot(datalong_degree_1, type="n", main = data_rep_1$ring[1], cex=0.3, bin=720, stack=TRUE, sep=0.035, shrink=)
axis.circular(at=circular(seq(0, 7*pi/4,pi/4)), labels=c("E","NE","N","NW","W","SW","S","SE"), rotation= "counter", cex=1)
rose.diag(datalong_degree_1, bins=24, col="skyblue2", cex=1, prop=2.5, add=TRUE)

###

# Create a sample data frame
df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))

# Create vectors of replication counts for each column
x_counts <- c(2, 3, 1)
y_counts <- c(3, 1, 2)

# Replicate each column of the data frame based on the counts vectors
replicated_df <- data.frame(x = rep(df$x, times = x_counts), y = rep(df$y, times = y_counts))

# Print the original and replicated data frames side-by-side
cbind(df, replicated_df)


