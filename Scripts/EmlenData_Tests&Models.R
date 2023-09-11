setwd("E:/Users/mlmah/OneDrive/Documentos/MLME/Maestria/Animal Ecology 2022-2024/Tesis/Database")
#read .csv data file and assign it a name
Emlen_IndData_2<-read.table("E:/Users/mlmah/OneDrive/Documentos/MLME/Maestria/Animal Ecology 2022-2024/Tesis/Database/EmlenData_Spring2023.txt", h=T)

#check data
names(Emlen_IndData_2) #returns names of the columns
head(Emlen_IndData_2) #returns first 6 rows
dim(Emlen_IndData_2) #returns dimensions - rows and columns
str(Emlen_IndData_2) #compilation of the above
#load libraries
library(circular)
library(dplyr)
library(data.table)
library(tidyr)
library(dplyr)
library(readr)
library(remotes)
library(bpnreg)
library(CircStats)

# ANALYSIS FOR INDIVIDUAL BIRDS - UNIMODAL --------------------------------------
#import my data, update column name and assigning a new row as heading
setnames(Emlen_IndData_2, old=c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24"), 
         new = c("0","15","30","45","60","75","90","105","120","135","150","165","180","195","210","225","240","255","270","285","300","315","330","345"), skip_absent=TRUE)

Emlen_IndData_2<-tibble::rowid_to_column(Emlen_IndData_2, "Identificator")
deg2rad <- function(deg) {(deg * pi) / (180)} #converts degrees to radians.
rad2deg <- function(rad) {(rad * 180) / (pi)} #converts radians to degrees.

Emlen_IndData_2$Date<-lubridate::dmy(Emlen_IndData_2$Date)
str(Emlen_IndData_2)
Emlen_IndData_2 <- Emlen_IndData_2 %>%
  mutate(Week = case_when(
    between(Date, as.Date("2023-04-20"), as.Date("2023-04-26")) ~ 1,
    between(Date, as.Date("2023-04-28"), as.Date("2023-05-04")) ~ 2,
    between(Date, as.Date("2023-05-08"), as.Date("2023-05-14")) ~ 3,
    between(Date, as.Date("2023-05-16"), as.Date("2023-05-22")) ~ 4,
    between(Date, as.Date("2023-05-25"), as.Date("2023-05-31")) ~ 5,
    TRUE ~ 6 # Defining values for each week with date intervals
  ))

View(Emlen_IndData_2)


datalong_EmlenInd_2<-pivot_longer( #makes it a frequency to rep after
  Emlen_IndData_2,
  cols=c("0","15","30","45","60","75","90","105","120","135","150","165","180","195","210","225","240","255","270","285","300","315","330","345"),
  names_to="sector",
  values_to="count")

uniqueValues_ID<-unique(datalong_EmlenInd_2$Identificator)

results_df <- data.frame(Identificator = uniqueValues_ID,   #create an empty dataframe to store results
                         test_statistic = numeric(length(uniqueValues_ID)),
                         p_value = numeric(length(uniqueValues_ID)),
                         Mean = numeric(length(uniqueValues_ID)),
                         rho= numeric(length(uniqueValues_ID)))

# Loop through unique identifiers
for (i in uniqueValues_ID) {
  subset_data <- subset(datalong_EmlenInd_2, Identificator == i) #subsets data for each identificator/each entry in the database
  IndData <- as.data.frame(as.numeric(unlist(subset_data)))
  IndData <- na.omit(IndData)
  longdata <- as.numeric(rep(subset_data$sector, times = subset_data$count)) #repeats the degrees the times we counted already
  longdata <- deg2rad(longdata)  # Convert to radians
  rayleigh_test <- rayleigh.test(longdata) #performs rayleigh test for each entry in the database
  r1 <- rho.circular(longdata, na.rm = FALSE)
  test_statistic <- rayleigh_test$statistic
  p_value <- rayleigh_test$p.value
  MeanDir<- mean.circular(longdata)
  
  # Convert the mean direction from radians to degrees and correct negative values
  MeanDir_deg <- rad2deg(MeanDir)  # Convert to degrees
  if (MeanDir_deg < 0) {
    MeanDir_deg <- 360 + MeanDir_deg  # Wrap around negative values
  }
  
  # Assign results to the correct rows
  row_index <- which(results_df$Identificator == i)
  results_df$rho[row_index] <- r1
  results_df$test_statistic[row_index] <- test_statistic
  results_df$p_value[row_index] <- p_value
  results_df$Mean[row_index]<- MeanDir_deg
}

View(results_df)

mergedEmlen<-merge(results_df, Emlen_IndData_2, by = "Identificator") #merge both dataframes by identificator
View(mergedEmlen)

mergedEmlen$Mean<-as.circular(deg2rad(mergedEmlen$Mean))
mergedEmlen<-filter(mergedEmlen, p_value<0.05)
str(mergedEmlen)
View(mergedEmlen)

#To check unique values and how many data points a ring has to perform a paired test
uniqueValues_Ring<-unique(mergedEmlen$Ring)
length(uniqueValues_Ring)

occurrences <- mergedEmlen %>% count(Ring) #to see how many times a ring occours in the dataframe (this is because we need to have the same size if we want to do a paired test)
print(occurrences)

#tO TESTS
EmlenData_ForTest<- dplyr::select(mergedEmlen, c("Identificator", "test_statistic","p_value", "rho", "Mean", "Species","Group","Treatment","Ring","BreedingDistribution", "Week"))

Emlen_local<-filter(EmlenData_ForTest, Group=="local") #just local
Emlen_local<-Emlen_local %>%
  group_by(Ring) %>%
  summarise(circular_mean = mean.circular(Mean))

Emlen_total<-filter(EmlenData_ForTest, Group=="total") #just 24h treatment

nrow(Emlen_local)
nrow(Emlen_total)

#watson.williams.test FOR ALL THE DATA

Watson_data <- list(
  Emlen_total$Mean <- as.circular(Emlen_total$Mean, units="degrees", template="geographics"),
  Emlen_local$circular_mean <- as.circular(Emlen_local$circular_mean, units="degrees", template="geographics")
)
watson.williams.test(Watson_data) #Watson test not paired



#To perform a paired test (Hotelling test)
#Circular_mean=control, mean= treatment

common_ids <- intersect(Emlen_local$Ring, Emlen_total$Ring)

# Filter dataframes to include only common IDs
Emlen_local_ID <- Emlen_local[Emlen_local$Ring %in% common_ids, ]
Emlen_total_ID <- Emlen_total[Emlen_total$Ring %in% common_ids, ]

nrow(Emlen_local_ID)
nrow(Emlen_total_ID)

Emlen_hotelling<-merge(Emlen_local_ID, Emlen_total_ID, by="Ring")
Emlen_hotelling<-dplyr::select(Emlen_hotelling, c("Ring","circular_mean","Mean", "Week"))
Emlen_hotelling$circular_mean<-as.vector(rad2deg(Emlen_hotelling$circular_mean))
Emlen_hotelling$circular_mean <- (Emlen_hotelling$circular_mean + 360) %% 360
Emlen_hotelling$Mean<-as.vector(rad2deg(Emlen_hotelling$Mean))

##Intentemos restarlos - para calcular el cambio angular

Emlen_hotelling$sustraction<-as.vector(Emlen_hotelling$circular_mean-Emlen_hotelling$Mean) #To calculate the difference between the agles
Emlen_hotelling$AngularChange<-case_when(Emlen_hotelling$sustraction > 180 ~ (Emlen_hotelling$sustraction - 360) #cuando un angulo es mayor a 180 para calcular su angulo complementario + dirección
                                   ,Emlen_hotelling$sustraction < -180 ~ (Emlen_hotelling$sustraction + 360) #cuando un angulo es menor de 180 para calcular su angulo complementario + su dirección
                                   ,TRUE ~ Emlen_hotelling$sustraction
)

#Emlen_hotelling$AngularChange <- as.circular(deg2rad(as.numeric(Emlen_hotelling$AngularChange)), type="angles", units="radians")
Emlen_hotelling$AngularChange <- as.circular(Emlen_hotelling$AngularChange, type="angles", units="degrees", template="geographics")

options(digits = 4) 

densityAll<-density(Emlen_hotelling$AngularChange, bw=5)
r <- rho.circular(Emlen_hotelling$AngularChange, na.rm = FALSE)
ray_all<-rayleigh.test(Emlen_hotelling$AngularChange)
plot(densityAll, main = "Angular change", points.plot=TRUE, xlim=c(-1.5,1), ylim=c(-1.1, 1.5))
arrows.circular(mean.circular(Emlen_hotelling$AngularChange), y=r, length=0.1)
legend(x="bottomleft", legend =(paste("p-value= ", sprintf("%3.2f",ray_all$p.value), sep="")))


Week1_AnCh<-filter(Emlen_hotelling, Week=="1")
week1.density<-density(Week1_AnCh$AngularChange, bw=5)
ray_w1<-rayleigh.test(Week1_AnCh$AngularChange)
rw1 <- rho.circular(Week1_AnCh$AngularChange, na.rm = FALSE)
plot(week1.density, main = "Angular change Week 1", points.plot=TRUE, xlim=c(-1.5,1), ylim=c(-1.1, 1.5))
arrows.circular(mean.circular(Week1_AnCh$AngularChange), y=rw1, length=0.1)
legend(x="bottomleft", legend =(paste("p-value= ", sprintf("%3.2f",ray_w1$p.value), sep="")))


Week2_AnCh<-filter(Emlen_hotelling, Week=="2")
week2.density<-density(Week2_AnCh$AngularChange, bw=5)
ray_w2<-rayleigh.test(Week2_AnCh$AngularChange)
rw2 <- rho.circular(Week2_AnCh$AngularChange, na.rm = FALSE)
plot(week2.density, main = "Angular change Week 2", points.plot=TRUE, xlim=c(-1.5,1), ylim=c(-1.1, 1.5))
arrows.circular(mean.circular(Week2_AnCh$AngularChange), y=rw2, length=0.1)
legend(x="bottomleft", legend =(paste("p-value= ", sprintf("%3.2f",ray_w2$p.value), sep="")))


Week3_AnCh<-filter(Emlen_hotelling, Week=="3")
week3.density<-density(Week3_AnCh$AngularChange, bw=5)
ray_w3<-rayleigh.test(Week3_AnCh$AngularChange)
rw3 <- rho.circular(Week1_AnCh$AngularChange, na.rm = FALSE)
plot(week3.density, main = "Angular change Week 3", points.plot=TRUE, xlim=c(-1.5,1), ylim=c(-1.1, 1.5))
arrows.circular(mean.circular(Week3_AnCh$AngularChange), y=rw3, length=0.1)
legend(x="bottomleft", legend =(paste("p-value= ", sprintf("%3.2f",ray_w3$p.value), sep="")))





colnames(Emlen_hotelling)=c("Ring", "Control_Mean", "Treatment_Mean", "Week", "Difference", "Angular_Change") #Change colnames because is confusing

source("https://raw.githubusercontent.com/olitroski/circular/master/paired.hotelling.r") #para correr el hotelling test. p.value<0.05 there are differences between the mean in the samples
paired.hotelling(Emlen_hotelling$Control_Mean, Emlen_hotelling$Treatment_Mean)




#And for a circular ANOVA
Emlen_Anova<-Emlen_hotelling
colnames(Emlen_Anova)<-c("Ring","local","total","Week") #REVISAR!!
#Emlen_Anova<-pivot_longer(data= Emlen_Anova, 
 #                         cols = "local":"total",
  #                        names_to = "Group",
   #                       values_to = "Mean")
#Emlen_Anova$Mean<-as.circular(deg2rad(Emlen_Anova$Mean))



emlenanova <- bpnr(pred.I = Mean ~ Group + Week, data = Emlen_Anova,
                  its = 10000, burn = 100, n.lag = 3, seed = 101)
summary(emlenanova)
emlenanova
coef_circ(emlenanova, type = "continuous", units = "degrees")
coef_circ(emlenanova, type = "categorical", units = "degrees")
fit(emlenanova)

#ESTA MONDA SIGUE SIN FUNCIONAR Y NO SE POR QUE!!!!!
# datalong_rep1<-data.frame(Ring=rep(datalong_EmlenInd_2$Ring, times=datalong_EmlenInd_2$count), NumericID=rep(datalong_EmlenInd_2$NumericID, times=datalong_EmlenInd_2$count),Treatment=rep(datalong_EmlenInd_2$Treatment, times=datalong_EmlenInd_2$count), BreedingDistribution=rep(datalong_EmlenInd_2$BreedingDistribution, times=datalong_EmlenInd_2$count), degree=rep(datalong_EmlenInd_2$sector, times=datalong_EmlenInd_2$count))
# datalong_rep1$degree<-as.numeric(datalong_rep1$degree)
# datalong_rep1$degree<-deg2rad(datalong_rep1$degree)
# datalong_rep1$degree<-as.circular(datalong_rep1$degree, units="radians", type="angles")
# datalong_rep1$NumericID<-as.numeric(datalong_rep1$NumericID)
# datalong_rep1$Treatment<-as.factor(datalong_rep1$Treatment)
# datalong_rep1$BreedingDistribution<-as.factor(datalong_rep1$BreedingDistribution)
# 
# str(datalong_rep1)
# 
# aa<- bpnme(degree ~ Treatment + (1|NumericID), data=datalong_rep1, its=100) #, burn=10, n.lag=3, seed=11)








########

#IndRing<-as.data.frame(as.numeric(unlist(Emlen_ind_long)))
#mean_direction<-mean.circular(longdata)
#Emlen_IndData_2$mean_direction[i]<-mean_direction

#datalong_rep_EmlenInd<-data.frame(ring=rep(datalong_EmlenInd$Ring, times=datalong_EmlenInd$count), degree=rep(datalong_EmlenInd$sector, times=datalong_EmlenInd$count))

#unique_values <- unique(datalong_rep_EmlenInd$ring)




#for (i in uniqueValues_ID) {
 # subset_data <- subset(datalong_EmlenInd_2, Identificator == i)
#  IndData<-as.data.frame(as.numeric(unlist(subset_data)))
#  IndData<-na.omit(IndData)
 # longdata<-rep(subset_data$sector, times=subset_data$count)
#  longdata<-deg2rad(longdata)
 # longdata<-as.circular(longdata)
#  rayleigh_test <- rayleigh.test(longdata)
 # r1 <- rho.circular(longdata, na.rm = FALSE)
#   test_statistic <- rayleigh_test$statistic
#   p_value <- rayleigh_test$p.value
#   Emlen_IndData_2$test_statistic[i] <- test_statistic
#   Emlen_IndData_2$p_value[i] <- p_value
# }
# View(Emlen_IndData_2)


# 
# # Define the operation function
# perform_operation <- function(row) {
#   pivot_df <- pivot_longer(
#     row,
#     cols=c("0","15","30","45","60","75","90","105","120","135","150","165","180","195","210","225","240","255","270","285","300","315","330","345"),
#     names_to="sector",
#     values_to="count")
#   #longdata<-data.frame(ring=rep(pivot_df$Ring, times=pivot_df$count), degree=rep(pivot_df$sector, times=pivot_df$count))
#   longdata<-as.vector(ring=rep(degree=rep(pivot_df$sector, times=pivot_df$count)))
#   mean_direction <- mean.circular(longdata$degree)
#   return(mean_direction)
# }
# 
# # Apply the operation to each row using apply
# Emlen_IndData_2$mean_direction <- apply(Emlen_IndData_2, 1, perform_operation)
# 
# # View the updated Emlen_IndData_2 dataframe
# View(Emlen_IndData_2)
# 
# 
# 
# 
# 
# for (i in 1:nrow(Emlen_IndData_2)) {
#   #pivot_row <- Emlen_IndData_2[i, ]
#   Emlen_ind_long <- #pivot_row %>% 
#     pivot_longer(
#       Emlen_IndData_2,
#       cols=c("0","15","30","45","60","75","90","105","120","135","150","165","180","195","210","225","240","255","270","285","300","315","330","345"),
#       names_to="sector",
#       values_to="count")
#   longdata<-as.vector(rep(Emlen_ind_long$sector, times=Emlen_ind_long$count))
#   longdata<-as.circular(longdata)
#   rayleigh_test <- rayleigh.test(longdata)
#   r1 <- rho.circular(longdata, na.rm = FALSE)
#   test_statistic <- rayleigh_test$statistic
#   p_value <- rayleigh_test$p.value
#   Emlen_IndData_2$test_statistic[i] <- test_statistic
#   Emlen_IndData_2$p_value[i] <- p_value
# }
# View(Emlen_IndData_2)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# for (i in 1:nrow(Emlen_IndData_2)) {
#   #pivot_row <- Emlen_IndData_2[i, ]
#   Emlen_ind_long <- #pivot_row %>% 
#     pivot_longer(
#       Emlen_IndData_2,
#       cols=c("0","15","30","45","60","75","90","105","120","135","150","165","180","195","210","225","240","255","270","285","300","315","330","345"),
#       names_to="sector",
#       values_to="count")
#   longdata<-data.frame(ring=rep(Emlen_ind_long$Ring, times=Emlen_ind_long$count), degree=rep(Emlen_ind_long$sector, times=Emlen_ind_long$count))
#   longdata$degree<-as.circular(longdata$degree)
#   rayleigh_test <- rayleigh.test(longdata$degree)
#   r1 <- rho.circular(longdata$degree, na.rm = FALSE)
#   test_statistic <- rayleigh_test$statistic
#   p_value <- rayleigh_test$p.value
#   Emlen_IndData_2$test_statistic[i] <- test_statistic
#   Emlen_IndData_2$p_value[i] <- p_value
# }
# View(Emlen_IndData_2)
# 
# rayleigh_test <- rayleigh.test(longdata$degree)
# r1 <- rho.circular(longdata$degree, na.rm = FALSE)
# test_statistic <- rayleigh_test$statistic
# p_value <- rayleigh_test$p.value





# Emlen_hotelling$circular_mean <- as.character(Emlen_hotelling$circular_mean)
# Emlen_hotelling$Mean<-as.character(Emlen_hotelling$Mean)
# Emlen_hotelling$circular_mean <- as.numeric(Emlen_hotelling$circular_mean)
# Emlen_hotelling$Mean<-as.double(Emlen_hotelling$Mean)
# 
# Emlen_hotelling$circular_mean <- as.circular(Emlen_hotelling$circular_mean, units="degrees", modulo="2pi")
# Emlen_hotelling$Mean<-as.circular(Emlen_hotelling$Mean, units="degrees", modulo="2pi")
# 
# options(digits = 6)
# str(Emlen_hotelling)
# 
# a<-as.vector(Emlen_hotelling$circular_mean)
# b<-as.vector(Emlen_hotelling$Mean)
# 
# a
# b

# #I wanted to check if week has something to do with this 
# matching_indices <- match(Emlen_hotelling$Ring, mergedEmlen$Ring)
# Emlen_hotelling$Week <- mergedEmlen$Week[matching_indices]
# 
# 
# week1<-filter(Emlen_hotelling, Week==1)
# week2<-filter(Emlen_hotelling, Week==2)
# week3<-filter(Emlen_hotelling, Week==3)
# 
# week1_h<-paired.hotelling(week1$circular_mean, week1$Mean)
# week2_h<-paired.hotelling(week2$circular_mean, week2$Mean)
# week3_h<-paired.hotelling(week3$circular_mean, week3$Mean)
