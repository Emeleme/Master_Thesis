setwd("E:/Users/mlmah/OneDrive/Documentos/MLME/Maestria/Animal_Ecology_2022_2024/Tesis/Database")
#read .csv data file and assign it a name
Emlen_IndData_2<-read.table("E:/Users/mlmah/OneDrive/Documentos/MLME/Maestria/Animal_Ecology_2022_2024/Tesis/Database/EmlenData_Spring2023.txt", h=T)

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
library(performance)
library(tectonicr)

# ANALYSIS FOR INDIVIDUAL BIRDS - UNIMODAL --------------------------------------
#import my data, update column name and assigning a new row as heading
#This are the angles: c("0","15","30","45","60","75","90","105","120","135","150",
#"165","180","195","210","225","240","255","270","285","300","315","330","345")
#We use the middle angle to make all calculations
#c("7.5","22.5","37.5","52.5","67.5","82.5","97.5","112.5","127.5","142.5","157.5","172.5","187.5","202.5","217.5","232.5","247.5","262.5","277.5","292.5","307.5","322.5","337.5","352.5")
setnames(Emlen_IndData_2, old=c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12","S13","S14","S15","S16","S17","S18","S19","S20","S21","S22","S23","S24"), 
         new = c("7.5","22.5","37.5","52.5","67.5","82.5","97.5","112.5","127.5","142.5","157.5","172.5","187.5","202.5","217.5","232.5","247.5","262.5","277.5","292.5","307.5","322.5","337.5","352.5"), skip_absent=TRUE)

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
  cols=c("7.5","22.5","37.5","52.5","67.5","82.5","97.5","112.5","127.5","142.5","157.5","172.5","187.5","202.5","217.5","232.5","247.5","262.5","277.5","292.5","307.5","322.5","337.5","352.5"),
  names_to="sector",
  values_to="count")

uniqueValues_ID<-unique(datalong_EmlenInd_2$Identificator)

results_df <- data.frame(Identificator = uniqueValues_ID,   #create an empty dataframe to store results
                         test_statistic = numeric(length(uniqueValues_ID)),
                         p_value = numeric(length(uniqueValues_ID)),
                         Mean = numeric(length(uniqueValues_ID)),
                         Lower_CI = numeric(length(uniqueValues_ID)),
                         Upper_CI = numeric(length(uniqueValues_ID)))

# Loop through unique identifiers
for (i in uniqueValues_ID) {
  subset_data <- subset(datalong_EmlenInd_2, Identificator == i) #subsets data for each identificator/each entry in the database
  IndData <- as.data.frame(as.numeric(unlist(subset_data)))
  IndData <- na.omit(IndData)
  longdata <- as.numeric(rep(subset_data$sector, times = subset_data$count)) #repeats the degrees the times we counted already
  conf_interval <- confidence_interval(longdata, conf.level = 0.95, axial = FALSE)
  longdata <- deg2rad(longdata)  # Convert to radians
  rayleigh_test <- rayleigh.test(longdata)#performs rayleigh test for each entry in the database
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
  results_df$test_statistic[row_index] <- test_statistic
  results_df$p_value[row_index] <- p_value
  results_df$Mean[row_index]<- MeanDir_deg
  results_df$Lower_CI[row_index]<- conf_interval$conf.interval[1]
  results_df$Upper_CI[row_index]<- conf_interval$conf.interval[2]
}

View(results_df)

mergedEmlen<-merge(results_df, Emlen_IndData_2, by = "Identificator") #merge both dataframes by identificator
View(mergedEmlen)

mergedEmlen$Mean<-as.circular(deg2rad(mergedEmlen$Mean))
#mergedEmlen<-filter(mergedEmlen, p_value<0.05) #Quitar los que no tienen direccion??
str(mergedEmlen)
View(mergedEmlen)

#To check unique values and how many data points a ring has to perform a paired test
uniqueValues_Ring<-unique(mergedEmlen$Ring)
length(uniqueValues_Ring)

occurrences <- mergedEmlen %>% count(Ring) #to see how many times a ring occours in the dataframe (this is because we need to have the same size if we want to do a paired test)
print(occurrences)

#tO TESTS
EmlenData_ForTest<- dplyr::select(mergedEmlen, c("Identificator", "test_statistic","p_value", "rho", "Mean", "Species", "Group", "Treatment","Ring","BreedingDistribution", "Week"))

Emlen_local<-filter(EmlenData_ForTest, Group=="local") #just local time (sunset and sunrise of the capture day)
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