library(RSQLite)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(data.table)
library(chron)
library(haven)
library(lubridate)
library(gridExtra)
library(lme4)
library(emmeans)
library(performance)
library(DHARMa)
library(visreg)
library(report)
library(car)
library(nortest)
library(glmmTMB)
library(actogrammr)

setwd("E:/Users/mlmah/OneDrive/Documentos/MLME/Maestria/Animal_Ecology_2022_2024/Tesis/Database/Activity_Data/Copia")
con<-dbConnect(SQLite(), dbname="ottenby2023-06-02_AVG_20min.db") #Conectarse con SQL para abrir el archibo db
complete_thesis_db<-read.table("E:\\Users\\mlmah\\OneDrive\\Documentos\\MLME\\Maestria\\Animal_Ecology_2022_2024\\Tesis\\Database\\ThesisDatabase_Spring_2023_Cages.txt", h=T) #para mas adelante copiar el numero del anillo

start_date<-"2023-04-20"
end_date<-"2023-05-31"
query<-paste0("SELECT * FROM activity WHERE datetime BETWEEN '", start_date, "' AND '", end_date, "'") #Separamos los datos reales
activity_complete<- dbGetQuery(con, query)
activity_complete<-as.data.frame(activity_complete)
#tHE DATABASE HAD A 2H DELAY SO i JUST ADDED IT 
activity_complete$datetime<- as.POSIXct(activity_complete$datetime) + (2*60*60) 

activity_pivot <- activity_complete %>% #Para hacer que los valores de actividad queden almacenados en una sola columna
  pivot_longer(cols = c("PirA","PirB","PirC","PirD"),
               names_to = "Pir",
               values_to = "Activity")
activity_pivot<- dplyr::select(activity_pivot, -VibA, -VibB, -VibC, -VibD) #Eliminar los valores Vib
activity_pivot$Cage<-paste(substr(activity_pivot$node, 3, 3), substr(activity_pivot$Pir, 4,4), sep = "") #Poner las jaulas como 1A,2A etc

str(activity_pivot)

# rmvalues<-as.Date(c("2023-04-27","2023-05-05","2023-05-06","2023-05-07","2023-05-15","2023-05-23","2023-05-24")) #To delete dates where I didnt had birds
# for (date in rmvalues) {
#   activity_pivot<-activity_pivot %>% filter(as.Date(datetime) !=date)
#   print(activity_pivot)
# }

activity_pivot <- activity_pivot %>%
  mutate(Week = case_when(
    between(datetime, as.Date("2023-04-20"), as.Date("2023-04-27")) ~ 1,
    between(datetime, as.Date("2023-04-28"), as.Date("2023-05-05")) ~ 2,
    between(datetime, as.Date("2023-05-08"), as.Date("2023-05-15")) ~ 3,
    between(datetime, as.Date("2023-05-16"), as.Date("2023-05-23")) ~ 4,
    between(datetime, as.Date("2023-05-25"), as.Date("2023-06-01")) ~ 5,
    TRUE ~ 6 # Defining values for each week with date intervals
  ))

activity_pivot<-subset(activity_pivot, activity_pivot$Week != "6") #Take week 6 away because I just did 5 experiments


activity_pivot <- activity_pivot %>%
  arrange(Week, datetime) %>%
  group_by(Week) %>%
  mutate(DayNumber = dense_rank(as.Date(date(datetime)))) #define each experimental day for each date (1-7)


activity_pivot <- activity_pivot %>%
  mutate(experiment = case_when(
    DayNumber == 1 ~ "Capture",
    (DayNumber == 2 | DayNumber == 3 | DayNumber == 4) ~ "Control",
    (DayNumber == 5 | DayNumber == 6 | DayNumber == 7) ~ "Treatment",
    TRUE ~ "Not trial" # Defining values for each week with date intervals
  ))




#Poner los valores del tipo que se necesita para hacer más análisis
activity_pivot<-data.table(activity_pivot)
complete_thesis_db<-data.table(complete_thesis_db)
str(activity_pivot)
str(complete_thesis_db)
complete_thesis_db$Week<-as.numeric(complete_thesis_db$Week)
View(complete_thesis_db)

setkey(activity_pivot, Cage, Week)
setkey(complete_thesis_db, Cage, Week)

merged_activity <- activity_pivot[complete_thesis_db, on=c("Cage", "Week"), nomatch = 0L] #Merge bothe databases to have complete activity information and complete individual information
View(merged_activity)

merged_activity$time <- format(as.POSIXct(
  merged_activity$datetime),format = "%H:%M:%S") #extract time as a column
merged_activity$date <- as.Date(ymd_hms(merged_activity$datetime)) #extract date as a column
merged_activity<-dplyr::select(merged_activity, -Date)#Tenia una columna de más con la fecha


handling_time_begin<-hms::hms(hours = 11, minutes = 00, seconds = 0)
handling_time_end<-hms::hms(hours = 13, minutes = 00, seconds = 0)
sunrise<-hms::hms(hours = 4, minutes = 00, seconds = 0)
sunset<-hms::hms(hours = 21, minutes = 00, seconds = 0)
horacero<-hms::hms(hours = 00, minutes = 00, seconds = 0)
hora24<-hms::hms(hours = 23, minutes = 59, seconds = 59)

# ring1<-as.data.frame(subset(merged_activity, Ring=="DK53341"))
# str(ring1)
# ring1_control<-filter(ring1, DayNumber==2|DayNumber==3|DayNumber==4)
# ring1_treatment<-filter(ring1, DayNumber==5|DayNumber==6|DayNumber==7)
# 
# ring1 %>%
#   mutate(time = as.POSIXct(hms::parse_hm(time))) %>%
#   ggplot(aes(time, Activity))+
#   ggtitle(ring1$Ring, "complete experiment")+
#   geom_point() +
#   geom_smooth()+
#   scale_x_datetime(date_labels = "%H:%M:%S")+
#   annotate("rect", xmin = as.POSIXct(handling_time_begin), xmax = as.POSIXct(handling_time_end), ymin = 0, ymax = 1, alpha = .2)+
#   annotate("rect", xmin = as.POSIXct(horacero), xmax = as.POSIXct(sunrise), ymin = 0, ymax = 1, alpha = .2)+
#   annotate("rect", xmin = as.POSIXct(sunset), xmax = as.POSIXct(hora24), ymin = 0, ymax = 1, alpha = .2)+
#   geom_vline(xintercept =as.numeric(c(handling_time_begin, handling_time_end, sunrise, sunset)), linetype = 2, color = 2, linewidth = 1)+
#   theme_classic()
# 
# 
# ring1_control %>%
#   mutate(time = as.POSIXct(hms::parse_hm(time)))# %>%
# PLOTRING<-ggplot(ring1_control,aes(time, Activity)) +
#   ggtitle(ring1$Ring, "control phase")+
#   geom_point() +
#   geom_smooth()+
#   scale_x_datetime(date_labels = "%H:%M:%S")+
#   annotate("rect", xmin = as.POSIXct(handling_time_begin), xmax = as.POSIXct(handling_time_end), ymin = 0, ymax = 1, alpha = .2)+
#   annotate("rect", xmin = as.POSIXct(horacero), xmax = as.POSIXct(sunrise), ymin = 0, ymax = 1, alpha = .2)+
#   annotate("rect", xmin = as.POSIXct(sunset), xmax = as.POSIXct(hora24), ymin = 0, ymax = 1, alpha = .2)+
#   geom_vline(xintercept =as.numeric(c(handling_time_begin, handling_time_end, sunrise, sunset)), linetype = 2, color = 2, linewidth = 1)+
#   theme_classic()
# PLOTRING
# 
# ring1_treatment %>%
#   mutate(time = as.POSIXct(hms::parse_hm(time))) %>%
#   ggplot(aes(time, Activity)) +
#   ggtitle(ring1$Ring, "experimental phase")+
#   geom_point() +
#   geom_smooth()+
#   scale_x_datetime(date_labels = "%H:%M:%S")+
#   annotate("rect", xmin = as.POSIXct(handling_time_begin), xmax = as.POSIXct(handling_time_end), ymin = 0, ymax = 1, alpha = .2)+
#   annotate("rect", xmin = as.POSIXct(horacero), xmax = as.POSIXct(sunrise), ymin = 0, ymax = 1, alpha = .2)+
#   annotate("rect", xmin = as.POSIXct(sunset), xmax = as.POSIXct(hora24), ymin = 0, ymax = 1, alpha = .2)+
#   geom_vline(xintercept =as.numeric(c(handling_time_begin, handling_time_end, sunrise, sunset)), linetype = 2, color = 2, linewidth = 1)+
#   theme_classic()





#Para calcular la actividad total en un numero entero y no como porcentaje o proporción
handling_time_begin<-hms::hms(hours = 11, minutes = 00, seconds = 0)
handling_time_end<-hms::hms(hours = 13, minutes = 00, seconds = 0)
sunrise<-hms::hms(hours = 4, minutes = 00, seconds = 0)
sunset<-hms::hms(hours = 21, minutes = 00, seconds = 0)
horacero<-hms::hms(hours = 00, minutes = 00, seconds = 0)
hora24<-hms::hms(hours = 23, minutes = 59, seconds = 59)

merged_activity <- merged_activity %>% #Delete capture data
  filter(!experiment %in% c("Capture"))


#To convert hours and minute into a decimal with hours as integrer and minutes as decimal

merged_activity$HourDecimal<-paste((substr(
  merged_activity$time, 1,2)), #Substract from the first to the second element from the column time in the table merged_activity (this are the hours)
  (substr((as.numeric(substr(merged_activity$time,4,5))/60),2,5)), #((Substract from merged_activity$time from the 4 to the 5 character /these are the minutes/)divide them by 60) from that substract from the second to the fifth character /these is the comma and the decimals/
  sep="") #Paste them to the hours and make a new variable 
merged_activity$HourDecimal<-as.numeric(merged_activity$HourDecimal)

####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!### UNDER CONSTRUCTION, BE AWARE!
#CAMBIAR LOS VALORES DEL DIA Y LA NOCHE!!!!!!!!

# week sunrise_decimal	sunset_decimal
# 1    5,567	          20,183
# 2    5,233	          20,467
# 3    4,850	          20,817
# 4    4,583	          21,083
# 5    4,317	          21,350

View(merged_activity)
merged_activity <- merged_activity %>%
  mutate(experiment = case_when(
    Week == 1 & DayNumber == 4 & HourDecimal > 20.183 ~ "Treatment",
    Week == 2 & DayNumber == 4 & HourDecimal > 20.467 ~ "Treatment",
    Week == 3 & DayNumber == 4 & HourDecimal > 20.817 ~ "Treatment",
    Week == 4 & DayNumber == 4 & HourDecimal > 21.083 ~ "Treatment",
    Week == 5 & DayNumber == 4 & HourDecimal > 21.350 ~ "Treatment",
    TRUE ~ experiment  # Keep original values for rows not meeting conditions
  ))

# Week  Emlen_Treatment_sunset_decimal
# 1     20,383
# 2     20,666
# 3     20,983
# 4     21,233
# 5     21,466
#Quitar 1h30 porque lo hice 1h antes del sunset. Restar 

merged_activity <- merged_activity %>%
  filter(!(Week == 1 & DayNumber == 6 & HourDecimal > (20.383-1.5)) & 
         !(Week == 2 & DayNumber == 6 & HourDecimal > (20.666-1.5)) &
         !(Week == 3 & DayNumber == 6 & HourDecimal > (20.983-1.5)) &
         !(Week == 4 & DayNumber == 6 & HourDecimal > (21.233-1.5)) &
         !(Week == 5 & DayNumber == 6 & HourDecimal > (21.466-1.5)))


merged_activity <- merged_activity %>% #Assign the day and night values to each of the weeks (including the "day - night" schedule of the 24h setup)
  mutate(
    DayNight = case_when(
      Week == "1" ~ ifelse(HourDecimal > 0 & HourDecimal < 5.567, "Night",
                           ifelse(HourDecimal > 20.186, "Night", "Day")),
      Week == "2" ~ ifelse(HourDecimal > 0 & HourDecimal < 5.233, "Night",
                           ifelse(HourDecimal > 20.467, "Night", "Day")),
      Week == "3" ~ ifelse(HourDecimal > 0 & HourDecimal < 4.850, "Night",
                           ifelse(HourDecimal > 20.817, "Night", "Day")),
      Week == "4" ~ ifelse(HourDecimal > 0 & HourDecimal < 4.583, "Night",
                           ifelse(HourDecimal > 21.083, "Night", "Day")),
      Week == "5" ~ ifelse(HourDecimal > 0 & HourDecimal < 4.317, "Night",
                           ifelse(HourDecimal > 21.350, "Night", "Day")),
      TRUE ~ NA  # Default value
    ))

str(merged_activity)

#delete feeding time between 11 and 13 for every day
feedingtime <- which((merged_activity$time) >= "11:00:00" & (merged_activity$time) <= "13:00:00")
merged_activity$Activity[feedingtime] <- NA

View(merged_activity)
hist(merged_activity$Activity)


#For sum of activity

# sum_activity_DayNight <- merged_activity %>%
#   group_by(Ring, DayNight, Week, experiment, Species, date) %>%
#   summarise(DayNight_activity = sum(Activity, na.rm = TRUE)) %>%
#   ungroup()
# View(sum_activity_DayNight)
# hist(sum_activity_DayNight$DayNight_activity, breaks = 1800)
# 
# sum_activity_Total <- merged_activity %>%
#   group_by(Ring, Week, experiment, Species, date) %>%
#   summarise(DayNight_activity = sum(Activity, na.rm = TRUE)) %>%
#   ungroup()
# View(sum_activity_Total)
# hist(sum_activity_Total$DayNight_activity, breaks=1800)

#####Mean and total activity####
mean_activity_DayNight <- merged_activity %>%
  group_by(Ring, DayNight, Week, experiment, Species, date, BreedingDistribution) %>%
  summarise(DayNight_activity = mean(Activity, na.rm = TRUE)) %>%
  ungroup()
View(mean_activity_DayNight)
hist(mean_activity_DayNight$DayNight_activity, breaks = 1800)


mean_activity_Total <- merged_activity %>%
  group_by(Ring, Week, experiment, Species, date, BreedingDistribution) %>%
  summarise(DayNight_activity = mean(Activity, na.rm = TRUE)) %>%
  ungroup()
View(mean_activity_Total)
hist(mean_activity_Total$DayNight_activity, breaks=1800)

#####modelo Experiment + DayNight####

mod_Exp_meanDayNight<- lmer(DayNight_activity ~ DayNight * experiment + (1|Ring),
                           data = mean_activity_DayNight, na.action = na.exclude)
check_model(mod_Exp_meanDayNight)
summary(mod_Exp_meanDayNight)
mean_mod1<-emmeans(mod_Exp_meanDayNight, ~DayNight * experiment)
pairs(mean_mod1)

gr <- ref_grid(mod_Exp_meanDayNight, cov.keep=c("DayNight","experiment"))
gr_emmeans<-as.data.frame(emmeans(gr, specs = c("DayNight","experiment"), level=0.95))
#Plotting - boxplot
raw_Exp_DayNight_MeanData <- mean_activity_DayNight[c("experiment", "DayNight_activity")]
plot_data <- merge(gr_emmeans, raw_Exp_DayNight_MeanData, by = c("experiment"))

ggplot(plot_data, aes(x = DayNight, y = DayNight_activity)) +
  geom_point() +  # Raw data points
  geom_boxplot(aes(ymin = lower.CL, ymax = upper.CL, fill = "CI"), alpha = 0.5)+
  geom_line(aes(y = emmean, color = "Estimated Marginal Mean")) +
  #geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = "CI"), alpha = 0.3) +  # CIs
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "red") +
  labs(x = "Day/Night", y = "Mean Activity") +
  ggtitle("Raw Data with Estimated Marginal Mean and Confidence Intervals")+
  facet_wrap(~ experiment)+
  theme_classic()

#####

mod_ExpBreed_meanDayNight<- lmer(DayNight_activity ~ DayNight * experiment * BreedingDistribution + (1|Ring) + (1|Species),
                            data = mean_activity_DayNight, na.action = na.exclude)
check_model(mod_ExpBreed_meanDayNight)
summary(mod_ExpBreed_meanDayNight)
mean_mod2<-emmeans(mod_ExpBreed_meanDayNight, ~DayNight * experiment * BreedingDistribution)
pairs(mean_mod2)

mean_mod2<-as.data.frame(mean_mod2)

ggplot(mean_mod2, aes(x = DayNight, y = emmean, color = BreedingDistribution, shape = experiment)) +
  geom_point(position = position_dodge2(width = 2)) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL, fill = "CI"), alpha = 0.5,
                 position = position_dodge2(width = 2))+
  facet_wrap(vars(experiment))+
  labs(x = "DayNight", y = "Estimated Marginal Mean") +
  ggtitle("Interaction Plot of DayNight, BreedingDistribution, and Experiment")

#####


mod_Exp_meanTotal<- lmer(DayNight_activity ~ experiment + (1|Ring),
                            data = mean_activity_Total, na.action = na.exclude)
check_model(mod_Exp_meanTotal)
summary(mod_Exp_meanTotal)
mean_mod_total<-emmeans(mod_Exp_meanTotal, ~ experiment)
pairs(mean_mod_total)
#hacer el plot con los intervalos de confianza y los pvalues bonitos
gr <- ref_grid(mod_Exp_meanDayNight, cov.keep=c("DayNight","experiment"))
gr_emmeans<-as.data.frame(emmeans(gr, specs = c("DayNight","experiment"), level=0.95))
#MAKE THE GGPLOT




#first model
mod_Exp_sumDayNight<- lmer(DayNight_activity ~ DayNight * experiment + (1|Ring),
                   data = sum_activity_DayNight, na.action = na.exclude)
jpeg(file="checkmodel_mod_Exp_sumDayNight.jpg")
check_model(mod_Exp_sumDayNight)
dev.off()
plot(DHARMa::simulateResiduals(mod_Exp_sumDayNight))
visreg(mod_Exp_sumDayNight, "DayNight", by="experiment")
plot(mod_Exp_sumDayNight)
summary(mod_Exp_sumDayNight)

mod_Exp_sumTotal<- lmer(DayNight_activity ~ experiment + (1|Ring),
                           data = sum_activity_Total, na.action = na.exclude)
jpeg(file="checkmodel_mod_Exp_sumTotal.jpg")
check_model(mod_Exp_sumTotal)
dev.off()
plot(DHARMa::simulateResiduals(mod_Exp_sumTotal))
visreg(mod_Exp_sumTotal, "DayNight", by="experiment")
plot(mod_Exp_sumTotal)

#__

mod_Exp_sumDayNight_glmm<- glmer(DayNight_activity ~ DayNight * experiment + (1|Ring),
                           data = sum_activity_DayNight, na.action = na.exclude, family = "poisson")
summary(mod_Exp_sumDayNight_glmm)
plot(DHARMa::simulateResiduals(mod_Exp_sumDayNight_glmm))
visreg(mod_Exp_sumDayNight_glmm, "DayNight", by="experiment")
plot(mod_Exp_sumDayNight_glmm)


#logit transformation. Also sqrt transformation
merged_activity$logitActivity <- logit(merged_activity$Activity, percents=FALSE)
merged_activity$sqrtActivity <- sqrt(merged_activity$Activity)

ad.test(merged_activity$logitActivity) #normality over this transformation
hist(merged_activity$logitActivity)

merged_activity$HourDecimalRadian<-2*pi*merged_activity$HourDecimal/24

mod_HourExp<- lmer(sqrtActivity ~ sin(HourDecimalRadian) * experiment + cos(HourDecimalRadian) * experiment + (1|Ring),
                        data = merged_activity, na.action = na.exclude)
check_model(mod_HourExp)
norm<-check_normality(mod_HourExp)
plot(norm)

qqnorm(residuals(mod_HourExp))
qqline(residuals(mod_HourExp))

mod_HourExp<- lmer(sqrtActivity ~ sin(HourDecimalRadian) * experiment + cos(HourDecimalRadian) * experiment + (1|Ring),
                   data = merged_activity, na.action = na.exclude)









library(glmmTMB)

glmm_HourExp <- glmmTMB((Activity + 0.000000000001) ~ sin(HourDecimalRadian) * experiment + cos(HourDecimalRadian) * experiment + (1|Ring),
                 data = merged_activity,
                 na.action = na.exclude,
                 family = beta_family(link = "logit"))


######pdf_plots#####


unique_values_activity <- unique(merged_activity$Ring)

pdf(paste("ActivityByRing_AllWeek_", Sys.Date(), ".pdf", sep=""),  width = 12)
for (i in unique_values_activity) {
  subset_data <- subset(merged_activity, Ring == i)
  #RingActivityComplete<-as.data.frame(as.numeric(unlist(subset_data)))
  #RingActivityComplete<-na.omit(RingActivityComplete)
  
  complete<-subset_data %>% 
    mutate(time = as.POSIXct(hms::parse_hm(time))) %>% 
    ggplot(aes(time, Activity)) +
    geom_point() +
    geom_smooth()+
    ggtitle(subset_data$Ring, "Complete Activity")+
    scale_x_datetime(date_labels = "%H:%M:%S")+
    annotate("rect", xmin = as.POSIXct(handling_time_begin), xmax = as.POSIXct(handling_time_end), ymin = 0, ymax = 1, alpha = .2)+
    annotate("rect", xmin = as.POSIXct(horacero), xmax = as.POSIXct(sunrise), ymin = 0, ymax = 1, alpha = .2)+
    annotate("rect", xmin = as.POSIXct(sunset), xmax = as.POSIXct(hora24), ymin = 0, ymax = 1, alpha = .2)+
    geom_vline(xintercept =as.numeric(c(handling_time_begin, handling_time_end, sunrise, sunset)), linetype = 2, color = 2, linewidth = 1)+
    theme_classic()
  
  ring1_control<-filter(subset_data, DayNumber==2|DayNumber==3|DayNumber==4)
  ring1_treatment<-filter(subset_data, DayNumber==5|DayNumber==6|DayNumber==7)
  
  control<-ring1_control %>% 
    mutate(time = as.POSIXct(hms::parse_hm(time))) %>% 
    ggplot(aes(time, Activity)) +
    geom_point() +
    geom_smooth()+
    ggtitle(subset_data$Ring, "Activity control phase")+
    scale_x_datetime(date_labels = "%H:%M:%S")+
    annotate("rect", xmin = as.POSIXct(handling_time_begin), xmax = as.POSIXct(handling_time_end), ymin = 0, ymax = 1, alpha = .2)+
    annotate("rect", xmin = as.POSIXct(horacero), xmax = as.POSIXct(sunrise), ymin = 0, ymax = 1, alpha = .2)+
    annotate("rect", xmin = as.POSIXct(sunset), xmax = as.POSIXct(hora24), ymin = 0, ymax = 1, alpha = .2)+
    geom_vline(xintercept =as.numeric(c(handling_time_begin, handling_time_end, sunrise, sunset)), linetype = 2, color = 2, linewidth = 1)+
    theme_classic()
  
  treatment<-ring1_treatment %>% 
    mutate(time = as.POSIXct(hms::parse_hm(time))) %>% 
    ggplot(aes(time, Activity)) +
    geom_point() +
    geom_smooth()+
    ggtitle(subset_data$Ring, "Activity 24h light phase")+
    scale_x_datetime(date_labels = "%H:%M:%S")+
    annotate("rect", xmin = as.POSIXct(handling_time_begin), xmax = as.POSIXct(handling_time_end), ymin = 0, ymax = 1, alpha = .2)+
    annotate("rect", xmin = as.POSIXct(horacero), xmax = as.POSIXct(sunrise), ymin = 0, ymax = 1, alpha = .2)+
    annotate("rect", xmin = as.POSIXct(sunset), xmax = as.POSIXct(hora24), ymin = 0, ymax = 1, alpha = .2)+
    geom_vline(xintercept =as.numeric(c(handling_time_begin, handling_time_end, sunrise, sunset)), linetype = 2, color = 2, linewidth = 1)+
    theme_classic()
  
  
  combined_plots <- grid.arrange(complete, control, treatment, ncol = 1)
  print(combined_plots)
  #print(complete)
  #print(control)
  #print(treatment)
  
}
dev.off()




####Hablar con nicholas de este modelo####


hist(merged_activity$Activity)
intentomodelo<- glmer  (Activity ~ sin(2*pi*HourDecimal/24) * experiment * DayNight + cos(2*pi*HourDecimal/24) * experiment * DayNight + (1|Ring),
     data = merged_activity, na.action = 'na.fail', family=binomial)

intentomodelo2<- glmer  (Activity ~ sin(2*pi*HourDecimal/24) * experiment * DayNight + cos(2*pi*HourDecimal/24) * experiment * DayNight + (1|Ring),
                        data = merged_activity, na.action = 'na.fail', family="poisson")
summary(intentomodelo)
plot(DHARMa::simulateResiduals(intentomodelo)) #Como deberia verse uno de estos modelos, los residuos de un glmer deberian ser normales si la distribucion es normal?
confint(intentomodelo, level = 0.95)
emmeans(intentomodelo, ~HourDecimal | experiment)
visreg(intentomodelo, "HourDecimal", by="experiment") #trans??? = FUN_RevTrans
fam.bi <- family(intentomodelo)

FUN_RevTrans <- function(x){
  return(fam.bi$linkinv(x)) 
}

x1 <- fitted(intentomodelo)
y1 <- jitter(resid(intentomodelo,type="deviance"), amount = 0.3)
plot(x1, y1) #residuals vs fitted
abline(h=0)
rug(y1, side = 2)

plot(fitted(intentomodelo), resid(intentomodelo,type="deviance")) #amount = 0.3)) #residuals vs fitted
abline(h=0)

plot(intentomodelo,HourDecimal~resid(.,type="deviance")) #plot residuals grouped by treatment (residuals vs predictor)

lattice::dotplot(ranef(intentomodelo,condVar=TRUE)) #produces two plots to show variance for each random effect



summary(intentomodelo2)
plot(DHARMa::simulateResiduals(intentomodelo2)) #Como deberia verse uno de estos modelos, los residuos de un glmer deberian ser normales si la distribucion es normal?
confint(intentomodelo2, level = 0.95)
emmeans(intentomodelo2, ~HourDecimal | experiment)
visreg(intentomodelo2, "HourDecimal", by="experiment")



merged_activity$HourDecimalRadian<-2*pi*merged_activity$HourDecimal/24
intentomodelo3<- lmer  (Activity ~ sin(HourDecimalRadian) * experiment + cos(HourDecimalRadian) * experiment + (1|Ring),
                         data = merged_activity, na.action = 'na.fail')
checkModel(intentomodelo3) #mirar por que no está funcionando
plot(DHARMa::simulateResiduals(intentomodelo3))
report(intentomodelo3)
visreg(intentomodelo3, "HourDecimalRadian", by="experiment")
qqnorm(residuals(intentomodelo3))
qqline(residuals(intentomodelo3))
check_model(intentomodelo3)












  
#####TODAVIA NO FUNCIONA, SEGUIR CACHARREANDO DE AQUI PARA ABAJO PARA SACAR LOS TEST PRELIMINARES ANTES DE LO QUE SEA QUE TOQUE HACER ON ACTIVITY PACKAGE
TotalActivity_data <- merged_activity %>% #Calculate total activity for control and experiment for each ring
  group_by(experiment, Ring) %>%
  summarize(TotalActivity = sum(Activity1000)) 
View(TotalActivity_data)
#To test differences in total activity between control and treatment groups
t.test(TotalActivity ~ experiment, data = TotalActivity_data, paired=TRUE)

TotalActivity_control <- merged_activity %>%
  filter(time >= sunrise, time <= sunset) %>%
  group_by(date, Ring) %>%
  summarize(TotalActivity_control = sum(Activity1000))

TotalActivity_treatment <- merged_activity %>%
  filter((time >= horacero & time <= sunrise) |
           (time >= sunset & time <= hora24)) %>%
  group_by(date, Ring) %>%
  summarize(TotalActivity_treatment = sum(Activity1000))

View(TotalActivity_control)
View(TotalActivity_treatment)



TotalActivity_data <- merged_activity %>%
  group_by(Ring) %>%
  summarize(TotalActivityWeek = sum(Activity1000)) 
#%>%
TotalActivity_data <- merged_activity %>%
  group_by(date, Ring) %>%
  filter(time >= sunrise, time <= sunset) %>%
  summarize(TotalActivity_control = sum(Activity1000)) %>%
  filter((time >= horacero & time <= sunrise) |
           (time >= sunset & time <= hora24)) %>%
  summarize(TotalActivity_treatment = sum(Activity1000))

View(TotalActivity_data)





merged_activity$Activity1000<- (merged_activity$Activity*1000)

TotalActivity_data <- merged_activity %>%
  group_by(date, Ring) %>%
  summarize(TotalActivityDay = sum(Activity1000))
View(TotalActivity_data)


###############
#start_date_S1<-"2023-04-20"
#end_date_S1<-"2023-04-26"
#query<-paste0("SELECT * FROM activity WHERE datetime BETWEEN '", start_date_S1, "' AND '", end_date_S1, "'") #Separamos la semana 1 de datos
#semana1<- dbGetQuery(con, query)
#semana1<-as.data.frame(semana1)

#semana1_pivot <- semana1 %>% #Para hacer que los valores de actividad queden almacenados en una sola columna
 # pivot_longer(cols = c("PirA","PirB","PirC","PirD"),
  #             names_to = "Pir",
   #            values_to = "Activity")
#semana1_pivot
#semana1_pivot$cage<-paste(substr(semana1_pivot$node, 3, 3), substr(semana1_pivot$Pir, 4,4), sep = "")



#activity_pivot<-activity_pivot %>% filter(as.Date(datetime) !="2023-04-27")
#activity_pivot<-activity_pivot %>% filter(as.Date(datetime) !="2023-05-05")
#activity_pivot<-activity_pivot %>% filter(as.Date(datetime) !="2023-05-06")
#activity_pivot<-activity_pivot %>% filter(as.Date(datetime) !="2023-05-07")
#activity_pivot<-activity_pivot %>% filter(as.Date(datetime) !="2023-05-15")
#activity_pivot<-activity_pivot %>% filter(as.Date(datetime) !="2023-05-23")
#activity_pivot<-activity_pivot %>% filter(as.Date(datetime) !="2023-05-24")


#plot(ring1$datetime,ring1$Activity)
#plot(ring1_control$datetime,ring1_control$Activity)
#plot(ring1_treatment$datetime,ring1_treatment$Activity)

#ring_day <- ggplot(ring1, aes(x = time, y = Activity))
#ring_day+geom_line(colour = "tomato", size = .7)+
 # labs(x = "interval", y = "Number of steps", title = "Average Daily Activity Pattern")

#ring_day_C <- ggplot(ring1_control, aes(x = time, y = Activity))
#ring_day_C+geom_line(colour = "tomato", size = .7)+labs(x = "interval", y = "Number of steps", title = "Average Daily Activity Pattern")+scale_x_datetime(labels= date_format("%H:%M:%S"))

#ring_day_T <- ggplot(ring1_treatment, aes(x = time, y = Activity))
#ring_day_T+geom_line(colour = "tomato", size = .7)+labs(x = "interval", y = "Number of steps", title = "Average Daily Activity Pattern")+scale_x_datetime(labels= date_format("%H:%M:%S"))


#View(ring1)

#date_activity<-split(merged_activity, merged_activity$date)
#total_activity <- sapply(date_activity, function(x) sum(x$Activity1000))
#qplot(total_activity, geom = "histogram", binwidth = 1000, xlab = "Total steps per day", ylab = "Number of Days", main = "Total steps per day Frequency")
#hist(total_activity, breaks=30)



#PLOT QUE FUNCIONO EN MODELO 2 PERO QUE AJÁ
# ggplot(mean_mod2, aes(x = DayNight, y = emmean, color = BreedingDistribution, shape = experiment)) +
#   geom_point(position = position_dodge2(width = 2)) +
#   geom_linerange(aes(ymin = lower.CL, ymax = upper.CL, fill = "CI"), alpha = 0.5,
#                  position = position_dodge2(width = 2))+
#   facet_wrap(vars(experiment))+
#   labs(x = "DayNight", y = "Estimated Marginal Mean") +
#   ggtitle("Interaction Plot of DayNight, BreedingDistribution, and Experiment")

