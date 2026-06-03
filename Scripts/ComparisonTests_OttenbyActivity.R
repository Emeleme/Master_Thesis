mod_Exp_meanDayNight_R<- lmer(DayNight_activity ~ DayNight * experiment + (1|Ring),
                              data = Robin_mean_activity_DayNight, na.action = na.exclude)

mod_Exp_meanDayNight_R_week<- lmer(DayNight_activity ~ DayNight * experiment + Week + (1|Ring),
                              data = Robin_mean_activity_DayNight, na.action = na.exclude)


null_mod1_R<-lmer(DayNight_activity ~ 1 + (1|Ring),
                  data = Robin_mean_activity_DayNight, na.action = na.exclude)

anova(null_mod1_R, mod_Exp_meanDayNight_R, test="LTR")
anova(null_mod1_R, mod_Exp_meanDayNight_R_week, test="LTR")
anova(mod_Exp_meanDayNight_R_week, mod_Exp_meanDayNight_R, test="LTR")



mod_Exp_meanDayNight_W<- lmer(DayNight_activity ~ DayNight * experiment + (1|Ring),
                              data = Willy_mean_activity_DayNight, na.action = na.exclude)

mod_Exp_meanDayNight_W_week<- lmer(DayNight_activity ~ DayNight * experiment + Week + (1|Ring),
                                   data = Willy_mean_activity_DayNight, na.action = na.exclude)


null_mod1_W<-lmer(DayNight_activity ~ 1 + (1|Ring),
                  data = Willy_mean_activity_DayNight, na.action = na.exclude)

anova(null_mod1_W, mod_Exp_meanDayNight_W, test="LTR")
anova(null_mod1_W, mod_Exp_meanDayNight_W_week, test="LTR")
anova(mod_Exp_meanDayNight_W_week, mod_Exp_meanDayNight_W, test="LTR")
