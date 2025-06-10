library(survival)
library(survminer)
library(MASS)
library(ggplot2)
library(dplyr)
library(car)

setwd('C:\\Users\\penghulu8\\Desktop\\Project3_Modsur')
# ubah direktori anda untuk menyesuaikan

data <- read.csv("AIDS_ClinicalTrial_GroupStudy175.csv")

# Stepwise-Model

full_model <- coxph(Surv(time, label) ~ trt + age + wtkg + hemo + homo + drugs + 
                      karnof + oprior + z30 + preanti + race + gender + str2 + strat +
                      symptom + treat + offtrt, data = data)

summary(full_model)

step_model <- stepAIC(full_model, direction = "both")
summary(step_model)

# Asumption Check

ph_test <- cox.zph(step_model)
summary(ph_test)
print(ph_test)
plot(ph_test)

# create new model based on that

new_model <- coxph(Surv(time, label) ~ age + drugs + karnof + z30 + preanti + symptom, data = data)

summary(new_model)

ph_test <- cox.zph(new_model)
print(ph_test)
plot(ph_test)

# multicolinearity test

vif(new_model)

# plot time vs log(H(t))

log_hazard <- predict(new_model, type = 'lp')

plot_data <- data.frame(time = data$time, log_hazard=log_hazard)

plot(plot_data$time, plot_data$log_hazard, 
     main = "Waktu vs Log(Hazard) Function", 
     xlab = "Waktu", 
     ylab = "Log(H(t))", 
     pch = 19, 
     col = "blue", 
     cex = 0.5)
lines(smooth.spline(plot_data$time, plot_data$log_hazard), col = "red")

surv_obj <- Surv(time = data$time, event = data$label)

fit <- survfit(surv_obj ~ symptom, data = data)

ggsurvplot(fit, data = data,
           fun = "cloglog",
           ggtheme = theme_classic(),
           censor = FALSE,
           main = "Kaplan-Meier Plot (Cumulative Log-Log)",
           xlab = "Waktu", ylab = "Log-Hazard",
           lty = 1:2)  # Lty menentukan jenis garis (misalnya garis putus-putus)




# try new ones
full_model2 <- coxph(Surv(time, label)~., data = data)
summary(full_model2)
step_model2 <- stepAIC(full_model2, direction = "both")
summary(step_model2)

ph_test <- cox.zph(step_model2)
print(ph_test)
plot(ph_test)

# create new model based on that again
new_model2 <- coxph(Surv(time, label) ~ age + drugs + karnof + z30 + preanti + strat +
                       symptom + cd820, data = data)
summary(new_model2)

ph_test <- cox.zph(new_model2)
print(ph_test)
plot(ph_test)
