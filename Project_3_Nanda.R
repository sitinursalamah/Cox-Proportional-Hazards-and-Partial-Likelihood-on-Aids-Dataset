# Muat library yang dibutuhkan
library(survival)
library(survminer)
library(dplyr)

# Membaca data
data <- read.csv("AIDS_ClinicalTrial_GroupStudy175.csv")

# Lihat struktur data
str(data)

# Pastikan variabel memiliki tipe data yang sesuai
data$treat <- factor(data$treat)   # Jenis pengobatan (0 atau 1)
data$gender <- factor(data$gender) # Gender
data$label <- as.numeric(data$label)  # Event (1 = meninggal, 0 = disensor)
data$age <- as.numeric(data$age)      # Usia
data$wtkg <- as.numeric(data$wtkg)    # Berat badan (kg)
data$hemo <- as.numeric(data$hemo)    # Hemoglobin
data$karnof <- as.numeric(data$karnof)  # Karnofsky score (kondisi fisik)
data$time <- as.numeric(data$time)    # Waktu bertahan hidup (hari)

# Membuat objek survival
surv_object <- Surv(time = data$time, event = data$label)

# Model Cox Proportional Hazards
cox_model <- coxph(surv_object ~ treat + age + wtkg + hemo + karnof, data = data)

# Ringkasan model
summary(cox_model)

# Urutkan koefisien berdasarkan nilai p
sorted_coef <- summary(cox_model)$coefficients[order(summary(cox_model)$coefficients[,5]),]
print(sorted_coef)

# Plot Kaplan-Meier berdasarkan jenis terapi
fit_treat <- survfit(surv_object ~ treat, data = data)

# Plot kurva survival
ggsurvplot(fit_treat,
           data = data,
           pval = TRUE,
           conf.int = TRUE,
           risk.table = TRUE,
           title = "Kurva Kaplan-Meier berdasarkan Jenis Terapi",
           xlab = "Waktu Bertahan Hidup (Hari)",
           ylab = "Probabilitas Bertahan Hidup",
           legend.title = "Terapi",
           legend.labs = c("Tidak Diberi Terapi", "Diberi Terapi"))

# Uji Asumsi Proportional Hazards
ph_test <- cox.zph(cox_model)
print(ph_test)

# Plot untuk melihat pelanggaran asumsi PH
plot(ph_test)




