library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(ggpubr)

source("param.r")
source("Nnorm.r")
source("Csample.r")

n <- 1000
m <- 300


PatientR <- tibble(Id= 1:n, Group = 0,
                       Temperature = Nnorm(n, nk[1,]),
                       Pulse = Nnorm(n, nk[2,]),
                       Hemoglobin = Nnorm(n, nk[3,]), 
                       BMI = Nnorm(n, nk[4,]),
                       Leu = Nnorm(n, nk[5,]),
                       Sugar = Nnorm(n, nk[6,]), 
                       Age = Nnorm(n, nk[7,]), 
                       Blood = Csample(c1),
                       Sex = Csample(c2),
                       Pressure = Csample(c3), 
                       Stage = Csample(c4),
                       Secuelas = Csample(c5)
                   )
PatientT <- tibble(Id = (n+1):(m+n), Group = 1,
                       Temperature = Nnorm(m, nk[1,], 1),
                       Pulse = Nnorm(m, nk[2,], 1), 
                       Hemoglobin = Nnorm(m, nk[3,], 1),
                       BMI = Nnorm(m,nk[4,], 1), 
                       Leu = Nnorm(m, nk[5,], 1),
                       Sugar = Nnorm(m, nk[6,], 1),
                       Age = Nnorm(m, nk[7,], 1), 
                       Blood = Csample(c11, size = m),
                       Sex = Csample(c21, size = m),
                       Pressure = Csample(c31, size = m), 
                       Stage = Csample(c41, size = m),
                       Secuelas = Csample(c51, size = m)
                   )

Patients <- rbind(PatientR, PatientT)
Patients$Group <-  as.logical(Patients$Group, levels = c('R','T'))
Patients$Blood <- factor(Patients$Blood, labels = c('A','O', 'B','AB'))
Patients$Sex <- factor(Patients$Sex, labels = c('female', 'male'))
Patients$Pressure <- factor(Patients$Pressure, labels = c('Low', 'Is_normal', 'High'))
Patients$Stage <- factor(Patients$Stage, labels = c('S0', 'S1', 'S2', 'S3', 'S4'))
Patients$Secuelas <- factor(Patients$Secuelas, labels = c('Sc0', 'Sc1', 'Sc2'))

library(tidyr)
Patients_for_plot <- Patients %>% 
  pivot_longer(cols = c("Temperature",
                        "Pulse",
                        "Hemoglobin",
                        "BMI",
                        "Leu",
                        "Sugar",      
                        "Age"),
               names_to = "names",
               values_to = "value")

source("GV.r")
GV(Patients_for_plot)

source("GH.r")
GH(Patients_for_plot)

source("GD.r")
GD(Patients_for_plot)


source("GB.r")
GB(Patients)

#####################################################################
##library(wakefield)
library(MatchIt)
##library(Matching)

#######################################


mod_match <- matchit(Group ~ Temperature + Pulse + Hemoglobin + BMI + Leu + Sugar + Age + Blood + Sex + Pressure + Secuelas + Stage, 
                     method = "nearest", data = Patients)
newdf <- match.data(mod_match)
library(tidyr)
New_Patients_for_plot <- newdf %>% 
  pivot_longer(cols = c("Temperature",
                        "Pulse",
                        "Hemoglobin",
                        "BMI",
                        "Leu",
                        "Sugar",      
                        "Age"),
               names_to = "names",
               values_to = "value")

GV(New_Patients_for_plot)










#Отсортировали
res_match <- mod_match$match.matrix
PatientT1 <- subset(newdf, Group == TRUE)
PatientR1 <- subset(newdf, Group == FALSE)

mydata <- subset(PatientR1, Id == res_match[1])
for (i in 2:n)
{
  mmm <- subset(PatientR1, Id == res_match[i])
  mydata <- rbind(mydata, mmm)
}

Patients_match <- rbind(mydata, PatientT1)
Patients_match$Group <- as.factor(Patients_match$Group)




