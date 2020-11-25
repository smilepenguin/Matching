
n1 <- c(mean_R = 36.4, sdev = 0.5, accuracy = 1, mean_T = 37.0)
n2 <- c(mean_R = 75, sdev = 6, accuracy = 0, mean_T = 70)
n3 <- c(mean_R = 140, sdev = 18, accuracy = 0, mean_T = 130)
n4 <- c(mean_R = 35.5, sdev = 8, accuracy = 2, mean_T = 37.5)
n5 <- c(mean_R = 6.5, sdev = 1.45, accuracy = 1, mean_T = 6.702) #*10^9
n6 <- c(mean_R = 6.3, sdev = 1.11, accuracy = 2, mean_T = 6.7)
n7 <- c(mean_R = 38, sdev = 6.8, accuracy = 0, mean_T = 40)
nk <- rbind(Temperature = n1, Pulse = n2, Hemoglobin = n3, BMI = n4, Leu = n5, Sugar = n6, Age = n7)

c1 <- c(A_group = .4, O_group = .3, B_group = .207, AB_group = .093)
c2 <- c(Female = 0.6, Male = 0.4)
c3 <- c(Low_pressure = .27, Pressure_is_normal = .4, High_pressure = .33)
c4 <- c(S0 = 0.2, S1 = .21, S2 = .19, S3 = .22, S4 = 0.18)
c5 <- c(Sec0 = 0.25, Sec1 = .4, Sec2 = .35)

c11 <- c(A_group = .3, O_group = .4, B_group = .207, AB_group = .093)
c21 <- c(Female = 0.46, Male = 0.54)
c31 <- c(Low_pressure = .35, Pressure_is_normal = .3, High_pressure = .35)
c41 <- c(S0 = 0.1, S1 = .24, S2 = .19, S3 = .28, S4 = 0.19)
c51 <- c(Sec0 = 0.3, Sec1 = .3, Sec2 = .4)


r <- as.numeric(format(round(rnorm(n, n6[1], n6[2]), n6[3]), nsmall = n6[3]))