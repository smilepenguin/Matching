num_T <- 10
num_R <- 20 

numerical_tests <- data.frame(
  name = c("Temperature", "Pulse", "Hemoglobin", "BMI", "Leu", "Sugar", "Age"),
  average_T = c(37, 70, 130, 37.5, 6.702, 6.7, 40),
  average_R = c(36.4, 75, 140, 35.5, 6.5, 6.3, 38),
  sd = c(0.5, 6, 18, 8, 1.45, 1.11, 6.8),
  digits = c(1,0, 0, 2, 1, 2, 0),
  stringsAsFactors = FALSE
)

Patients <- c()
for (k in 1:nrow(numerical_tests)){
  test_name <- numerical_tests[k,"name"]
  mu_T <- numerical_tests[k,"average_T"]
  mu_R <- numerical_tests[k,"average_R"]
  random_values <- 
    c(round(rnorm(num_T, mean = mu_T, sd = numerical_tests[k,"sd"]),digits = numerical_tests[k,"digits"]),
      round(rnorm(num_R, mean = mu_R, sd = numerical_tests[k,"sd"]),digits = numerical_tests[k,"digits"]))
  Patients <- cbind(Patients,random_values)
}

colnames(Patients) <- numerical_tests$name

Patients <- data.frame(Patients)

Patients$Group <- c(rep(1, num_T),
              rep(0, num_R))


categorical_tests <- data.frame(
  name = c("Blood_type", "Sex", "Pressure", "Stage", "Secuelas"),
 
  var_1_T = c(.4, .6, .27, .2, .25),
  var_2_T = c(.3, .4, .4, .21, .4),
  var_3_T = c(.207, 0, .33, .19, .35),
  var_4_T = c(.093, 0, 0, .22, 0),
  var_5_T = c(0, 0, 0, .18, 0),
     
  var_1_R = c(.3, .46, .35, .1, .3),
  var_2_R = c(.4, .54, .3, .24, .3),
  var_3_R = c(.207, 0, .35, .19, .4),
  var_4_R = c(.093, 0, 0, .28, 0),
  var_5_R = c(0, 0, 0, .19, 0),
  stringsAsFactors = FALSE
)

for (k in 1:nrow(categorical_tests)){
  length_var <- (ncol(categorical_tests) - 1)/2
  random_values <- 
    c(sample(length_var, size = num_T, replace = TRUE, prob = categorical_tests[k,1:length_var+1]),
      sample(length_var, size = num_R, replace = TRUE, prob = categorical_tests[k,(length_var+2):ncol(categorical_tests)]))
  Patients <- cbind(Patients, random_values)
  colnames(Patients)[nrow(numerical_tests)+1+k] <- categorical_tests[k,"name"] 
}

Patients$Group <-  as.logical(Patients$Group, levels = c('R','T'))
Patients$Blood <- factor(Patients$Blood, labels = c('A','O', 'B','AB'))
Patients$Sex <- factor(Patients$Sex, labels = c('female', 'male'))
Patients$Pressure <- factor(Patients$Pressure, labels = c('Low', 'Is_normal', 'High'))
Patients$Stage <- factor(Patients$Stage, labels = c('S0', 'S1', 'S2', 'S3', 'S4'))
Patients$Secuelas <- factor(Patients$Secuelas, labels = c('Sc0', 'Sc1', 'Sc2'))