library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(ggpubr)
library(MatchIt)

source("numerical_tests.r")

source("categorical_tests.r")

max_T = 500
All_tests<- seq(100, max_T, by = 10)

for (tests in 1:5) {

  All_num_R<-c()
  tests<- 1
    #num_T<-100
  for (num_T in seq(100, max_T, by = 10)){
    P_values = 0
    num_R <- num_T
    while(P_values != tests){
    P_values = 0
    num_R <- num_R + 1
    Patients <- c()
    
  for (k in 1:tests) {
    test_name <- numerical_tests[k, "name"]
    mu_T <- numerical_tests[k, "average_T"]
    mu_R <- numerical_tests[k, "average_R"]
    random_values <-
      c(
        round(rnorm(
          num_T, mean = mu_T, sd = numerical_tests[k, "sd"]
        ), digits = numerical_tests[k, "digits"]),
        round(rnorm(
          num_R, mean = mu_R, sd = numerical_tests[k, "sd"]
        ), digits = numerical_tests[k, "digits"])
      )
    Patients <- cbind(Patients, random_values)
    colnames(Patients)[k] <- numerical_tests[k, "name"] 
  }
  
 
  
  Patients <- data.frame(Patients)

  
  
  for (k in 1:tests) {
    length_var <- (ncol(categorical_tests) - 1) / 3
    N_a <- sum(is.na(categorical_tests[k,]))
    random_values <-
      c(
        sample(
          length_var,
          size = num_T,
          replace = TRUE,
          prob = categorical_tests[k, 1:length_var + 1]
        ),
        sample(
          length_var,
          size = num_R,
          replace = TRUE,
          prob = categorical_tests[k, (length_var + 2):(2 * length_var + 1)]
        )
      )
    Patients <- cbind(Patients, random_values)
    Patients$random_values <-
      factor(Patients$random_values)
             #, labels = categorical_tests[k, (2 * length_var +2):(ncol(categorical_tests) - N_a)])
    colnames(Patients)[ncol(Patients) ] <-
      categorical_tests[k, "name"]
  }
  Patients$ID <- c(1:(num_R + num_T))
  
  Patients$Group <-  as.logical(c(rep(1, num_T),
                                  rep(0, num_R)), levels = c('R', 'T'))
  
  
  
  fmla <- as.formula(paste("Group ~ ", paste(colnames(Patients)[1:(ncol(Patients)-2)], collapse = "+")))
  
  mod_match <-
    matchit(
      fmla,
      method = "nearest",
      data = Patients
    )
  newdf <- match.data(mod_match)
  library(tidyr)
  New_Patients_for_plot <- newdf %>%
    pivot_longer(cols = numerical_tests[c(1:tests),1],
                 names_to = "names",
                 values_to = "value")
  
  newdf$Group <-  factor(newdf$Group, labels = c('R', 'T'))
 
  for (z in 1:tests){
    if (
      wilcox.test(newdf[,z] ~ Group, newdf, var.equal = TRUE)$p.value > 0.05 &
      chisq.test(newdf[,(tests + z)], newdf$Group)$p.value > 0.05)
    P_values <- P_values + 1  
      }
  
    }
    All_num_R <- rbind(All_num_R, num_R)
    print(num_R)
  }
  All_tests <- cbind(All_tests, All_num_R)
}
All_tests <- data.frame(All_tests)
colnames(All_tests)[1] <- "number_T"

ggplot(All_tests, aes(x = number_T)) + 
  geom_point(size = 1, aes(y = V2)) + 
  geom_line( alpha= 0.5, aes(y = V2, color="num_tests = 2")) +
  geom_point(size = 1, aes(y = V3)) + 
  geom_line( alpha= 0.5, aes(y = V3, color="num_tests = 4")) +
  geom_point(size = 1, aes(y = V4)) + 
  geom_line( alpha= 0.5, aes(y = V4, color="num_tests = 6")) +
  geom_point(size = 1, aes(y = V5)) + 
  geom_line( alpha= 0.5, aes(y = V5, color="num_tests = 8")) +
  geom_point(size = 1, aes(y = V6)) + 
  geom_line( alpha= 0.5, aes(y = V6, color="num_tests = 10")) +
  theme_bw() +
  theme() +
  ylab("„исло пациентов, принимающих плацебо") +
  xlab("„исло пациентов, принимающих лекарства") +
  ggtitle(" оличество пациентов при разном количестве тестов") +
  scale_colour_manual("„исло тестов", values = c('red','blue', 'orange', 'green', 'yellow')) +
  scale_x_continuous(breaks = seq(100, max_T, 25)) +
  scale_y_continuous(breaks = seq(100, 2000, 50)) +
  theme(strip.text.x = element_text(size = 10, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size=10))
  
  
  
  #save(All_tests, file = "All_tests_wilcox.RData")
