library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(ggpubr)
library(MatchIt)

source("numerical_tests.r")

step_T = 50
step_R = 20
min_T = 100
max_T = 500
col_tests = 5
av_num_R <- matrix(0, (max_T - min_T)/step_T + 1, col_tests + 1)
col_repeat = 10


for (rep in 1:col_repeat ){
All_tests <- seq(min_T, max_T, by = step_T)
for (tests in 1:col_tests) {
  All_num_R <- c()
  
  
  for (num_T in seq(min_T, max_T, by = step_T)) {
    P_values = 0
    num_R <- num_T
    while (P_values != tests) {
      P_values = 0
      num_R <- num_R + step_R
      Patients <- c()
      
      for (k in 1:tests) {
        test_name <- numerical_tests[k, "name"]
        mu_T <- numerical_tests[k, "average_T"]
        mu_R <- numerical_tests[k, "average_R"]
        random_values <-
          c(
            round(
              rnorm(num_T, mean = mu_T, sd = numerical_tests[k, "sd"]),
              digits = numerical_tests[k, "digits"]
            ),
            round(
              rnorm(num_R, mean = mu_R, sd = numerical_tests[k, "sd"]),
              digits = numerical_tests[k, "digits"]
            )
          )
        Patients <- cbind(Patients, random_values)
        colnames(Patients)[k] <- numerical_tests[k, "name"]
      }
      
      
      
      Patients <- data.frame(Patients)
      
      
      Patients$ID <- c(1:(num_R + num_T))
      
      Patients$Group <-  as.logical(c(rep(1, num_T),
                                      rep(0, num_R)), levels = c('R', 'T'))
      
      
     ### tests for matching 
      fmla <-
        as.formula(paste("Group ~ ", paste(colnames(Patients)[1:(ncol(Patients) -
                                                                   2)], collapse = "+")))
      
      mod_match <-
        matchit(fmla,
                method = "nearest",
                data = Patients)
      newdf <- match.data(mod_match)
      library(tidyr)
      New_Patients_for_plot <- newdf %>%
        pivot_longer(cols = numerical_tests[c(1:tests), 1],
                     names_to = "names",
                     values_to = "value")
      
      newdf$Group <-  factor(newdf$Group, labels = c('R', 'T'))
      
      for (z in 1:tests) {
        if (wilcox.test(newdf[, z] ~ Group, newdf, var.equal = TRUE)$p.value > 0.05)
          P_values <- P_values + 1
      }
      
    }
    All_num_R <- rbind(All_num_R, num_R)
    print(num_R)
  }
  All_tests <- cbind(All_tests, All_num_R)
  print(All_tests)
}
av_num_R <- av_num_R + All_tests
rep
}
av_num_R <- av_num_R/col_repeat
av_num_R <- data.frame(av_num_R)


colnames(av_num_R)[1] <- "number_T"

ggplot(av_num_R, aes(x = number_T)) +
  geom_point(size = 1, aes(y = V2)) +
  geom_line(alpha = 0.5, aes(y = V2, color = "num_tests = 1")) +
  geom_point(size = 1, aes(y = V3)) +
  geom_line(alpha = 0.5, aes(y = V3, color = "num_tests = 2")) +
  geom_point(size = 1, aes(y = V4)) +
  geom_line(alpha = 0.5, aes(y = V4, color = "num_tests = 3")) +
  geom_point(size = 1, aes(y = V5)) +
  geom_line(alpha = 0.5, aes(y = V5, color = "num_tests = 4")) +
  geom_point(size = 1, aes(y = V6)) +
  geom_line(alpha = 0.5, aes(y = V6, color = "num_tests = 5")) +
  theme_bw() +
  theme() +
  ylab("„исло пациентов, принимающих плацебо") +
  xlab("„исло пациентов, принимающих лекарства") +
  ggtitle(" оличество пациентов при разном количестве тестов") +
  scale_colour_manual("„исло тестов",
                      values = c('red', 'blue', 'orange', 'green', 'yellow')) +
  scale_x_continuous(breaks = seq(100, max_T, step_T)) +
  scale_y_continuous(breaks = seq(100, 3000, step_T)) +
  theme(
    strip.text.x = element_text(size = 10, face = 'bold'),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )


library(tidyr)

tmydf <- setNames(data.frame(t(av_num_R[, -1])), av_num_R[,1])
#colnames(tmydf) <- c("T100","T150","T200","T250","T300","T350","T400","T450","T500")
tmydf <- cbind(tmydf, tests = seq(1, 5, by = 1))
All_tests_for_plot <- tmydf %>%
  pivot_longer(cols = colnames(tmydf[,-ncol(tmydf)]) ,
               names_to = "names",
               values_to = "value")

ggplot(All_tests_for_plot, aes(x = tests)) +
  geom_point(size = 1, aes(y = value)) +
  geom_line(alpha = 0.5, aes(y = value)) +
  facet_wrap(.~names, scales = "free") +
  ylab("Num_R") +
  ggtitle(" оличество пациентов, принимающих плацебо, при разном количестве тестов") +
  theme_bw() +
  theme() +
  theme(strip.text.x = element_text(size = 16, face = 'bold'),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size=14))


#save(All_tests, file = "All_tests_wilcox.RData")
