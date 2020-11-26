library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(ggpubr)
library(MatchIt)

num_T <- 50
num_R <- 300

source("numerical_tests.r")


source("categorical_tests.r")

Patients <- c()
for (k in 1:nrow(numerical_tests)) {
  test_name <- numerical_tests[k, "name"]
  mu_T <- numerical_tests[k, "average_T"]
  mu_R <- numerical_tests[k, "average_R"]
  random_values <-
    c(round(rnorm(num_T, mean = mu_T, sd = numerical_tests[k, "sd"]), digits = numerical_tests[k, "digits"]),
      round(rnorm(num_R, mean = mu_R, sd = numerical_tests[k, "sd"]), digits = numerical_tests[k, "digits"]))
  Patients <- cbind(Patients, random_values)
  colnames(Patients)[k] <- numerical_tests[k, "name"]
}



Patients <- data.frame(Patients)


for (k in 1:nrow(categorical_tests)) {
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
    factor(Patients$random_values, labels = categorical_tests[k, (2 * length_var +
                                                                    2):(ncol(categorical_tests) - N_a)])
  colnames(Patients)[nrow(numerical_tests) + k] <-
    categorical_tests[k, "name"]
}
Patients$ID <- c(1:(num_R + num_T))
Patients$Group <-  as.logical(c(rep(1, num_T),
                                rep(0, num_R)), levels = c('R', 'T'))

library(tidyr)
Patients_for_plot <- Patients %>%
  pivot_longer(cols = numerical_tests$name,
               names_to = "names",
               values_to = "value")

source("plots/GV.r")
GV(Patients_for_plot)

source("plots/GH.r")
GH(Patients_for_plot)

source("plots/GD.r")
GD(Patients_for_plot)


source("plots/GB.r")
GB(Patients)


fmla <- as.formula(paste("Group ~ ", paste(colnames(Patients)[1:(ncol(Patients)-2)], collapse= "+")))

mod_match <-
  matchit(
    fmla,
    method = "nearest",
    data = Patients
  )
newdf <- match.data(mod_match)
library(tidyr)
New_Patients_for_plot <- newdf %>%
  pivot_longer(cols = numerical_tests$name,
               names_to = "names",
               values_to = "value")
GV(New_Patients_for_plot)


newdf$Group <-  factor(newdf$Group, labels = c('R', 'T'))
t.test(Temperature ~ Group, newdf, var.equal = TRUE)
chisq.test(newdf$Sex, newdf$Group)

