library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(ggpubr)
library(MatchIt)
library("heatmaply")
source("numerical_tests_1.r")


#av_num_R <- matrix(0, (max_T - min_T)/step_T + 1, col_tests + 1)
#col_repeat = 10
min_av_R <- numerical_tests[1, "average_R"]
col_av_R = 6
step <- (1 - min_av_R)/(col_av_R - 1)
col_tests = 5
num_T = 100
step_R = 10
#for (rep in 1:col_repeat ){
  All_tests <- c()
  for (tests in 1:col_tests) {
    All_num_R <- c()
    
    
    for (num_av_R in 1:col_av_R) {
      P_values = 0
      num_R <- num_T
      while (P_values != tests) {
        P_values = 0
        num_R <- num_R + step_R
        Patients <- c()
        
        for (k in 1:tests) {
          test_name <- numerical_tests[k, "name"]
          mu_T <- numerical_tests[k, "average_T"]
          mu_R <- numerical_tests[k, "average_R"] + step*(num_av_R - 1)
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
        
        
        newdf$Group <-  factor(newdf$Group, labels = c('R', 'T'))
        
        for (z in 1:tests) {
          if (wilcox.test(newdf[, z] ~ Group, newdf, var.equal = TRUE)$p.value > 0.05)
            P_values <- P_values + 1
        }
        
      }
      All_num_R <- rbind(All_num_R, num_R/num_T)
      print(num_R)
    }
    All_tests <- cbind(All_tests, All_num_R)
    print(All_tests)
  }
  All_tests <- data.frame(All_tests)
  colnames(All_tests) <- c("1_test","2_tests","3_tests","4_tests","5_tests")
  rownames(All_tests) <- c(seq(min_av_R, 1, by = step))
 # print(rep)
#}

  heatmaply(
    All_tests,
    colors = viridis(n = 256,  option = "magma"),
    k_col = 2, 
    k_row = 2
  )
  
  
  gradient_col <- ggplot2::scale_fill_gradient2(
    low = "blue", high = "red", 
    midpoint = 4
  )
  heatmaply(
    All_tests,
    scale_fill_gradient_fun = gradient_col
  )
  