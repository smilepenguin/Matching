
categorical_tests <- data.frame(
  name = c("Blood", "Sex", "Pressure", "Stage", "Secuelas"),
  
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
  
  labels_1 = c('A',  'female','Low',       'S0', 'Sc0'),
  labels_2 = c('O',  'male',  'Is_normal', 'S1', 'Sc1'),
  labels_3 = c('B',   NA,     'High',      'S2', 'Sc2'),
  labels_4 = c('AB',  NA,      NA,         'S3',  NA) ,
  labels_5 = c(NA ,   NA,      NA,         'S4',  NA),
  
  stringsAsFactors = FALSE
)