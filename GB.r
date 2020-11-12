library(tidyr)
GB<-function(x){
  Patients_for_plot <- x %>% 
    pivot_longer(cols = c(
                          "Blood",
                          "Sex",
                          "Pressure", 
                          "Stage",
                          "Secuelas"),
                 names_to = "names",
                 values_to = "value")
  
  return(ggplot(Patients_for_plot, aes(x = value, fill=Group)) +
           geom_bar(aes(fill = Group),alpha = 0.5, position = position_dodge(), width = 0.5) +
           ylab("Count") +
           ggtitle("Histogram") +
           facet_wrap(.~names, scales = "free") +
           scale_fill_manual (values = c("Blue", "Pink"), 
                              name = "Group name", 
                              labels = c("Recipient", "Treatment")) +
           theme_bw() +
           theme() +
           theme(strip.text.x = element_text(size = 16, face = 'bold'),
                 axis.title.x = element_text(size = 16),
                 axis.title.y = element_text(size = 16),
                 axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size=14))
  )
  
}