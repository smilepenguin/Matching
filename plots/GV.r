GV <- function(x){
  ggplot(x, aes(y = value, x = Group, fill = Group, group = Group)) +
    geom_violin(alpha = 0.3) +
    geom_boxplot(alpha = 0.1) +
    stat_compare_means(size = 4) +
    ggtitle("violin plots") +
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
  
}
