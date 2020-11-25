GD <- function(x){
  return(ggplot(x, aes(x = value, fill = Group)) +
           geom_density(aes(fill = Group), alpha = 0.35) + geom_rug() +
           theme_bw() +
           ggtitle(("Distribution density of"))+
           facet_wrap(.~names, scales = "free") +
           scale_fill_manual (values = c("Blue", "Pink"), 
                              name = "Group name", 
                              labels = c("Recipient", "Treatment")) + 
           theme() +
           theme(strip.text.x = element_text(size = 16, face = 'bold'),
                 axis.title.x = element_text(size = 16),
                 axis.title.y = element_text(size = 16),
                 axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 legend.text = element_text(size = 14),
                 legend.title = element_text(size=14)))
}