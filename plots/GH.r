
GH <- function(x){
  return (ggplot(x, aes(x = value, group = Group)) +
            geom_histogram(aes(fill = Group), alpha = 0.3, binwidth = 2 ) +
            scale_x_continuous() +
            ylab("Count") +
            ggtitle("histogram")+
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