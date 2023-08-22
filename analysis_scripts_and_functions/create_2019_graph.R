create_2019_graph <- function(data, category){
  data %>% 
    filter(area == "Scotland", measurement == "Percent", {{category}} != "All",
           year == "2019") %>% 
    select({{category}}, data, value) %>% 
    ggplot(aes(data, value, fill = {{category}})) +
    geom_col(position = "dodge") +
    geom_text(aes(label = paste(value, "%")), vjust = -1, position = position_dodge(0.9)) +
    scale_y_continuous(limits = c(0, 50)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 16)) +
    labs(
      title = "Community Belonging in 2019",
      x = "Community Belonging",
      y = "Percentage of survey responses",
      fill = "Classification"
    ) #+
  #scale_fill_manual(values = c("Rural" = "darkgreen", "Urban" = "darkgrey"))
}