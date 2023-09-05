plot_scores <- function(category, title_wording = "Average Community Belonging"){
  data_2019 %>% 
    group_by(council, {{category}}) %>% 
    summarise(average_score = mean(community_score)) %>% 
    ggplot(aes({{category}}, average_score)) +
    geom_boxplot() +
    theme_light() +
    labs(
      title = str_c("Community Belonging by ", title_wording),
      y = "Average Community Belonging Score",
      x = title_wording
    ) +
    theme(text = element_text(size = 14))
}