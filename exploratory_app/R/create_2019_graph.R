create_2019_graph_community <- function(category){
  
  community_belonging %>% 
    filter(area == "Scotland", measurement == "Percent", {{category}} != "All",
           year == "2019") %>% 
    select({{category}}, community_belonging, value) %>% 
    ggplot(aes(community_belonging, value, fill = {{category}})) +
    geom_col(position = "dodge") +
    geom_text(aes(label = paste(value, "%", sep = "")), vjust = -1,
              position = position_dodge(0.9), size = 3) +
    scale_y_continuous(limits = c(0, 50)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 16),
          ) +
    labs(
      title = "Community Belonging in 2019",
      x = "Community Belonging",
      y = "Percentage of survey responses",
      fill = str_wrap(str_to_title(str_replace_all(deparse(substitute(category)),
                                                   "_", " ")), 20)
    )
}

create_2019_graph_neighbourhood <- function(category){
  # legend_title <- category %>% 
  #   substitute() %>% 
  #   deparse() %>% 
  #   str_replace_all("_", " ") %>% 
  #   str_to_title()
  
  neighbourhood_rating %>% 
    filter(area == "Scotland", measurement == "Percent", {{category}} != "All",
           year == "2019") %>% 
    select({{category}}, neighbourhood_rating, value) %>% 
    ggplot(aes(neighbourhood_rating, value, fill = {{category}})) +
    geom_col(position = "dodge") +
    geom_text(aes(label = paste(value, "%", sep = "")), vjust = -1,
              position = position_dodge(0.9), size = 3) +
    scale_y_continuous(limits = c(0, 100)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 16)) +
    labs(
      title = "Neighbourhood Rating in 2019",
      x = "Neighbourhood Rating",
      y = "Percentage of survey responses",
      fill = str_wrap(str_to_title(str_replace_all(deparse(substitute(category)),
                                                   "_", " ")), 20)
    )
}