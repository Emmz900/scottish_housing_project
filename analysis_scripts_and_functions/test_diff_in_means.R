test_diff_in_means <- function(data, column, category, n_reps = 500){
  null_dist <- data %>% 
    filter({{column}} != "All") %>% 
    mutate(test = ({{column}} == category)) %>% 
    specify(score ~ test) %>% 
    hypothesise(null = "independence") %>% 
    generate(reps = n_reps, type = "permute") %>% 
    calculate(stat = "diff in means", order = c(TRUE, FALSE))
  
  obs_stat <- data %>% 
    filter({{column}} != "All") %>% 
    mutate(test = ({{column}} == category)) %>% 
    specify(score ~ test) %>%  
    calculate(stat = "diff in means", order = c(TRUE, FALSE))
  
  p <- null_dist %>% 
    get_p_value(obs_stat = obs_stat, direction = "both") %>% 
    pull()
  
  cat(category, ": p = ", p, sep = "")
}
