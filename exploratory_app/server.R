# Server ------------------
server <- function(input, output, session) {
  
  ## Summary plots ----------------
  output$gender_plot <- renderPlot({
    plot_scores(gender, "Gender")
  })
  
  output$urban_plot <- renderPlot({
    plot_scores(urban_rural, "Urban or Rural")
  })
  
  output$simd_plot <- renderPlot({
    plot_scores(simd, "SIMD")
  })
  
  output$house_plot <- renderPlot({
    plot_scores(household_type, "Household Type")
  })
  
  output$tenure_plot <- renderPlot({
    plot_scores(tenure, "Type of Tenure")
  })
  
  output$green_plot <- renderPlot({
    plot_scores(greenspace, "Distance to Greenspace")
  })
  
  ## Neighbourhood plots --------------------
  output$score_plot_n <- renderPlot({
    neighbourhood_rating %>% 
      filter(area == input$area_input_n, measurement == "Percent",
             get(input$variable_input_n) != "All") %>% 
      summarise(mean_score = sum(score),
                .by = c(year, input$variable_input_n)) %>% 
      ggplot(aes(year, mean_score, colour = get(input$variable_input_n))) +
      geom_line() +
      geom_point(size = 5) +
      scale_x_continuous(breaks = seq(2013, 2019, 1)) +
      #scale_y_continuous(limits = c(-1, 1)) +
      geom_vline(xintercept = input$year_input_n,
                 colour = "black", alpha = 0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Neighbourhood Rating Score",
        x = "Year",
        y = "Average Score (-1 to 1)",
        colour = "Classification"
      )
  })
  
  output$percentage_plot_n <- renderPlot({
    neighbourhood_rating %>% 
      filter(area == input$area_input_n, measurement == "Percent",
             get(input$variable_input_n) != "All",
             year == input$year_input_n) %>% 
      select(input$variable_input_n, neighbourhood_rating, value) %>% 
      ggplot(aes(neighbourhood_rating, value, fill = get(input$variable_input_n))) +
      geom_col(position = "dodge") +
      geom_text(aes(label = paste(value, "%")), vjust = -1,
                position = position_dodge(0.9), size = 8) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Neighbourhood Rating Responses",
        x = "Neighbourhood Rating",
        y = "Percentage of survey responses",
        fill = "Classification"
      )
  })
  
  ## Community plots -----------------
  output$score_plot_c <- renderPlot({
    community_belonging %>% 
      filter(area == input$area_input_c, measurement == "Percent",
             get(input$variable_input_c) != "All") %>% 
      summarise(total_score = sum(score),
                .by = c(year, input$variable_input_c)) %>% 
      ggplot(aes(year, total_score, colour = get(input$variable_input_c))) +
      geom_line() +
      geom_point(size = 5) +
      scale_x_continuous(breaks = seq(2013, 2019, 1)) +
      geom_vline(xintercept = input$year_input_c,
                 colour = "black", alpha = 0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Community Belonging Score",
        x = "Year",
        y = "Average Score (-1 to 1)",
        colour = "Classification"
      )
  })
  
  
  output$percentage_plot_c <- renderPlot({
    community_belonging %>% 
      filter(area == input$area_input_c, measurement == "Percent",
             get(input$variable_input_c) != "All",
             year == input$year_input_c) %>% 
      select(input$variable_input_c, community_belonging, value) %>% 
      ggplot(aes(community_belonging, value, fill = get(input$variable_input_c))) +
      geom_col(position = "dodge") +
      geom_text(aes(label = paste(value, "%")), vjust = -1,
                position = position_dodge(0.9), size = 8) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Community Belonging Responses",
        x = "Community Belonging",
        y = "Percentage of survey responses",
        fill = "Classification"
      )
  })
  
  ## Maps ------------------------
  
  ### Neighbourhood -------------
  output$neighbourhood_map <- renderPlot({
    ggplot(spatial_neighbourhood_joined, aes(fill = score)) +
      geom_sf() +
      scale_fill_distiller(palette = "PuBu", direction = 1) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Average Neighbourhood Ratings",
        subtitle = "Scale is -1 to 1",
        fill = "Score"
      )
  } #, height = 520, width = 450
  )
  
  output$neighbourhood_top <- renderTable({
    neighbourhood_rating %>% 
      filter(measurement == "Percent", gender != "All") %>% 
      group_by(feature_code, area, year, gender) %>% 
      summarise(total_score = sum(score)) %>% 
      group_by(area) %>% 
      summarise(avg_neighbourhood_score = mean(total_score)) %>% 
      slice_max(avg_neighbourhood_score, n = 5) %>% 
      rename("Average Neighbourhood Rating" = avg_neighbourhood_score) %>% 
      rename("Council Area" = area)
  })
  
  output$neighbourhood_bottom <- renderTable({
    neighbourhood_rating %>% 
      filter(measurement == "Percent", gender != "All") %>% 
      group_by(feature_code, area, year, gender) %>% 
      summarise(total_score = sum(score)) %>% 
      group_by(area) %>% 
      summarise(avg_neighbourhood_score = mean(total_score)) %>% 
      slice_min(avg_neighbourhood_score, n = 5) %>% 
      rename("Average Neighbourhood Rating" = avg_neighbourhood_score) %>% 
      rename("Council Area" = area)
  })
  
  ## Community ---------------
  output$community_map <- renderPlot({
    ggplot(spatial_community_joined, aes(fill = score)) +
      geom_sf() +
      scale_fill_distiller(palette = "PuBu", direction = 1) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Average Community Belonging",
        subtitle = "Scale is -1 to 1",
        fill = "Score"
      )
  } #, height = 520, width = 450
  )
  
  output$community_top <- renderTable({
    community_belonging %>% 
      filter(measurement == "Percent", gender != "All") %>% 
      group_by(feature_code, area, year, gender) %>% 
      summarise(total_score = sum(score)) %>% 
      group_by(area) %>% 
      summarise(avg_score = mean(total_score)) %>% 
      slice_max(avg_score, n = 5) %>% 
      rename("Average Community Belonging" = avg_score) %>% 
      rename("Council Area" = area)
  })
  
  output$community_bottom <- renderTable({
    community_belonging %>% 
      filter(measurement == "Percent", gender != "All") %>% 
      group_by(feature_code, area, year, gender) %>% 
      summarise(total_score = sum(score)) %>% 
      group_by(area) %>% 
      summarise(avg_score = mean(total_score)) %>% 
      slice_min(avg_score, n = 5) %>% 
      rename("Average Community Belonging" = avg_score) %>% 
      rename("Council Area" = area)
  })
  
  ## Neighbourhood change -------------
  
  output$neighbourhood_change <- renderPlot({
    ggplot(spatial_neighbourhood_joined, aes(fill = diff)) +
      geom_sf() +
      scale_fill_gradient2(low = "#e9a3c9", mid = "#f7f7f7", high = "#a1d76a") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Change in Neighbourhood Ratings",
        subtitle = "2013-2019",
        fill = "Change"
      )
  } #, height = 520, width = 450
  )
  
  output$neighbourhood_change_top <- renderTable({
    spatial_neighbourhood_joined %>%
      as.data.frame() %>% 
      select(local_auth, diff) %>% 
      slice_max(diff, n = 5) %>% 
      mutate(diff = diff * 100) %>% 
      rename("Change" = diff) %>% 
      rename("Council Area" = local_auth)
  })
  
  output$neighbourhood_change_bottom <- renderTable({
    spatial_neighbourhood_joined %>%
      as.data.frame() %>%
      select(local_auth, diff) %>% 
      slice_min(diff, n = 5) %>% 
      mutate(diff = diff * 100) %>% 
      rename("Change" = diff) %>% 
      rename("Council Area" = local_auth)
  })
  
  ## Community change ------------
  output$community_change <- renderPlot({
    ggplot(spatial_community_joined, aes(fill = diff)) +
      geom_sf() +
      scale_fill_gradient2(low = "#e9a3c9", mid = "#f7f7f7", high = "#a1d76a") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Change in Community Belonging",
        subtitle = "2013-2019",
        fill = "Change"
      )
  } #, height = 520, width = 450
  )
  
  output$community_change_top <- renderTable({
    spatial_community_joined %>%
      as.data.frame() %>% 
      select(local_auth, diff) %>% 
      slice_max(diff, n = 5) %>% 
      mutate(diff = diff * 100) %>% 
      rename("Change" = diff) %>% 
      rename("Council Area" = local_auth)
  })
  
  output$community_change_bottom <- renderTable({
    spatial_community_joined %>%
      as.data.frame() %>% 
      select(local_auth, diff) %>% 
      slice_min(diff, n = 5) %>% 
      mutate(diff = diff * 100) %>% 
      rename("Change" = diff) %>% 
      rename("Council Area" = local_auth)
  })
  
}