# Get the goal_data ------------------------------------------------------------
ga_mcf_data <- function(viewId, start, end, goal_slot) {
  mcf_data <- google_analytics_3(id = view_id,
                                 start = start,
                                 end = end,
                                 dimensions = c("basicChannelGroupingPath", "conversionGoalNumber"),
                                 metrics = c("totalConversions"),
                                 type = "mcf",
                                 filter = paste0("mcf:conversionGoalNumber=~", goal_slot),
                                 max_results = 100000)
  mcf_data
}

# Tidy messy data
get_attribution_models <- function(goal_data) {
  # Fix messy data
  goal_data <- goal_data  %>% 
    mutate(totalConversions = as.numeric(totalConversions),
           basicChannelGroupingPath = stringr::str_replace_all(basicChannelGroupingPath, c("(NA|IMPRESSION|CLICK|\\:)" = "",
                                                                                           "Paid Search" = "Paid_Search",
                                                                                           "Organic Search" = "Organic_Search",
                                                                                           "Social Network" = "Social_Network",
                                                                                           "Display Advertising" = "Display_Advertising",
                                                                                           "Other Advertising" = "Other_Advertising")))
  
  # Run the models ----------------------------------------------------------
  
  # Build simple heuristic model
  H <- heuristic_models(goal_data, 'basicChannelGroupingPath', 'totalConversions')
  
  # Build the markov model
  M <- markov_model(goal_data, 'basicChannelGroupingPath', 'totalConversions', order = 1) 
  
  # Merge the two models
  R1 <- left_join(H, M, by = "channel_name") %>% 
    select(channel_name, first_touch,last_touch, linear_touch,
           markov_model = total_conversions) %>% 
    gather(key = model_type,
           value = attribution_value, -channel_name) %>% 
    arrange(model_type, channel_name) %>% 
    mutate(channel_name = forcats::fct_reorder(channel_name, desc(attribution_value)))
  R1
}
