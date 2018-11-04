library(googleAnalyticsR)
library(tidyverse)
library(ChannelAttribution)
library(stringr)

source("helper-functions.R")

# Set the parameters for our MCF data  
start <-"2018-01-01"
end <-  "2018-04-01"
view_id <- "your_view_id"
# Note the leading zero below, 
# for single-digit goal slots
goal_slot <- "04" 

# Get goal data from the MCF API
goal_data <- ga_mcf_data(view_id,
                         start, 
                         end, 
                         goal_slot)

# Tidy up the data, ready for 
# ChannelAttribution package
att_models <- get_attribution_models(goal_data)

# Plot the total conversions
ggplot(att_models, 
       aes(channel_name, attribution_value, fill = model_type)) +
  geom_bar(stat='identity', position='dodge') +
  labs(
    title = "Total Conversions by model",
    subtitle = paste0("Source: Google Analytics MCF API, ",
                      start, " to ", end),
    y = "Conversion value",
    x = "Channel",
    fill = "Model Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 90)
  )

ggsave("example_models.png", width = 9.5,
       height = 5,
       units = "in",
       dpi = 300)


# Trended attribution values over time, by channel
trendable_att <- function(start, end) {
  view_id <- "your_view_id"
  goal_slot <- "04" 
  
  goal_data <- ga_mcf_data(view_id,
                           start, 
                           end, 
                           goal_slot)
  
  # Tidy up the data, ready for 
  # ChannelAttribution package
  att_models <- get_attribution_models(goal_data) %>% 
    mutate(start = start, end = end)
  att_models
}

dates_start <- seq.Date(from = as.Date("2017-04-01"), 
                       by = "month", length.out = 8)
dates_end <- seq.Date(from = as.Date("2017-04-30"),
                       by = "month", length.out = 8)

att_models_trended <- map2_df(dates_start, dates_end, trendable_att)

# Plot the trended view
att_models_trended %>% 
  mutate(channel_name = str_replace_all(channel_name, ".*unavailable.*", "Affiliate")) %>% 
  filter(model_type == "markov_model") %>% 
  ggplot(aes(start, 
             attribution_value, 
             colour = channel_name)) +
  geom_point() + geom_line() +
  labs(
    title = "Total Conversion value over time, Markov Model",
    subtitle = paste0("Source: Google Analytics MCF API, ",
                      dates_start[1], " to ", 
                      dates_start[length(dates_start)]),
    y = "Conversion value",
    x = "Channel",
    colour = "Channel Name"
  ) +
  scale_y_continuous(labels = scales::comma)

ggsave("example_models_trended.png", width = 9.5,
       height = 5,
       units = "in",
       dpi = 300)
