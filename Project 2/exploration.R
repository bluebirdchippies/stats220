library(tidyverse)

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRetKKj9bMRzvsbYOusSWE0uw2oEJIxmDjFP6C2U79SCUlN05jlOH4OHHo5kVT4mpo5BdYi9q9NZh7w/pub?output=csv")

# testing
# view(logged_data)

# Renaming Variable Names
latest_data1 <- logged_data %>%
    rename(journey_mode = 2,
           duration_min = 3,
           journey_start_time = 4
           )

# Narrowing Dataframe to usable Vectors
latest_data <- latest_data1 %>%
  select( journey_mode,
          duration_min,
          journey_start_time)

view(latest_data)

# Viewing Summaries
max(latest_data$duration_min) # Longest Journey
min(latest_data$duration_min) # Shortest Journey


# Making new dataframe for mean values of duration
mean_data <- latest_data %>%
  group_by(journey_mode) %>%
  summarise(mean_duration = mean(duration_min, na.rm=TRUE))
view(mean_data)

# Plot Creation to compare mean Travel Times
ggplot(mean_data) +
         geom_col(aes(x =journey_mode, 
                  y = mean_duration),
                  fill = "#DE8F6E") +
         labs(title = "Time of Transport by Travel Mode",
              x = "Journey Mode",
              y = "Mean Duration")

# Plot Creatio to compare Frequency of Travel Modes
ggplot(latest_data) +
  geom_bar(aes(x =journey_mode),
           fill = "#ADFC92") +
  labs(title = "Frequency of Travel Modes",
       x = "Travel Mode",
       y = "Frequency")


  