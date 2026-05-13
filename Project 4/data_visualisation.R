library(tidyverse)

logged_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRetKKj9bMRzvsbYOusSWE0uw2oEJIxmDjFP6C2U79SCUlN05jlOH4OHHo5kVT4mpo5BdYi9q9NZh7w/pub?output=csv")


logged_data <- logged_data %>%
  rename(journey_mode = 2,
         duration = 3,
         start_time = 4)

view(logged_data)

#Number of each mode and its mean duration
quantity_modes <- logged_data %>%
  group_by(journey_mode) %>%
  summarise(n=n(),
            mean = mean(duration))

# makes bins for times of day
binned_data <- logged_data %>%
  mutate(start_time = hms(start_time),
         peakhour_bin = case_when(
           start_time >= hms("06:30:00") & start_time <= hms('08:30:00') ~ "Morning Peak",
           start_time >= hms("15:30:00") & start_time <= hms('18:00:00') ~ "Afternoon Peak",
           TRUE ~ "Off-peak hours"),
         daytime_bins = case_when(
           start_time >= hms("05:00:00") & start_time <= hms('12:00:00') ~ "Morning",
           start_time >= hms("12:00:00") & start_time <= hms('17:30:00') ~ "Afternoon",
           start_time >= hms("17:30:00") & start_time <= hms('23:00:00') ~ "Evening",
           TRUE ~ "Night"
         ))

# number of time journeys occurred in each mode and its mean
daytime_count <- binned_data %>%
  group_by(daytime_bins) %>%
  summarise(count=n(),
            mean = mean(duration))

# number of peak hours journeys occurred in each mode and it means
 #peak_peak_count <- binned_data %>%
  #group_by(peakhour_bin) %>%
#  summarise(count=n(),
        #    mean = mean(duration))

peak_peak_count <- binned_data %>%
  group_by(peakhour_bin, journey_mode) %>%
  summarise(count=n(),
            mean = mean(duration))

view(peak_peak_count)
# box and whisker plot with the time stamps

timestamp_data <- binned_data %>%
  separate(Timestamp, into = c("date", "time"), sep = " ") %>%
  mutate(date = dmy(date),
         time = as.numeric(hms(time)) / 3600)

date_data <- timestamp_data %>%
  group_by(date) %>%
  summarise(n_journeys = n())



view(timestamp_data)

graph1 <- ggplot(peak_peak_count) +
  geom_col(aes(x = reorder(peakhour_bin, -mean),
               y = mean, 
               fill = journey_mode),
           position = "dodge") +
  scale_fill_manual(
    values = c(
      "Bus" = "#058ED9",
      "Car" = "#F4EBD9"
    )
  ) +
  labs(title = "Mean Travel Duration during peak and off-peak hours",
       subtitle = "Grouped by Travel Mode",
       x = "Times of Day",
       y = "Mean Travel Time (min)",
       fill = "Journey Mode") +
  theme_minimal()
graph1  

graph2<- timestamp_data %>%
  ggplot(aes(x = time, y = "Box")) +
  geom_boxplot(outlier.shape = NA,
               width = 0.3,
               colour = "#D7AF70") +
  geom_jitter(aes(x = time),
              width = 0.02,
              height = 0.2,
              size = 1.2,
              colour = "#E76B74") +
  geom_text(data = quantity_modes,
            aes(x = 1,
                y = Inf,
                label = paste("count =", n)),
            colour = "black",
            size = 3,
            hjust = 0,
            vjust = 2
            ) +
  labs(x = "Time of Day (24h) ",
       title = "What time of day were Austin and I most likely to enter data?") +
  facet_wrap(~journey_mode, 
             scales = "free",
             labeller = labeller(
               journey_mode = c(
                 "Car" = "Armand",
                 "Bus" = "Austin"
               )
             )) +
  theme_bw()

graph2

graph3 <- date_data %>%
  ggplot(aes(x=date, y = n_journeys)) +
  geom_line(colour="#3B1F2B") +
  geom_point(colour = "#C73E1D") +
  labs(x = "Date",
       y = "Number of Entries Done",
       title = "When did Austin and I put in our data?") +
  theme(panel.background = element_rect(fill = "#ACACDE"),
        plot.background = element_rect(fill = "#ABDAFC"),
        
  )

graph3  

summary(timestamp_data)
ggsave("plot1.png",graph1)
ggsave("plot2.png",graph2)
ggsave("plot3.png",graph3)
