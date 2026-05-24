library(tidyverse)
apple_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR6jVuO3F3DNwX1WApTvCfYqfjehcNKHmuDqupk2_0vJe0lnf81dmUlsXZGkZKmaCeallS5Dqch05ks/pub?gid=1338968646&single=true&output=csv") %>%
  slice(10 : 66)

result <- apple_data %>%
  select(trackName) %>%
  slice(45) %>%
  nchar()
result

new_data <- apple_data %>%
  mutate(track_name_lower = str_to_lower(trackName))
new_data$track_name_lower[55]

newdata1 <- new_data %>%
  mutate(track_name_clean = str_remove_all(track_name_lower,"[[:punct:]]"))
newdata1$track_name_clean[26]

newdata2 <- newdata1 %>%
  separate_rows(track_name_clean) 
nrow(newdata2)

length(unique(newdata2$track_name_clean))
