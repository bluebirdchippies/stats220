library(tidyverse)
library(jsonlite)
library(magick)
library(rvest)
library(lubridate)

beehive <- readRDS("beehive.rds")
ministers <- readRDS("ministers.rds")
playlist <- read_csv("spotify_playlist_tracks.csv")

# We are making the primary key as the date
playlist <- playlist %>%
  mutate(year_added = year(date_added),
         month_day_added = format(date_added, "%m-%d"),
         month_added = month(date_added, label = TRUE))

beehive <- beehive %>%
  mutate(date_added = as.Date(datetime)) %>%
  mutate(month_day_added = format(date_added, "%m-%d")) %>%
  mutate(month_added = month(date_added, label = TRUE)) %>%
  separate_rows(portfolios, sep = ";")


#------ Creation of Line and Point Graph Data -----

# Created new datasets based on frequency of songs per day and then frequency of articles per day
line_playlist <- playlist %>%
  group_by(month_day_added) %>%
  summarise(count =n()) %>%
  ungroup()

line_beehive <- beehive %>%
  group_by(month_day_added) %>%
  summarise(count =n()) %>%
  ungroup()

# Combined datasets
line_graph_df <- right_join(line_playlist, line_beehive, by ="month_day_added") %>%
  rename(songs_added = 2,
         articles_created = 3)

# Normalised the data because ggplot was doing an error where it would not
# display some of the datapoints because they are out of range
line_graph_normalised <- line_graph_df %>%
  mutate(
    songs_added_scaled = songs_added / max(songs_added, na.rm = TRUE),
    articles_created_scaled = articles_created / max(articles_created, na.rm = TRUE)
  )
view(line_graph_normalised)
# ------------Creation of other graph Data -----------

# Wanted to do a second graph based off Artist for Playlist and
# Portfolio for articles PER MONTH
month_beehive <- beehive %>%
  count(month_added, portfolios, sort = TRUE) %>%
  distinct(month_added, .keep_all = TRUE)

month_playlist <- playlist %>%
  count(month_added, artist, sort = TRUE) %>%
  distinct(month_added, .keep_all = TRUE)

month_df <- inner_join(month_beehive, month_playlist, by = "month_added") %>%
  rename(portfolios = 2,
         portfolio_count = 3,
         artist_count =5)

view(month_df)
# WE are gonna make two line and point graphs for the frequency of songs added 
# vs frequency of articles per day regardless of year
# Then also frequency of portfolio vs frequency of artist per month


# Additional comment regarding the data, in some cases there were a lot more
# dates or months which had songs added but not articles created, so to display
# the most amount of data, some of the songs added data were removed in the joins.

graph1 <- ggplot(line_graph_normalised) +
  geom_line(aes(x = month_day_added, 
                y = songs_added_scaled, 
                colour = "Songs added", 
                group = 1), 
            linewidth = 0.8) +
  geom_point(aes(x = month_day_added, 
                 y = songs_added_scaled, 
                 colour = "Songs added"), 
             size = 1.1) +
  
  geom_line(aes(x = month_day_added, 
                y = articles_created_scaled, 
                colour = "Articles Created", 
                group = 1), 
            linewidth = 0.8) +
  geom_point(aes(x = month_day_added, 
                 y = articles_created_scaled, 
                 colour = "Articles Created"), 
             size = 1.1) +
  
  labs(title = "Frequency of songs added to my playlist vs articles regarding David Seymour created",
       x = "Date Added",
       y = "Normalised values of Songs added/articles created") +
  
  scale_colour_manual(
    values = c(
      "Songs added" = "#058ED9",
      "Articles Created" = "#Fe5f55"
    )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 6))
graph1     

graph2 <- ggplot(month_df, aes(x = month_added)) +
  geom_col(
    aes(y = portfolio_count, fill = "Articles"),
    width = 0.35,
    position = position_nudge(x = -0.2)
  ) +
  geom_col(
    aes(y = artist_count, fill = "Playlist"),
    width = 0.35,
    position = position_nudge(x = 0.2)
  ) +
  geom_text(
    aes(y = portfolio_count, label = portfolios),
    position = position_nudge(x = -0.2),
    vjust = -0.3,
    size = 2
  ) +
  geom_text(
    aes(y = artist_count, label = artist),
    position = position_nudge(x = 0.2),
    vjust = -0.3,
    size = 2
  ) +
  scale_fill_manual(
    values = c(
      "Articles" = "#F4A261",
      "Playlist" = "#058ED9"
    )
  ) +
  labs(
    title = "Most common Article portfolio and Playlist Artist by Month",
    x = "Month",
    y = "Count",
    fill = "Data source"
  ) +
  theme_minimal()

graph2

ggsave("my_viz1.png", graph1)
ggsave("my_viz2.png", graph2)

ggsave