library(tidyverse)
library(jsonlite)
library(rvest)

html_files <- list.files("html", full.names = TRUE)

source("scrape_html.R")

beehive <- map_df(html_files, scrape_search_results)
view(beehive)

# removes duplicates
beehive <- beehive %>%
  distinct()
  
saveRDS(beehive, "beehive.rds")

# seperates rows based off the semi colon and removes NA values
minister_names1 <- beehive %>%
  separate_rows(ministers, sep = ";") %>%
  drop_na()

# turns it into a vector and no duplicates
minister_names <- minister_names1$ministers %>%
  unique()

# ------------ WIKIPEDIA TIME -----------

source("get_wikipedia_infobox.R")

ministers <- map_df(minister_names, get_wikipedia_infobox)

view(ministers)

saveRDS(ministers, "ministers.rds")
