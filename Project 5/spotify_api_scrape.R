# ------------ DISCLAIMER -----------
# MADE COMPLETELY WITH AI TO GET SPOTIFY METADATA

library(httr2)
library(tidyverse)
library(lubridate)
library(jsonlite)

client_id <- "32e7afada83a4e2fb892e072bb57fb56"
client_secret <- "41f16a8d8b7b45cbbd56815e3cbf4934"
redirect_uri <- "http://127.0.0.1:1410/"

scope <- "playlist-read-private playlist-read-collaborative"

auth_url <- paste0(
  "https://accounts.spotify.com/authorize?",
  "client_id=", client_id,
  "&response_type=code",
  "&redirect_uri=", URLencode(redirect_uri, reserved = TRUE),
  "&scope=", URLencode(scope, reserved = TRUE)
)

browseURL(auth_url)


returned_url <- "http://127.0.0.1:1410/?code=AQC7wR7JGd0Yb7vOnrKVH_DnD4pJFuR7MEVwVqN0IXPPdHXeYCdkQgjds5gxAu_y7sI1DhsLHJF4ZbRlKx4SyeuN-bG5N8LzKBPpuyt-24uvc4DP8huoxNVFYpV5e0i_pZeHGEO85KWmPtmOVhtENh7vx5-RHg3hmcrbYa3kgmBQuw4WYezQrt67nihyhOatIJjbEgwqDQHBBm4NSvwP_TFUltz6bLmCvuoSWIJ2"

auth_code <- returned_url %>%
  str_extract("(?<=code=)[^&]+")

token_response <- request("https://accounts.spotify.com/api/token") %>%
  req_auth_basic(client_id, client_secret) %>%
  req_body_form(
    grant_type = "authorization_code",
    code = auth_code,
    redirect_uri = redirect_uri
  ) %>%
  req_perform() %>%
  resp_body_json()

access_token <- token_response$access_token

access_token

playlists_raw <- request("https://api.spotify.com/v1/me/playlists") %>%
  req_auth_bearer_token(access_token) %>%
  req_url_query(limit = 50) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE)

my_playlists <- as_tibble(playlists_raw$items) %>%
  transmute(
    playlist_name = name,
    playlist_id = id
  )

View(my_playlists)

playlist_id <- "3glPdwjR75sGNcw8dLVX1G"

# Get the playlist, including the first 100 tracks
playlist_full <- request(
  paste0("https://api.spotify.com/v1/playlists/", playlist_id)
) %>%
  req_auth_bearer_token(access_token) %>%
  req_perform() %>%
  resp_body_json(simplifyVector = FALSE)

# First page of tracks
all_items <- playlist_full$items$items

# Spotify gives a "next" URL if there are more tracks
next_url <- playlist_full$items[["next"]]

# Keep collecting pages while there is a next page
while (!is.null(next_url)) {
  
  Sys.sleep(0.5)  # polite pause so we don't spam Spotify
  
  next_page <- request(next_url) %>%
    req_auth_bearer_token(access_token) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = FALSE)
  
  all_items <- c(all_items, next_page$items)
  
  next_url <- next_page[["next"]]
}

length(all_items)

get_first_artist_name <- function(x) {
  artists <- x$item$artists
  
  if (is.null(artists) || length(artists) == 0) {
    return(NA_character_)
  }
  
  name <- artists[[1]]$name
  
  if (is.null(name) || length(name) == 0) {
    return(NA_character_)
  }
  
  name
}

get_first_artist_id <- function(x) {
  artists <- x$item$artists
  
  if (is.null(artists) || length(artists) == 0) {
    return(NA_character_)
  }
  
  id <- artists[[1]]$id
  
  if (is.null(id) || length(id) == 0) {
    return(NA_character_)
  }
  
  id
}

get_safe_value <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  as.character(x)
}

playlist_tracks <- tibble(item = all_items) %>%
  mutate(
    added_at = map_chr(item, ~ get_safe_value(.x$added_at)),
    date_added = as.Date(added_at),
    
    song = map_chr(item, ~ get_safe_value(.x$item$name)),
    
    artist = map_chr(item, get_first_artist_name),
    artist_id = map_chr(item, get_first_artist_id),
    
    album = map_chr(item, ~ get_safe_value(.x$item$album$name)),
    release_date = map_chr(item, ~ get_safe_value(.x$item$album$release_date))
  ) %>%
  select(date_added, song, artist, artist_id, album, release_date)

View(playlist_tracks)

write_csv(playlist_tracks, "spotify_playlist_tracks.csv")
