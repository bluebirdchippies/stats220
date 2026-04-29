library(tidyverse)
library(httr)
library(magick)

api_key <- "w3fvkUmcE3AzHRStvJMJ7ra0zfxyJ97qDpf9rTw5qwLSryDsNB5mk5Jt"

url <- "https://api.pexels.com/v1/search?query=cool%20meerkats&per_page=80&limit=80"

response <- httr::GET(url, 
                      add_headers(Authorization = api_key))

data <- httr::content(response, 
                      as = "parsed", 
                      type = "application/json")

photo_data <- tibble(photos = data$photos) %>%
  unnest_wider(photos) %>%
  unnest_wider(src)

selected_photos <- photo_data %>% 
  mutate(photographer_longer_than_my_name = ifelse(str_count(photographer) > 13, "nah YEA", "Yea NAH nah yea nah")) %>% 
  # just wanted to see if the photographers name is longer then my name "Armand Spencer" (including spaces)
  mutate(name_words = str_count(photographer, " ") + 1) %>% 
  # counts number of words in photographers name
  mutate(meerkats_or_meercats = ifelse(str_detect(url, "meerkats"), "MEERKATS", "weirdos")) %>% 
  # sees what spelling they use for meerkats
  filter(str_detect(meerkats_or_meercats, "MEERKATS"))
 # filters based on whether they used MEERKATS or not, cause I only trust people who write Meerkats and NOT Meercats. It looks weird!
 # additionally, out of 80 original rows, only 16 uses meerkats so the 3rd mutate category does do something
write_csv(selected_photos, "selected_photos.csv")

# Summary Values

mean_height <- selected_photos$height %>% mean(na.rm = TRUE) %>% signif(4) #mean height
mean_width <- selected_photos$width %>% mean(na.rm = TRUE) %>% signif(4) # mean width
mean_size <- paste0("The mean size of photos in the selected photos are: ", mean_width, "x", mean_height) # just shows the mean values together
mean_size

max_name_size <- max(selected_photos$name_words) # shows the highest number of words in a name
paste0("The name with the most number of words has " ,max_name_size, " words in it.")

summary_for_longer_than_my_name <- selected_photos %>% 
  group_by(photographer_longer_than_my_name) %>% #grouped by Yea NAH nah yea nah or nah Yea
  summarise(mean_height_summary = signif(mean(height,na.rm = TRUE), 4) ) # grabbed mean height of the groups based on Yea NAH or nah Yea
nah <-summary_for_longer_than_my_name %>% select(mean_height_summary) %>% slice(1)
yea <-summary_for_longer_than_my_name %>% select(mean_height_summary) %>% slice(2)
paste("The mean height of photos taken by photographers with names longer then mine is:", yea, "and the mean height of photos taken by photographers 
      with names shorter than mine is:" ,nah)

average_height_for_photographer_shorter_than_my_name <- summary_for_longer_than_my_name$mean_height_summary[1] 
# grabs the first row, which is Yea Nah nah yea nah, so the average height for names of photographers shorter then mine
average_height_for_photographer_shorter_than_my_name
paste0("the average height for photographers with names shorter than mine is: ", average_height_for_photographer_shorter_than_my_name)

# Meme MAKING TIME

main_image_url <- selected_photos$landscape[8]
main_image <-image_read(path = large_url)
main_image_nearly <- image_scale(main_image, "1200") 
main_image_final <- main_image_nearly %>%
  image_crop("500x281", gravity = "center") %>% # zooms into the lemur face
  image_scale("1200") # resizes back to fit meme size



secondary_image_url <- selected_photos$landscape[10]
secondary_image <- image_read(path=secondary_image_url)
secondary_image_nearly <- image_scale(secondary_image, "1200")
secondary_image_final <- secondary_image_nearly %>%
  image_crop("800x450 + (-100) + 200") %>% # zooms into lemur face again for second image
  image_scale("1200") # resizes to fit meme size
secondary_image_final

tertiary_image_url <- selected_photos$landscape[12]
tertiary_image <- image_read(path=tertiary_image_url)
tertiary_image_final <- image_scale(tertiary_image, "1200")


text1 <- image_blank(width =1200,
                     height =200, 
                     color = "white") %>%
  image_annotate(text = "Me : Yo bro, DON'T turn around but-",
                 gravity = "Center", 
                 size = 40,
                 color = "black",
                 font = "Impact")

text2 <- image_blank(width =1200,
                     height =200, 
                     color = "white") %>%
  image_annotate(text = "Bro 0.00001s later",
                 gravity = "Center", 
                 size = 40,
                 color = "black",
                 font = "Impact")

frame1 <- c(text1, tertiary_image_final)
frame2 <- c(text2, secondary_image_nearly)
frame3 <- c(text2, secondary_image_final)
frame4 <- c(text2, main_image_nearly)
frame5 <- c(text2, main_image_final) # creates the image vector

frame1_f <- image_append(frame1, stack = TRUE)
frame2_f <- image_append(frame2, stack = TRUE)
frame3_f <- image_append(frame3, stack = TRUE)
frame4_f <- image_append(frame4, stack = TRUE)
frame5_f <- image_append(frame5, stack = TRUE) # adds the two images together

animation <- c(frame1_f, frame2_f, frame3_f, frame4_f, frame5_f)

lemur_meme <- image_animate(animation, fps=1) # gif completed

upload <-image_write(lemur_meme, "creativity.gif")




