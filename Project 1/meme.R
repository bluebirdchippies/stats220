library(magick)
sunflower <- image_read(path = "https://camo.githubusercontent.com/a307c7dd605ef679655e973c10921107e6363d2282fa54c6f46eff5642cb5c9f/68747470733a2f2f692e696d67666c69702e636f6d2f366d376931382e706e673f61343932333132"
                        ) 
# new image
waltUHHH <- image_read(path ="https://images-r2-1.thebrag.com/rs/uploads/2020/09/breaking-bad.jpg")

sunflower.adj <- image_scale(sunflower, 500)
waltUHHH.adj <- image_scale(waltUHHH, 500)

# ORIGINAL TEXT/ Frame 1
content <- image_blank(width = 500,
                    height = 150,
                    color = "#FFFFFF") %>%
  image_annotate(text = "How I look at bro after Austin Sarney \n just  ran over five kids:",
                 color = "#000000",
                 size = 25,
                 font = "Impact",
                 gravity = "Center")
# Frame 2 Text
frame2text <- image_blank(width = 500,
                          height = 150,
                          color = "#FFFFFF") %>%
  image_annotate(text = "How I look at bro after AUSTIN SARNEY \n just ran over five kids:",
                 color = "#000000",
                 size = 30,
                 font ="Impact",
                 gravity ="Center")
# Frame 3 Text
frame3text <- image_blank(width = 500,
              height = 150,
              color = "#FFFFFF") %>%
          image_annotate(text = "How I look at bro after AUSTIN SARNEY \n justran over five kids??:",
                 color = "#000000",
                 size = 33,
                 font ="Impact",
                 gravity ="Center")
# Frame 4 Text
frame4text <- image_blank(width = 500,
                          height = 150,
                          color = "#FFFFFF") %>%
              image_annotate(text = "WALTUHHHHHHHHH",
                 color = "#000000",
                 size = 36,
                 font ="Impact",
                 gravity ="Center")


# composing it together
image_vector1 <- c(content, sunflower.adj)
image_vector2 <- c(frame2text, sunflower.adj)
image_vector3 <- c(frame3text, sunflower.adj)
image_vector4 <- c(frame4text, waltUHHH.adj)

#Stacking images together
finished1 <- image_append(image_vector1,
                         stack =TRUE )
finished2 <- image_append(image_vector2,
                          stack =TRUE)
finished3 <- image_append(image_vector3,
                          stack =TRUE)
finished4 <- image_append(image_vector4,
                          stack =TRUE)

finalised <- c(finished1, finished2, finished3, finished4)

actuallyDONE <- image_animate(finalised, fps = 1)


image_write(actuallyDONE, "meme.gif")
