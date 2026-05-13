library(magick)
mad <- image_read(path ="https://media1.tenor.com/m/21VaRs50mDUAAAAC/danganronpa-makoto-naegi.gif")
mad_small <- image_scale(mad, 300)

words <- image_blank(width =300, height = 100, color ="#FFFFFF") %>%
  image_annotate(text ="How it feels telling my girlfriend that one  \n of my  friends is getting married:", 
                 font="Impact",
                 size = "17",
                 gravity = "center")

done <- image_apply(mad_small, function(frame) {
  image_append(c(words, frame), stack = TRUE)
})


done <- image_animate(done)
done

image_write(done, "dml.gif")

