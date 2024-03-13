# Packages
library(magick)
library(tidyverse)

# Frame 1
frm1_words <- image_blank(width = 250, height = 50, color = "#91d2f2") %>% 
  image_annotate("Normal Distribution", gravity = "center", size = 20)

frm1_image <- image_read("https://www.scribbr.com/wp-content/uploads/2023/02/standard-normal-distribution-example.webp") %>% 
  image_scale("250x200")

frm1 <- c(frm1_words, frm1_image) %>% 
  image_append(stack = TRUE) %>% 
  image_extent("250x250", color = "#91d2f2") %>% 
  image_border(color = "black", geometry = "2x2")
  


# Frame 2
frm2_image <- image_read("https://i.ebayimg.com/images/g/jw4AAOSwvv1krzzl/s-l1200.webp") %>% 
  image_scale(geometry = "250x250")
frm2 <- frm2_image %>% 
  image_extent("250x250", color = "#91d2f2") %>% 
  image_border(color = "black", geometry = "2x2")



# Frame 3
frm3_words <- frm1_words

frm3_image <- image_read("https://www.investopedia.com/thmb/vEnFyFaX2zK96wVerxokPmEt3dU=/1500x0/filters:no_upscale():max_bytes(150000):strip_icc()/Clipboard01-fdb217713438416cadafc48a1e4e5ee4.jpg")  %>% 
  image_scale("250x200")

frm3 <- c(frm3_words, frm3_image) %>% 
  image_append(stack = TRUE) %>% 
  image_extent("250x250", color = "#91d2f2") %>% 
  image_border(color = "black", geometry = "2x2")



# Frame 4
frm4 <- frm2_image %>% 
  image_negate() %>% 
  image_extent("250x250", color = "#91d2f2") %>% 
  image_border(color = "black", geometry = "2x2")




# Assemble meme
meme_p1 <- c(frm1, frm2) %>% 
  image_append()

meme_p2 <- c(frm3, frm4) %>% 
  image_append()

meme <- c(meme_p1, meme_p2) %>% 
  image_append(stack = TRUE)

# View and save meme
meme

image_write(meme, "my_meme.png")



# Animate meme
meme_animation <- c(frm1, frm2, frm3, frm4) %>% 
  image_scale(geometry = "500x500") %>% 
  image_animate(fps = 0.5)
meme_animation

image_write(meme_animation, "my_animation.gif")
