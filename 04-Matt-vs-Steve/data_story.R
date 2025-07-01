# Libraries
library(magick)
library(jsonlite)
library(tidyverse)
library(rvest)

# Colour Schemes
matt_colours <- c("#e5b985", "#ae2809", "#3f535c", "#262e2e")
steve_colours <- c("#a3ac5c", "#5b5261", "#ffffff")

# More Info
# Steve created an API that has some info concerning his and Matt's (Stand Up Maths) channels
# Here I am using it to get subscriber information for the two channels
data <- read_json("https://stevemould.com/api")
subs <- as_tibble(data$subsData)

# Load images
plot1 <- image_read("plot1.png")
plot2 <- image_read("plot2.png")
plot3 <- image_read("plot3.png")

# Backgrounds
matt_background <- image_blank(width = 600, height = 400, 
                               color = matt_colours[1])
steve_background <- image_blank(width = 600, height = 400, 
                                color = steve_colours[1])

# Intro
matt_banner <- image_read("https://yt3.googleusercontent.com/9RhKOSYxlwhy4jSygs7c2v2YG-bejGGXAARU7GVS21RaM2ceItieb9UHfvt2HshBD_bGr1UCQyU=w1707-fcrop64=1,00005a57ffffa5a8-k-c0xffffffff-no-nd-rj") %>% 
  image_scale(geometry = "600x300")

frame1 <- matt_background %>% 
  image_annotate("MATHS", 
                 gravity = "north", 
                 color = matt_colours[2], 
                 size = 100) %>%
  image_annotate(paste("Subscribers: ", subs$MattCount),
                 gravity = "south",
                 color = matt_colours[2], 
                 size = 50) %>% 
  image_composite(matt_banner,
                  gravity = "center",
                  operator = "atop")

steve_banner <- image_read("https://yt3.googleusercontent.com/tayEfGBw7rz5Kec_-Z4sN21m1gzBKEEYJXYRLI_GbGu8wVl8INWft32kgYEIcRr1P0ho6kHd=w1707-fcrop64=1,00005a57ffffa5a8-k-c0xffffffff-no-nd-rj") %>% 
  image_scale(geometry = "600x300")

frame2 <- steve_background %>% 
  image_annotate("SCIENCE",
                 gravity = "north",
                 color = steve_colours[2],
                 size = 100) %>% 
  image_annotate(paste("Subscribers: ", subs$SteveCount),
                 gravity = "south",
                 color = steve_colours[2], 
                 size = 50) %>% 
  image_composite(steve_banner,
                  gravity = "center",
                  operator = "atop")

intro_frame <- image_append(c(frame1, frame2)) %>% 
  image_annotate("&", gravity = "north", size = 100) %>% 
  image_border(color = "black") %>% 
  image_scale(geometry = "1200x400")

# Story Frames
# SF1
frame1 <- matt_background %>% 
  image_composite(plot1 %>% 
                    image_scale("550x350") %>% 
                    image_border(color = "black", geometry = "5x5"),
                  gravity = "center")

frame2 <- steve_background %>% 
  image_annotate(str_wrap("We can see hints to these creators' educational content in the common use of question words like: how, why, when, and does. We can also see that Steve is a bit 'wierd' :D", 40),
                 gravity = "center", 
                 color = steve_colours[3],
                 size = 30)

sf1 <- image_append(c(frame1, frame2)) %>%  
  image_border(color = "black") %>% 
  image_scale(geometry = "1200x400")

# SF2
frame1 <- matt_background %>% 
  image_annotate(str_wrap("Here we can see how the amount of content on each channel grew over time.", 40),
                 gravity = "center",
                 color = matt_colours[3],
                 size = 30)

frame2 <- steve_background %>% 
  image_composite(plot2 %>% 
                    image_scale("550x350") %>% 
                    image_border(color = "black",
                                 geometry = "5x5"),
                  gravity = "center")

sf2 <- image_append(c(frame1, frame2)) %>%  
  image_border(color = "black") %>% 
  image_scale(geometry = "1200x400")

# SF3
frame1 <- matt_background %>% 
  image_composite(plot3 %>% 
                    image_scale("550x350") %>% 
                    image_border(color = "black",
                                 geometry = "5x5"),
                  gravity = "center")

frame2 <- steve_background %>% 
  image_annotate(str_wrap("We can see that longer videos correlate with more views and that Steve seems to get more views than Matt on average, but has shorter videos.", 40),
                 gravity = "center", 
                 color = steve_colours[3],
                 size = 30)

sf3 <- image_append(c(frame1, frame2)) %>%  
  image_border(color = "black") %>% 
  image_scale(geometry = "1200x400")

# Conclusion Frame

frame1 <- image_append(rep(matt_background, 2)) %>% 
  image_annotate(str_wrap("To conclude, we have discovered that the curious nature of the creators are evident in the words they choose to use in their video titles. We also learned that Matt (Stand Up Maths) preferes to make longer videos than Steve. Finally, we found positive correlation between the length of a video and the number of views it got.", 80),
                 gravity = "center", 
                 color = matt_colours[3],
                 size = 30) %>% 
  image_border(color = matt_colours[2],
               geometry = "30x30") %>% 
  image_scale("1150x350")

conclusion_frame <- image_append(rep(steve_background, 2)) %>% 
  image_composite(frame1, 
                  gravity = "center") %>% 
  image_border(color = steve_colours[2]) %>% 
  image_scale(geometry = "1200x400")

# Animation
frames <- c(rep(intro_frame, 3),
            rep(sf1, 10), rep(sf2, 7), rep(sf3, 8),
            rep(conclusion_frame, 15))

gif <- image_animate(frames, fps = 1)

image_write(gif, "data_story.gif")
