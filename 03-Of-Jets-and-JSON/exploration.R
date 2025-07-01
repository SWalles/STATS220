# Load packages
library(tidyverse)
library(jsonlite)
library(magick)
library(ggwordcloud)

# Load data
json_data <- fromJSON("pixabay_data.json")
pixabay_photo_data <- json_data$hits

# Filter and select some of the photos
# Get list of column names
names(pixabay_photo_data)

# Keep only some variables, create new variables, and filter images to reduce number
# download_view_proportion is the proportion of people who downloaded an image after viewing it
# popularity ranks images logarithmically into 4 categories according to views
# A popular user is onkelglocke so is_onkelglocke checks if an image's user is onkelglocke
# We filter first by imageWidth and imageHeight to keep only photos of better than 4k resolution
# Since there were still to many photos we filtered to keep only half of the images with imageSize less than the median
selected_photos <- pixabay_photo_data %>% 
  select(previewURL, pageURL, tags, imageWidth, imageHeight, imageSize, views, downloads, collections, likes, comments, user) %>% 
  mutate(download_view_proportion = downloads / views,
         popularity = ifelse(views >= 1e5, "Very Popular",
                             ifelse(views < 1e5 & views >= 1e4, "Popular",
                                    ifelse(views < 1e4 & views >= 1e3, "Average",
                                           "Unpopular"))),
         is_onkelglocke = (user == "onkelglocke")) %>% 
  filter(imageWidth >= 3840 &
           imageHeight >= 2160) %>% 
  filter(imageSize < median(imageSize))

# Save new dataframe as csv file
write_csv(selected_photos, "selected_photos.csv")

# List most popular users for selected photos
selected_photos %>% 
  group_by(user) %>% 
  summarise(n()) %>% 
  arrange(desc(`n()`))

# Data exploration and summary values
glimpse(selected_photos)

# Some exploratory plots
selected_photos %>% 
  ggplot() +
  geom_point(aes(x = download_view_proportion, y = likes,), 
             colour = "#50c4f2")

selected_photos %>% 
  ggplot() +
  geom_point(aes(x = is_onkelglocke, y = download_view_proportion), 
             colour = "#50c4f2")

selected_photos %>% 
  ggplot() +
  geom_point(aes(x = popularity, y = download_view_proportion), 
             colour = "#50c4f2")

selected_photos %>% 
  ggplot() +
  geom_point(aes(x = collections, y = log(views)), 
             colour = "#50c4f2")

selected_photos %>% 
  ggplot() +
  geom_point(aes(x = views, y = likes), 
             colour = "#50c4f2")


# mean likes score (adjusted for views) by popularity
# Do higher popularity photos (logarithmically according to views) get more likes once we account for the effect of views?
selected_photos %>% 
  mutate(like_adj = likes / views * 1e4) %>% 
  group_by(popularity) %>% 
  summarise(mean_likes = mean(like_adj))
# No, there does not seem to be such an effect

# Range of views
view_range <- selected_photos %>% 
  pull(views) %>% 
  range()

# Number of different users
num_users <- selected_photos %>% 
  pull(user) %>% 
  unique() %>% 
  length()

# Number of photos by onkelglocke
selected_photos %>% 
  pull(is_onkelglocke) %>% 
  sum()

# 75% of the selected photos have a percentage of downloads per views between these values
mid_75_down_view_percentage <- selected_photos %>% 
  pull(download_view_proportion) %>% 
  quantile(c(0.125, 0.875)) * 100 %>% 
  round()

# Create animated GIF
photos <- selected_photos %>% 
  pull(previewURL) %>% 
  image_read() %>%
  image_resize(geometry = "250x250")

animated_photos <- image_animate(photos, fps = 1)

animated_photos %>% 
  image_write(path = "my_photos.gif", format = "gif")

# Tags Analysis
# Separate tags into different rows
tags <- selected_photos %>% 
  separate_rows(tags, sep = ", ")

# Arrange tags according to how frequently they are used
popular_tags <- tags %>% 
  group_by(tags) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt)) %>% 
  pull(tags)

# Five most popular tags
popular_tags[1:5]

# How many unique tags
length(popular_tags)

# Bar chart of the five most popular tags
tags %>% 
  filter(tags %in% popular_tags[1:5]) %>% 
  ggplot() +
  geom_bar(aes(x = tags), fill = "#50c4f2")

# Interesting table
tags_table <- tags %>% 
  group_by(tags) %>% 
  summarise(tag_count = n(),
            mean_views = mean(views),
            mean_downloads = mean(downloads),
            mean_likes = mean(likes),
            largest = paste0(round(max(imageSize) / 1e6, 2), "MB")) %>% 
  arrange(desc(tag_count))

# Try word cloud
tags_table %>% 
  ggplot() +
  geom_text_wordcloud(aes(label = tags, size = tag_count, colour = tag_count)) +
  scale_size_area(max_size = 6) +
  theme_minimal()
