library(tidyverse)
library(rvest)
library(magick)

# My data context is different cat breeds and their qualities

# I need a list of some kind with different cat breeds. I also need some information about each breed, eg. personality, colour, etc.

# Website I want to scrape: https://www.petfinder.com/cats-and-kittens/breeds/
# Terms and Conditions: https://www.petfinder.com/terms-of-service/
# Note that bots are prohibited from accessing user generated data, but not information provided by petfinder
# robots.txt: https://www.petfinder.com/robots.txt
# Only prohibits access to some pages with /user/

#=============
# Functions
#=============
# Create a vector of attribute values with the names as the attribute names
create_attr_vector <- function(list) {
  names <- character(0)
  values <- numeric(0)
  for (i in 1:length(list)) {
    names <- c(names, str_to_lower(list[[i]][1]))
    values <- c(values, as.numeric(str_remove_all(list[[i]][2], " in 5")))
  }
  names(values) <- names %>% 
    str_replace_all(" ", "_")
  return(values)
}

# A function to visit each page and get a list of the urls that refers to the page that contains information on each breed
get_breed_urls <- function(urls){
  
  url_list <- character(0)
  
  for (i in 1:length(urls)) {
    
    Sys.sleep(2)
    
    breeds_page <- read_html(urls[i])
    
    links <- breeds_page %>% 
      html_elements("#breeds-grid") %>% 
      html_elements(".Card-module--articleCard--a4de8") %>% 
      html_attr("href")
    
    url_list <- c(url_list, links)
  }
  return(url_list)
}

# Takes url to page with a breed's data and returns a dataframe
breed_data <- function(url) {
  #Delay requests
  Sys.sleep(2)
  single_page <- read_html(url)
  
  # Get attributes
  # Breed Name
  breed_name <- single_page %>% 
    html_elements(".BreedCard-module--breedCard__title--eaa90") %>% 
    html_text2() %>% 
    str_remove_all(" Cats & Kittens")
  
  # Breed Image
  temp_img <- single_page %>% 
    html_elements(".BreedCard-module--breedCard--247d8 .gatsby-image-wrapper") %>% 
    html_elements("picture") %>% 
    html_elements("img") %>% 
    html_attr("src") %>% 
    unique()
  
  breed_img <- paste0("https://www.petfinder.com", temp_img)
  
  # Description
  breed_description <- single_page %>% 
    html_elements(".hasWysiwygContent") %>% 
    html_text2()
  breed_description <- breed_description[1]
  
  # Personality attributes
  temp_attr <- single_page %>% 
    html_elements(".ProgressBar-module--wrapper--c21aa") %>% 
    html_text2() %>% 
    unique()
  
  temp_attr_list <- str_split(temp_attr, "level ")
  breed_attr <- create_attr_vector(temp_attr_list)
  
  # Make dataframe from all gathered info 1 row
  breed_df <- tibble(breed_name, breed_img, breed_description)
  for (i in 1:length(breed_attr)) {
    breed_df <- bind_cols(breed_df, breed_attr[i])
  }
  breed_df <- breed_df %>% 
    rename_with(function(t){names(breed_attr)}, .cols = paste0("...", 4:15))
  
  return(breed_df)
}

#======
# Work
#======
url <- "https://www.petfinder.com/cats-and-kittens/breeds/"


# Multiple pages of results: (This did not work!)
# ----------------------------------------------------------------------------
# I originally wanted to scrape all 5 pages but each page kept giving the same results

# There are 5 pages of results that I want
# urls_5_pages <- paste0(url, "?page=",1:5)

# Get a list of url extensions that point to pages with information on each breed
# breed_urls <- paste0("https://www.petfinder.com", get_breed_urls(urls_5_pages))

# Test cases with 2 different pages give same results
# test <- read_html("https://www.petfinder.com/cats-and-kittens/breeds/?page=1")
# test %>% 
#   html_elements(".Card-module--articleCard--a4de8") %>% 
#   html_attr("href")

# test2 <- read_html("https://www.petfinder.com/cats-and-kittens/breeds/?page=2")
# test2 %>% 
#   html_elements(".Card-module--articleCard--a4de8") %>% 
#   html_attr("href")
# ----------------------------------------------------------------------------

# Get URLs to breed pages
breed_urls <- paste0("https://www.petfinder.com", get_breed_urls(url))

# Make dataframe with all data
cat_breed_data <- map_df(breed_urls, breed_data)

# Name and output named vectors
name <- cat_breed_data$breed_name
images <- cat_breed_data$breed_img
description <- cat_breed_data$breed_description
playfulness <- cat_breed_data$playfulness

name
images
description
playfulness

# Cat Bribe
cat_breed_data$breed_img %>% 
  image_read() %>% 
  image_scale(geometry = "500x500") %>% 
  image_animate(fps = 1)


