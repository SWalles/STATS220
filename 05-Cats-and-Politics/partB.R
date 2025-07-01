library(tidyverse)
library(rvest)

# I have selected Minister Hon Erica Stanford
# Features I have noticed:
# My first and second article has vastly different *numbers of words*
# Paragraphs seem quite short only one or two sentences per paragraph
# Number of words and *number of paragraphs* are probably correlated.
# These releases also seem quote heavy, we could count the number of quotes
# Releases also all come with a date


# Get release URLs with given code
url <- "https://www.beehive.govt.nz/minister/hon-erica-stanford"

pages <- read_html(url) %>%
  html_elements(".release__wrapper") %>%
  html_elements("h2") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  paste0("https://www.beehive.govt.nz", .)

# Get Title and content for one page
page_url <- pages[1]

page <- read_html(page_url)

release_title <- page %>% 
  html_elements(".article__title") %>% 
  html_text2()

release_content <- page %>% 
  html_elements(".prose") %>% 
  html_text2()

# Function to get info from page (with given code)
get_release <- function(page_url){
  Sys.sleep(2)
  # print the page_url so if things go wrong
  # we can see which page caused issues
  print(page_url)
  page <- read_html(page_url)
  
  # add code to scrape the release title and release content
  release_title <- page %>% 
    html_elements(".article__title") %>% 
    html_text2()
  
  release_content <- page %>% 
    html_elements(".prose") %>% 
    html_text2()
  # add code to return a tibble created using these data objects
  return(tibble(release_title, release_content))
}

# Make Data Frame
release_data <- map_df(pages, get_release)

# Analysis
# Number of words in each release content
release_data <- release_data %>% 
  separate_rows(release_content) %>% 
  count(release_title) %>% 
  rename(num_words = n) %>% 
  inner_join(release_data)

# Number of paragraphs per release content
release_data <- release_data %>% 
  separate_rows(release_content, sep = "\n\n") %>% 
  count(release_title) %>% 
  rename(num_paragraphs = n) %>% 
  inner_join(release_data)

# Number of quotes
# Assuming only one quote per paragraph
release_data  <- release_data %>% 
  separate_rows(release_content, sep = "\n\n") %>% 
  mutate(has_quote = str_detect(release_content, '“')) %>% 
  group_by(release_title) %>% 
  summarise(num_quotes = sum(has_quote)) %>% 
  select(release_title, num_quotes) %>% 
  inner_join(release_data)

# Hon Erica Stanford is the Minister for Education and Immigration
# We want to classify each release to one of the two departments
# We will count the number of times "education" or "immigration" appears and classify accordingly

release_data <- release_data %>% 
  mutate(release_content = str_to_lower(release_content),
         release_content = str_remove_all(release_content, "[[:punct:]]")) %>% 
  separate_rows(release_content) %>% 
  mutate(is_education = str_detect(release_content, "education"),
         is_immigration = str_detect(release_content, "immigration")) %>% 
  group_by(release_title) %>%
  summarise(total_education = sum(is_education),
            total_immigration = sum(is_immigration)) %>% 
  mutate(department = ifelse(total_education > total_immigration,
                            "Education", "Immigration")) %>% 
  select(release_title, department) %>% 
  inner_join(release_data)

# Named summary values
mean_number_of_words <- round(mean(release_data$num_words), 2)

mean_number_of_words_per_paragraph <- round(
  sum(release_data$num_words) / sum(release_data$num_paragraphs), 2)

mean_number_of_quotes <- round(mean(release_data$num_quotes), 2)

percentage_of_education_releases <- round(
  sum(release_data$department == "Education") / length(release_data$department) * 100, 2)
  