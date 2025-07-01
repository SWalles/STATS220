library(tidyverse)
library(rvest)

# Given code, slightly modified
keywords <- c("education", "immigration", "data")

make_df <- function(i) {
search1 <- read_html(paste0("https://datalandscapes.online/scrapeable/speeches.php?search=", keywords[i])) %>%
  html_elements(".speech_summary") %>%
  html_text2() %>%
  tibble(keyword = keywords[i],
         results = .) %>%
  separate(results, into = c("year", "num_speeches"), sep = "--") %>%
  mutate(num_speeches = parse_number(num_speeches),
         year = parse_number(year))
return(search1)
}

# "When you are copy and pasting three time do something better."
# Create data frame
search1 <- make_df(1)
search2 <- make_df(2)
search3 <- make_df(3)

combined_search <- bind_rows(search1, search2, search3)

# Speeches Governments data
speeches_governments <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRHTFJcFmsIkjaFUCEwFWASaBOAR4X2Upx66C5Bhgc_WNc2JxxdRbbvyoewmvt_EjNdCNZZzzkENwLg/pub?gid=0&single=true&output=csv")

speech_data <- inner_join(combined_search, speeches_governments, by = "year")

# Visualisation
speech_data %>% 
  ggplot(aes(x = year, y = num_speeches, fill = keyword)) +
  geom_col(position = "dodge2") +
  labs(title = "Number of speeches on three different topics from 1996 to 2023",
       x = "Year",
       y = "Number of Speeches",
       fill = "Speech Topic") +
  theme_bw()
