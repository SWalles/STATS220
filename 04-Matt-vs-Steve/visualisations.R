# Libraries
library(tidyverse)

# Read Data
yt_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQJKLxOOzYvnjWlvI8FwpELzy_UzvIIKObq_GmloKkQPE-OSCdskILSedjfyZrrv8NpdChmYTR9kiUX/pub?gid=0&single=true&output=csv")

# Colour Schemes
matt_colours <- c("#e5b985", "#ae2809", "#3f535c", "#262e2e")
steve_colours <- c("#a3ac5c", "#5b5261", "#ffffff")

# Plots
# Scales
scale_col <- scale_color_manual(values = c(matt_colours[2], steve_colours[1]),
                                labels = c("Stand Up Maths", "Steve Mould"))
scale_fill <- scale_fill_manual( values = c(matt_colours[2], steve_colours[1]),
                                 labels = c("Stand Up Maths", "Steve Mould"))

# Cumulative Duration
cumm_data <- yt_data %>% 
  arrange(datePublished) %>%
  group_by(channelId) %>% 
  mutate(cumm_min = cumsum(duration / 60)) %>% 
  ungroup()

cumm_dur_plot <- cumm_data %>%
  ggplot() +
  geom_area(aes(x = datePublished, 
                y = cumm_min, 
                fill = channelName,
                colour = channelName), 
            alpha = 0.7,
            position = "identity") +
   scale_col + scale_fill +
  theme(legend.position = "bottom", 
        panel.background = element_rect(fill = "#f5f5f5")) +
  labs(title = "Cumulative Duration of Videos over Time",
       x = "Publish Date",
       y = "Cumulative Duration (min)",
       colour = "Channel",
       fill = "Channel")

cumm_dur_plot

cumm_data %>% 
  group_by(channelName) %>% 
  summarise(sum(duration),
            max(cumm_min))

# Views vs. Duration
view_dur_plot <- yt_data %>% 
  ggplot(aes(x = duration / 60, y = log(viewCount), colour = channelName)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_col +
  theme(panel.background = element_rect(fill = "#f5f5f5")) +
  labs(title = "Cumulative Duration of Videos over Time",
       x = "Video Duration (min)",
       y = "Log Number of Views",
       colour = "Channel",
       fill = "Channel")

view_dur_plot

# Number of shorts
yt_data %>% 
  filter(duration <= 60) %>% 
  group_by(channelName) %>% 
  summarise(n())
  
# Titles
# Data manipulation
title_data <- yt_data %>% 
  mutate(is_short = duration <= 60) %>% 
  select(channelName, title, is_short) %>% 
  separate_rows(title, sep = " ") %>% 
  mutate(title = str_to_lower(title))

title_plot_data <- title_data %>% 
  group_by(channelName, title) %>% 
  summarise(count = n()) %>% 
  filter(!(str_detect(title, "#shorts|the|a|you|-"))) %>% 
  arrange(desc(count)) %>%
  slice(1:10)

# To name facets
facet_labels <- c("Stand Up Maths", "Steve Mould")
names(facet_labels) <- c("@standupmaths", "@SteveMould")

title_plot <- title_plot_data %>% 
  ggplot() +
  geom_col(aes(x = reorder(title, desc(count)),
               y = count,
               fill = channelName)) +
  facet_wrap(vars(channelName), ncol = 1,
             labeller = labeller(channelName = facet_labels)) +
  scale_fill +
  theme(legend.position = "none", 
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "#f5f5f5")) +
  labs(title = "Words Commonly Used in Titles",
       x = "Common Words")

title_plot
  
# Save plots as images

ggsave("plot1.png", title_plot, scale = 2)
ggsave("plot2.png", cumm_dur_plot, scale = 2)
ggsave("plot3.png", view_dur_plot, scale = 2)
