library(ggplot2)
library(dplyr)
library(stringr)
library(ggwordcloud)
library(ggthemes)

netflix <- read.csv('netflix.csv', sep = ';')

netflix <- netflix %>%
  mutate(Tags = str_replace_all(Tags, ",", " ")) %>%
  mutate(vector_tags = strsplit(Tags, split = " ", fixed = TRUE))

netflix_words <- data.frame() 

for (i in 1:nrow(netflix)) {
  for(j in 1:length(netflix$vector_tags[i][[1]])) {
    netflix_words <- netflix_words %>%
      rbind(netflix$vector_tags[i][[1]][j])
  }
}

words <- netflix_words %>%
  group_by(`X.Comedy.`) %>%
  summarise(nr = n())
  
ggplot(words, aes(label = `X.Comedy.`, size = nr)) +
  geom_text_wordcloud_area(rm_outside = TRUE, color = "white") +
  scale_size_area(max_size = 100) +
  theme(panel.background = element_rect(fill = "darkred"),
        plot.background = element_rect(fill = "darkred")) 
