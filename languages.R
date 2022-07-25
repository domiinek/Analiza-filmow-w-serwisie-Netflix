library(ggplot2)
library(dplyr)
library(stringr)
library(ggthemes)
library(treemapify)
library(viridis)
library(wesanderson)
library(gameofthrones)

netflix <- read.csv('netflix.csv', sep = ';')

netflix <- netflix %>%
  mutate(vector_languages = strsplit(Languages, split = ", ", fixed = TRUE))

netflix_languages <- data.frame() 

for (i in 1:nrow(netflix)) {
  for(j in 1:length(netflix$vector_languages[i][[1]])) {
    netflix_languages <- netflix_languages %>%
      rbind(netflix$vector_languages[i][[1]][j])
  }
}

languages <- netflix_languages %>%
  group_by(`X.Swedish.`) %>%
  summarise(nr = n()) %>%
  na.omit() 

ggplot(languages, aes(area = nr, fill = `X.Swedish.`, label = `X.Swedish.`)) +
  geom_treemap() +
  theme(legend.position = "none") +
  geom_treemap_text(colour = "white",
                    place = "centre") +
  # scale_fill_viridis(discrete = TRUE, option = 'F')
  scale_fill_got_d(option = "Lannister")
  
