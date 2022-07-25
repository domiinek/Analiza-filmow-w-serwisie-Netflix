library(ggplot2)
library(dplyr)
library(stringr)
library(ggthemes)
library(treemapify)
library(viridis)
library(wesanderson)
library(maps)
library(paletteer)
library(gameofthrones)

netflix <- read.csv('netflix.csv', sep = ';')

netflix <- netflix %>%
  mutate(vector_countries = strsplit(`Country.Availability`, split = ",", fixed = TRUE))

netflix_countries <- data.frame() 

for (i in 1:nrow(netflix)) {
  for(j in 1:length(netflix$vector_countries[i][[1]])) {
    netflix_countries <- netflix_countries %>%
      rbind(netflix$vector_countries[i][[1]][j])
  }
}

world_map <- map_data("world")

countries <- netflix_countries %>%
  na.omit() %>%
  setNames("region") %>%
  group_by(region) %>%
  summarise(nr = n()) %>%
  mutate(region = ifelse(region == "United States", "USA", region),
         region = ifelse(region == "United Kingdom", "UK", region),
         region = ifelse(region == "Hong Kong", "China", region)) %>%
  full_join(world_map, by = "region")

ggplot(countries, aes(long, lat, group = group))+
  geom_polygon(aes(fill = nr), color = "antiquewhite")+
  # theme_map() +
  # scale_fill_distiller(palette = "YlOrRd") +
  # theme(plot.background = element_rect(fill = "antiquewhite")) +
  theme_wsj(title_family = "sans") +
  # scale_fill_distiller(palette = "YlOrRd") +
  scale_fill_got(option = "Lannister") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 12)) +
  labs(fill = "Liczba pozycji")
