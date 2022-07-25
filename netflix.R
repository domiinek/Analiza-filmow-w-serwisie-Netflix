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

#długość tytułu - liczba słów
netflix <- netflix %>%
  mutate(number_words_title = str_count(Title, " ") 
                            + str_count(Title, "-") 
                            + 1) %>%
  mutate(number_char_title = nchar(Title))

#liczba słów w tytule
words_labels <- netflix %>%
  group_by(number_words_title) %>%
  summarise(the_label = n())

# wykres - liczba słów w tytule
ggplot(words_labels, aes(x = number_words_title, y = the_label)) +
  geom_col(aes(fill = number_words_title)) +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Liczba słów w tytule",
       y = "Liczba pozycji") +
  scale_fill_got(option = "Lannister") +
  scale_x_discrete(limits = factor(1:20)) +
  geom_text(aes(label = the_label), vjust = -0.25)
  
#liczba słów w tytule
char_labels <- netflix %>%
  group_by(number_char_title) %>%
  summarise(the_label = n())

# wykres - liczba znaków w tytule
ggplot(netflix, aes(x = number_char_title)) +
  geom_bar(fill = "darkred")+
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Liczba znaków w tytule",
       y = "Liczba pozycji") +
  scale_fill_got(option = "Lannister") +
  scale_x_continuous(breaks = seq(0,110,10)) +
  scale_y_continuous(breaks = seq(0,900,100))

# zoom na większe wartości
ggplot(netflix, aes(x = number_char_title)) +
  geom_bar(fill = "darkred")+
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Liczba znaków w tytule",
       y = "Liczba pozycji") +
  scale_x_continuous(limits = seq(50,110,5)) +
  scale_y_continuous(breaks = seq(0,50,2)) +
  xlim(50,110)

# ile pozycji dla danych gatunków
netflix <- netflix %>%
  mutate(vector_genre = strsplit(Genre, split = ", ", fixed = TRUE))

netflix_genre <- data.frame() 

for (i in 1:nrow(netflix)) {
  for(j in 1:length(netflix$vector_genre[i][[1]])) {
    netflix_genre <- netflix_genre %>%
      rbind(netflix$vector_genre[i][[1]][j])
  }
}

#ile pozycji danych gatunków
genres <- netflix_genre %>%
  na.omit() %>%
  setNames("genre") %>%
  group_by(genre) %>%
  summarise(nr = n()) %>%
  filter(nr > 1200)

#wykres - najpopularniejsze gatunki
ggplot(genres, aes(x = reorder(genre, desc(nr)), y = nr)) +
  geom_col(aes(fill = reorder(genre, desc(nr)))) +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Gatunek",
       y = "Liczba pozycji") +
  scale_fill_got_d(option = "Lannister") +
  geom_text(aes(label = nr), vjust = -0.25)

#podział daty premiery na dzień tygodnia, miesiąc i rok
netflix <- netflix %>%
  mutate(netflix_day = substr(`Netflix.Release.Date`, 1, 2),
         netflix_month = substr(`Netflix.Release.Date`, 4, 5),
         netflix_year = substr(`Netflix.Release.Date`, 7, 10))

#dni tygodnia
netflix_dates_days <- netflix %>%
  filter(! (netflix_year == "2015" & netflix_month == "04")) %>%
  filter(! (netflix_year == "2021" & netflix_month == "03")) %>%
  group_by(netflix_day) %>%
  summarise(nr = n())

# wykres ile w danym dniu tygodnia
ggplot(netflix_dates_days, aes(netflix_day, nr, fill = netflix_day)) +
  geom_col() +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Dzień miesiąca",
       y = "Liczba pozycji") +
  scale_fill_got_d(option = "Lannister") +
  geom_text(aes(label = nr), vjust = -0.25, size = 3)

#miesiace
netflix_dates_months <- netflix %>%
  filter(netflix_year != "2015" & netflix_year != "2021") %>%
  group_by(netflix_month) %>%
  summarise(nr = n())

# wykres ile w danym miesiącu
ggplot(netflix_dates_months, aes(netflix_month, nr, fill = netflix_month)) +
  geom_col() +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Miesiąc",
       y = "Liczba pozycji") +
  scale_fill_got_d(option = "Lannister") +
  geom_text(aes(label = nr), vjust = -0.25)

#lata
netflix_dates_years <- netflix %>%
  group_by(netflix_year) %>%
  summarise(nr = n())

# wykres ile w danym miesiącu
ggplot(netflix_dates_years, aes(netflix_year, nr, fill = netflix_year)) +
  geom_col() +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Rok",
       y = "Liczba pozycji") +
  scale_fill_got_d(option = "Lannister") +
  geom_text(aes(label = nr), vjust = -0.25)

# ile seriali a ile filmów dodali
series_film <- netflix %>%
  group_by(`Series.or.Movie`) %>%
  summarise(nr = n())

# wykres seriale/filmy
ggplot(series_film, aes(`Series.or.Movie`, nr, fill = `Series.or.Movie`)) +
  geom_col() +
  coord_flip() +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none",
        axis.text.y = element_text(size = 12)) +
  labs(x = "Serial/Film",
       y = "Liczba pozycji") +
  scale_fill_got_d(option = "Lannister") +
  geom_text(aes(label = nr), color = "white", size = 7, hjust = 1)

#podział na czas trwania
how_long <- netflix %>%
  group_by(Runtime) %>%
  summarise(nr = n()) %>%
  filter(Runtime != "")

# wykres długość trwania
ggplot(how_long, aes(Runtime, nr, fill = Runtime)) +
  geom_col() +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none",
        axis.text = element_text(size = 11)) +
  labs(x = "Czas trwania",
       y = "Liczba pozycji") +
  geom_text(aes(label = nr), vjust = -0.25) +
  scale_x_discrete(limits = c("< 30 minutes", "30-60 mins", "1-2 hour", "> 2 hrs")) +
  scale_fill_got_d(option = "Lannister")

# ocena IMDb
ggplot(netflix, aes(`IMDb.Score`)) +
  geom_boxplot(fill = "darkorange", color = "gray26") +  
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none",
        axis.text = element_text(size = 11),
        axis.text.y = element_blank()) +
  labs(x = "Ocena IMDb", y = "")

# liczba głosów
ggplot(netflix, aes(`IMDb.Votes`)) +
  geom_histogram(fill = "darkred") +  
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Liczba głosów na IMDb", y = "Liczba pozycji")

ggplot(netflix, aes(`IMDb.Votes`)) +
  geom_freqpoly(color = "darkred") +  
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Liczba głosów na IMDb", y = "Liczba pozycji")

#ile filmów ile nagród otrzymały
awards_received <- netflix %>%
  group_by(`Awards.Received`) %>%
  summarise(nr = n()) %>%
  filter(!is.na(`Awards.Received`))

#liczba otrzymanych nagród
ggplot(awards_received, aes(`Awards.Received`, nr)) +
  geom_line(color = "darkred", size = 1) +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Liczba otrzymanych nagród", y = "Liczba pozycji")

#ile nominacji dostały
awards_nominated <- netflix %>%
  group_by(`Awards.Nominated.For`) %>%
  summarise(nr = n()) %>%
  filter(!is.na(`Awards.Nominated.For`))

#wykres - liczba otrzymanych nominacji
ggplot(awards_nominated, aes(`Awards.Nominated.For`, nr)) +
  geom_line(color = "darkred", size = 1) +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  labs(x = "Liczba otrzymanych nominacji", y = "Liczba pozycji")

# ocena od liczby głosów
ggplot(netflix, aes(`IMDb.Votes`, `IMDb.Score`)) +
  geom_point(alpha = 0.2, color = "darkred") +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  scale_x_continuous(limits = c(1, 5000)) +
  labs(x = "Liczba oddanych na IMDb",
       y = "Ocena na IMDb")

# ocena od liczby głosów
ggplot(netflix, aes(`IMDb.Score`, `Awards.Nominated.For`)) +
  geom_point(alpha = 0.2, color = "darkred") +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  labs(x = "Ocena na IMDb",
       y = "Liczba otrzymanych nominacji")

ggplot(netflix, aes(`Awards.Nominated.For`, `Awards.Received`)) +
  geom_point(alpha = 0.2, color = "darkred") +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Liczba otrzymanych nagród",
       y = "Liczba otrzymanych nominacji")

netflix %>%
  filter( !is.na(`IMDb.Score`)) %>%
  group_by(netflix_year) %>%
  summarise(mean_votes = mean(`IMDb.Score`)) %>%
  ggplot(aes(netflix_year, mean_votes, fill = netflix_year)) +
  geom_col() +  
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none",
        axis.text = element_text(size = 11)) +
  labs(x = "Rok",
       y = "Średnia ocena według IMDb") +
  geom_text(aes(label = round(mean_votes, 2)), vjust = -0.25) +
  scale_fill_got_d(option = "Lannister") +
  ylim(0,7.5)

netflix %>%
  filter( !is.na(`IMDb.Score`)) %>%
  group_by(netflix_year, `Series.or.Movie`) %>%
  summarise(mean_votes = mean(`IMDb.Score`)) %>%
  ggplot(aes(netflix_year, mean_votes, fill = `Series.or.Movie`)) +
  geom_col(position = "dodge") +  
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        axis.text = element_text(size = 11),
        legend.text = element_text(color = "black")) +
  labs(x = "Rok",
       y = "Średnia ocena według IMDb",
       fill = "") +
  geom_text(aes(label = round(mean_votes, 2)), 
            position = position_dodge(width = .9), vjust = -0.25) +
  scale_fill_got_d(option = "Lannister") +
  ylim(0,7.5)

#seriale per year
netflix %>%
  filter(`Series.or.Movie` == "Series") %>%
  group_by(netflix_year) %>%
  summarise(nr = n()) %>%
  mutate(nr = round(nr/sum(nr)*100, 1)) %>%
  filter(!is.na(netflix_year)) %>%
  ggplot(aes(x = "", y = nr, fill = netflix_year)) +
  geom_bar(stat="identity", width=1, color="white") +
  theme_solarized() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(color = "black", size = 20)) +
  scale_fill_got_d(option = "Lannister") +
  labs(fill = "",
       title = "Seriale") +
  geom_text(aes(label = paste(nr, "%")),
            position = position_stack(vjust = 0.5),
            color = "white") +
  coord_polar(theta = "y")

#filmy per year  
netflix %>%
  filter(`Series.or.Movie` == "Movie") %>%
  group_by(netflix_year) %>%
  summarise(nr = n()) %>%
  mutate(nr = round(nr/sum(nr)*100, 1)) %>%
  filter(!is.na(netflix_year)) %>%
  ggplot(aes(x = "", y = nr, fill = netflix_year)) +
  geom_bar(stat="identity", width=1, color="white") +
  theme_solarized() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(color = "black", size = 20)) +
  scale_fill_got_d(option = "Lannister") +
  labs(fill = "",
       title = "Filmy") +
  geom_text(aes(label = paste(nr, "%")),
            position = position_stack(vjust = 0.5),
            color = "white") +
  coord_polar(theta = "y")

# ocena od otrzymanych nagród
ggplot(netflix, aes(`IMDb.Score`, `Awards.Received`)) +
  geom_point(alpha = 0.2, color = "darkred") +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  labs(x = "Ocena na IMDb",
       y = "Liczba otrzymanych nagród")

ggplot(netflix, aes(`Awards.Nominated.For`, `Awards.Received`)) +
  geom_point(alpha = 0.2, color = "darkred") +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.position = "none") +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = "Liczba otrzymanych nagród",
       y = "Liczba otrzymanych nominacji")

netflix <- netflix %>%
  mutate(netflix_date = paste(as.character(netflix_year), "-", 
                              as.character(netflix_month), "-01"),
         netflix_date = gsub(" ", "", netflix_date),
         netflix_date = as.Date(netflix_date))

awards_nominated_month <- netflix %>%
  filter(!is.na(`Awards.Nominated.For`), netflix_date != "2015-04-01") %>%
  group_by(netflix_date) %>%
  summarise(awards_nominated = sum(`Awards.Nominated.For`))

awards_month <- netflix %>%
  filter(!is.na(`Awards.Received`), netflix_date != "2015-04-01") %>%
  group_by(netflix_date) %>%
  summarise(awards_received = sum(`Awards.Received`)) %>%
  left_join(awards_nominated_month, by = "netflix_date")

colors_awards <- c("received" = "darkorange", "nominated" = "darkred")

ggplot(awards_month) +
  geom_line(aes(netflix_date, awards_nominated, color = "nominated",), size = 1) +
  geom_line(aes(netflix_date, awards_received, color = "received"), size = 1) +
  theme_solarized() +
  theme(axis.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        legend.background = element_blank()) +
  scale_color_manual(values = colors_awards, "labels" = c("Otrzymane", "Nominacje")) +
  labs(x = "Data premiery na Netflixie",
       y = "Liczba nagród", 
       color = "") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "6 months")
  
#ocena na imdb a nagrody
geom_point(netflix, aes(`Awards.Received`, ))
