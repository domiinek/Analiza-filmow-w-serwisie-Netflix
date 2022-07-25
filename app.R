library(shiny)
library(ggplot2)
library(lubridate)
library(dplyr)
library(treemapify)
library(viridis)
library(wesanderson)
library(maps)
library(paletteer)
library(gameofthrones)
library(ggthemes)


netflix <- read.csv('netflix.csv', sep = ';')

#podział daty premiery na dzień tygodnia, miesiąc i rok
netflix <- netflix %>%
  mutate(netflix_day = substr(`Netflix.Release.Date`, 1, 2),
         netflix_month = substr(`Netflix.Release.Date`, 4, 5),
         netflix_year = substr(`Netflix.Release.Date`, 7, 10)) %>%
  mutate(netflix_date = paste(as.character(netflix_year), "-", 
                              as.character(netflix_month), "-",
                              as.character(netflix_day)),
         netflix_date = gsub(" ", "", netflix_date),
         netflix_date = as.Date(netflix_date))


ui <- fluidPage(

    titlePanel(" "),

    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dates",
                        "Wybierz zakres dat",
                        start = "2015-04-14",
                        end = "2021-03-04",
                        min = "2015-04-14",
                        max = "2021-03-04",
                        format = "dd/mm/yyyy", 
                        separator = " - ")
        ),

        mainPanel(
           plotOutput("netflixPlot")
        )
    )
)

server <- function(input, output) {

    output$netflixPlot <- renderPlot({
      
      netflix %>%
        filter(netflix_date >= input$dates[1] & netflix_date <= input$dates[2]) %>%
        group_by(netflix_date) %>%
        summarise(nr = n()) %>%
        ggplot(aes(netflix_date, nr)) +
        geom_line(color = "darkred", size = 1) +
        theme_solarized() +
        theme(axis.title = element_text(color = "black", size = 16),
              legend.text = element_text(color = "black"),
              legend.title = element_text(color = "black")) +
        labs(x = "Data premiery na Netflixie",
             y = "Liczba dodanych filmów", 
             color = "") +
        scale_x_date(date_labels = "%d/%m/%y")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
