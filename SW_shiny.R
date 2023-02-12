library(shiny)
library(dplyr)
library(tidyverse)
library(DT)

view(starwars)

#Przetworzenie danych
starwars_data = starwars %>%
  mutate(
    height = case_when(
      name == 'Finn' ~ as.integer(178),
      name == 'Rey' ~ as.integer(170),
      name == 'Poe Dameron' ~ as.integer(172),
      name == 'BB8' ~ as.integer(67),
      name == 'Captain Phasma' ~ as.integer(200),
      TRUE ~ height
    ),
    mass = case_when(
      name == 'Finn' ~ 73,
      name == 'Rey' ~ 54,
      name == 'Poe Dameron' ~ 80,
      name == 'BB8' ~ 18,
      name == 'Captain Phasma' ~ 76,
      TRUE ~ mass
    )
  ) 

starwars_data['hair_color'][is.na(starwars_data['hair_color'])] <- "not applicable" #zamiana na na not applicable
starwars_data <- starwars_data %>% separate(hair_color, c('hair_color1', 'hair_color2'), sep = ", ") #rozdzielnie koloru włosów na dwie osobne kolumny

hair_shiny <- sort(unique(c(starwars_data$hair_color1, starwars_data$hair_color2)))
species_shiny <- sort(unique(starwars_data$species))

#Shiny frontend
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput('hair', 'Kolor włosów', hair_shiny, multiple = TRUE),
      selectInput('species', 'Gatunek', species_shiny, multiple = TRUE)
    ),
    mainPanel(
      plotlyOutput('plot'),
      dataTableOutput('table')
    )
  )
)


#Shiny server
srv <- function(input, output){
  
  d = reactive({
    starwars_data %>% 
      filter(species == input$species)
  })
  
  output$plot <- renderPlotly({
  plot_ly(d() %>%
    add_markers(
      x = ~homeworld,
      y = ~height
    )
  )
  })
  
}

#uruchomienie aplikacji
shinyApp(ui, srv)

