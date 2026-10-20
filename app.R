
library(gapminder)
library(ggplot2)
library(dplyr)
library(shiny)

ui <- fluidPage(
  
  titlePanel("Gapminder"),
  
  #pannello laterale che conterrà tutti i controlli interattivi
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", # input slider per scegliere l'anno
                  "Year",
                  min = 1952,
                  max = 2007,
                  step = 5,
                  value = 1952),
      selectInput("continent", # input per la scelta del continente
                  "Continent",
                  choices = c("Africa", "Asia", "Americas", "Europe", "Oceania", "All"),
                  selected = "All"),
      #selezione variabile asse X
      radioButtons("xaxis", "X",
                   choices = c("Aspettativa di vita" = "lifeExp",
                               "Popolazione" = "pop",
                               "GDP procapite" = "gdpPercap"),
                   selected = "gdpPercap"),
      #selezione variabile asse Y
      radioButtons("yaxis", "Y",
                   choices = c("Life expectancy" = "lifeExp", 
                               "Population" = "pop", 
                               "GDP per capita" = "gdpPercap"), 
                   selected = "lifeExp")
    ),
    
    #pannello principale dove viene mostrato il grafico
    mainPanel(
      plotOutput("scatterPlot")
    ) 
    
  ) 
)

server = function(input, output){
  output$scatterPlot = renderPlot({
    #filtraggio dinamico del dataset rieseguito a ogni modifica dell'input
    if(input$continent == "All"){
      dataset = filter(gapminder, year == input$year)
    }else{
      dataset = filter(gapminder, year == input$year, continent == input$continent)
    }
    
    #disegnamo lo scatterplot
    ggplot(dataset, aes_string(input$xaxis,input$yaxis, size = "pop", color = "country")) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_color_manual(values = country_colors) +
      #scale_color_viridis_d(option = "plasma") +
      scale_size(range = c(2,20)) +
      scale_x_log10() +
      scale_y_log10() +
      theme_classic()
  })
}

#avvio app
shinyApp(ui = ui, server = server)