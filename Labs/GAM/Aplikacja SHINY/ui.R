library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  
  sidebarLayout(
    sidebarPanel(width=3,
                        radioButtons("model", label = h3("Wybierz model:"),
                              choices = list("Regresja liniowa" = 1, "LOESS" = 2,
                                             "Splajny" = 3, "Regresja kaw. wielomianowa"= 4,
                                             "Naturalne splajny kubiczne"=5,
                                             "Splajny wygładzone"=6),selected = 1),
                 sliderInput("wezly", label = h3("Liczba węzłów"),
                             min = 1, max = 6, value = 3),
                 sliderInput("stopien", label = h3("Stopien wielomianu/splajnu"),
                             min = 0, max = 20, value = 3),
                 sliderInput("span", label = h3("Span LOESS"),
                             min = 0.1, max = 3, value = 0.3,step = 0.01),
                 sliderInput("df", label = h3("Efektywne stopnie swobody"),
                             min = 1.5, max = 30, value = 5,step = 0.1)),
    mainPanel(tabsetPanel(
      tabPanel("Modele", plotOutput("plocik")),
      tabPanel("Bazy", plotOutput("bazy")))
  ))
  
  
))