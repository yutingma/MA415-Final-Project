library(ggmap)
library(dplyr)
library(shiny)
library(rsconnect)
require(datasets.load)
library(scales)

rsconnect::setAccountInfo(name='yutingma',
                          token='8C9C60B0CEA36DB3015E8C2D3C6FE59C',
                          secret='HXj2SqfjU30v814Rv6zp92jePn3n4/ZakgkMp7Wz')

s5 <- read.csv("totalsentiment.csv", row.names = 1)

s5 <- mutate(s5, popup_info=paste(sep = "<br/>", paste0("<b>", s5$screenName, "</b>"), paste0 ("retweet count: ", s5$retweetCount), paste0 ("sentiment score: ",s5$score)))

inputlist <- state.name


ui <- fluidPage(
  titlePanel("The US Consumers' Perception towards Walt Disney Company by State"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "select",
                  label = "Select State",
                  choices = inputlist)),
    mainPanel(plotOutput("plotiris"))
  ))


scaled_sentiment <- rescale(s5$sentiment, to = c(0,1))

server <- function(input, output) {
   
   output$plotiris <- renderPlot({
     Map <- get_googlemap(as.character(input$select), zoom = 7, maptype = "roadmap", crop = TRUE)
     ggmap(Map) +
       geom_point(aes(x=s5$lon, y=s5$lat,colour = scaled_sentiment), data=s5, alpha=0.4) + 
       scale_colour_gradient(low = "Grey", high = "Red")
   })
}


shinyApp(ui = ui, server = server)

