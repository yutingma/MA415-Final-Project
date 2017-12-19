library(leaflet) 
library(shiny)
library(plyr)
library(rsconnect)

rsconnect::setAccountInfo(name='yutingma',
                          token='8C9C60B0CEA36DB3015E8C2D3C6FE59C',
                          secret='HXj2SqfjU30v814Rv6zp92jePn3n4/ZakgkMp7Wz')




s5 <- read.csv("totalsentiment.csv", row.names = 1)

s5 <- mutate(s5, popup_info=paste(sep = "<br/>", paste0("<b>", s5$screenName, "</b>"), paste0 ("retweet count: ", s5$retweetCount), paste0 ("sentiment score: ",s5$score)))

factorpal<- colorFactor(
  palette = "RdPu",
  domain = c(s5$sentiment),
  level = NULL,
  ordered= FALSE, 
  na.color = "#808080"
)



r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("The US Consumers' Perception towards Walt Disney Company"),
  leafletOutput("PopularityMap"),
  p()
)


server <- function(input, output, session) {
  
  
  output$PopularityMap <- renderLeaflet({
    leaflet(s5) %>%
      addTiles(
      ) %>%  
      addCircleMarkers(lng=~lon,
                       lat = ~lat, 
                       popup= ~popup_info,
                       radius = 4,
                       color = ~factorpal(s5$sentiment),
                       fillOpacity = 1) %>%
      addProviderTiles("Stamen.Watercolor") %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
    
  })
  
  
  
}


shinyApp(ui, server)
