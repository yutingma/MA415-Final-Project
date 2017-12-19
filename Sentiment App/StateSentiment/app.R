library(ggmap) 
library(shiny)
library(plyr)
library(rsconnect)


s5 <- read.csv("totalsentiment.csv", row.names = 1)

s5 <- mutate(s5, popup_info=paste(sep = "<br/>", paste0("<b>", s5$screenName, "</b>"), paste0 ("retweet count: ", s5$retweetCount), paste0 ("sentiment score: ",s5$score)))

inputlist <- c("AL", "AK", "AZ", "AR", 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 
               'MO', 'MT', 'NE', "NV", 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR','PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI',
               'WY')


ui <- fluidPage(
  titlePanel("The US Consumers' Perception towards Walt Disney Company"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "select",
                  label = "Select State",
                  choices = inputlist)),
    mainPanel(plotOutput("plotiris"))
  ))

Map <- get_googlemap(as.character(input$select), zoom = 6, maptype = "roadmap", crop = FALSE)



server <- function(input, output) {
   
   output$plotiris <- renderPlot({
     ggmap(Map) +
       geom_point(aes(x=s5$lon, y=s5$lat), col=ifelse(((total$sentiment>=0)),"brown1", "blue"), data=total, alpha=0.4, size= s5$sentiment)
   })
}


shinyApp(ui = ui, server = server)

