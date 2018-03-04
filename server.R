library(dplyr)

nba <- read.csv("data/nba_season_data.csv", stringsAsFactors=FALSE)
nba <- nba %>% filter(height > 0)

server <- function(input, output) {
  reactive(
    nba %>% filter(year == input$year[1])
  )
  
  output$heightPlot <- renderPlot({
     plot(nba$height, nba[, input$stat])
  })
   
   # scatter.x <- nba %>% select(height)
   # scatter.y <- reactive({
   #   nba %>% select(input$stat)
   # })
   # 
   # output$plot1 <- renderPlot({
   #   ggplot(nba) +
   #     geom_point(mapping = aes(x = Scatter.x, y = Scatter.y()), color = "red", size = 4) +
   #     labs(title = paste("Height vs", input$stat)
   #     )
   # })
}

shinyServer(server)