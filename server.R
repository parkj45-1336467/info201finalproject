library(dplyr)
library(ggplot2)
library(shiny)

nba <- read.csv("data/nba_season_data.csv", stringsAsFactors=FALSE)
nba2004 <- nba %>% filter(year >= 2004 & truesalary != "")
nba2004$height.feet <- nba2004$height * 0.0833333;

server <- function(input, output){
  specData <- eventReactive(input$update,{
    teamData <- nba2004 %>% filter(tm == input$team & year == input$year)
    teamData$truesalary <- as.numeric(gsub("[\\$,]","",teamData$truesalary))
    teamData <- arrange(teamData, truesalary)
  }, ignoreNULL = FALSE)
  
  gradient <- c("turquoise2", "violet", "navy")
  
  output$plot <- renderPlot({
    ggplot(data = specData()) +
      geom_bar(mapping = aes(x = reorder(player, height.feet),
                             y = truesalary,
                             fill = height.feet),
               stat = "identity") +
      labs(x = "Player Name",
           y = "Salary (Dollars)")+
      coord_flip() +
      scale_fill_gradientn(colors = gradient, 
                           guide = guide_colorbar(title = "Height (Feet)")) +
      scale_y_continuous(name = "Salary (US Dollars)", labels =
                           scales::dollar_format(
                             prefix = "$", suffix = "",
                             largest_with_cents = 1e+05,
                             big.mark = ",",
                             negative_parens = FALSE)) +
      theme(text = element_text(size=17))
    },
  height = 500, width = 800)
}

shinyServer(server)