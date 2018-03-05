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
  output$plot <- renderPlot({
    ggplot(data = specData()) +
      geom_bar(mapping = aes(x = reorder(player, truesalary), y = truesalary, fill = height.feet), stat = "identity") +
      labs(x = "Players",
           y = "Salary(Dollars)")+
      coord_flip()
  })
}

shinyServer(server)