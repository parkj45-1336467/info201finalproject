library(dplyr)
library(ggplot2)
library(shiny)

nba <- read.csv("./data/nba_season_data.csv", stringsAsFactors=FALSE)
nba2004 <- nba %>% filter(year >= 2004 & truesalary != "" )
nba2004$height.feet <- nba2004$height * 0.0833333;

modifieddata<- nba %>% filter(height >0) %>% mutate(heightinft = height*0.083 )
heightsdata <- modifieddata %>% group_by(year) %>% summarize(mean = mean(heightinft))


server <- function(input, output){
  specData <- eventReactive(input$update,{
    teamData <- nba2004 %>% filter(tm == input$team & year == input$year)
    teamData$truesalary <- as.numeric(gsub("[\\$,]","",teamData$truesalary))
    teamData <- arrange(teamData, truesalary)
  }, ignoreNULL = FALSE)
  
  gradient <- c("turquoise2", "violet", "navy")
  
  output$plot <- renderPlot({
    validate(need(nrow(specData()) > 0,
                  "No data is availible for this query."))
    validate(need(input$year >= 2004 & input$year <= 2016,
                  "Please choose a year between 2004 and 2016!"))
    ggplot(data = specData()) +
      geom_bar(mapping = aes(x = reorder(player, height.feet),
                             y = truesalary / 1e+6,
                             fill = height.feet),
               stat = "identity") +
      labs(x = "Player Name",
           y = "Salary (Dollars)")+
      coord_flip() +
      scale_fill_gradientn(colors = gradient, 
                           guide = guide_colorbar(title = "Height (Feet)")) +
      scale_y_continuous(name = "Salary (US Dollars)",
                         labels = scales::dollar_format(
                           prefix = "$", suffix = " million",
                           largest_with_cents = 1e+5,
                           big.mark = "",
                           negative_parens = FALSE)) +
      theme(text = element_text(size=17))
  },
  height = 500, width = 800)
  
  output$heightvsyearPlot <- renderPlot({
    
    
    yearseq<- seq(input$Years[1], input$Years[2])
    x<- heightsdata[which(heightsdata$year %in% yearseq),]
    ggplot(data = x, aes(x = year, y = mean)) + 
      geom_point(shape = 22, color = "white", fill = "black", size = 4, stroke = 4) +
      xlab("Range of Years") + ylab("Range of Heights (in feet)") +
      geom_smooth(method = lm, se= TRUE, color = "red") + ylim(c(6.35,6.6)) +
      ggtitle("Change in Players' Height Over Time") + 
      theme(plot.title = element_text(size = "20", hjust = "0.5"), axis.text.x = element_text( size = "12"), axis.text.y = element_text( size = "12"),
            axis.title.x = element_text(size = "13", face= "bold"), axis.title.y = element_text(size = "13", face ="bold"),
            panel.background = element_rect(fill= "lightblue" , color = "blue"))
  
 }) }

shinyServer(server)