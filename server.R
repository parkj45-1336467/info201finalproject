library(dplyr)
library(ggplot2)
library(shiny)

nba <- read.csv("./data/nba_season_data.csv", stringsAsFactors=FALSE)
nba2004 <- nba %>% filter(year >= 2004 & truesalary != "" & height > 0) %>%
  mutate(height.feet = height * 0.0833333)
modifieddata <- nba %>% filter(height > 0) %>% mutate(heightinft = height*0.083 )
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
  
  output$statsGraph <- renderPlot({
    nba <- modifieddata %>% filter (year >= input$years[1] & year <= input$years[2] & per > 0 & orb > 0 & drb > 0 & trb > 0 &
                             blk > 0 & stl >0 & tov > 0) %>% select(per, orb, drb, trb, blk, stl, tov, heightinft)
    
    if (input$stats == "Efficiency") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(per))
      ggplot(data = stats, aes(x = heightinft, y = mean, group = 1)) +
        geom_line(col = "blue") +
        geom_point() +
        ggtitle("Player Efficiency Rate vs. Height")+
        labs(x = "Height in ft", y = "Average Player Efficiency Rate based on the Height")
    } else if (input$stats == "Rebound") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(trb))
      ggplot(data = stats, aes(x = heightinft, y = mean, group = 1)) +
        geom_line(col = "red") +
        geom_point() +
        ggtitle("Rebound vs. Height")+
        labs(x = "Height in ft", y = "Average Rebound based on the Height")
    } else if (input$stats == "Block") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(blk))
      ggplot(data = stats, aes(x = heightinft, y = mean, group = 1)) +
        geom_line(col = "purple") +
        geom_point() +
        ggtitle("Block vs. Height")+
        labs(x = "Height in ft", y = "Average Block based on the Height")
    } else if (input$stats == "Offensive Rebound") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(orb))
      ggplot(data = stats, aes(x = heightinft, y = mean , group = 1)) +
        geom_line(col = "green") +
        geom_point() +
        ggtitle("Offensive Rebound vs. Height")+
        labs(x = "Height in ft", y = "Average Offensive Rebound based on the Height")
    } else if (input$stats == "Defensive Rebound") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(drb))
      ggplot(data = stats, aes(x = heightinft, y = mean , group = 1)) +
        geom_line(col = "cyan") +
        geom_point() +
        ggtitle("Defensive Rebound vs. Height")+
        labs(x = "Height in ft", y = "Average Defensive Rebound based on the Height")
    } else if (input$stats == "Steal") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(stl))
      ggplot(data = stats, aes(x = heightinft, y = mean , group = 1)) +
        geom_line() +
        geom_point() +
        ggtitle("Steal vs. Height")+
        labs(x = "Height in ft", y = "Average Steal based on the Height")
    } else {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(tov))
      ggplot(data = stats, aes(x = heightinft, y = mean , group = 1)) +
        geom_line(col = "violet") +
        geom_point() +
        ggtitle("Trunover vs. Height")+
        labs(x = "Height in ft", y = "Average Turnover based on the Height")
    }
  })
  
  output$heightvsyearPlot <- renderPlot({
    yearseq <- seq(input$Years[1], input$Years[2])
    x <- heightsdata[which(heightsdata$year %in% yearseq),]
    ggplot(data = x, aes(x = year, y = mean)) + 
      geom_point(shape = 22,
                 color = "white",
                 fill = "black",
                 size = 4,
                 stroke = 4) +
      xlab("Range of Years") +
      ylab("Range of Heights (in feet)") +
      geom_smooth(method = lm, se= TRUE, color = "red") +
      ylim(c(6.35,6.6)) +
      ggtitle("Change in Players' Height Over Time") + 
      theme(plot.title = element_text(size = "20", hjust = "0.5"),
            axis.text.x = element_text( size = "12"),
            axis.text.y = element_text( size = "12"),
            axis.title.x = element_text(size = "13", face= "bold"),
            axis.title.y = element_text(size = "13", face ="bold"),
            panel.background = element_rect(fill= "lightblue" , color = "blue"))
  
 }) }

shinyServer(server)