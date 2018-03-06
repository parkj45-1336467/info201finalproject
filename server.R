library(dplyr)
library(ggplot2)
library(shiny)
library(sourcetools)

### Data Wrangling

# our initial dataset from the CSV file
nba <- read.csv("./data/nba_season_data.csv", stringsAsFactors=FALSE)

# The NBA reorganized its teams in 2004, so we need this dataset for
# functions that organize statistics by team
nba2004 <- nba %>% filter(year >= 2004 & truesalary != "" & height > 0) %>%
  mutate(height.feet = height * 0.0833333)

# For our functions that do not organize by team, we can include all the teams from
# 1978 - 2016 (the year range provided in the CSV file)
modifieddata <- nba %>% filter(height > 0) %>% mutate(heightinft = height * 0.083)
heightsdata <- modifieddata %>% group_by(year) %>% summarize(mean = mean(heightinft))

### Creating Pages 
server <- function(input, output){
  
  ### --------------- Tab 1: Height Over Time --------------- ###
  
  # Add a header
  output$header.t1 <- renderUI({HTML(
    "<h1>Trends in Average Height<h1/>")})
  
  # Add an interactive plot
  output$plot.t1 <- renderPlot({
    yearseq <- seq(input$p1.slider[1], input$p1.slider[2])
    x <- heightsdata[which(heightsdata$year %in% yearseq),]
    ggplot(data = x, aes(x = year, y = mean)) + 
      geom_point(shape = 22,
                 color = "black",
                 fill = "white",
                 size = 4,
                 stroke = 4) +
      xlab("Range of Years") +
      ylab("Range of Heights (in feet)") +
      geom_smooth(method = lm, se= TRUE, color = "darkred", linetype= "dotted") +
      ylim(c(6.45,6.6)) +
      ggtitle("Height vs. Year") + 
      theme(plot.title = element_text(size = "20", hjust = "0.5", face= "bold"),
            axis.text.x = element_text( size = "12"),
            axis.text.y = element_text( size = "12"),
            axis.title.x = element_text(size = "13"),
            axis.title.y = element_text(size = "13"),
            panel.background = element_rect(fill= "lightblue" , color = "blue"))
    
  })
  
  # Add a summary paragraph
  output$text.t1 <- renderUI({
    HTML("<h4>Summary<h4/>",
         "<p>", read("txt/tab1/p1.txt"),
         "<p>", read("txt/tab1/p2.txt"),
         strrep("<br/>", 2)
    )
  })
  
  ### --------------- Tab 2: Height Versus Game Stats --------------- ###
  
  # Add a header
  output$header.t2 <- renderUI({HTML(
    "<h1>Player Heights vs Game Statistics<h1/>")})
  
  # Add an interactive plot
  output$plot.t2 <- renderPlot({
    
    # Filter our data to get rid of zero values in the game stats we are looking at
    nba <- modifieddata %>% filter (year >= input$p2.slider[1] &
                                      year <= input$p2.slider[2] &
                                      per > 0 & orb > 0 & drb > 0 & trb > 0 &
                                      blk > 0 & stl >0 & tov > 0) %>%
      select(per, orb, drb, trb, blk, stl, tov, heightinft)
    
    # Draw our plot based on user input
    if (input$stats == "Efficiency") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(per))
      ggplot(data = stats, aes(x = heightinft, y = mean, group = 1)) +
        geom_line(col = "blue") +
        geom_point() +
        ggtitle("Player Efficiency Rate vs. Height")+
        labs(x = "Height in ft", y = "Average Player Efficiency Rate based on the Height")
      
    } else if (input$stats == "Rebound Percentage") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(trb))
      ggplot(data = stats, aes(x = heightinft, y = mean, group = 1)) +
        geom_line(col = "red") +
        geom_point() +
        ggtitle("Rebound Percentage vs. Height")+
        labs(x = "Height in ft", y = "Average Rebound Percentage based on the Height")
      
    } else if (input$stats == "Blocks") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(blk))
      ggplot(data = stats, aes(x = heightinft, y = mean, group = 1)) +
        geom_line(col = "purple") +
        geom_point() +
        ggtitle("Blocks vs. Height")+
        labs(x = "Height in ft", y = "Average Blocks based on the Height")
      
    } else if (input$stats == "Offensive Rebound Percentage") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(orb))
      ggplot(data = stats, aes(x = heightinft, y = mean , group = 1)) +
        geom_line(col = "green") +
        geom_point() +
        ggtitle("Offensive Rebound vs. Height")+
        labs(x = "Height in ft", y = "Average Offensive Rebound Percentage based on the Height")
      
    } else if (input$stats == "Defensive Rebound Percentage") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(drb))
      ggplot(data = stats, aes(x = heightinft, y = mean , group = 1)) +
        geom_line(col = "cyan") +
        geom_point() +
        ggtitle("Defensive Rebound vs. Height")+
        labs(x = "Height in ft", y = "Average Defensive Rebound Percentage based on the Height")
      
    } else if (input$stats == "Steals") {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(stl))
      ggplot(data = stats, aes(x = heightinft, y = mean , group = 1)) +
        geom_line() +
        geom_point() +
        ggtitle("Steal vs. Height")+
        labs(x = "Height in ft", y = "Average Steals based on the Height")
      
    } else {
      stats <- nba %>% arrange(heightinft) %>% group_by(heightinft) %>% summarise(mean = mean(tov))
      ggplot(data = stats, aes(x = heightinft, y = mean , group = 1)) +
        geom_line(col = "violet") +
        geom_point() +
        ggtitle("Trunover vs. Height")+
        labs(x = "Height in ft", y = "Average Turnovers based on the Height")
    }
  })
  
  # Add a summary paragraph
  output$text.t2 <- renderUI({
    HTML(strrep("<br/>", 2),
         "<h4>Summary<h4/>",
         "<p>", read("txt/tab2/p1.txt"),
         strrep("<br/>", 2)
    )
  })
  
  ### --------------- Tab 3: Height Versus Salary by Team --------------- ###
  
  # Add a header
  output$header.t3 <- renderUI({HTML(
    "<h1>Player Heights vs Salaries by Team and Season<h1/>")})
  
  # Add an interactive plot
  output$plot.t3 <- renderPlot({
    
    # Reactive function to read data from users
    specData <- eventReactive(input$update,{
      teamData <- nba2004 %>% filter(tm == input$team & year == input$year)
      teamData$truesalary <- as.numeric(gsub("[\\$,]","",teamData$truesalary))
      teamData <- arrange(teamData, truesalary)
    }, ignoreNULL = FALSE)
    
    # Handle exceptions with user input
    validate(need(nrow(specData()) > 0,
                  "No data is availible for this query."))
    validate(need(input$year >= 2004 & input$year <= 2016,
                  "Please choose a year between 2004 and 2016!"))
    
    # Create the plot
    ggplot(data = specData()) +
      geom_bar(mapping = aes(x = reorder(player, height.feet),
                             y = truesalary / 1e+6,
                             fill = height.feet),
               stat = "identity") +
      labs(x = "Player Name",
           y = "Salary (Dollars)")+
      coord_flip() +
      scale_fill_gradientn(colors = c("turquoise2", "violet", "navy"), 
                           guide = guide_colorbar(title = "Height (Feet)")) +
      scale_y_continuous(name = "Salary (US Dollars)",
                         labels = scales::dollar_format(
                           prefix = "$", suffix = " million",
                           largest_with_cents = 1e+5,
                           big.mark = "",
                           negative_parens = FALSE)) +
      theme(text = element_text(size=17)) +
      ggtitle(paste0(input$team, ", ", input$year, " Season"))
  },
  height = 500, width = 800)
  
  # Add a summary paragraph
  output$text.t3 <- renderUI({
    HTML(strrep("<br/>", 5),
         "<h4>Summary<h4/>",
         "<p>", read("txt/tab3/p1.txt"),
         "<p>", read("txt/tab3/p2.txt"),
         strrep("<br/>", 2)
    )
  })
  
  }

shinyServer(server)