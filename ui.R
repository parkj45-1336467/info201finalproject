library(dplyr)
library(ggplot2)
library(shiny)

nba2004 <- nba %>% filter(year >= 2004 & truesalary != "")
# Define UI for app that draws a histogram ----
  ui <- navbarPage("NBA VS Height",
                   tabPanel("Home",
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput("Years",
                                   "Range of Years",
                                   min = min(heightsdata$year),
                                   max = max(heightsdata$year),
                                   step = 1 ,
                                   value = c(1978, 2016),
                                   sep = "")
                
                       
                     ),
                     mainPanel(
                       plotOutput("heightvsyearPlot")
                     )
                    )
                   ),
                   
    tabPanel("Stats vs. Height",
      sidebarLayout(
      sidebarPanel(
        hr(),
        sliderInput("years", "Years:",
                    min = 2004, max = 2016,
                    value = c(2004,2016)),
        br(),
        radioButtons("stats", h4("Stats"),
                      c("Efficiency", "Defensive Rebound", "Offensive Rebound", "Rebound", "Block", "Steal", "Turnover")),
        hr()
      ),
      mainPanel(
        plotOutput("statsGraph")
        )
      )
    ),               
                                      
    tabPanel("Height VS Salary",
    sidebarLayout(
      sidebarPanel(
        selectInput("team", "Select a team:", nba2004 %>%
                      distinct(tm, .keep_all = TRUE) %>%
                      rename(Team=tm) %>%
                      select(Team),
                    selected = "GSW"),
        numericInput("year", "Enter a year:", 2015),
        helpText("Please type a year between 2004 and 2016"),
        actionButton("update", "Update View")
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
)
shinyUI(ui)