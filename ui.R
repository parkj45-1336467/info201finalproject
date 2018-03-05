library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)

nba2004 <- nba %>% filter(year >= 2004 & truesalary != "")

# Define UI for app that has multiple tabs

ui <- navbarPage( theme = shinytheme("cerulean"), "NBA Interactive Height Statistics",

    ## Page 1: Height Over Time
    tabPanel("Height Over Time",
             sidebarLayout(
               sidebarPanel(
                 tags$img(src = "nbalogo.jpg", height = 150),
                 sliderInput(
                   "Years",
                   "Range of Years",
                   min = min(heightsdata$year),
                   max = max(heightsdata$year),
                   step = 1 ,
                   value = c(1978, 2016),
                   sep = ""),
                 tags$img(src = "shaq.jpg", height = 300)
               ),
               mainPanel(plotOutput("plot.p1"))
             )),
    
    ## Page 2: Height vs. Game Stats
    tabPanel("Height vs. Game Stats",
             sidebarLayout(
               sidebarPanel(
                 hr(),
                 sliderInput(
                   "years",
                   "Years:",
                   min = 2004,
                   max = 2016,
                   value = c(2004, 2016)
                 ),
                 br(),
                 radioButtons("stats",
                   h4("Game Statistic"),
                   c(
                     "Efficiency",
                     "Defensive Rebound %",
                     "Offensive Rebound %",
                     "Rebounds",
                     "Blocks",
                     "Steals",
                     "Turnovers"
                   )
                 ),
                 hr()
               ),
               mainPanel(plotOutput("plot.p2"))
             )),
    
    ## Page 3: Height vs. Salary
    tabPanel("Height vs. Salary",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "team",
                   "Select a team:",
                   nba2004 %>%
                     distinct(tm, .keep_all = TRUE) %>%
                     rename(Team = tm) %>%
                     select(Team),
                   selected = "GSW"
                 ),
                 numericInput("year", "Enter a year:", 2015),
                 helpText("Please type a year between 2004 and 2016"),
                 actionButton("update", "Update View")
               ),
               mainPanel(plotOutput("plot.p3"))
             ))
  )

shinyUI(ui)