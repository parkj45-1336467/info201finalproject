library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)

# Define UI for app that has multiple tabs

ui <- navbarPage(theme = shinytheme("cerulean"), "NBA Interactive Height Statistics",

    ## Tab 1: Height Over Time
    tabPanel("Height vs. Time",
             sidebarLayout(
               sidebarPanel(
                 tags$img(src = "nbalogo.jpg", height = 150),
                 sliderInput(
                   "p1.slider",
                   "Range of Years",
                   min = min(heightsdata$year),
                   max = max(heightsdata$year),
                   step = 1 ,
                   value = c(1978, 2016),
                   sep = ""),
                 helpText("Select a range of years to see statistics from within that time frame."),
                 tags$img(src = "shaq.jpg", height = 300)
               ),
               mainPanel(htmlOutput("header.t1"),
                         plotOutput("plot.t1"),
                         htmlOutput("text.t1"))
             )),
    
    ## Tab 2: Height vs. Game Stats
    tabPanel("Height vs. Game Stats",
             sidebarLayout(
               sidebarPanel(
                 hr(),
                 sliderInput(
                   "p2.slider",
                   "Years:",
                   min = 1978,
                   max = 2016,
                   step = 1,
                   value = c(1978, 2016),
                   sep = ""
                 ),
                 helpText("Select a range of years to see statistics from within that time frame."),
                 br(),
                 radioButtons("stats",
                   h4("Game Statistic"),
                   c(
                     "Efficiency",
                     "Defensive Rebound Percentage",
                     "Offensive Rebound Percentage",
                     "Rebound Percentage",
                     "Blocks",
                     "Steals",
                     "Turnovers"
                   )
                 ),
                 hr()
               ),
               mainPanel(htmlOutput("header.t2"),
                         plotOutput("plot.t2"),
                         htmlOutput("text.t2"))
             )),
    
    ## Tab 3: Height vs. Salary
    tabPanel("Height vs. Salary",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "team",
                   "Select a team:",
                   nba %>%
                     filter(year >= 2004 & truesalary != "") %>%
                     distinct(tm, .keep_all = TRUE) %>%
                     rename(Team = tm) %>%
                     select(Team),
                   selected = "GSW"
                 ),
                 numericInput("year", "Enter a year:", 2015),
                 helpText("Please type a year between 2004 and 2016"),
                 actionButton("update", "Update View")
               ),
               mainPanel(htmlOutput("header.t3"),
                         plotOutput("plot.t3"),
                         htmlOutput("text.t3"))
             ))
  )

shinyUI(ui)