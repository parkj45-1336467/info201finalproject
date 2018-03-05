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
                                   value = c(min, max))
                
                       
                     ),
                     mainPanel(
                       plotOutput("heightvsyearPlot")
                     )
                    )
                   ),
                       
    tabPanel("Height VS Salary",
    sidebarLayout(
      sidebarPanel(
        selectInput("team", "Select a team:", nba2004 %>%
                      distinct(tm, .keep_all = TRUE) %>%
                      rename(Team=tm) %>%
                      select(Team)),
        numericInput("year", "Enter a year", 2004),
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