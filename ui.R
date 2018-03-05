library(dplyr)
library(ggplot2)
library(shiny)

nba2004 <- nba %>% filter(year >= 2004 & truesalary != "")
# Define UI for app that draws a histogram ----
  ui <- fluidPage(
    titlePanel("Salary & Height"),
    sidebarLayout(
      sidebarPanel(
        selectInput("team", "pick your team:", nba2004 %>% distinct(tm, .keep_all = TRUE) %>% select(tm)),
        numericInput("year", "type a year", 2004),
        helpText("Please type a year between 2004 - 2016"),
        actionButton("update", "Update View")
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
shinyUI(ui)