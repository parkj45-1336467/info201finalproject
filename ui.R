library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("NBA Data!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "slider",
                  label = "Year playing",
                  min = min(nba$year),
                  max = max(nba$year),
                  step = 1,
                  value = c(2000, 2016),
                  sep = "")
    ),
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
)