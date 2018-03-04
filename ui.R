library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("NBA Data!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year",
                  label = "Year playing",
                  min = min(nba$year),
                  max = max(nba$year),
                  step = 1,
                  value = c(2000, 2016),
                  sep = ""),
      radioButtons(
        inputId="stat",
        label="Stat",
        choices=c(
          "ORB"="orb",
          "DRB"="drb",
          "TRB"="trb"
        )
      )
    ),
    mainPanel(
      plotOutput(outputId = "heightPlot")
    )
  )
)