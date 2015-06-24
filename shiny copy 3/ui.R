source("Attach.R")

fluidPage(
  # Application title
  titlePanel("Word Cloud"),

  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose Domain:",
                  choices = Domain),
      
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 3,  max = 50, value = 5),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 160,  value = 15)
    ),

    # Show Word Cloud
    mainPanel(
      column(6,
      h3("High Risk", align = "center"),
      plotOutput("plot")),
      
      column(6,
      h3("Low Risk", align="center"),
      plotOutput("plot1"))
    )
  )
)
