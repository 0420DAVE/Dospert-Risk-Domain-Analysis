source("Attach.R")
library(markdown)
fluidPage(
  navbarPage("Word Cloud", 
  # multi-page user-interface that includes a navigation bar.
  tabPanel("Data Visualization",
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
  ),
  tabPanel("About",
           mainPanel(
             includeMarkdown("README.md")
           )
)
))
