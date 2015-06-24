# Load data processing file
source("Attach.R")


function(input, output, session) {
  # Prepare dataset 
  dataH<-reactive({
    getTextH(collapse,input$selection)
  })
  
  dataL<-reactive({
    getTextL(collapse,input$selection)
  })
  
  
  # Plot data
  output$plot <-renderPlot(
    plot(dataH(), input$freq, input$max)
    ) 
  output$plot1 <-renderPlot(
    plot(dataL(), input$freq, input$max)
  ) 
  
  
}