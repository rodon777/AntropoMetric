library(shiny)
source('function.R')

shinyServer(function(input,output, session){
  
  ### ALL DATA ###
  
  # Return the requested data
  datasetInput <- reactive({
    
    if (is.null(input$file1))
      return(NULL)
    
    inFile <- input$file1
    
    read.csv(inFile$datapath)
    
  })
  
  output$RawData <- renderTable(
  {
      
      datasetInput()
      
    },
    filter = "top",
    selection = "multiple",
    style = "bootstrap",
    )
  
  ### OPERACION ###
  
  # Return the requested individuos
  datasetIndividuo <- reactive({
    
    if (is.null(datasetInput()))
      return(NULL)
    else
      estatura(datasetInput(),method = input$met, lateral = input$lat)
    
  })
  
  # Generate a tabla of the dataset
  output$tabla <- renderTable(
    {
      
      datasetIndividuo()
      
    },
    filter = "top",
    selection = "multiple",
    style = "bootstrap",
    )
  
  observeEvent(datasetInput(), {
    updateSelectInput(session, "ind", choices = datasetInput()[,1])
  })
  
  datasetSummary <- reactive({
   dat <- setNames(data.frame(t(datasetIndividuo())[-1,]), datasetIndividuo()[,1]) 
   subset(dat, select=input$ind)
  })
  
  datasetPlot <- reactive({
   dat <- setNames(data.frame(t(datasetIndividuo())[-1,]), datasetIndividuo()[,1])
   subset(dat, select=input$ind)
  })
  
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    
    if (is.null(input$ind))
      return("seleccione los individuos")
    
    summary(datasetSummary())
  })
  
  #################################################
  ### BOX PLOT ####################################
  #################################################
  
  output$plot <- renderPlot({
    
    ## var <- t(datasetInput())
    
    if (is.null(input$ind))
      return(NULL)
    
    boxplot(datasetPlot(), frame=FALSE, col="cornflowerblue", 
            ylab = "Estatura (cm)", xlab = "Individuos")
    
  })
  
  
})