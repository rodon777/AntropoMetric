library(shiny)
source('function.R')

shinyUI(fluidPage(
  
  
  tabsetPanel(tabPanel("All Data",
                       titlePanel("Tabla Antropométrica para la Estimación de Estatura (mm)"),
                       fileInput('file1', 'Choose CSV file', accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                       
                       fluidRow(column(tableOutput("RawData"),width = 12))
  ),
  
  tabPanel("Operación",
           sidebarPanel(
             selectInput("met", "Método:", 
                         choices = c("TrotterGleser", "Pearson", "Telkka", "Mendoza"), multiple = F),
             
             selectInput("lat", "Lateral:",
                         choices = c(Izquierda = "I", Derecha = "D")),
             
             helpText("Nota: Seleccione el método que desea emplear",
                      " y seleccione el lado que desea evaluar. ",
                      "Observe los resultados."),
             
             h3("Observaciones"),
             selectInput("ind", "Individuos:", multiple = TRUE, selected = TRUE, choices = c()),
             
             submitButton("Refrescar")
           ),
           
           # Show a summary of the dataset and an HTML table with the requested
           # number of observations. Note the use of the h4 function to provide
           # an additional header above each output section.
           mainPanel(
             h3("Tabla (cm)"),
             tabPanel("",
                      fluidRow(column(tableOutput("tabla"),width = 12))
             ),
             h3("Sumario"),
             fluidRow(column(12,
                             verbatimTextOutput("summary")))
             
           )
  ),
  tabPanel("Gráficos",
           mainPanel(
             h3("Box Plot"),
             plotOutput("plot")
           )
           
  )
  
  
  )
  
))