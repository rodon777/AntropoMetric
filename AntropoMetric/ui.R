library(shiny)
source('function.R')

shinyUI(fluidPage(
  
  
  tabsetPanel(tabPanel("Datos",
                       titlePanel("Tabla Antropométrica para la Estimación de Estatura (mm)"),
                       fileInput('file1', 'Importar archivo CSV', accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                       
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
           
  ),
  tabPanel("Ayuda",
           mainPanel(
             sidebarPanel(width = 12,h3("Instrucciones de uso"),
                  helpText("Esta aplicación necesita de un archivo csv separado por comas codificado en formato UTF-8.",
                      "La tabla de datos ha de constar con 11 columnas enumeradas en el siguiente orden:"),
                 h5( helpText("IND : numero, sigla o ID del individuo"),
                  helpText("LMH : Longitud Máxima de Húmero"),
                  helpText("LMC : Longitud Máxima de Cúbito"),
                  helpText("LFC : Longitud Fisiológica de Cúbito"),
                  helpText("LMR : Longitud Máxima de Radio"),
                  helpText("LFR : Longitud Fisilógica de Radio"),
                  helpText("LMF : Longitud Máxima de Fémur"),
                  helpText("LFF : Longitud Fisiológica de Fémur"),
                  helpText("LT  : Longitud de Tibia"),
                  helpText("LMP : Longitud Máxima de Peroné"),
                  helpText("Lat : Lateralidad de la unidad")),
                  helpText("Mendoça, M.C. (2000) Estimation of Height from the Length of Long Bones in a Portuguese Adult Population. AJPA 112: 39-48")
           ))
           
  )
  
  
  )
  
))
