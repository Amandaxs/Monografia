library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(DT)
library(writexl)

data(iris)
data(mtcars)
#### Ler o banco inicial
dadosOK <- read.csv("dadosOK.csv")
#### Ler o modelo
mod <- readRDS("modeloLogistico.rds")  


########################## UI ##############################
############################################################
# UI for app
ui<-(pageWithSidebar(
  # title
  headerPanel("Select Options"),
  
  #input
  sidebarPanel
  (
    # Input: Select a file ----
    
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep", "Separator",
                 choices = c(Semicolon = ";",
                             Comma = ",",
                             Tab = "\t"),
                 selected = ","),
    # Horizontal line ----
    tags$hr(),
    selectInput("dataset","Data:",
                choices =list(iris = "iris", mtcars = "mtcars", dadosOK = "dadosOK",
                              uploaded_file = "inFile"), selected="dadosOK")
    #actionButton("do", "Processar resultados")
  ),
  
  # output
  mainPanel(
    tabsetPanel(
      tabPanel("Graficos",
        #h3(textOutput("caption")),
        DT::dataTableOutput("tab"),
        downloadButton('downloadData', 'Download data')
      ),
      tabPanel("teste",
        plotOutput("histo")
      )
    )
  )
))

######################## Server  ##############################
###############################################################



server <- function(input, output, session) {
############Avaliar se esse observe é nescessário


############## Lendo o arquivo de dados
upload_data<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep)
})

###############  Atualizando o meu arquivo
observeEvent(input$file1,{
  inFile<<-upload_data()
})
###########
get_data<-reactive({
  
  if(!exists(input$dataset)) return() # if no upload
  
  check<-function(x){is.null(x) || x==""}
  if(check(input$dataset)) return()
  
  return(get(input$dataset))
  
})
################# utilizando o meu modelo para fazer predição

ajustamod <- reactive({
  d<- predict(mod, newdata=subset (get_data(), select = -c(y)), type="response")
  return(as.data.frame(d))
  
})

output$tab <- DT::renderDataTable({
  head(ajustamod())
})
    

output$histo <- renderPlot({
  hist(ajustamod()$d)
})

########### disponibilizando tabela para doload
output$downloadData <- downloadHandler(
  filename = function() { 
    paste("dataset-", Sys.Date(), ".xlsx")
},
content = function(file) {
    write_xlsx(ajustamod(), file)
})

}
shinyApp(ui, server)