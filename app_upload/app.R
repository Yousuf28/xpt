webr::install("DT")
webr::install("haven")
library(shiny)
library(DT)
library(haven)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Uploading Files"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

shiny::fileInput('file1',label = 'Choose xpt file',
                 multiple = FALSE,
                 accept = ".xpt")


    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      DT::DTOutput('contents')
      # tableOutput("contents")

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  output$contents <- DT::renderDT({
  
    req(input$file1)

   
        df <- haven::read_xpt(input$file1$datapath)
    
        df

 })


}

# Create Shiny app ----
shinyApp(ui, server)
