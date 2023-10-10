library(shiny)
# library(DT)
# library(haven)

# Define UI for data upload app ----
ui <- fluidPage(
      
      shiny::fileInput('file1', "Choose xpt file",
                       accept = '.xpt'),

      tags$hr(),
      downloadButton('down_xpt', 'Download file'),

      DT::DTOutput('contents')

    # )

  
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  v <- shiny::reactiveValues()
  observeEvent(input$file1,{
   req(input$file1)
    # print(input$file1$name)
  tab <- haven::read_xpt(input$file1$datapath)
  v$tab <- tab  
  
  })
  
# df <-  shiny::eventReactive(input$file1, {
#    req(input$file1)
#   tab <- haven::read_xpt(input$file1$datapath)
#   tab
#  })
  output$contents <- DT::renderDT({
    req(input$file1)
        df <- DT::datatable(v$tab,selection = 'none', editable = TRUE)
        df

  })
  # proxy = DT::dataTableProxy('contents')
  # 
  # observeEvent(input$contents_cell_edit, {
  #   info = input$contents_cell_edit
  #   str(info)
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   df[i, j] <<- DT::coerceValue(v, df[i, j])
  #   replaceData(proxy, df, resetPaging = FALSE)  # important
  # })
  # 
  
  # proxy5 = dataTableProxy('contents')
  observeEvent(input$contents_cell_edit, {
    # info = input$contents_cell_edit
    v$tab <<- DT::editData(v$tab,input[['contents_cell_edit']], 'contents')
    # str(info)  # check what info looks like (a data frame of 3 columns)
    # print(info)
    # t <- df()
    # print(head(t))
    # df <<- editData(t, info)
    # replaceData(proxy5, df, resetPaging = FALSE)  # important
    # the above steps can be merged into a single editData() call; see examples below
  })
  
  output$down_xpt <- shiny::downloadHandler(
    filename = function() {
      # Sys.sleep(2)
      # file_name <- basename(input$file1$datapath)
     file_name <- strsplit(input$file1$name, '.xpt')[[1]]

      paste0(file_name,"_edited", ".xpt")

    },
    content = function(file) {
      df <- v$tab
      Sys.sleep(2)
      haven::write_xpt(df, file)
    }
  )
  

}

# Create Shiny app ----
shinyApp(ui, server)
