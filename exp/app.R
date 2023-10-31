## library(shiny)
## library(SASxport)
# library(DT)
# library(haven)

downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}
# Define UI for data upload app ----
ui <- shiny::fluidPage(
      
      shiny::fileInput('file1', "Choose xpt file",
                       accept = '.xpt'),

      shiny::tags$hr(),
      downloadButton('down_xpt', 'Download file'),
      shiny::tags$hr(),
      shiny::tags$hr(),
      DT::DTOutput('contents')

    # )

  
)

# Define server logic to read selected file ----
server <- function(input, output) {
  v <- shiny::reactiveValues()
  shiny::observeEvent(input$file1,{
   shiny::req(input$file1)
    # print(input$file1$name)
  ## tab <- haven::read_xpt(input$file1$datapath)
  ## tab <- SASxport::read.xport(input$file1$datapath)
  tab <- Hmisc::sasxport.get(input$file1$datapath, lowernames = F)
  v$tab <- tab  
  
  })
  
# df <-  shiny::eventReactive(input$file1, {
#    req(input$file1)
#   tab <- haven::read_xpt(input$file1$datapath)
#   tab
#  })
  output$contents <- DT::renderDT({
    shiny::req(input$file1)
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
  shiny::observeEvent(input$contents_cell_edit, {
    # info = input$contents_cell_edit
    v$tab <<- DT::editData(v$tab,input[['contents_cell_edit']], 'contents')
    print(names(v$tab))
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

      paste0(file_name, ".xpt")
      ## paste0('bw.xpt')

    },
    content = function(file) {
      df <- v$tab
      ## print(df)
      ## print(names(df))
      ## Sys.sleep(2)
     ## write.csv(df, file, row.names = FALSE)
     ## SASxport::write.xport(df, file=file)
    ## print(file)
## kk <- sub(tools::file_path_sans_ext(basename(file)), 'hello',file)
      temp_dir <- tempdir()
      print(temp_dir)
      path <- fs::path(temp_dir, 'dm.xpt')

      ## fs::file_move(file, path)

      ## fs::file_copy(file, kk)

print(path)
     xportr::xportr_write(df, path = path )
      ## haven::write_xpt(df, file)
      fs::file_copy(path, file)
    }
  )
  

}

# Create Shiny app ----
shiny::shinyApp(ui, server)
