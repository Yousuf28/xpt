## library(shiny)
# library(DT)
# library(haven)

downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}
# Define UI for data upload app ----
ui <- shiny::fluidPage(
               shiny::titlePanel('xptView'),

               shiny::sidebarLayout(
                        shiny::sidebarPanel(width = 2,

                                 shiny::fileInput('file1', "Choose xpt file",
                                                  accept = '.xpt'),
                                 shiny::tags$hr(style = "border-top: 1px dashed black"),
                                 ## shiny::tags$br(),
                                 shiny::actionButton('add_row', label = "Add Row"),
                                 shiny::tags$hr(style = "border-top: 1px dashed black"),

                                 shiny::actionButton('delete_row', label = "Delete Row"),
                                 shiny::tags$hr(style = "border-top: 1px dashed black"),

                                 ## shiny::tags$br(),

                                 downloadButton('down_xpt', 'Download file as xpt'),
                                 shiny::tags$hr(style = "border-top: 1px dashed black"),
                                 downloadButton('down_csv', 'Download file as csv')

                               ),

               shiny::mainPanel(
                        DT::DTOutput('contents')
                      )))


# Define server logic to read selected file ----
server <- function(input, output) {
  v <- shiny::reactiveVal()
  shiny::observeEvent(input$file1,{
    shiny::req(input$file1)
    tab <- haven::read_xpt(input$file1$datapath)
    ## tab <- SASxport::read.xport(input$file1$datapath)
    v(tab)
  })
 # render tabel
  output$contents <- DT::renderDT({
    shiny::req(input$file1)
    DT::datatable(v(),
                  ## selection = 'single',
                  ## options = list(dom = 't'),
                  editable = T)
  })
# add row , by default add last row to the begining of table
  shiny::observeEvent(input$add_row,{
    df <- v()
    nr <- df[nrow(df), ]
    t <- rbind(nr,df)
    v(t)
  })
  ## selected row will be deleted. if no row seletecd, no row deleted
  shiny::observeEvent(input$delete_row, {
    t  <- v()
    if (!is.null(input$contents_rows_selected)) {
      t <- t[-as.numeric(input$contents_rows_selected),]
    }
    v(t)
  })
# update cell value
  shiny::observeEvent(input$contents_cell_edit, {
    info <- input$contents_cell_edit
    edit_row <-  info$row
    edit_col <-  info$col
    edit_value <-  info$value
    t  <-  v()
    t[edit_row,edit_col] <- edit_value
    v(t)
})
# download file as xpt
  output$down_xpt <- shiny::downloadHandler(
                              filename = function() {
                                file_name <- strsplit(input$file1$name, '.xpt')[[1]]
                                
                                 # print(file_name)
                                paste0(file_name, ".xpt")
                              

                              },
                              content = function(file) {
                                df <- v()
                                domain <- strsplit(input$file1$name, '.xpt')[[1]]
                               temp_dir <- tempdir()
                               print(temp_dir)
                               path <- fs::path(temp_dir, paste0(toupper(domain), '.xpt'))
                                ## SASxport::write.xport(df, file=file)
                                
                                haven::write_xpt(data = df, path=path, version = 5)
                                fs::file_copy(path, file)
                              }
                            )

 # download csv
  output$down_csv <- shiny::downloadHandler(
                              filename = function() {
                                file_name <- strsplit(input$file1$name, '.xpt')[[1]]
                                print(file_name)
                                paste0(file_name, ".csv")

                              },
                              content = function(file) {
                                df <- v()
                                write.csv(df, file=file, quote= FALSE, row.names=FALSE)
                              }
                            )

}
  # Create Shiny app ----
shiny::shinyApp(ui, server)
