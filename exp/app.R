##  This code is for creating shiny app that can read and write xpt file.
## xpt file also can be exported as xlsx (Excel) and csv file

## -----------------------------------------------------------------------------
##   Date                     Programmer
## ----------   --------------------------------------------------------------
##   Oct-05-2023    Md Yousuf Ali (md.ali@fda.hhs.gov)

# required R packages
# library(shiny)
# library(DT)
# library(haven)
# library(writexl)
# library(readxl)
# library(fs)

source('create_xlsx.R')
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

    # file upload
      shiny::fileInput('file1', 'Choose xpt/xlsx file',
        accept = c('.xpt', '.xlsx')),
      shiny::tags$hr(style = 'border-top: 1px dashed black'),
      # add row
      shiny::actionButton('add_row', label = 'Add Row'),
      shiny::tags$hr(style = 'border-top: 1px dashed black'),
      # delete row
      shiny::actionButton('delete_row', label = 'Delete Row'),
      shiny::tags$hr(style = 'border-top: 1px dashed black'),
      # download as xpt
      downloadButton('down_xpt', 'Download file as xpt'),
      shiny::tags$hr(style = 'border-top: 1px dashed black'),
      # download as xlsx
      downloadButton('down_xls', 'Download as excel file'),
      shiny::tags$hr(style = 'border-top: 1px dashed black'),
      # download as csv
      downloadButton('down_csv', 'Download file as csv')

    ),
# table
    shiny::mainPanel(
      DT::DTOutput('contents')
    )))


# Define server logic to read selected file ----
server <- function(input, output) {
  v <- shiny::reactiveVal()
  shiny::observeEvent(input$file1,{
    shiny::req(input$file1)
    ext <- basename(input$file1$datapath)
    if(grepl('xpt', ext, ignore.case = T)){
      tab <- haven::read_xpt(input$file1$datapath)
      v(tab)
    } else{
      tab <- get_xlsx(input$file1$datapath)
      v(tab)
    }
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
# get file name so it can be use in download section
  get_file_name <- shiny::eventReactive(input$file1, {

    ext <- basename(input$file1$datapath)
    if(grepl('xpt', ext, ignore.case= TRUE)) {
      file_name <- strsplit(input$file1$name, '.xpt')[[1]]
    } else {
      file_name <- strsplit(input$file1$name, '.xlsx')[[1]]
    }
    ## filename <- paste0(file_name, ".xpt")
    file_name


  })
  # download file as xpt
  output$down_xpt <- shiny::downloadHandler(
    filename = function() {
      file_name <- get_file_name()
      file_name <- paste0(file_name, '.xpt')
                              },
    content = function(file) {
      df <- v()
      domain <- get_file_name()
      temp_dir <- tempdir()
      path <- fs::path(temp_dir, paste0(domain, '.xpt'))
      haven::write_xpt(data = df, path=path, version = 5)
      fs::file_copy(path, file)
    }
  )

  # download csv
  output$down_csv <- shiny::downloadHandler(
    filename = function() {
      file_name <- get_file_name()
      file_name <- paste0(file_name, '.csv')
    },
    content = function(file) {
      df <- v()
      write.csv(df, file=file, quote= FALSE, row.names=FALSE)
    }
  )

  # download xlsx (xcel file)
  output$down_xls  <- shiny::downloadHandler(
    filename = function() {
      file_name <- get_file_name()
      paste0(file_name, '.xlsx')
    },
    content = function(file) {
      df <- v()
      var_meta <- create_xlsx(df)
      col <- dim(df)[2]
      var_n <- dim(df)[1]
      dataset_name <- toupper(get_file_name())
      df_meta <- data.frame(Dataset=c(dataset_name),
        Label=c(NA), Variable=c(col),
        Records=c(var_n))

     out <- list('Dataset Metadata'=df_meta, 'Variable Metadata'= var_meta)
      out[[dataset_name]] <- df
      print(names(out))
      writexl::write_xlsx(out, path = file)

    }
  )
}
# Create Shiny app ----
shiny::shinyApp(ui, server)
