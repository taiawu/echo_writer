# library(tidyverse) # needed for all data handling
# library(shinyalert) # needed for uploading

# genreal function to attempt actions
try_catch_popup <- function( execute_this,
                             error_title = "Error!",
                             error_subtitle = "Try, try again",
                             warning_title = "Warning!",
                             warning_subtitle = "You've been warned") {
  tryCatch( {
    execute_this
  }, warning = function(w) {
    shinyalert(warning_title, warning_subtitle)
  },  error = function(e) {
    shinyalert(error_title, error_subtitle)
  })
}


##### ---- working layout reading module
uploadLayoutUI <- function(id, .placeholder = "Upload layout file") {
  tagList(
    fileInput( NS(id, "file"),
               label = "",
               placeholder = .placeholder,
               multiple = FALSE,
               accept = c(".csv", ".tsv", ".xls", ".xlsx")),
    tableOutput(NS(id,"layout_table"))
  )
}




uploadLayoutServer <- function(id) {
  moduleServer(id, function(input, output, session) { # data path comes from "input"

    layout_raw <- reactive({
      req(input$file)
      
      ## should add an extension validate here
      # e.g. validate(need(ext == "csv", "Please upload a csv file"))

      try_catch_popup(execute_this = dsfworld::read_plate_layout(input$file$datapath) ,
                      error_title = glue::glue("Can't read {id} layout"),
                      error_subtitle = glue::glue("Please check {id} layout format."))

    })

    output$layout_table <- renderTable({ layout_raw()  })
    reactive({layout_raw()}) # return this as the value for uploadLayoutServer
  })

}

