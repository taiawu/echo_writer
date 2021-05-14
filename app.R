# this app is a minimal example of uploading a layout file using the layout uploads module
library(tidyverse)
library(dsfworld)
library(echowritr)
library(shiny)
library(shinyalert) # needed to upload layout files
source("R/upload_layout_module_2.R")

ui <- fluidPage(useShinyalert(), # required for uploads module
                sidebarLayout(
                  sidebarPanel(width = 4,
                               uploadLayoutUI("daughter", "Upload daughter layout")[[1]], # upload panel
                               uploadLayoutUI("mother", "Upload mother layout")[[1]]
                  ),
                  mainPanel(
                    p("Daughter standard") %>% strong(),
                    tableOutput("table_external_daughter"), # table of the layout, accessed outside the module
                    p("Mother standard") %>% strong(),
                    tableOutput("table_external_mother"), # table of the layout, accessed outside the module
                    p("Transfers)") %>% strong(),
                    tableOutput("transfer_table")
                  )
                )
)

server <- function(input, output, session) {
  #### upload layout ####
  daughter_raw <- uploadLayoutServer("daughter") # upload the data
  daughter <- reactive({
    daughter_raw() %>%
      standardize_layout("daughter",
      )
  })
  output$table_external_daughter <- renderTable(head(daughter()))
  
  ## mother
  mother_raw <- uploadLayoutServer("mother") # upload the data
  mother <- reactive({
    mother_raw() %>%
      standardize_layout("mother")
  })
  output$table_external_mother <- renderTable(head(mother()))
  
  layouts <- reactive({
    repair_layout(mother(),
                  daughter(),
                  if_missing = "drop",
                  if_varied = "keep_max",
                  if_impossible = "scale_down")})
  
  transfers <- reactive(calculate_transfers(layouts()$daughter, layouts()$mother, 2.5))
  output$transfer_table <- renderTable(transfers())
  # depletion <- monitor_source_depletion(transfers, 35)

}


shinyApp(ui = ui, server = server)
