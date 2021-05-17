library(tidyverse)
library(dsfworld)
library(echowritr)
library(glue)

library(stringdist)
library(glue) # required for custom names within the selectors'
library(shinyWidgets) # contains pickerInput
library(shinyalert) # needed to upload layout files
library(shiny)

library(reactlog) # make the reactlog, to better understand the app!
# at the start of each session:
reactlog::reactlog_enable()
options(shiny.reactlog=TRUE)
# run the app
# command-fn-F3

source("picker_input_for_columns_module.R")
source("upload_layout_module_2_previous.R")
source("static_text.R")
source("utils.R") #

# implement modularized app
ui <- 
  fluidPage(useShinyalert(),
            tags$head(
              tags$style(HTML(
                "label { font-size:100%; font-family:Helvetica Neue; margin-bottom: 
    10px; }"
              ))
            ),
            titlePanel("Write instructions for Echo transfers of (almost!) anything"),
            
            #tags$style(".well {background-color:white; border-color:white}"),
            sidebarLayout(
              sidebarPanel(
p("Step 1. Upload layouts for the mother and daughter plates.") %>% strong(),
                uploadLayoutUI("daughter", 
                               "Upload daughter layout")[[1]], # upload panel
                pickColUI("daughter_cols")[[1]], # the actual pickerInput
                pickColUI("daughter_cols")[[2]], # should be the same
                verbatimTextOutput("selection_external_daughter"),
                
                uploadLayoutUI("mother", 
                               "Upload mother layout")[[1]],
                pickColUI("mother_cols")[[1]], #[[1]],  # the actual pickerInput
                pickColUI("mother_cols")[[2]], # should be the same
                #tags$hr(style="border-color: #d9d9d9;"),
                numericInput(
                  "echo_vol", 
                  "Step 2. Set the droplet volume (nL) for the Echo instrument.",
                  value = 25, 
                  min = 0, 
                  max = 100),
                pickerInput(
                  "repair_methods",
                  label = "Step 3.  Resolve any issues in the requested transfers", # user can change the label
                  choices = repair_list$choices, # displays list NAMES
                  selected =  repair_list$selected, 
                  multiple = TRUE,
                  inline = FALSE, # put label in-line with drop-downs
                  options = 
                    pickerOptions( # good place for ... in later version
                      "max-options-group" = 1,
                      title = "Choose methods to repair"
                    )),
                #tags$hr(style="border-color: #d9d9d9;"),
textInput("save_name", "Step 4. Enter names for the downloaded plots and files.", value = Sys.Date()),
textAreaInput("notes", "", rows = 2, value = "Add any notes here"),
                actionButton("go", "Calculate Echo transfers", class = "btn-block")
              ),
              mainPanel(
                # Output: Tabset w/ instructions, plots, and user instructions ----
                tabsetPanel(type = "tabs",
                            tabPanel("Echo instructions", 
                                     p("tables will go here"),
                                     p("Transfers") %>% strong(),
                                     verbatimTextOutput("transfer_print"),
                                     tableOutput("transfer_table")),
                            tabPanel("Plots", p("plots will go here")),
                            tabPanel("How to use this app", p("instructions will go here"))
                )
              )
            )
  )


server <- 
  function(input, # read-only from UI
           output, # write-only to UI
           session # unique space for each instance
  ) {
    #__________________upload raw layouts and map column names_____________________#
    #_____daughter layout______
    daughter_raw <- # upload 
      uploadLayoutServer("daughter") 
    
    picker_list_daughter <- 
      pickColServer(
        "daughter_cols", 
        data = daughter_raw, 
        groups = reactive(groups_daughter_text), # see static_text.R
        guess_selections = reactive(guess_daughter_cols_text), # see static_text.R
        .multiple = TRUE,
        .inline = FALSE,
        drop_options = "disp", 
        picker_title = "this doesn't show up", 
        picker_label = "Which variables contain the needed information?"#"Daughter"
        )
    
    daughter_cols <- 
      reactive(
        picker_list_daughter() %>%
          sapply( . , function(x) 
          {str_split(x, pattern = "<>")[[1]][2]}))
    
    daughter <- 
      reactive(
        daughter_raw() %>%
          standardize_layout( . ,
                              "daughter",
                              .well_col = daughter_cols()[[1]],
                              .compound_col = daughter_cols()[[2]],
                              .concentration_col = daughter_cols()[[3]],
                              .volume_col = daughter_cols()[[4]]))
    
    output$table_external_daughter <- 
      renderTable(head(daughter()))
    
    output$selection_external_daughter <- 
      renderPrint(picker_list_daughter())
    
    #mother layout______
    mother_raw <- 
      uploadLayoutServer("mother") # upload

    picker_list_mother <- 
      pickColServer(
        "mother_cols", 
        data = mother_raw, 
        groups = reactive(groups_mother_text), # see static_text.R
        guess_selections = reactive(guess_mother_cols_text), # see static_text.R
        .multiple = TRUE,
        .inline = FALSE,
        drop_options = "disp", 
        picker_title = "this doesn't show up", 
        picker_label = "Which variables contain the needed information?" #"",#"Mother"

        )
    
    mother_cols <- 
      reactive(
        picker_list_mother() %>%
          sapply( . , function(x) 
          {str_split(x, pattern = "<>")[[1]][2]}))
    
    
    mother <- 
      reactive(
        mother_raw() %>%
          standardize_layout( . ,
                              "mother",
                              .well_col = mother_cols()[[1]],
                              .compound_col = mother_cols()[[2]],
                              .concentration_col = mother_cols()[[3]]))
    
    output$selection_external_mother <- 
      renderPrint(picker_list_mother())
    
    output$table_external_mother <- 
      renderTable({head(mother())})
    
    ###### pop up the error messages automatically, and on their own...
    ####### how to repair issues
    layouts <- reactive({
      # req(mother()) # cancel until mother exists
      # req(daughter()) # cancel until daughter exists
      #     #input$accept_and_run # depend on user saying "accept these fixes and run with it"
      
      try_catch_popup(
        execute_this = rlang::abort(all(daughter()$compound %in% mother()$compound)),
        error_title = "Daughter plate has compounds not present in the mother plate",
        error_subtitle = 
          glue::glue("Edit your layout, or see the 'Repair methods' 
                 drop-down menu for ways to proceed anyway. 
                 \nMissing compounds: {
                 glue_collapse(unique(
                 daughter()$compound[!daughter()$compound %in% mother()$compound]), 
                 sep = ', ')}"))
      
      try_catch_popup(
        execute_this = rlang::abort(get_varied_errors(mother())$outcome),
        error_title = "Single compound present in mother at multiple concentrations",
        error_subtitle = 
          glue::glue("This isn't supported at this time. 
                 Edit your mother layout, or see the 'Repair methods' 
                 drop-down menu for ways to proceed anyway. 
                \nThis issue was found in compounds:
                {glue_collapse(get_varied_errors(mother())$compounds, sep = ', ')}"))
      
      try_catch_popup(
        execute_this = rlang::abort(get_conc_errors(daughter(), mother())$outcome),
        error_title = "Daughter plate has un-achievably high concentrations",
        error_subtitle = 
          glue::glue("Edit your layout, or see the 'Repair methods' 
                        drop-down menu for ways to proceed anyway. 
                      \nThis issue was found in wells: 
                      {glue_collapse(get_conc_errors(daughter(), 
                      mother())$wells, sep = ', ')},
                      \nCorresponding to compounds:
                      {glue_collapse(get_conc_errors(daughter(), 
                       mother())$compounds, sep = ', ')}"))
      
      # try_catch_popup( execute_this = repair_layout(mother(),
      #                                               daughter(),
      #                                               if_missing = "drop", # input$repair_choices$something something -- need f() for pickerInput to these values conversion
      #                                               if_varied = "keep_max", # input$repair_choices$something something -- need f() for pickerInput to these values conversion
      #                                               if_impossible = "scale_down"), # input$repair_choices$something something -- need f() for pickerInput to these values conversion
      #                  error_title = "Couldn't clean and repair the layouts",
      #                  error_subtitle = glue::glue("Please check your layouts and try again")
      # )
      
    })
    
    observeEvent(input$go, {
      repairs <- 
        reactive(
          get_repairs(
            input$repair_methods, #repair_list$selected, # comes from UI
            translate_repairs) 
        )
      
      try_catch_popup(
        execute_this = rlang::abort(all(daughter()$compound %in% mother()$compound)),
        error_title = "Daughter plate has compounds not present in the mother plate",
        error_subtitle = 
          glue::glue("Edit your layout, or see the 'Repair methods' 
                 drop-down menu for ways to proceed anyway. 
                 \nMissing compounds: {
                 glue_collapse(unique(
                 daughter()$compound[!daughter()$compound %in% mother()$compound]), 
                 sep = ', ')}"))
      
      try_catch_popup(
        execute_this = rlang::abort(get_varied_errors(mother())$outcome),
        error_title = "Single compound present in mother at multiple concentrations",
        error_subtitle = 
          glue::glue("This isn't supported at this time. 
                 Edit your mother layout, or see the 'Repair methods' 
                 drop-down menu for ways to proceed anyway. 
                \nThis issue was found in compounds:
                {glue_collapse(get_varied_errors(mother())$compounds, sep = ', ')}"))
      
      try_catch_popup(
        execute_this = rlang::abort(get_conc_errors(daughter(), mother())$outcome),
        error_title = "Daughter plate has un-achievably high concentrations",
        error_subtitle = 
          glue::glue("Edit your layout, or see the 'Repair methods' 
                        drop-down menu for ways to proceed anyway. 
                      \nThis issue was found in wells: 
                      {glue_collapse(get_conc_errors(daughter(), 
                      mother())$wells, sep = ', ')},
                      \nCorresponding to compounds:
                      {glue_collapse(get_conc_errors(daughter(), 
                       mother())$compounds, sep = ', ')}"))
      
      layouts <- 
        reactive(
        repair_layout(
          mother(), 
          daughter(),
          if_missing = repairs()[["repair_missing"]], 
          if_varied = repairs()[["repair_varied"]], 
          if_impossible = repairs()[["repair_conc"]]))
      
      transfers <- 
        reactive(
          calculate_transfers(
            layouts()$daughter, 
            layouts()$mother, 
            input$echo_vol) 
        )
        
      depletion <-
        reactive(
          monitor_source_depletion(
            transfers(), 
            35) ### THIS SHOULD BE USER-SET!! Or at least, have the option to be
          )
      
      output$transfer_table <-
        renderTable({
          transfers()
        })
        
    })
    
    
    # ##### what's going on here? 
    # # how do you wait to execute until a reactive expression exits?
    # transfers <- reactive({
    #   # req( layouts() ) # cancel if layout() doesn't exist yet
    #   calculate_transfers(layouts()$daughter, layouts()$mother, input$echo_vol)
    # })
    # 
    # depletion <- reactive({
    #   # req( is.reactive(transfers() )) # what is transfers fails--should we do a if(success) or return NULL for transfers calc?
    #   monitor_source_depletion(transfers(), 35)
    # })
    
    # output$transfer_print <-
    #   renderPrint({
    #     req(is.reactive(transfers()))
    #     
    #     print(transfers())
    #     transfers()
    #   })
  }

shinyApp(ui = ui, server = server)

