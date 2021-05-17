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
# reactlog::reactlog_enable()
# options(shiny.reactlog=TRUE) 
# run the app
# command-fn-F3

source("picker_input_for_columns_module.R")
source("upload_layout_module_2.R")
source("static_text.R")

get_conc_errors <- #____(helper) trigger informative alert: conc error----#
  function(daughter, mother){
    cols <- 
      c("Destination Well", 
        "compound", 
        "mother_conc",
        "daughter_conc", 
        "daughter_final_vol")
    
    to_repair <- 
      left_join(daughter, mother, by = "compound") %>%
      select(all_of(cols)) %>%
      distinct() %>%
      group_by(.data$compound) %>%
      mutate(repair = 
               if_else(.data$daughter_conc > .data$mother_conc, 
                       true = "repair", false = "ok")) %>%
      select(.data$`Destination Well`, .data$compound, .data$repair)
    
    
    list("outcome" = !"repair" %in% to_repair$repair, # to trigger abort
         "wells" = to_repair %>% # print problem wells in pop-up
           filter(repair == "repair") %>% 
           pull(.data$`Destination Well`) %>% 
           unique() ,
         "compounds" = to_repair %>% # print problem compounds in pop-up
           filter(repair == "repair") %>% 
           pull(.data$compound) %>% 
           unique())
  }

get_varied_errors <- #____(helper) trigger informative alert: mother error----#
  function(mother) {
    tallied_by_cmpd <- 
      mother %>%
      group_by(.data$compound) %>%
      mutate(n_conc = n_distinct(.data$mother_conc), # how many conc. in mother
             max_conc = max(.data$mother_conc),
             min_conc = min(.data$mother_conc)) %>%
      group_by(.data$compound, .data$mother_conc) %>% # needed if keep_most selected
      mutate(well_per_conc = n_distinct(.data$`Source Well`)) %>%
      group_by(.data$compound) %>%
      mutate(most_wells = max(.data$well_per_conc))
    
    list("outcome" = !(max(tallied_by_cmpd$n_conc) > 1),
         "compounds" = tallied_by_cmpd %>% 
           filter(n_conc > 1) %>% 
           pull(compound) %>% 
           unique()
    )
  }

try_catch_popup <- #___(util) ahinyalter pop-ups with custom messages
  function(execute_this, 
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

repair_list <-    
  make_picker_lists(
    groups = repair_options$groups,
    options = repair_options$options,
    guess_selections = repair_options)

# implement modularized app
ui <- 
  fluidPage(useShinyalert(),
            titlePanel("Picker Input module -- example"),
            sidebarLayout(
              sidebarPanel(
                uploadLayoutUI("daughter", 
                               "Upload daughter layout")[[1]], # upload panel
                pickColUI("daughter_cols")[[1]], # the actual pickerInput
                pickColUI("daughter_cols")[[2]], # should be the same
                hr(),
                
                uploadLayoutUI("mother", 
                               "Upload mother layout")[[1]],
                pickColUI("mother_cols")[[1]], #[[1]],  # the actual pickerInput
                pickColUI("mother_cols")[[2]], # should be the same
                hr(),
                
                numericInput(
                  "echo_vol", 
                  "Echo droplet volume (nL)", 
                  value = 25, 
                  min = 0, 
                  max = 100),
                
                pickerInput(
                  "repair_methods",
                  label = "Repair methods", # user can change the label
                  choices = repair_list$choices, # displays list NAMES
                  selected =  repair_list$selected, 
                  multiple = TRUE,
                  inline = TRUE, # put label in-line with drop-downs
                  options = 
                    pickerOptions( # good place for ... in later version
                      "max-options-group" = 1,
                      title = "Choose methods to repair"
                    ))
              ),
              mainPanel(
                # p("Daughter standard") %>% strong(),
                # tableOutput("table_external_daughter"), # table of the layout, accessed outside the module
                # p("Mother standard") %>% strong(),
                # tableOutput("table_external_mother")#, # table of the layout, accessed outside the module
                p("Transfers") %>% strong(),
                verbatimTextOutput("transfer_print"),
                tableOutput("transfer_table")
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
        daughter_raw, 
        reactive(make_picker_lists(groups_daughter_text)), # see static_text.R
        reactive(guess_daughter_cols_text), # see static_text.R
        drop_options = "disp", 
        picker_title = "this doesn't show up", 
        picker_label = "Daughter")
    
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
        mother_raw, 
        reactive(make_picker_lists(groups_mother_text)), # see static_text.R
        reactive(guess_mother_cols_text), # see static_text.R
        drop_options = "disp", 
        picker_title = "this doesn't show up", 
        picker_label = "Mother")
    
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
    
    
    ##### what's going on here? 
    # how do you wait to execute until a reactive expression exits?
    transfers <- reactive({
      #req( is.reactive(layouts() ) )# cancel if layout() doesn't exist yet
      calculate_transfers(layouts()$daughter, layouts()$mother, input$echo_vol)
    })
    
    depletion <- reactive({
      # req( is.reactive(transfers() )) # what is transfers fails--should we do a if(success) or return NULL for transfers calc?
      monitor_source_depletion(transfers(), 35)
    })
    
    output$transfer_print <-
      renderPrint({
        req(is.reactive(transfers()))
        
        print(transfers())
        transfers()
      })
  }

shinyApp(ui = ui, server = server)

