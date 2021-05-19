library(tidyverse)
library(echowritr)
library(DT)
library(stringdist)
library(glue) # required for custom names within the selectors'
library(shinyWidgets) # contains pickerInput
library(shinyalert) # needed to upload layout files
library(shiny)

source("picker_input_for_columns_module.R")
source("upload_layout_module_2_previous.R")
source("static_text.R")
source("utils.R") 
source("plateview_plots.R")
#source("echowritr_package_functions.R") # ha! not sure how to deal with internal packages so just... plopped them all in tthis file.  # update---it didn't work. gotta be more careful or something..

# implement modularized app
ui <- 
  fluidPage(useShinyalert(),
            tags$head(
              tags$style(HTML(
                "label { font-size:100%; font-family:Helvetica Neue; margin-bottom: 
    10px; }"
              ))
            ),
            titlePanel("Write instructions for Echo transfers"),
 
            sidebarLayout(
              sidebarPanel(
p("Step 1. Upload layouts for the mother and daughter plates.") %>% strong(),
                uploadLayoutUI("daughter", 
                               "Upload daughter layout")[[1]], # upload panel
                pickColUI("daughter_cols")[[1]], # the actual pickerInput
                
                uploadLayoutUI("mother", 
                               "Upload mother layout")[[1]],
                pickColUI("mother_cols")[[1]], #[[1]],  # the actual pickerInput
               
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
 
textInput("save_name", "Step 4. Enter names for the downloaded plots and files.", value = make_exp_header()),
               
numericInput(
  "max_vol",
  "Notify if a single mother well is depleted by more than uL to draw from a single well.",
  value = 35,
  min = 0,
  max = 100),

actionButton("go", "Calculate Echo transfers", class = "btn-block"),
br(),
uiOutput("download"),
br(),
verbatimTextOutput("save_text")

              ),
              mainPanel(
                # Output: Tabset w/ instructions, plots, and user instructions ----
                tabsetPanel(type = "tabs",
                            tabPanel("Echo instructions", 
                                     dataTableOutput("instructions"), style = "overflow-x: scroll;overflow-y: scroll;"),
                            tabPanel("Calculated transfers", 
                                     dataTableOutput("transfer_table"), style = "overflow-x: scroll;overflow-y: scroll;"),
                            tabPanel("Plots", 
                                    
                                            wellPanel(
                                                      p("Plot is ready when this grey box appears white. You may have to scroll down in this window to see your results."),
                                                      plotOutput("all_plots",
                                                                 height = "5000px"
                                                      ),
                                                      style = "overflow-y:scroll; max-height: 600px")
  
                                            
                                     
                                    
                                     ),
                            tabPanel("How to use this app", p("instructions will go here")),
                            tabPanel("Echo instrument protocol", 
                                     p("instructions for the echo will go here"),
                                     p("download PDF instructions"),
                                     p("video protocol"))
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
    
    
    output$table_external_daughter <- 
      renderDataTable(head(daughter()))
    
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
    
    
    output$selection_external_mother <- 
      renderPrint(picker_list_mother())
    
    output$table_external_mother <- 
      renderTable({head(mother())})
  
    
    observeEvent(input$go, {
      try({
      try_catch_popup(
        daughter <<- ## added << 
          reactive(
            daughter_raw() %>%
              standardize_layout( . ,
                                  "daughter",
                                  .well_col = daughter_cols()[[1]],
                                  .compound_col = daughter_cols()[[2]],
                                  .concentration_col = daughter_cols()[[3]],
                                  .volume_col = daughter_cols()[[4]])) ,
        error_title = "daughter standard error"
        
      )
      
      try_catch_popup(
        mother <<- ## added << 
          reactive(
            mother_raw() %>%
              standardize_layout( . ,
                                  "mother",
                                  .well_col = mother_cols()[[1]],
                                  .compound_col = mother_cols()[[2]],
                                  .concentration_col = mother_cols()[[3]])),
        error_title = "mother standard error"
      )

      repairs <<- ## added << 
        reactive(
          get_repairs(
            input$repair_methods, #repair_list$selected, # comes from UI
            translate_repairs) 
        )
      
      if (repairs()[["repair_missing"]] == "stop") {
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
      }

      if (repairs()[["repair_varied"]] == "stop") {
      try_catch_popup(
        execute_this = rlang::abort(get_varied_errors(mother())$outcome),
        error_title = "Single compound present in mother at multiple concentrations",
        error_subtitle = 
          glue::glue("This isn't supported at this time. 
                 Edit your mother layout, or see the 'Repair methods' 
                 drop-down menu for ways to proceed anyway. 
                \nThis issue was found in compounds:
                {glue_collapse(get_varied_errors(mother())$compounds, sep = ', ')}"))
      }
      
      if (repairs()[["repair_conc"]] == "stop") {
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
      }
      
      try_catch_popup(
      layouts <<- ## added << 
        reactive(
        repair_layout(
          mother(), 
          daughter(),
          if_missing = repairs()[["repair_missing"]], 
          if_varied = repairs()[["repair_varied"]], 
          if_impossible = repairs()[["repair_conc"]])),
      error_title = "issue with making layout"
      )
      
      try_catch_popup(
      transfers <<- ## added << 
        reactive(
          calculate_transfers(
            layouts()$daughter, 
            layouts()$mother, 
            input$echo_vol) 
        ),
      error_title = "issue with getting transfers"
      )
        
      
      try_catch_popup(
      depletion <<- ## added <<
        reactive(
          monitor_source_depletion(
            transfers(), 
            input$max_vol
            ) ### THIS SHOULD BE USER-SET!! Or at least, have the option to be .. input$max_vol
          ),
      error_title = "depletions!"
      )
      
      try_catch_popup(
        instructions <<- ## added <<
          reactive(write_instructions_file(transfers())),
        error_title = "Issue writing echo instructions from transfers"
      )
      
      
      output$transfer_table <-
        renderDataTable({
          transfers()
        })
      
      output$instructions <-
        renderDataTable({
          instructions()
        })
      
      save_text <- reactive({
        paste0(input$save_name, "echo_instructions")
      })
      
      
      output$download <- renderUI({
        req(instructions())
        downloadButton("download_data", 
                       label = paste0("Download echo instructions folder") %>% strong(), #p("Download instructions and plots") %>% strong(), 
                       class = "btn-block btn-primary")
      })
      
      output$save_text <- renderText({
        paste0("Download folder default name: ", save_text())
      })
      
   
           
      for_plotting <<- ## added <<
          reactive(
            all_plateview_vars(daughter(), transfers(), depletion())
          )
         
 
        all_plots <<- ## added <<
          reactive(
            make_all_plots(
              for_plotting()$data, 
              for_plotting()$plot_vars)
            )

        
        final_layouts <<- 
          reactive(
            make_updated_layout(
              transfers(), 
              daughter_raw())
          )
        
      #  print(input$save_name)

    output$test_this <- renderTable({plot_data()})
    output$all_plots <- renderPlot({all_plots()$all_plots_fig})
      })
    })

    
    
    output$download_data <- downloadHandler(
      filename = function() {
        paste0(input$save_name,  "echo_instructions.zip")
      },
      
  
    
 
      

      content = function(filename) {
        tmpdir <- tempdir()
        setwd(tempdir())
        print(tempdir())
        
        all_plots_save <<- ## added <<
          reactive(
            make_all_plots(
              for_plotting()$data, 
              for_plotting()$plot_vars)
          )
        
      file_names <- c(paste0(input$save_name, "echo_instructions.csv"),  # main folder
                      paste0(input$save_name,"plateview_plots.pdf"), # main folder
                      
                      paste0(input$save_name,"final_layout.csv"), # layouts folder
                      
                      paste0(input$save_name, "transfers.csv"), # supporting files
                      paste0(input$save_name, "repaired_mother_layout.csv"), # supporting files
                      paste0(input$save_name, "repaired_daughter_layout.csv"), # supporting files
                      paste0(input$save_name,"source_depletion.csv") # supporting files
      )
      
        fs <- c(paste0(input$save_name, "echo_instructions.csv"),  # main folder
                paste0(input$save_name,"plateview_plots.pdf"), # main folder
                
                paste0(input$save_name,"final_layout.csv"), # layouts folder
                
                paste0(input$save_name, "transfers.csv"), # supporting files
                paste0(input$save_name, "repaired_mother_layout.csv"), # supporting files
                paste0(input$save_name, "repaired_daughter_layout.csv"), # supporting files
                paste0(input$save_name,"source_depletion.csv") # supporting files
        )
        
         write_csv(instructions(), file = file_names[[1]],  quote_escape = FALSE)
         ggsave(file_names[[2]], all_plots_save()$all_plots_fig, width = all_plots_save()$save_width, height = all_plots_save()$save_height)

        write_csv(x = final_layouts()$wide_layout,  file_names[[3]]) # not made yet

         write_csv(x = transfers(),  file_names[[4]])
         write_csv(x = layouts()$mother, file_names[[5]])
         write_csv(x = layouts()$daughter, file_names[[6]])
         write_csv(x = depletion(),  file_names[[7]])
        
        print (fs)

        zip(zipfile = filename, files = fs)
      },
      contentType = "application/zip"
    )
    

    

  
  }

shinyApp(ui = ui, server = server)

