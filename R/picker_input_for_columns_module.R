# library(tidyverse)
# library(glue) # required for custom names within the selectors'
# library(shinyWidgets) # contains pickerInput
# library(shiny)

#### what it does
# (1) allows users to select columns by name from a reactive tibble
# (2) these selections can be grouped -- easy feature with pickerInput, see choices = list(...) line 55
# (3)defaults to one selection per group (you can set this) -- easy feature with pickerInput, see options = pickerOptions("max-options-group" = 1)

# (4) THIS MODULE DOESN"T SUPPORT: requires one selection per group (no more, no less) -- js workaround from  https://stackoverflow.com/questions/62630479/how-to-ensure-that-in-pickerinput-choices-at-least-one-item-is-selected-in-each
# REQUIRED! to make (4) work, two things must match the js:
# the pickerInput ID name to which it will apply (here:PICKER_INPUT_ID)
# the name of the output creates (here: picker)
# something with namespacing here that's too complex to be worth figuring out right now

# (5) compatible with the same names appearing in multiple groups -- semi-gotcha: see picker_names_by_group() function. Picker needs unique values, but to the user, it displays the value's name, and these names need not be unique. 
# this js will apply only to the elements identified here by id name (linevars). linevars appears only once


guess_col <- #___(helper function) guesses, robust to small variance in names___
  function(target, opts) {
    sapply(opts, stringdist, target) %>%
      sort() %>%
      names()
  }

picker_names_by_group <- #___(helper function) names lists for pickerInput, to permit non-unique options___
  function(groups, 
           options, 
           sep = "<>") {
    # groups: vector of names of each group in the pickerInput
    # options: list of vectors, each vector containing options for that group
    
    sapply(options, function(x) glue::glue(groups, sep, x)) %>% # values are uniquely identifying
      set_names(options) # UI displays the names; don't require uniqueness
  }

make_picker_lists <- #___(primary function) formats and names lists for pickerInput___
  function(groups, 
           options, 
           guess_selections) {
    # groups: vector of names of each group in the pickerInput
    # options: list of vectors, each vector containing options for that group
    # guess_selection: the name to look for, e.g. like default selections
    
    choices <- 
      map2(groups, options, picker_names_by_group ) %>%
      set_names(groups) # names of the groups in pickerInput
    
    closest <- # get the closest using stringdist
      map2(guess_selections, options, function(x,y) guess_col(x, y)[1]) 
    
    selected <- # pickerInput needs the uniquely-identified version of this
      map2(choices, closest, function(x,y) {x[which(names(x)  == y)]})
    
    list("choices" = choices, # choices for pickerInput (unique values, non-unique names)
         "closest" = closest, # the values chosed, in intuitive format for other options 
         "selected" = selected) # selected, for pickerInput (unique values only)
  }

#####___
# should return something that can be used to filter the layout outside of the module
pickerInputServer <- function(id, 
                              data, 
                              groups, 
                              options, 
                              guess_selections, 
                              .multiple = TRUE,
                              .inline = TRUE,
                              picker_title = "picker_title", 
                              picker_label = "picker_label") {
  
  stopifnot(is.reactive(data)) # layout should be reactive
  stopifnot(is.reactive(groups)) # maybe based on data in some uses
  stopifnot(is.reactive(options)) # maybe based on data in some uses
  stopifnot(is.reactive(guess_selections)) # maybe based on data in some uses
  
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      # req(data())
      # req(groups())
      # req(options())
      # req(guess_selections())
      
      #### code within the server module
      picker_lists <- # passed to pickerInput
        reactive( # user sets some of these reactively
          make_picker_lists(
            groups = groups(),
            options = options(), 
            guess_selections = guess_selections())
        )
      
      output$picker <- # the UI element
        renderUI({
          pickerInput(
            # THANK YOU for renderUI in modules https://gist.github.com/tbadams45/38f1f56e0f2d7ced3507ef11b0a2fdce 
            session$ns("take_these"),
            #"PICKER_INPUT_ID", # session$ns("take_these") # must match js to enforce one selection per group
            label = picker_label, # user can change the label
            choices = picker_lists()$choices, # displays NAMES of the list elements, not elements itself
            selected =  picker_lists()$selected, #list(var1[1], var2[2], var3[3]), # also works
            multiple = .multiple,
            inline = .inline, # put label in-line with drop-downs
            options = 
              pickerOptions( # good place for ... in later version
                "max-options-group" = 1,
                title = picker_title
              ))
        })
      
      output$selection <- renderPrint({ input$take_these })
    })
    
    reactive(input$take_these)
 })
}


pickerInputUI <- function(id) {
  ns <- NS(id)
  tagList(
      uiOutput(ns("picker")),
    verbatimTextOutput(ns("selection"))
  )
}


## outer module, for the special case of handling only tibble names
pickColUI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInputUI(ns("picker_input"))[[1]],
    pickerInputUI(ns("picker_input"))[[2]]
  )
}

pickColServer <- function(id, 
                          data, # used to get options
                          groups, # passed to pickerInputServer
                          guess_selections, # passed to pickerInputServer
                          drop_options = "", 
                          .multiple = TRUE,
                          .inline = TRUE,
                          picker_title = "picker_title", 
                          picker_label = "picker_label") {
  
  stopifnot(is.reactive(data)) # layout should be reactive
  stopifnot(is.reactive(groups)) # maybe based on data in some uses
  stopifnot(is.reactive(guess_selections)) # maybe based on data in some uses
  
  moduleServer(id, function(input, output, session) {
    
    options <- # generates column names as the options for all groups
      reactive(
        lapply(
          groups(), 
          function(x) { 
            names(data() 
                  %>% select(-any_of(drop_options))) # option to mask util columns like "column" or "row"
          }
        )
      )
    
    picker_list <- pickerInputServer("picker_input",
                                     data,
                                     groups = groups, # passed directly
                                     options = options, # calculated above from data()
                                     guess_selections = guess_selections, # passed directly
                                     .multiple = .multiple,
                                     .inline = .inline,
                                     picker_title = picker_title, # passed directly
                                     picker_label = picker_label) # passed directly
    
    output$selection_external <- renderPrint(picker_list())
    reactive(picker_list())
  })
}