groups_daughter_text <- 
  c("Which variable in your daughter layout contains well names?", 
    "Which variable in your daughter layout contains compound names?", 
    "Which variable in your daughter layout contains compound concentrations?",
    "Which variable in your daughter layout contains volumes (nL) in the final daughter wells")

guess_daughter_cols_text <-
  c("well", "compound", "concentration", "volume")

groups_mother_text <- 
  c("Which variable in your mother layout contains well names?", 
    "Which variable in your mother layout contains compound names?", 
    "Which variable in your mother layout contains compound concentrations?")

guess_mother_cols_text <-
  c("well", "compound", "concentration")

repair_options <- 
  list(
    groups = # vector of groups for repair pickerInput
      c("If a compound in the daughter is not in the mother...",
        "If a compound is present in the mother in multiple concentrations...",
        "If the daughter contains un-achievably high concentrations..."),
    
    options = # list of options for repair pickerInput
      list(
        c("Stop and warn me",
          "Remove these compounds from the daughter"),
        c("Stop and warn me",
          "Keep the wells with the highest concentration",
          "Keep the concentration with the most wells"),
        c("Stop and warn me",
          "Remove these compounds from the daughter",
          "Replace with the highest achieveable concentration",
          "Scale down the concentration of all wells of this compound")),
    
    values = 
      list(
        c("stop",
          "drop"),
        c("stop",
          "keep_max",
          "keep_most"),
        c("stop",
          "drop",
          "make_max",
          "scale_down")
      ),
    
    guess_selections = # default to stop--no un-anncounced repairs without warning
      c("Stop and warn me",
        "Stop and warn me",
        "Stop and warn me")
  )

##3 translating from the UI text to the programmer text... 
## for repairs, which do not interact with the data
repair_group_names <- 
  tibble(argument = 
           c("repair_missing", 
             "repair_varied", 
             "repair_conc"),
         group = repair_options$groups
  )

repair_list <-    
  make_picker_lists(
    groups = repair_options$groups,
    options = repair_options$options,
    guess_selections = repair_options$guess_selections)

translate_repairs <- 
  tibble(group = repair_options$groups,
         response = repair_options$options,
         values = repair_options$values) %>%
  unnest(cols = c(response, values)) %>%
  left_join( . ,repair_group_names)

## for column, which dos interact with the data
# daughter
daughter_group_names <- 
  tibble(argument = 
           c("well_col", 
             "cmpd_col", 
             "conc_col",
             "vol_col"),
         group = groups_daughter_text
  )


# mother
mother_group_names <- 
  tibble(argument = 
           c("well_col", 
             "cmpd_col", 
             "conc_col"),
         group = groups_mother_text
  )