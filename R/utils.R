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

## translate user choices
get_repairs <- 
  function(selections, # list from pickerInput, format: {group}<>{choice}
           translator, # df with cols: argument, and values
           names_into = c("group", "response"),
           sep_by = "<>") {
    trans <- tibble(selected = selections) %>% 
      separate(selected, 
               into = names_into, 
               sep = sep_by) %>%
      left_join( . ,translator, by = c("group", "response")) 
    
    repair_actions <- c(trans$values) %>%
      set_names(trans$argument)
  }

# ### for the repairs, which reauires no reactivity
# repair_actions <- 
#   translate_picked_repairs(repair_list$selected, # comes from UI
#                            ranslate_repairs)
# 
# # repair_actions[["repair_missing"]] # call it like this

get_cols <- 
  function(selections,
           group_names,
           .into = c("group", "response"),
           .sep = "<>"){
    
    trans <- 
      selections %>%
      tibble(temp = .) %>%
      separate(temp, 
               into = .into, 
               sep = .sep) %>%
      left_join( . , group_names, 
                 by = c("group")) 
    
    selected <- trans$response %>%
      set_names(trans$argument)
  }