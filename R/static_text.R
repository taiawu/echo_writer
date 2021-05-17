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

    guess_selections = # default to stop--no un-anncounced repairs without warning
      c("Stop and warn me",
        "Stop and warn me",
        "Stop and warn me")
  )


# groups = 
#   c("If a compound in the daughter is not in the mother...", 
#     "If a compound is present in the mother in multiple concentrations...", 
#     "If the concentration for a compound in the daughter exceeds the concentration in the mother..."),
# 
# options = 
#   list(
#     c("stop", "drop"),
#     c("stop", "keep_max", "keep_most"),
#     c("stop", "drop","make_max", "scale_down")),
# 
# guess_selections = 
#   c("stop", 
#     "stop",
#     "stop"))