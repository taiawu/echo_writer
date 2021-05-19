
calculate_transfers <- #_______(primary function) Write all transfer steps from layouts, rounding where necessary_______#
  function(daughter, mother, .echo_drop_nL = 25, .dilutant_name = "DMSO") {
    
    compound_transfers <-  #__compound transfers__
      concentrations_to_transfers(daughter, mother, .echo_drop_nL) %>% # rounds where necessary based on .echo_drop_nL
      mutate(dilution_vol = .data$daughter_final_vol - .data$mother_vol) %>%
      distribute_shared(.echo_drop_nL) %>%
      mutate(transfer_type = "compound_transfer")
    
    dilution_transfers <- #__calc dilution transfers separately, to respect necessary rounding and desired final volume__
      make_dilutions_plate(compound_transfers, mother, .echo_drop_nL = .echo_drop_nL, .dilutant_name = .dilutant_name)  %>%
      mutate(transfer_type = "dilution_transfer")
    
    
    transfers <-  #__All transfer steps and final conditions__#
      bind_rows(compound_transfers, dilution_transfers) %>%
      select(-c(.data$dilution_vol, .data$mother_dil)) %>% # helper column for dilution_transfers
      mutate(across(where(is.numeric), round, 2)) %>% # for readability
      select(c(.data$`Destination Well`, .data$`Source Well`, .data$compound, .data$daughter_conc, .data$mother_conc, .data$daughter_final_vol, .data$mother_vol, .data$final_conc, .data$rounded_up, .data$rounded_up_perc, .data$transfer_type)) # return in reader-friendly order
    
  }


monitor_source_depletion <- #_______(helper function) Monitor overdrawing of source plate wells, and warn_______#
  function(transfers, max_uL_pull = 35) {
    #_____Catch argument errors____
    if (!is.numeric(max_uL_pull)) {
      abort_bad_argument("max_uL_pull",
                         must = "be numeric",
                         not = typeof(max_uL_pull)) }
    # catching errors in transfers input would require a different abort and I don't want to write that right now
    
    #_____Calculate the depletion of each source well____
    source_depletions <- transfers %>%
      group_by(.data$`Source Well`) %>%
      mutate(uL_used = round(sum(.data$mother_vol)/1000, 3)) %>%
      select(.data$`Source Well`, .data$compound, .data$uL_used) %>%
      distinct() %>%
      mutate(over_drawn = if_else(.data$uL_used > max_uL_pull,
                                  true = "Overdrawn", # helps expressively throw error, for shinyAlerts()
                                  false = "Ok"))
    
    #_____Catch source overdraws and warn user____
    if ("Overdrawn" %in% source_depletions$over_drawn) {
      overdrawn <- source_depletions %>% filter(.data$over_drawn == "Overdrawn")
      warning(glue::glue("Warning!
          The requested transfers will overdraw {glue::glue_collapse(nrow(overdrawn))} well(s) in the mother plate.
          Specified maximum transfer volume is {max_uL_pull} uL per source well.
          Well(s) {glue::glue_collapse(overdrawn %>% pull(.data$`Source Well`), sep = ', ')} have the following excessive volumes (uL) transferred:
          {glue::glue_collapse(overdrawn %>% pull(.data$uL_used), sep = ', ')},
          corresponding to compounds {glue::glue_collapse(overdrawn %>% pull(.data$compound) %>% unique(), sep = ', ')}.
          To fix this, add more wells of any over-drawn compounds to your mother plate, or reduce their use in the daughter.")
      )
    }
    source_depletions %>% ungroup()# return for visualization and/or download
  }

concentrations_to_transfers <- #_______(helper function) Convert concentrations to transfer steps, rounding where necessary_______#
  function(daughter, mother, .echo_drop_nL = 25) { # if there are compounds in the daughter not in the mother
    #_____Catch argument errors____
    if (!is.numeric(.echo_drop_nL)) {
      abort_bad_argument(".echo_drop_nL",
                         must = "be numeric",
                         not = typeof(.echo_drop_nL)) }
    
    by_compound_m <-  #__INTERMEDIATE: mother layout, nested by compound__
      mother %>%
      group_by(.data$compound, .data$mother_conc) %>%
      nest()
    
    
    transfers <-  #__OUTPUT:  all calculated transfer steps__#
      daughter %>%
      left_join( . , by_compound_m, by = "compound")  %>%
      mutate(mother_dil = (.data$daughter_conc/.data$mother_conc) * ( .data$daughter_final_vol),
             mother_vol = plyr::round_any(.data$mother_dil, .echo_drop_nL, ceiling),
             final_conc = (.data$mother_conc*.data$mother_vol)/.data$daughter_final_vol,
             rounded_up = .data$final_conc - .data$daughter_conc,
             rounded_up_perc = if_else(.data$daughter_conc == 0, true = 0, false = round(100*.data$rounded_up/.data$daughter_conc, 1))) %>%
      filter(is.na(.data$mother_vol) == FALSE)
  }

make_dilutions_plate <-  #_______(helper function) Calculate dilution transfers separately to avoid rounding errors_______#
  function(transfers, mother, .echo_drop_nL = 25, .dilutant_name = "DMSO") {
    #_____Catch argument errors____
    if (!.dilutant_name %in% mother$compound) {
      abort_bad_argument(".dilutant_name",
                         must = "be a compound in the mother plate. {.dilutant_name} not found in mother.",
                         not = NULL) }
    # .echo_drop_nL used only in concentrations_to_transfer, errors caught there
    
    dil_mother <-  #__INTERMEDIATE: mother, with dilutant only__
      mother %>%
      filter(.data$compound == .dilutant_name) %>%
      mutate(mother_conc = 1) # just needs to match daughter for concentrations_to_transfers
    
    dil_daughter <- #__INTERMEDIATE: daughter, with dilutions only__
      transfers %>%
      filter(.data$dilution_vol > 0) %>%
      mutate(daughter_final_vol = .data$dilution_vol) %>%
      select(.data$`Destination Well`, .data$daughter_final_vol, ) %>%
      mutate(compound = .dilutant_name,
             daughter_conc = 1) %>%  # just needs to match mother for concentrations_to_transfers
      distinct() # remove duplicates for each transfer
    
    dil_transfer <- #__OUTPUT: dilution transfers__#
      concentrations_to_transfers(dil_daughter, dil_mother, .echo_drop_nL) %>%
      distribute_shared(.echo_drop_nL) %>%
      ungroup()
  }

distribute_shared <- #_______(helper function) Distribute transfers over common source wells_______#
  function(transfers, .echo_drop_nL = 25) {
    #_____Catch argument errors____
    if (!is.numeric(.echo_drop_nL)) {
      abort_bad_argument(".echo_drop_nL",
                         must = "be numeric",
                         not = typeof(.echo_drop_nL)) }
    
    distributed <- #__OUTPUT: transfers, distributed over common source wells__#
      transfers %>%
      mutate(n_wells = map_dbl(.data$data, nrow)) %>% #  the number of wells of mother
      mutate(per_well = (.data$mother_vol/.data$n_wells) - (.data$mother_vol/.data$n_wells)%%.echo_drop_nL, # how many transfers per well?
             extra_transfer = .data$mother_vol - .data$per_well*.data$n_wells) %>% # how many left over transfers, after even division over wells?
      rename(mother_vol_total = .data$mother_vol,
             mother_vol = .data$per_well) %>%
      unnest(cols = c(.data$data)) %>%  # this unnesting step adds the divided transfer volume to all mother source wells
      group_by(.data$compound, .data$mother_conc, .data$`Destination Well`, .add = TRUE) %>%
      mutate(mother_vol = if_else(row_number() == 1, .data$mother_vol + .data$extra_transfer, .data$mother_vol)) %>% # add the extra transfer to just one of the wells
      select(-c(.data$extra_transfer, .data$n_wells, .data$extra_transfer, .data$mother_vol_total)) %>% # drop unneeded column to match input
      ungroup()
  }

utils::globalVariables("where") # workaround: tidyselect::where() is not an exported function
# See: https://github.com/r-lib/tidyselect/issues/201#issuecomment-650547846


#######
#' Update starting layout with final conditions
#'
#' @param transfers the transfers tibble
#' @param raw_layout the raw layout, as uploaded by the user
#'
#' @return a list of 2 elements. layout, the updated layout, and wide_layout, the updated layout in wideform, ready to be saved as a csv.
#'
#' @importFrom dplyr filter rename select across right_join
#' @importFrom tidyselect all_of everything
#' @importFrom purrr set_names
#' @export
make_updated_layout <- function(transfers, raw_layout) {
  
  updated_layout <- transfers %>%
    filter(.data$transfer_type  == "compound_transfer") %>% # not dilutions
    rename("well" = .data$`Destination Well`) %>%
    select(all_of(c("well", "compound", "final_conc", "rounded_up"))) %>%
    set_names(c("well", "final_compound", "final_concentration", "concentration_rounded_by")) %>%
    right_join( . , raw_layout, by = "well")
  
  to_layout_block <-  updated_layout %>%
    select(-any_of(c("row", "column", "well"))) %>%
    names()
  
  for_wide <- updated_layout %>%
    mutate(across(.cols = everything(), as.character)) %>%
    add_empty_wells() %>%
    replace(is.na(.), "Empty") %>%
    bind_layouts( . , to_layout_block )
  
  list(layout =  updated_layout,
       wide_layout = for_wide)
  
}

#' Make wideform layouts for multiple variables
#'
#' A helper function for make_updated_layout().
#' Relies on its own helper function, make_layout_wide().
#' Ultimately, this function and its helpers should be a part of a different package--likely the layouts package, if we ever get that thing done.
#'
#' @param data a layout
#' @param var_list the columns in the layout to be make into individual layout blocks
#'
#' @return a tibble of wideform layout blocks, ready to be saved as a csv
#'
#' @importFrom dplyr bind_rows
#' @importFrom purrr set_names
#'
#' @export
bind_layouts <- function(data, var_list) {
  var_list %>%
    lapply( . , function(x) make_layout_wide(data, x)) %>%
    bind_rows()   %>%
    set_names(c("Type", "row", c(1:(ncol(.)-2))))
}


make_layout_wide <- function(data, .filt_col, .fill_empties = "Empty") {
  data %>%
    select(.data$row, .data$column, {{ .filt_col }}) %>%
    mutate("{.filt_col}" := replace_na(!!sym(.filt_col), .fill_empties)) %>%
    distinct() %>%
    mutate(column = as.numeric(.data$column)) %>%
    arrange(.data$column) %>%
    pivot_wider(id_cols = .data$row, names_from = .data$column, values_from = {{ .filt_col }}) %>%
    mutate(across(cols = everything(), as.character())) %>%
    rbind(names(.), .) %>%
    mutate(Type = {{ .filt_col }} ) %>%
    relocate(all_of("Type")) %>%
    unnest(cols = c(everything())) # this is a save that keeps bind_rows() from failing if these are list columns, but they really shouldn't be...
}


#' Make a Plate-view plot
#'
#' Makes a plot that mimics looking down at a well plate. Wells are colored based on a user-defined column from a layout.
#'
#' @param plate_data a layout tibble (as created by dsfworld::read_plate_layout()), containing columns for plate row (called "row"), plate column (called "column"), and one variable by which the plate will be colored
#' @param fill_col the column in the input layout by which to color in the wells in the plot
#' @param title_var the column for which the plot will be named. Shuldn't be an argument in the future; I struggled with tidyeval here so just put it at as different variable... Gets sent to glue::glue(). See make_all_plots function for the origin of this issue, with !!sym(x) working for the fill_col argument, but un-passable to glue.
#' @param shape the shape of the points used to make the wells. Defaults to 22 (filled square).
#' @param size the shape of the points used to make the wells. Defaults to 4.
#' @param title_prefix a prefix to be added to the title. Functionally, this can remain the same while title_var changes, to create consistent names when mapping over many variables. Also hopefully only a temporary argument, to be fixed in later versions.
#' @param ... to be passed to the ggplot2 aesetheics inside this function. Not actually used inside the function yet... another place to improve things here in the near furture.
#'
#' @return a plate-view plot, with wells colored based on the information on that well in the user-defined variable given in .fill_var.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter rename
#' @importFrom ggplot2 ggplot aes scale_y_discrete geom_point scale_x_continuous theme labs scale_fill_viridis_d scale_fill_viridis_c  element_rect element_blank element_line
#' @importFrom plyr rbind.fill
#' @importFrom tidyselect any_of
#' @importFrom tidyr fill
#' @importFrom stringr str_extract_all str_to_upper
#' @importFrom purrr as_vector
#' @importFrom readr parse_number
#'
#' @export
#'
plateview_plot <-
  function(plate_data,
           fill_col,
           title_var,
           shape = 22,
           size = 4,
           title_prefix = Sys.Date(),
           ...) {
    
    plot_title <- glue::glue("{title_prefix} Plate-view plot: {title_var}")
    
    fill_scale <- plate_data %>%
      pull({{ fill_col }})  %>%
      get_fill_scale( )
    
    plate_data
    ggplot(plate_data, aes(x = .data$column, y = .data$row)) +
      blank_plate(shape = shape, size = size)+
      
      geom_point(data = plate_data %>%
                   filter(is.na({{ fill_col }}) == FALSE),
                 aes(fill = {{ fill_col }}),
                 color = "#969696",
                 shape = shape,
                 size = size) +
      fill_scale +
      plate_theme_dark() +
      labs(title = plot_title)
  }


#' Add empty wells to a partially-filled layout
#'
#' A helper function for making plate plots using the plate_plot() function
#'
#' @param df the layout file to have empty wells appended
#' @param n_wells the number of wells in the plate. Options are "384" and "96". Defaults to "384".
#' @param .df_well_col the name of the colum containing well information. Defaults to "well".
#' @param .fill_down_cols a vector of any columns which should be filled in for the empty wells. These will be filled with a single value, determined by the plyr::fill(.direction = "down") function.
#' @param add_rows_cols should plate rows and columns be added to the layout (e.g. A, B, . . ., 1, 2, 3, ...)? Defaults to TRUE.
#'
#' @return the input layout, with all empty wells appended. These values are NA, unless specified in the .fill_down_cols argument of this function.
#'
#' @importFrom ggpubr ggarrange
#'
#' @export
add_empty_wells <- function(df,
                            n_wells = "384",
                            .df_well_col = "well",
                            .fill_down_cols = "",
                            add_rows_cols = TRUE) {
  well_vec <- switch(n_wells,
                     "384" = c("A1","B1","C1","D1","E1","F1","G1","H1","I1","J1","K1","L1","M1","N1","O1","P1","A2","B2","C2","D2","E2","F2","G2","H2","I2","J2","K2","L2","M2","N2","O2","P2","A3","B3","C3","D3","E3","F3","G3","H3","I3","J3","K3","L3","M3","N3","O3","P3","A4","B4","C4","D4","E4","F4","G4","H4","I4","J4","K4","L4","M4","N4","O4","P4","A5","B5","C5","D5","E5","F5","G5","H5","I5","J5","K5","L5","M5","N5","O5","P5","A6","B6","C6","D6","E6","F6","G6","H6","I6","J6","K6","L6","M6","N6","O6","P6","A7","B7","C7","D7","E7","F7","G7","H7","I7","J7","K7","L7","M7","N7","O7","P7","A8","B8","C8","D8","E8","F8","G8","H8","I8","J8","K8","L8","M8","N8","O8","P8","A9","B9","C9","D9","E9","F9","G9","H9","I9","J9","K9","L9","M9","N9","O9","P9","A10","B10","C10","D10","E10","F10","G10","H10","I10","J10","K10","L10","M10","N10","O10","P10","A11","B11","C11","D11","E11","F11","G11","H11","I11","J11","K11","L11","M11","N11","O11","P11","A12","B12","C12","D12","E12","F12","G12","H12","I12","J12","K12","L12","M12","N12","O12","P12","A13","B13","C13","D13","E13","F13","G13","H13","I13","J13","K13","L13","M13","N13","O13","P13","A14","B14","C14","D14","E14","F14","G14","H14","I14","J14","K14","L14","M14","N14","O14","P14","A15","B15","C15","D15","E15","F15","G15","H15","I15","J15","K15","L15","M15","N15","O15","P15","A16","B16","C16","D16","E16","F16","G16","H16","I16","J16","K16","L16","M16","N16","O16","P16","A17","B17","C17","D17","E17","F17","G17","H17","I17","J17","K17","L17","M17","N17","O17","P17","A18","B18","C18","D18","E18","F18","G18","H18","I18","J18","K18","L18","M18","N18","O18","P18","A19","B19","C19","D19","E19","F19","G19","H19","I19","J19","K19","L19","M19","N19","O19","P19","A20","B20","C20","D20","E20","F20","G20","H20","I20","J20","K20","L20","M20","N20","O20","P20","A21","B21","C21","D21","E21","F21","G21","H21","I21","J21","K21","L21","M21","N21","O21","P21","A22","B22","C22","D22","E22","F22","G22","H22","I22","J22","K22","L22","M22","N22","O22","P22","A23","B23","C23","D23","E23","F23","G23","H23","I23","J23","K23","L23","M23","N23","O23","P23","A24","B24","C24","D24","E24","F24","G24","H24","I24","J24","K24","L24","M24","N24","O24","P24"),
                     "96" = c("A1","B1","C1","D1","E1","F1","G1","H1","A2","B2","C2","D2","E2","F2","G2","H2","A3","B3","C3","D3","E3","F3","G3","H3","A4","B4","C4","D4","E4","F4","G4","H4","A5","B5","C5","D5","E5","F5","G5","H5","A6","B6","C6","D6","E6","F6","G6","H6","A7","B7","C7","D7","E7","F7","G7","H7","A8","B8","C8","D8","E8","F8","G8","H8","A9","B9","C9","D9","E9","F9","G9","H9","A10","B10","C10","D10","E10","F10","G10","H10","A11","B11","C11","D11","E11","F11","G11","H11","A12","B12","C12","D12","E12","F12","G12","H12"))
  
  
  df_wells <- df %>% pull({{ .df_well_col }}) %>% unique()
  
  df_out <- tibble("well" = well_vec %>% as.character()) %>%
    filter(!.data$well %in% df_wells) %>%
    plyr::rbind.fill(df,  . ) %>%# fills in everything
    as_tibble() %>% # convert back to tibble from data.frame
    fill(any_of(.fill_down_cols), .direction = "down")
  
  if (add_rows_cols == TRUE) {
    
    df_out <- df_out %>%
      mutate(row =  str_extract_all(.data$well, "[A-Z; a-z]", simplify = TRUE) %>%
               str_to_upper(locale = "en") %>%
               as_vector(),
             column = parse_number(.data$well))
  }
  
  df_out
  
}


#' make_all_plots
#'
#' Make a plateview plot for a every variable in a provided list.
#'
#' @param plate_data the layout tibble from which the plots will be made
#' @param plot_these a list of column names in plate_data--a plateview plot will be made for each element in this list.
#'
#' @return A list, containing (1) "individual_plots", which has each individual variable plotted, stored as a sub-itme under the name of the variable it contains. (2) "all_plots_fig", a ggpubr object containing all of the plotted variables in a single figure, useful for simple batch download. (3) save_width: the recommended widith to save the ggpubr object, set to 12. (4) save_height: the recommended height to save the all_plots_fig, calculated to depend on the number of plots that this figure contains.
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map
#' @importFrom rlang sym
#' @export
make_all_plots <- function(plate_data, plot_these) {
  
  plate_plots <- map(plot_these,
                     function(x) {
                       plateview_plot(plate_data,
                                      fill_col = !!sym(x),
                                      title_var = x) }) %>%
    set_names(plot_these)
  
  plate_fig <- plate_plots %>%
    ggpubr::ggarrange(plotlist = .,
                      ncol = 1,
                      align = "v")
  
  list(individual_plots = plate_plots,
       all_plots_fig = plate_fig,
       save_width = 12,
       save_height = 3*length(plate_plots))
  
}

## not exported
blank_plate <- # shared between most plots
  function(col_breaks = 1,
           shape = 22,
           size = 4,
           alpha = 1,
           ...) {
    list(
      scale_y_discrete(limits = rev), # start with row A
      geom_point(color ="#737373", # background
                 shape = shape,
                 size = size,
                 alpha = alpha,
                 ...),
      scale_x_continuous(breaks = seq(from = 1, to = 24, by = col_breaks))
    )
  }

#' Extract variables worth making plateview plots
#'
#' Mostly just here to mask out things like wells, rows, and columns
#'
#' @param data layout tibble
#' @param drop_vars vector of variables not worth plotting. Defaults to: c("Destination Well","Source Well","well","Well","row","column","mother_vol","rounded_up_perc","mother_conc" )
#'
#' @return a character vector of variables from the input data maybe work making into a plateview plot.
#' @export
get_plotworthy_vars <-
  function(data,
           drop_vars = c("Destination Well",
                         "Source Well",
                         "well",
                         "Well",
                         "row",
                         "column",
                         "mother_vol",
                         "rounded_up_perc",
                         "mother_conc" )) {
    data %>%
      select(-any_of(drop_vars)) %>%
      names()
  }

plate_theme_dark <-
  function() {
    theme(
      aspect.ratio = 16/24,
      
      panel.background  =
        element_rect(
          color = "#525252",
          fill = "#525252"),
      
      panel.grid.minor =
        element_blank(),
      
      panel.grid.major =
        element_line(
          color = "#737373",
          size = 0.5),
      
      legend.position = "right",
      
      axis.title =
        element_blank(),
      
      plot.background =
        element_rect(
          fill = "transparent",
          colour = NA),
    )
  }

get_fill_scale <- # handle discrete or continuous
  function(fill_vec) {
    scale_type <- # if unsure, use discrete
      if_else(is.numeric(fill_vec),
              true = "use_numeric",
              false = "use_discrete",
              "use_discrete")
    
    switch(scale_type,
           "use_numeric" = scale_fill_viridis_c(),
           "use_discrete" = scale_fill_viridis_d())
  }


#' Create a single tibble summarizing the transfer conditions and any important changes and repairs
#'
#' @param daughter_raw the standardized daughter
#' @param transfers the transfers tibble
#' @param depletion the depletion tibble
#'
#' @return a list, with elements data: a tibble containins pertinent columns from the three input tibbles, and plot_vars, a list of variables from this tibble to be passed to make_all_plots
#'
#' @importFrom purrr set_names
#' @importFrom dplyr rename
#'
#' @export
all_plateview_vars <- function(daughter_raw, transfers, depletion) {
  
  daughter_plotworthy <- daughter_raw %>%
    get_plotworthy_vars()
  
  daughter_vars <- daughter_raw %>%
    select(all_of(c("Destination Well", daughter_plotworthy))) %>%
    rename("original_compound_layout" = .data$compound) %>%
    rename("original_concentrations" = .data$daughter_conc)
  
  transfer_vars <- transfers %>%
    filter(.data$transfer_type == "compound_transfer") %>% # not the dilutant
    summarise_rounding() %>% # just final_conc, rounded_up, rounded_up_perc
    select(c(.data$`Destination Well`, .data$original_daughter_conc, .data$rounded_daugher_conc, .data$rounded_up_by, .data$compound)) %>%
    rename("concentrations_after_repairs" = .data$original_daughter_conc)
  
  depletion_vars <- depletion %>%  # just uL used and over_drawn
    select(c(.data$`Source Well`, .data$uL_used, .data$over_drawn)) %>%
    set_names(c("well", "uL_used_in_mother", "mother_well_overdrawn")) # purrr
  
  for_plateview <- left_join(daughter_vars, transfer_vars, by = "Destination Well")  %>%
    mutate(well = .data$`Destination Well`) %>%
    select(-.data$`Destination Well`) %>%
    left_join(. , depletion_vars, by = "well") %>%
    add_empty_wells()
  
  
  list(data = for_plateview,
       plot_vars = for_plateview %>% get_plotworthy_vars())
}


#' Repair impossible transfer requests
#'
#' Repairs mother and daughter layouts resulting impossible transfers. Allows users to choose options to minimally modify layouts to remove impossible transfers.
#'
#' @param mother A standardized mother layout.
#' @param daughter A standardized daughter layout.
#' @param if_missing How to resolve cases where a compound is present in the daughter, but not the mother.
#'  \itemize{
#'  \item{"stop": Halt execution, and return an informative error message.}
#'  \item{"drop": Remove these compounds from the daughter layout.}
#' }
#' @param if_varied How to resolve cases where a single compound is present in the mother at multiple concentrations.
#'  \itemize{
#'  \item{"stop": Halt execution, and return an informative error message.}
#'  \item{"keep_max": Transfer only from wells containing the highest concentration of this compound.}
#'  \item{"keep_most": Transfer only from wells containing the most abundant concentration of this compound.}
#' }
#' @param if_impossible How to resolve cases where a compound in the daughter at a concentration exceeding its concentration in the mother.
#'  \itemize{
#'  \item{"stop": Halt execution, and return an informative error message.}
#'  \item{"drop": Remove these concentrations from the daughter layout.}
#'  \item{"make_max": Replace these concentrations with the highest concentration achievable with the given mother plate.}
#'  \item{"scale_down": Replace all concentartions of offending compounds with a scaled-down concentration, such that the relative concentrations of this compound are retained, and all requested concentrations are equal to or less than the concentration of this compound in the mother plate. }
#' }
#'
#' @return A list
#' \itemize{
#' \item{mother: a mother plate, with any issues repaired as specified}
#' \item{daughter: a daughter plate, with any issues repaired as specified}}
#'
#' @importFrom dplyr distinct group_by group_modify if_else left_join n_distinct pull ungroup filter
#' @importFrom utils globalVariables
#' @importFrom rlang .data
#' @importFrom glue glue glue_collapse
#'
#' @export
repair_layout <- #_______(primary function) Repair design errors in layouts_______#
  function(mother, daughter,
           if_missing = "stop",    # stop gives error for shinyAlert()
           if_varied = "stop",     # stop gives error for shinyAlert()
           if_impossible  = "stop" # stop gives error for shinyAlert()
  ) {
    
    mother_rep <- #__Remove unsupported cases: multile conc. for single compounds in mother__
      repair_varied(mother, if_varied = if_varied)
    
    daughter_rep <- #__Remove impossible cases: missing compounds and conc. in excess of mother__
      daughter %>%
      repair_missing(mother_rep, ., if_missing = if_missing) %>% # especially useful for typos
      repair_conc(mother_rep, ., if_impossible = if_impossible) # common mistake
    
    list("mother" = mother_rep,
         "daughter" = daughter_rep)
  }

repair_missing <- #_______(helper function) Remove orphan compounds from daughter_______#
  function(mother, daughter, if_missing = "stop") { ## used to be  handle_missing_compounds()
    
    #_____Catch argument errors____
    if (!if_missing %in% c("stop", "drop")) { # catch typos etc.
      abort_bad_argument("if_missing",
                         must = glue::glue("be 'stop' or 'drop'"),
                         not = if_missing )
    }
    
    #_____Repair missing compounds____
    daughter_cmpds <- daughter$compound
    mother_cmpds <- mother$compound
    
    if (all(daughter_cmpds %in% mother_cmpds) == FALSE) {
      missing_cmpd <- # prints in errors and warnings to ease manual repair
        daughter_cmpds[!daughter_cmpds %in% mother_cmpds]
      
      if (if_missing == "drop") {
        #____Alert users of the one-shot function____
        warning(glue::glue("Warning!
          {glue::glue_collapse(length(unique(missing_cmpd)), sep = ', ')} compounds in daughter not present in the mother.
          The following compounds were missing, and have been dropped from the daughter:
          {glue::glue_collapse(unique(missing_cmpd), sep = ', ')}")
        )
        
        daughter <- # only supported repair is to remove
          daughter %>%
          filter(.data$compound %in% mother_cmpds)
        
      } else if (if_missing == "stop") {
        abort_bad_argument("Mother layout",
                           must = glue::glue("contain all compounds in the daughter layout.
                                    {glue_collapse(unique(missing_cmpd), sep = ',')}
                                    found in daughter layout, but not mother layout.
                                    To drop {glue_collapse(length(missing_cmpd))}
                                    well(s) containing missing compound(s) from the
                                    daughter, set if_missing = 'drop'"),
                           not = NULL )
      }
    }
    daughter # return unchanged if no mismatched
  }


make_max <- #_______(helper function - repair_conc) Overwrites to max conc_______#
  function(by_compound) {
    by_compound %>%
      mutate(daughter_conc = if_else(.data$daughter_conc > .data$mother_conc, # impossible ask
                                     true = .data$mother_conc, # closest possible conc.
                                     false = .data$daughter_conc))
  }

scale_down <-  #_______(helper function - repair_conc) Scales all concentrations down to preserve DRCs_______#
  function(by_compound) {
    by_compound %>%
      group_modify(~ {
        .x %>%
          mutate(daughter_conc = .data$daughter_conc*(.data$mother_conc/max(.data$daughter_conc)))
      })
  }


repair_conc <- #_______(helper function - repair_layout) Repairs unacievably high daughter concentrations_______#
  function(mother, daughter, if_impossible = "stop") {
    #_____Catch argument errors____
    if (!if_impossible %in% c("stop", "drop", "make_max", "scale_down")) {
      abort_bad_argument("if_impossible",
                         must = glue::glue("be 'stop', 'drop', 'make_max' or 'scale_down'"),
                         not = if_impossible )
    }
    
    #_____Identify compounds needing repair____
    df <- left_join(daughter, mother, by = "compound") %>%
      select(.data$`Destination Well`, .data$compound, .data$mother_conc, .data$daughter_conc, .data$daughter_final_vol) %>%
      distinct() %>%
      group_by(.data$compound) %>%
      mutate(repair = if_else(.data$daughter_conc > .data$mother_conc, true = "repair", false = "ok"))
    
    if ("repair" %in% df$repair) {
      to_repair <- # wells / compounds / concentrations to repair
        df %>%
        filter(.data$repair == "repair")
      
      #____Alert users of the one-shot function____
      warning(glue::glue("Warning!
          {glue::glue_collapse(length(unique(to_repair$`Destination Well`)))} well(s) in daughter plate are at a concentration in excess of the mother concentration for that compound.
          These wells are: {glue::glue_collapse(unique(to_repair$`Destination Well`), sep = ', ')}
          Containing compunds: {glue::glue_collapse(unique(to_repair$compound), sep = ', ')}
          These issues were repaired with the method: {if_impossible}, specified by the `if_impossible` argument."))
      
      #_____User specifies repair method____
      out <- switch(if_impossible,
                    "stop" =  if (!all(df$repair == "ok")) { # interfaces with shinyAlerts() to prompt user decision
                      abort_bad_argument("Each compound",
                                         must = "be be present in the daughter layout
                                       at or below its concentration in the mother. ",
                                         not = NULL)}, # throw an error if repairs are needed
                    "drop" = df %>% filter(.data$repair == "ok"),
                    "make_max" = df %>% make_max(),
                    "scale_down" = df %>% scale_down())
      
      #_____Match input format____
      out <- out %>%
        ungroup() %>%
        select(-c(.data$repair, .data$mother_conc)) %>% # temporary cols created for this function only
        select(c(.data$`Destination Well`, .data$compound, .data$daughter_conc, .data$daughter_final_vol)) # the expected column order, visually
    } else {
      out <- daughter
    }
    out
  }

repair_varied <- #_______(helper function - repair_layout) Repairs unacievably high daughter concentrations_______#
  function(mother, if_varied = "keep_max") {
    #_____Catch argument errors____
    if (!if_varied %in% c("keep_max", "keep_min", "keep_most", "stop")) {
      abort_bad_argument("if_varied",
                         must = glue::glue("be 'stop', 'keep_max' or 'keep_most'"),
                         not = if_varied ) }
    
    #_____Identify compounds with multiple concentrations____
    tallied_by_cmpd <- mother %>%
      group_by(.data$compound) %>%
      mutate(n_conc = n_distinct(.data$mother_conc), # how many conc. in mother
             max_conc = max(.data$mother_conc),
             min_conc = min(.data$mother_conc)) %>%
      group_by(.data$compound, .data$mother_conc) %>% # needed if keep_most selected
      mutate(well_per_conc = n_distinct(.data$`Source Well`)) %>%
      group_by(.data$compound) %>%
      mutate(most_wells = max(.data$well_per_conc))
    
    #_____User specifies repair method____
    if(max(tallied_by_cmpd$n_conc) > 1) {
      to_repair <- # for more helpful errors and warnings
        tallied_by_cmpd %>%
        filter(.data$n_conc > 1)
      
      #____Alert users of the one-shot function____
      warning(glue::glue("Warning!
          {glue::glue_collapse(length(unique(to_repair$compound)))} compound(s) present in multiple concentrations in the mother plate.
          These compounds are: {glue::glue_collapse(unique(to_repair$compound), sep = ', ')}
          These issues were repaired with the method: {if_varied}, specified by the `if_varied` argument."))
      
      if (if_varied == "stop") { # interfaces with shinyAlerts() to prompt user decision
        multi_conc <- # prints in error to ease manual repair
          tallied_by_cmpd %>%
          filter(.data$n_conc > 1) %>%
          pull(.data$compound) %>%
          unique()
        
        rlang::abort(message = glue::glue("Different concentrations of the same compound
                                          in the mother plate are not supported.
                                          Compound(s) `{glue_collapse(multi_conc)}`
                                          present in multiple concentrations in the mother plate.
                                          To resolve this, direct how a single concentration
                                          is chosen for each compound by setting
                                          `if_varied` to 'keep_max' or 'keep_most'. "))
      } else {
        out <-
          switch(if_varied,
                 "keep_max" = tallied_by_cmpd %>% filter(.data$mother_conc == .data$max_conc),
                 "keep_min" = tallied_by_cmpd %>% filter(.data$mother_conc == .data$min_conc),
                 "keep_most" = tallied_by_cmpd %>%
                   filter(.data$well_per_conc == .data$most_wells) %>%
                   mutate(max_conc = max(.data$mother_conc)) %>% # if >1 conc. meets criteria
                   filter(.data$mother_conc == .data$max_conc)
          )
        
        out <- # remove the columns created by these operations
          out %>% select(-c(.data$n_conc, .data$max_conc, .data$min_conc, .data$well_per_conc, .data$most_wells ))
      }
    } else { # don't mess with error-free mothers
      out <- mother
    }
    out %>% ungroup() # downstream expects ungrouped
  }

utils::globalVariables(c("."))

#' Standardize plate layouts
#'
#'
#'
#'
#' @param layout A plate layout, containing columns with compound, concentration, and, for daughter plates only, volume.
#' @param which_plate The type of layout to standardize. Can be "mother" or "daughter".
#' @param .well_col,.compound_col,.concentration_col,.volume_col Names of the columns in the input layout containg necessary information.
#'  \itemize{
#'  \item{.well_col: The well names.}
#'  \item{.compound_col: The compounds to be transferred. Compounds must go by the same name in mother and daughter plates.}
#'  \item{.concentration_col: The concentration of each compound, either in the mother plate, or the desired concentration in the daughter plate.}
#'  \item{.volume_vol: For daughter plates only. The desired volume in nL in each well of the daughter. }
#' }
#'
#' @return A plate layout, containing standardized columns and column names for use in downstream echowritr functions.
#'
#' @importFrom magrittr "%>%"
#' @importFrom glue glue glue_collapse
#' @importFrom dplyr filter across select mutate
#' @importFrom tidyselect all_of everything any_of
#' @importFrom purrr set_names
#' @importFrom tidyr drop_na
#'
#'
#' @export
standardize_layout <- function(layout,
                               which_plate,
                               .well_col = "well",
                               .compound_col = "compound",
                               .concentration_col = "concentration",
                               .volume_col = "volume") {
  
  # ensure valid plate type selection, or return error
  
  if (!which_plate %in% c("mother", "daughter")) {
    abort_bad_argument("which_plate", must = "be either 'daughter' or 'mother", not = which_plate)}
  
  # update the column names and columns to be selected according to plate selected, or return error
  col_names <- switch(which_plate, "daughter" = c("Destination Well", "compound", "daughter_conc", "daughter_final_vol"),
                      "mother" = c("Source Well", "compound", "mother_conc"))
  
  which_cols <- switch(which_plate,
                       "daughter" = c({{ .well_col }}, {{ .compound_col }}, {{ .concentration_col }}, {{ .volume_col }}),
                       "mother" = c({{ .well_col }}, {{ .compound_col }}, {{ .concentration_col }}))
  
  numeric_cols <- switch(which_plate,
                         "daughter" = c({{ .concentration_col }}, {{ .volume_col }}),
                         "mother" = c({{ .compound_col }}))
  
  if (!.well_col %in% names(layout)) {
    abort_bad_argument(".well_col", must = glue::glue("be in {which_plate} layout names. Looked for {.well_col}, but layout names are {glue_collapse(names(layout),  sep = ', ')}"),
                       not = NULL)
  }
  
  # condition the layout
  layout %>%
    filter(is.na({{ .compound_col }}) == FALSE) %>%
    select(all_of(which_cols)) %>%
    mutate(across(.cols = everything(), as.character),
           across(any_of(c("concentration", "volume")), as.numeric)) %>%
    set_names(col_names) %>%
    drop_na() ## if anything became NA when mutated to numeric--but this is something that folks should probably be alerted to.
}

#' Summarise the rounding resulting from the transfer calculations
#'
#' Wil likely get built out further for a more comprehensive changes report.
#'
#' @param transfers the transfers tibble
#' @param return_filtered if TRUE, returns a tibble containins only rows corresponding to rounded values. If FALSE, no filtering is performed.
#'
#' @return the transfers tibbl, with only the columns relevant to rounding. These columns are renamed so that users can guess at their meaning. Verbose but hopefully clearer.
#'
#' @importFrom dplyr select
#' @importFrom purrr set_names
#' @export
summarise_rounding <- function(transfers, return_filtered = FALSE) {
  rounded <-  transfers %>%
    #filter(.data$rounded_up != 0) %>%
    select(c(.data$`Destination Well`, .data$compound, .data$daughter_conc, .data$final_conc, .data$rounded_up, .data$rounded_up_perc )) %>%
    set_names(c("Destination Well", "compound", "original_daughter_conc", "rounded_daugher_conc", "rounded_up_by", "percent_conc_increase_by_rounding"))
  
  if (return_filtered){
    out <- rounded %>%
      filter(.data$rounded_up != 0)
  } else {
    out <- rounded
  }
  
  out
}

#' Utilities
#'
#' abort_bad_argument taken from Advanced R! Written by Hadley Wichkham.
#' @param arg the argument
#' @param must allowed values of argument
#' @param not the bare argument
#'
#' @return utilities
#'
#' @importFrom glue glue
#' @importFrom rlang abort
#'
#' @export
#'

abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    msg <- glue::glue("{msg}; not `{not}`.")
  }
  
  rlang::abort("error_bad_argument",
               message = msg,
               arg = arg,
               must = must,
               not = not
  )
}


#' Make experiment header
#'
#' Not sure what package this really belong in in the end, but its useful here, so adding it here for now.
#'
#' @param expnum the experiment number. Defaults to "0000"
#'
#' @return a string fo the standard experiment number prefix format: "Exp<expnum>--YYYYMMDD_"
#'
#' @export
make_exp_header <- function(expnum = "0000") {
  paste0("Exp",
         expnum,
         "--",
         Sys.Date() %>% gsub("-", "", .),
         "_")
  
}



#' Write instructions file for by Echo Plate Reformat software
#'
#' Minor reformatting of calculated transfers tibble for uploading to the Echo Plate Reformat software
#'
#' @param transfers A tibble containing all transfer steps, returned by echowritr::calculate_transfers()
#' @param save_file If TRUE, saves instructions file under user-specified name.
#' @param .save_name File name for saved echo instructions
#' @param save_default A string appended to the end of the .save_name. Defaults to "_echo_transfer_instructions".
#'
#' @return A tibble containing echo instructions. If save_file = TRUE, this tibble is also saved as a csv.
#'
#' @importFrom dplyr select rename mutate select
#' @importFrom tidyselect all_of
#' @importFrom readr write_csv
#'
#' @export
write_instructions_file <- #_______(primary function) Save instructions for uploading to echo plate reformat software_______#
  function(transfers, save_file = FALSE, .save_name, save_default = "_echo_transfer_instructions") {
    instructions <- transfers %>%
      select(.data$`Destination Well`, .data$`Source Well`, .data$mother_vol) %>% # only these columns go to instructions
      rename("Transfer Volume" = .data$mother_vol) %>% # plate reformat software expects this name
      
      mutate("Source Plate Name" = "Source[1]", # these may be somewhat protocol variant, but also easy to change manually
             "Destination Plate Name" = "Destination[1]",
             "Destination Well X Offset"	= NA,
             "Destination Well Y Offset"	= NA) %>%
      
      select(all_of(c("Source Plate Name",	"Source Well",
                      "Destination Plate Name",	"Destination Well",
                      "Transfer Volume",	"Destination Well X Offset",
                      "Destination Well Y Offset")))
    
    if (save_file == TRUE) {
      instructions %>%
        readr::write_csv(x = . , paste0(Sys.Date(), "_", .save_name, save_default, ".csv"))
    }
    instructions
  }



