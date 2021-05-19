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

blank_plate <- # shared between most plots
  function(col_breaks = 1, 
           shape = 22,
           size = 4,
           alpha = 1) {
    list(
      scale_y_discrete(limits = rev), # start with row A
      geom_point(color ="#737373", # background
                 shape = shape,
                 size = size,
                 alpha = alpha),
      scale_x_continuous(breaks = seq(from = 1, to = 24, by = col_breaks)) 
    )
  }

flags <- # call out changes
  function(){
    
  }

plateview_plot_app <- 
  function(plate_data, 
           fill_col,
           title_var,
           shape = 22,
           size = 4,
           title_prefix = Sys.Date()) {
    
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

get_plot_vars <- 
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

plot_all_fig <- function(plate_data, plot_these) {
  
  plate_plots <- map(plot_these,
                     function(x) {
                       plateview_plot_app(plate_data, 
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