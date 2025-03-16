#' Column Chart
#'
#' Creates a column chart using either ggplot2 (static) or plotly (dynamic).
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly) output.
#'   Default is FALSE, which will return a static ggplot output.
#' @param base A base ggplot object to add the column chart to. Default is NULL.
#' @param params A named list containing arguments used to create the plot:
#'   \itemize{
#'     \item df: The dataframe containing the data to be plotted
#'     \item x: The x value column name to be plotted
#'     \item y: The y value column name to be plotted
#'     \item group_var: The variable used to group the bars i.e. region if plotting by region
#'     \item fill: The colour with which to fill the columns
#'     \item y_axis: Either "y1" for primary y-axis or "y2" for secondary y-axis
#'     \item position: The positions of the bars to be plotted i.e."dodge", "stack" etc
#'     \item ci: Indicator for using ribbon or error bar geom (if required), enter 'e' for error bar, enter any other value for ribbon
#'     \item lower: Lower value for error/ribbon geom (mandatory if ci argument passed)
#'     \item upper: Upper value for error/ribbon geom (mandatory if ci argument passed)
#'     \item error_colour: If not plotting by group this is the colour of the error bars or ribbon
#'     \item h_line: Will display a horizontal line if valid integer passed
#'     \item y_label: For provision of an y axis label
#'     \item x_label: For provision of an x axis label
#'     \item x_label_angle: To adjust the x axis label by the degrees of the integer provided
#'     \item y_label_angle: To adjust the y axis label by the degrees of the integer provided
#'     \item x_labels_reverse: Enter an argument of any value i.e. 'y' to reverse the x labeling order
#'     \item y_min_limit: Set the limit on the y axis scaling by proving an integer
#'     \item y_max_limit: Set the limit on the x axis scaling by proving an integer
#'     \item x_axis_breaks: Modify the x axis breaks by providing an integer
#'     \item legend_pos: Modify the position of the legend (where applicable)
#'     \item remove_gridlines: Enter an argument of any value i.e. 'y' to remove grid lines
#'     \item percent: Enter an argument of any value i.e. 'y' to include the % symbol
#'     \item cap_text: Enter text for a caption to appear below plot
#'     \item no_shift: If no shift should be applied to the secondary y-axis
#'   }
#' @param ... Additional arguments passed to geom_col for static (ggplot2) plots
#'   or to plot_ly/add_trace for dynamic (Plotly) plots, allowing custom
#'   styling of the columns (e.g., alpha, width, marker, etc.).
#' @import assertthat
#'
#' @return A ggplot or plotly object depending on the value of dynamic parameter
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' data <- epiviz::lab_data |>
#'  group_by(organism_species_name) |>
#'  summarise(Count=n())
#'
#' col_chart(params = list(
#'   df = data,
#'   x = "organism_species_name",
#'   y = "Count"
#' ))
#' }
col_chart <- function(
    dynamic = FALSE,
    base = NULL,
    params = list(
      df = NULL,
      x = NULL,
      y = NULL, 
      group_var = NULL,
      fill = "blue",
      y_axis = "y1",
      position = "dodge",
      ci = NULL,
      lower = NULL,
      upper = NULL,
      error_colour = "red",
      h_line = NULL,
      y_label = NULL,
      x_label = NULL,
      x_label_angle = NULL,
      y_label_angle = NULL,
      x_labels_reverse = NULL,
      y_min_limit = NULL, 
      y_max_limit = NULL,
      x_axis_breaks = NULL,
      legend_pos = "bottom",
      remove_gridlines = NULL,
      percent = NULL,
      cap_text = NULL,
      no_shift = FALSE
    ),
    ...  # Add ellipsis parameter
) {
  # Set Arial font
  set_Arial()

  # Filter out plotly-specific parameters from ellipsis for ggplot
  dots <- list(...)
  plotly_params <- c("opacity", "hoverlabel", "showlegend")
  ggplot_dots <- dots[!names(dots) %in% plotly_params]
  
  # Where relevant, assign defaults to any parameters not specified by the user
  if(!exists('df',where=params)) stop("A data frame argument is required")
  if(!exists('x',where=params)) stop('Please include argument data frame variable for x axis, ie x = "variable_name"')
  if(!exists('y',where=params)) stop('Please include argument data frame variable for y axis, ie y = "variable_name"')
  if(!exists('fill',where=params)) params$fill <- "blue"
  if(!exists('y_axis',where=params)) params$y_axis <- "y1"
  if(!exists('position',where=params)) params$position <- "dodge"
  if(!exists('error_colour',where=params)) params$error_colour <- "red"
  if(!exists('legend_pos',where=params)) params$legend_pos <- "bottom"
  if(!exists('no_shift',where=params)) params$no_shift <- FALSE

  # Check that the data frame provided is not empty, else stop
  assertthat::assert_that(not_empty(params$df))

  # Assign variables from params list for easier referencing
  df <- params$df
  x <- params$x
  y <- params$y
  group_var <- params$group_var
  fill <- params$fill
  y_axis <- params$y_axis
  position <- params$position
  ci <- params$ci
  lower <- params$lower
  upper <- params$upper
  error_colour <- params$error_colour
  h_line <- params$h_line
  y_label <- params$y_label
  x_label <- params$x_label
  x_label_angle <- params$x_label_angle
  y_label_angle <- params$y_label_angle
  x_labels_reverse <- params$x_labels_reverse
  y_min_limit <- params$y_min_limit
  y_max_limit <- params$y_max_limit
  x_axis_breaks <- params$x_axis_breaks
  legend_pos <- params$legend_pos
  remove_gridlines <- params$remove_gridlines
  percent <- params$percent
  cap_text <- params$cap_text
  no_shift <- params$no_shift

  # Check that the data frame provided is not empty, else stop
  assertthat::assert_that(not_empty(df))

  if (is.null(base)) {
    base <- ggplot2::ggplot() + theme_minimal() + # Change to minimal theme
        theme(
      axis.title.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = legend_pos,
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      panel.grid = element_blank(),  # Remove grid lines by default
      axis.line = element_line(color = "black", linewidth = 0.5),  # Add axis lines in black
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Center-align and bold title
      plot.caption = element_text(size = 10, hjust = 0),  # Left-align caption
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)  # Add consistent margins
    )
  # Add gridlines back if remove_gridlines is not specified
  if(is.null(remove_gridlines)) {
    base <- base + theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor = element_line(color = "grey95", linewidth = 0.1)
    )
  }
  }

  # If user wants to plot on the secondary y-axis
  if (y_axis == "y2") {
    # Get limits of current plotted data (returns -Inf if no data currently plotted)
    current_plotted_data_max <- max(layer_scales(base)$y$range$range)
    current_plotted_data_min <- min(layer_scales(base)$y$range$range)
    # Get limits of new data to plot
    y2_max <- max(df[[y]])
    y2_min <- min(df[[y]])

    # If no secondary y data has been plotted yet
    if (is.null(base$secondary_y_shift) & is.null(base$secondary_y_scale)) {
      if (is.finite(current_plotted_data_max)) {
        # If data has already been plotted on y1
        # scale and shift variables calculated based on desired mins and maxes
        scale = (y2_max - y2_min)/(current_plotted_data_max - current_plotted_data_min)
        shift = current_plotted_data_min - y2_min
        # Add variables to chart "metadata"
        base$secondary_y_shift <- shift
        base$secondary_y_scale <- scale
        # Get current y1 axis name
        current_y_axis_name <- ggplot_build(base)$layout$panel_params[[1]]$y$name

      } else {
        # Data hasn't already been plotted on y1
        # Just plot data as normal but on y2
        current_y_axis_name <- NULL
        scale <- 1
        shift <- 0
      }

    } else {
      # If secondary y data has already been plotted
      shift <- base$secondary_y_shift
      scale <- base$secondary_y_scale
      current_y_axis_name <- ggplot_build(base)$layout$panel_params[[1]]$y$name
    }

  } else {
    scale <- 1
    shift <- 0
  }

  if (no_shift == TRUE) {
    shift <- 0
    base$secondary_y_shift <- 0
  }

  #### Apply the inv_scale_function to the values that will be plotted on the scaled secondary y axis (if they've been supplied) ####
  if (!is.null(y)) {
   df[[y]] <- inv_scale_function(df[[y]], scale, shift)
  }

  if (!is.null(lower)) {
   df[[lower]] <- inv_scale_function(df[[lower]], scale, shift)
  }

  if (!is.null(upper)) {
   df[[upper]] <- inv_scale_function(df[[upper]], scale, shift)
   }

  if (!is.null(h_line)) {
    hline <- inv_scale_function(hline, scale, shift)
  }

  # if group variable not provided build plot accordingly
  if(is.null(group_var)){
    base <- base + geom_col(data = df, 
                           aes(x = .data[[x]], y = .data[[y]]), 
                           fill = fill,  # Move fill outside aes()
                           position = position,
                           !!!ggplot_dots)  # Pass filtered aesthetics
  } else if (!is.null(group_var) && is.null(fill)) {
    base <- base + geom_col(data = df, 
                           aes(x = .data[[x]], y = .data[[y]], fill = .data[[group_var]]), 
                           position = position,
                           !!!ggplot_dots)  # Pass filtered aesthetics
  } else if (!is.null(group_var) && !is.null(fill)) {
    base <- base + geom_col(data = df, 
                           aes(x = .data[[x]], y = .data[[y]], 
                               group = .data[[group_var]], fill = .data[[group_var]]), 
                           position = position,
                           !!!ggplot_dots)  # Pass filtered aesthetics
  }

  # confidence interval; ribbon \ error bar
  if(!is.null(ci) && is.null(group_var)){

    # continue if arguments for ci and bounds are provided
    ifelse(!is.null(lower) && !is.null(upper),

           # continue if type geom required is error else ribbon
           ifelse(ci == 'e',

                  # Apply error bar with separate legends for line and ci
                  base <- base + ggplot2::geom_errorbar(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]]), linewidth = 1, colour = error_colour)
                    ,

                  # Apply ribbon with separate legends for line and ci
                  base <- base + ggplot2::geom_ribbon(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]], group = 1), fill = error_colour, alpha = .5))

           )

  }

  if(!is.null(ci) && !is.null(group_var)){

    # continue if arguments for ci and bounds are provided
    ifelse(!is.null(lower) && !is.null(upper),

           # continue if type geom required is error else ribbon
           ifelse(ci == 'e',

                  # Apply error bar with separate legends for line and ci
                  base <- base + ggplot2::geom_errorbar(data = df, aes(x = .data[[x]], ymin = .data[[lower]], ymax = .data[[upper]],group = .data[[group_var]], colour = 'ci'), linewidth = 1)
                    ,

                  # Apply ribbon with separate legends for line and ci
                  base <- base +
                          ggplot2::geom_ribbon(data = df, aes(x = .data[[x]],
                                                              ymin = .data[[lower]],
                                                              ymax = .data[[upper]],
                                                              group =.data[[group_var]],  fill = "ci"), alpha = .5))

           )}

  # apply horizontal line if h_line argument exists
  if(!is.null(h_line)){

    base <- base + ggplot2::geom_hline(yintercept = h_line, linetype = "dashed", colour = "black")

  }

  # Theme settings - Update to match age_sex_pyramid style
  base <- base + 
    theme_minimal() +  # Use minimal theme as base
    theme(
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = legend_pos,
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      panel.grid = element_blank(),  # Remove grid lines
      axis.line = element_line(color = "black"),  # Add axis lines in black
      axis.text.x = element_text(angle = x_label_angle, vjust = 0.5)
    )

  # Remove old theme settings that are now handled above

  #append percentage labels
  if(!is.null(percent)){

    base <- base + ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"))

  }

  if(y_axis == "y2") {
    current_y_axis_name <- ggplot_build(base)$layout$panel_params[[1]]$y$name
    base <- base  + scale_y_continuous(name = current_y_axis_name, sec.axis = sec_axis(~scale_function(., scale, shift), name=y_label))
  } else {
  base <- base + ggplot2::labs(y = y_label)
  }

  # Apply x label using arguments provided
  if(!is.null(x_label)){

    base <- base + ggplot2::labs(x = x_label)

  }

  if(!is.null(cap_text)){

    base  <- base  + ggplot2::labs(caption = cap_text)

  }

  if (dynamic) {
    # produce plotly graph if 'dynamic' is set to TRUE
    
    # Extract plotly-specific parameters from the ellipsis
    plotly_params <- list(...)
    
    # Create a new plotly object directly instead of converting from ggplot
    p <- plotly::plot_ly()
    
    # Handle grouped vs non-grouped plotting
    if(is.null(group_var)) {
      # Single group plotting
      p <- plotly::add_bars(p,
                      data = df,
                      x = df[[x]],
                      y = df[[y]],
                      marker = list(color = fill),
                      name = y)
    } else {
      # Multiple group plotting
      unique_groups <- unique(df[[group_var]])
      
      for(i in seq_along(unique_groups)) {
        group_data <- df[df[[group_var]] == unique_groups[i],]
        
        p <- plotly::add_bars(p,
                        data = group_data,
                        x = group_data[[x]],
                        y = group_data[[y]],
                        name = as.character(unique_groups[i]),
                        marker = if(!is.null(fill)) list(color = fill[i]) else list())
      }
    }

    # Apply plotly-specific parameters
    if(length(plotly_params) > 0) {
      # Apply opacity if provided
      if("opacity" %in% names(plotly_params)) {
        p <- plotly::style(p, opacity = plotly_params$opacity)
      }
      
      # Apply hoverlabel if provided
      if("hoverlabel" %in% names(plotly_params)) {
        p <- plotly::style(p, hoverlabel = plotly_params$hoverlabel)
      }
      
      # Apply showlegend if provided
      if("showlegend" %in% names(plotly_params)) {
        p <- plotly::layout(p, showlegend = plotly_params$showlegend)
      }
    }

    # Define fonts
    title_font <- list(
      family = "Arial",
      size = 12,
      color = "black")

    # Add basic layout with standard formatting
    p <- plotly::layout(p,
                  # Title and axis labels
                  xaxis = list(
                    title = list(text = x_label %||% x, font = title_font),
                    showgrid = FALSE,  # Remove grid lines
                    showline = TRUE,   # Show axis line
                    linewidth = 1,     # Axis line width
                    ticks = "outside", # Show ticks
                    ticklen = 3,       # Tick length
                    tickformat = ",",  # Use commas for thousands without 'k' suffix
                    tickfont = list(family = "Arial", size = 10)
                  ),
                  yaxis = list(
                    title = list(text = y_label %||% y, font = title_font),
                    showgrid = FALSE,  # Remove grid lines
                    showline = TRUE,   # Show axis line
                    zeroline = FALSE,  # Don't show zero line
                    linewidth = 1,     # Axis line width
                    ticks = "outside", # Show ticks
                    ticklen = 3,       # Tick length
                    tickformat = ",",  # Use commas for thousands without 'k' suffix
                    tickfont = list(family = "Arial", size = 10)
                  ),
                  # Set margins
                  margin = list(l = 50, r = 50, b = 50, t = 30),
                  # Paper and plot background
                  paper_bgcolor = "white",
                  plot_bgcolor = "white"
    )

    # Return plotly object
    return(p)
  }

  return(base)
}
