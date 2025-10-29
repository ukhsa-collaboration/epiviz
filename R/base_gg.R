#' Creates base ggplot object for use across other functions.
#' Parameters are not passed explicitly to the function, so
#' function call needs to be proceeded by environment(base_gg) <- environment()
#'
#' @return ggplot object
#' @noRd
#'
#' @examples
#' \dontrun{
#' environment(base_gg) <- environment()
#' base <- base_gg()
#' }
#' @keywords internal
base_gg <- function() {


  # Ensure that unused variables exist
  if(!exists("x_time_series")) {x_time_series <- FALSE}
  if(!exists("axis_flip")) {axis_flip <- FALSE}


  # Suppress spurious 'Scale for X is already present' messages
  #   -Will suppress other messages
  suppressMessages({


  ##### Create base ggplot object

  # create base plot
  if (is.null(base)) {
    base <- ggplot()
  } else {
    # Store base chart in unmodified state
    base_chart <- base
  }

  # Add theme if theme argument provided
  if (!is.null(st_theme)) {
    base <- base + st_theme
  } else {
    # If not provided, use the default theme
    base <- base + ggplot2::theme_classic()
  }



  ##### Add chart titles, footers, and axis labels

  # Add title
  if (!is.null(chart_title)) {
    base <- base + ggplot2::labs(title = chart_title) +
      # Add title and axis base styling
      theme(plot.title = element_text(hjust = 0.5,
                                      size = chart_title_size,
                                      colour = chart_title_colour,
                                      family = chart_font,
                                      face = "bold"),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"))
  }

  # Add footer
  if (!(is.null(chart_footer))) {
    base  <- base  + ggplot2::labs(caption = chart_footer) +
      # Add title and axis base styling
      theme(plot.caption = element_text(size = chart_footer_size,
                                        colour = chart_footer_colour,
                                        family = chart_font))

  }

  # Apply x-axis label
  if (!is.null(x_axis_title)) {
    base <- base + labs(x = x_axis_title)
  } else {
    base <- base + labs(x = x) # default to variable name if label not provided
  }

  # Apply y-axis label
  if (!is.null(y_axis_title) & (y_sec_axis == FALSE)) {
    base <- base + labs(y = y_axis_title)
  } else {
    base <- base + labs(y = y) # default to variable name if label not provided
  }



  # Rotate axis text

  # x-axis
  if (!is.null(x_axis_label_angle)) {

    # Anchor middle-right of text box when angle is 1-89 degrees (to match plotly)
    if(x_axis_label_angle >= 1 & x_axis_label_angle <= 89) {
      base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
                                                      vjust = 1, hjust = 1))

    } else if(x_axis_label_angle == 90) {
      # Center justify when y-axis label is vertical
      base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
                                                      vjust = 0.5, hjust = 1))

    } else if(x_axis_label_angle == -90) {
      # Center justify when y-axis label is vertical
      base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
                                                      vjust = 0.5, hjust = 0))

    } else if(abs(x_axis_label_angle) %in% c(270,-270)) {
      # Center justify when y-axis label is vertical
      base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
                                                      vjust = 0.5, hjust = 0))

    } else if(x_axis_label_angle <= -1 & x_axis_label_angle >= -89) {
      # Anchor middle-right of text box when angle is 1-89 degrees (to match plotly)
      base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
                                                      vjust = 1, hjust = 0))

    } else if(x_axis_label_angle >= 181 & x_axis_label_angle <= 269) {
      # Anchor middle-right of text box when angle is 181-359 degrees (to match plotly)
      base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
                                                      vjust = 0, hjust = 0))

    } else if(x_axis_label_angle >= 271 & x_axis_label_angle <= 359) {
      # Anchor middle-right of text box when angle is 181-359 degrees (to match plotly)
      base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
                                                      vjust = 0.5, hjust = 0))

    } else {
      base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
                                                      vjust = 0.5))
    }


    # base <- base + theme(axis.text.x = element_text(angle  = x_axis_label_angle,
    #                                                 vjust = 1,
    #                                                 hjust = 0))
  }



  # y-axis
  if (!is.null(y_axis_label_angle)) {

    # Anchor middle-right of text box when angle is 1-89 degrees (to match plotly)
    if(y_axis_label_angle >= 1 & y_axis_label_angle <= 89) {
      base <- base + theme(axis.text.y = element_text(angle  = y_axis_label_angle,
                                                      vjust = 0.5, hjust = 1))

    } else if(abs(y_axis_label_angle) %in% c(90,270,-90,-270)) {
      # Center justify when y-axis label is vertical
      base <- base + theme(axis.text.y = element_text(angle  = y_axis_label_angle,
                                                      vjust = 0, hjust = 0.5))

    } else if(y_axis_label_angle <= -1 & y_axis_label_angle >= -89) {
      # Anchor middle-right of text box when angle is 1-89 degrees (to match plotly)
      base <- base + theme(axis.text.y = element_text(angle  = y_axis_label_angle,
                                                      vjust = 1, hjust = 1))

    } else if(y_axis_label_angle >= 201 & y_axis_label_angle <= 359) {
      # Anchor middle-right of text box when angle is 201-359 degrees (to match plotly)
      base <- base + theme(axis.text.y = element_text(angle  = y_axis_label_angle,
                                                      vjust = 1, hjust = 1))

    } else {
      base <- base + theme(axis.text.y = element_text(angle  = y_axis_label_angle,
                                                      vjust = 0.5))
    }
  }


  # Axis title font formatting
  if (exists("x_axis_title_font_size")) {
    base <- base + theme(axis.title.x = element_text(size = x_axis_title_font_size))
  }

  if (exists("y_axis_title_font_size")) {
    base <- base + theme(axis.title.y = element_text(size = y_axis_title_font_size))
  }


  # Axis label font formatting
  if (exists("x_axis_label_font_size")) {
    base <- base + theme(axis.text.x = element_text(size = x_axis_label_font_size))
  }

  if (exists("y_axis_label_font_size")) {
    base <- base + theme(axis.text.y = element_text(size = y_axis_label_font_size))
  }



  ##### Grid and axis lines

  ##dev separate options for major / minor, x / y ?

  # Remove or apply grid lines
  if (show_gridlines) {
    base <- base + ggplot2::theme(
      panel.grid.major = element_line(colour = "grey83", linewidth = 0.2),
      panel.grid.minor = element_line(colour = "grey93", linewidth = 0.1)
    )
  } else {
    base <- base + ggplot2::theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
    )

  }

  # Remove or apply axis lines
  if (show_axislines) {
    base <- base + theme(
      axis.line.x = element_line(colour = "black", linewidth = 0.5),
      axis.line.y = element_line(colour = "black", linewidth = 0.5)
    )
  } else {
    base <- base + theme(
      axis.line.x = element_blank(),
      axis.line.y = element_blank()
    )
  }



  ##### Legend formatting
  if(exists("legend_font_size")) {
    base <- base +
      theme(legend.text=element_text(size=legend_font_size))
  }
  if(exists("legend_title_font_size")) {
    base <- base +
      theme(legend.title=element_text(size=legend_title_font_size))
  }



  ##### Define secondary axis

  # Applied separately below

  # If user wants to plot on the secondary y-axis
  if (y_sec_axis == TRUE) {

    # Get limits of current plotted data, use y_limits instead if provided.
    current_plotted_data_max <-
      if (!is.null(y_limit_max)) {y_limit_max} else {max(layer_scales(base_chart)$y$range$range)}
    current_plotted_data_min <-
      if (!is.null(y_limit_min)) {y_limit_min} else {min(layer_scales(base_chart)$y$range$range)}

    # Get limits of new data to plot
    y2_max <- max(df[[y]], na.rm=T)
    y2_min <- min(df[[y]], na.rm=T)

    # If y_sec_axis_percent_full = TRUE, then reset y2 limits
    #   to 0 and 1 for a full 0-100% scale
    if(y_sec_axis_percent_full == TRUE) {
      y2_max <- 1
      y2_min <- 0
    }

    # Get current y1 axis name from base_chart
    current_y1_title <- base_chart$labels$y


    # If no secondary y data has been plotted yet
    if ((is.null(base$secondary_y_shift) | base$secondary_y_shift == 0) &
        is.null(base$secondary_y_scale)) {

      if (is.finite(current_plotted_data_max)) {
        # If data has already been plotted on y1;
        #   Scale and shift variables calculated based on desired mins and maxes
        #scale = (y2_max - y2_min) / (current_plotted_data_max - current_plotted_data_min)
        scale = y2_max / current_plotted_data_max
        shift = current_plotted_data_min - y2_min
        # Add variables to chart "metadata"
        base$secondary_y_shift <- shift
        base$secondary_y_scale <- scale


      } else {
        # If data hasn't already been plotted on y1;
        #   Just plot data as normal but on y2
        current_y_axis_name <- NULL
        scale <- 1
        shift <- 0
      }

      # If secondary y data has already been plotted
    } else {
      shift <- base$secondary_y_shift
      scale <- base$secondary_y_scale
    }

    # User does not want to plot on secondary axis
  } else {
    scale <- 1
    shift <- 0
  }

  # Force shift to 0 if y_sec_axis_no_shift == TRUE
  if (y_sec_axis_no_shift == TRUE) {
    shift <- 0
    base$secondary_y_shift <- 0
  }

  # Rescale values that will be plotted on the scaled secondary y axis (if they've been supplied)
          # utils/inv_scale_function no longer used
  if (!is.null(y)) {
    #df[[y]] <- inv_scale_function(df[[y]], scale, shift)
    df[[y]] <- (df[[y]] + shift) / scale
  }

  if (!is.null(ci_lower)) {
    #df[[ci_lower]] <- inv_scale_function(df[[ci_lower]], scale, shift)
    df[[ci_lower]] <- (df[[ci_lower]] + shift) / scale
  }

  if (!is.null(ci_upper)) {
    #df[[ci_upper]] <- inv_scale_function(df[[ci_upper]], scale, shift)
    df[[ci_upper]] <- (df[[ci_upper]] + shift) / scale
  }

  if (!is.null(hline)) {
    #hline <- inv_scale_function(hline, scale, shift)
    hline <- (hline + shift) / scale
  }




  ##### Stop if 'base' x-axis is reversed but x_axis_reverse = FALSE

  # Axis direction of 'base' and over-drawn plot must be the same

  # If statement identifies existence of base plot;
  #   -If base plot not present then axis limits = NULL (if base plot is present without
  #    axis limits then limits = (NA, NA) ).
  if (!is.null(base$coordinates$limits$x)) {

    # Gather axis range of 'base'
    base_x_min <- abs((ggplot_build(base)$layout$panel_scales_x[[1]]$range$range)[1]) #(base$coordinates$limits$x)[1]
    base_x_max <- abs((ggplot_build(base)$layout$panel_scales_x[[1]]$range$range)[2]) #(base$coordinates$limits$x)[2]

    # If base x-axis minimum range greater than maximum, and x_axis_reverse = FALSE, then stop
    if ((base_x_min > base_x_max) & (x_axis_reverse == FALSE)) {
        stop("If the x-axis of the base plot is reversed, then x_axis_reverse must equal 'TRUE'
              (i.e. the x-axis of the base plot and over-drawn plot must be in the same direction)")
    }

  }



  ##### Apply axis breaks to factored x-axis

  if (is.factor(df[[x]])) {
    base <- base + scale_x_discrete(limits = levels(df[[x]]))
  }


  ##### Apply user-specified axis breaks if provided

  # Apply specified axis_break_labels

  if (!is.null(x_axis_break_labels)) {
    # handle factor axis
    if (!is.factor(df[[x]])) {
      base <- base + scale_x_continuous(breaks = x_axis_break_labels)
    } else {
      base <- base + scale_x_discrete(breaks = x_axis_break_labels)
    }
  }

  if (!is.null(y_axis_break_labels)) {
    base <- base + scale_y_continuous(breaks = y_axis_break_labels)
  }


  # Apply specified axis_n_breaks

  if (!is.null(x_axis_n_breaks)) {
    # n.breaks will not work for dates, ignore arguement and issue warning
    if (lubridate::is.Date(df[[x]])) {
      warning("x_axis_n_breaks will be ignored if x is a date variable, consider using
              x_axis_date_breaks or x_axis_break_labels instead")
    } else {
      base <- base + scale_x_continuous(n.breaks = x_axis_n_breaks)
    }
  }

  if (!is.null(y_axis_n_breaks)) {
    base <- base + scale_y_continuous(n.breaks = y_axis_n_breaks)
  }


  # Apply x-axis date intervals

  if (!is.null(x_axis_date_breaks)) {
    # Ignore arguement and issue warning if x is not a date variable
    if (!lubridate::is.Date(df[[x]])) {
      warning("x is not a date variable, x_axis_date_breaks will be ignored")
    } else {
      base <- base + scale_x_date(date_breaks = x_axis_date_breaks)
    }
  }




  ##### Reverse axes

  # Note: Done at this stage as it alters x/y limits
      # https://github.com/tidyverse/ggplot2/issues/4014

  # x-axis
  if (x_axis_reverse == TRUE) {

    # Limits need to be inverted if axis is reversed

    # 1) Obtain limits from base plot if available
    if (!is.null((base$coordinates$limits$x)[2])) {x_limit_max <- (base$coordinates$limits$x)[2]}
    if (!is.null((base$coordinates$limits$x)[1])) {x_limit_min <- (base$coordinates$limits$x)[1]}

    # 2) Supercede with newly specified limits if provided
    if (!is.null(params$x_limit_max)) {x_limit_max <- params$x_limit_max}
    if (!is.null(params$x_limit_min)) {x_limit_min <- params$x_limit_min}

    # 3) Invert these limits
    x_limit_max_start <- x_limit_max  # Redefine starting vars here else the inversion below
    x_limit_min_start <- x_limit_min  #    will pick up the inverted var and duplicate

    x_limit_max <- x_limit_min_start
    x_limit_min <- x_limit_max_start

    # 4) Convert to dates if needed
    x_limit_max <- if (lubridate::is.Date(df[[x]]) & !is.null(x_limit_max)) {as.Date(x_limit_max)} else {x_limit_max}
    x_limit_min <- if (lubridate::is.Date(df[[x]]) & !is.null(x_limit_min)) {as.Date(x_limit_min)} else {x_limit_min}


    # Apply axis reversal
    suppressMessages(
      if (lubridate::is.Date(df[[x]])) {
        # Handle date axes
          if (!is.null(x_axis_break_labels)) { # take into account x_axis_break_labels if provided
            base <- base + scale_x_continuous(trans = c("date", "reverse"), breaks = x_axis_break_labels)
          } else {
            base <- base + scale_x_continuous(trans = c("date", "reverse"))
          }
      } else if (is.factor(df[[x]])) {
        # Handle discrete / categorical axes (i.e. factors)
        base <- base + scale_x_discrete(limits = rev(levels(df[[x]])))
      } else {
        base <- base + scale_x_reverse()
      }
    )

  }




  ##### Apply axis limits

  # Build ylim and xlim vars for coord_cartesian

  # y lower limit only
  if (!is.null(y_limit_min) & is.null(y_limit_max)) {
    ylim <- c(y_limit_min, NA)
    # y upper limit only
  } else if (is.null(y_limit_min) & !is.null(y_limit_max)) {
    ylim <- c(NA, y_limit_max)
    # y upper and lower limits
  } else if (!is.null(y_limit_min) & !is.null(y_limit_max)) {
    ylim <- c(y_limit_min, y_limit_max)
  } else {
    ylim <- c(NA,NA)
  }

  # x lower limit only
  if (!is.null(x_limit_min) & is.null(x_limit_max)) {
    xlim <- c(x_limit_min, NA)
    # x upper limit only
  } else if (is.null(x_limit_min) & !is.null(x_limit_max)) {
    xlim <- c(NA, x_limit_max)
    # x upper and lower limits
  } else if (!is.null(x_limit_min) & !is.null(x_limit_max)) {
    xlim <- c(x_limit_min, x_limit_max)
  } else {
    xlim <- c(NA,NA)
  }


  # Convert any date axis limits to dates if necessary
  if (lubridate::is.Date(df[[y]]) & !is.null(ylim)) {ylim <- as.Date(ylim)}
  if (lubridate::is.Date(df[[x]]) & !is.null(xlim)) {xlim <- as.Date(xlim)}


  # Handle base_gg() being called inside epi_curve() & col_chart where the x-limits are redefined
  if (substr(deparse(sys.calls()[[sys.nframe()-1]]),1,9)[1] == "epi_curve" |
        (substr(deparse(sys.calls()[[sys.nframe()-1]]),1,9)[1] == "col_chart" & x_time_series == TRUE)) {
    xlim <- c(NA,NA)
  }
  # In epi_curve(), and in col_chart() where x_time_series = TRUE, a full time series is
  #    defined from date_start/x_limit_min to date_end/x_limit_min and the full x-axis
  #    plotted from this, thus meaning that xlim must be set to c(NA,NA) so that the
  #    whole axis is plotted. Where x_time_series = FALSE conventional
  #    limits can be defined, and thus xlim does not need to be reset to c(NA,NA).


  # Apply axis limits to base plot
  if (!is.null(ylim) & is.null(xlim)) {
    base <- base + coord_cartesian(ylim = ylim)
  } else if (is.null(ylim) & !is.null(xlim)) {
    base <- base + coord_cartesian(xlim = xlim)
  } else if (!is.null(ylim) & !is.null(xlim)) {
    base <- base + coord_cartesian(xlim = xlim, ylim = ylim)
  } else {
    base <- base
  }




  ##### Apply hlines

  # if no hline, return hline_xpos = NULL for function export list at end
  if (is.null(hline) | is.null(hline_label)) {hline_xpos <- NULL}

  # adds horizontal line at the y value specified for hline
  if (!is.null(hline)) {
    base <-
      base + geom_hline(yintercept = hline,
                        colour = hline_colour,
                        linewidth = hline_width,
                        linetype = hline_type)
  }

  # If specified, add label to the start of the horizontal line
  if (!is.null(hline) && !is.null(hline_label)) {

    # Define x-position of hline label
    # If lower x-axis limit present use that
    if (!is.na(base$coordinates$limits$x[[1]])) {
      hline_xpos <- base$coordinates$limits$x[[1]]
    } else {
      # else use min value of x, or max if values reversed
      if (!is.factor(df[[x]])) {
        hline_xpos <- if (x_axis_reverse == TRUE) {max(df[[x]])} else {min(df[[x]])}
      } else {
        # handle factor x-axes
        hline_xpos <- if (x_axis_reverse == TRUE) {last(df[[x]])} else {first(df[[x]])}
      }
    }

    # Account for flipped axes
    if (axis_flip == TRUE) {
      # If upper x-axis limit present use that
      if (!is.na(base$coordinates$limits$x[[2]])) {
        hline_xpos <- base$coordinates$limits$x[[2]]
      } else {
        # else use max value of x, or min if values reversed
        if (!is.factor(df[[x]])) {
          hline_xpos <- if (x_axis_reverse == TRUE) {min(df[[x]])} else {max(df[[x]])}
        } else {
          # handle factor x-axes
          hline_xpos <- if (x_axis_reverse == TRUE) {first(df[[x]])} else {last(df[[x]])}
        }
      }
    }


    # Apply hline label to plot
    base <- base +
      geom_text(
        aes(
          x = hline_xpos,
          y = hline,
          label = hline_label,
          vjust = -0.5,
          hjust = 0
        ),
        colour = hline_label_colour)

  }





  ##### Change y-axis to percentage scale

  if (y_percent == TRUE) {
    suppressMessages(
      base <-
        base + scale_y_continuous(
          ##labels = function(x) paste0(x, "%")
          labels = scales::label_percent()
        )
    )
  }



  ##### Apply secondary axis

  if (y_sec_axis == TRUE) {
    # apply percentage scale if invoked
    if (y_percent == FALSE) {
      base <- base +
        ylab(current_y1_title) +
        # scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift),
        #                                        name = y_axis_title))
        scale_y_continuous(sec.axis = sec_axis(~ . * scale + shift,
                                               name = y_axis_title))
    } else {
      base <- base +
        ylab(current_y1_title) +
        # scale_y_continuous(sec.axis = sec_axis(~scale_function(., scale, shift),
        #                                        name = y_axis_title,
        #                                        labels = scales::label_percent()))
        scale_y_continuous(sec.axis = sec_axis(~ . * scale + shift,
                                               name = y_axis_title,
                                               labels = scales::label_percent()))
    }
  }

  }) # SUPPRESS MESSAGES END


  ##### Return df, base, and hline_xpos as list
  #       (df$y may have been modified through sec axis scaling)
  #       (hline_xpos required in case hline needs to be reapplied over other plots)

  return_list <- list("base" = clean_gg_labels(base), # use utils/clean_gg_labels() to prevent 'Ignoring unknown labels:' messages
                      "df" = df,
                      "hline_xpos" = hline_xpos,
                      "xlim" = xlim,
                      "ylim" = ylim)

  return(return_list)

  #return(base)

}

