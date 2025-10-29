#' Age Sex Pyramid using grouped data
#'
#' @noRd
#'
#' @param df Data frame which has the following columns with names given below
#'              age_group: char type>> age group can be given as ranges (e.g. "0-4","5-18", "19-64", "65+")
#'              sex: char type>> must be coded as "Female" and "Male"
#'              value: num type>> numerical value used for pyramid (can be proportion or cases)
#'              Optional (set ci parameter to 'errorbar' if error bars are required):
#'                ci_lower: num type>> lower confidence limits for value
#'                ci_upper: num type>> upper confidence limits for value
#'
#' @param mf_colours List of colours for the genders in HEX format in the order Female, Male
#'                Default values for parameter: c("#003B5C", "#007C91")
#' @param x_breaks Number of ticks on X axis (default is 20)
#' @param y_axis_title  Title that  appears on the X axis in as a string
#' @param text_size Text size can be modified by providing this number (default set to 15)
#' @param ci Character variable (set to 'errorbar' if error bars are required).
#'                    If set to 'errorbar' the data frame should include the ci_lower and ci_upper columns
#'
#' @return Returns an age-sex pyramid as a ggplot object
#' @keywords internal
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'  age_group = rep(c("0-4", "5-18", "19-64", "65+"), each = 2),
#'  sex = rep(c("Female", "Male"), times = 4),
#'  value = c(110, 90, 80, 70, 60, 50, 40, 30)
#' )
#'
#' agesex_pyramid_grouped(data, x_breaks=10)
#' }
agesex_pyramid_grouped <- function(df,
                                   mf_colours,
                                   ci_colour,
                                   x_breaks,
                                   y_axis_title,
                                   text_size,
                                   ci)
{
  ##### Data wrangling before plotting the chart


  # # Convert Female value to negative to flip columns
  # df <- df |>
  #   dplyr::mutate(value = ifelse(sex == "Female",-value, value)) |>

 # Convert Male value to negative to flip columns
 df <- df |>
   dplyr::mutate(value = ifelse(sex == "Male",-value, value)) |>

    #order age bands
    # Remove "<" and "+" from agebands, then take strings starting with one or more digits and arrange them
    arrange(as.integer(sub("^(\\d+).*", "\\1", sub("[<+]", "", age_group)))) |>
    # Use the unique age groups in their original order (not reversed)
    mutate(age_group = factor(age_group, levels = unique(df$age_group))) |>
    na.omit()


  # # Convert Female confidence limits to negative to flip columns
  # if (ci == 'errorbar') {
  #   df <- df |>
  #     mutate(ci_lower = ifelse(sex == "Female",-ci_lower, ci_lower),
  #            ci_upper = ifelse(sex == "Female",-ci_upper, ci_upper))
  # }

 # Convert Male confidence limits to negative to flip columns
 if (ci == 'errorbar') {
   df <- df |>
     mutate(ci_lower = ifelse(sex == "Male",-ci_lower, ci_lower),
            ci_upper = ifelse(sex == "Male",-ci_upper, ci_upper))
 }

  # Set value axis limits (will be different when errorbars are added)
  if (ci == 'errorbar') {
    # val_min <- min((df |> filter(sex == 'Female'))$ci_upper)
    # val_max <- max((df |> filter(sex == 'Male'))$ci_upper)
    val_min <- min((df |> filter(sex == 'Male'))$ci_upper)
    val_max <- max((df |> filter(sex == 'Female'))$ci_upper)
  } else {
    val_min <- min(df$value)
    val_max <- max(df$value)
  }



  ##### Plotting the chart
  plot <-
    df |>
    ggplot() +
    # Switch X and Y axis of graph
    coord_flip() +
    # base column chart
    geom_col(aes(x = age_group, fill = sex, y = value), colour = "black") +
    {
      # add confidence limits
      if (ci == 'errorbar')
        geom_errorbar(
          aes(x = age_group, ymin = ci_lower, ymax = ci_upper),
          colour = ci_colour,
          width = 0.5,
          linewidth = 0.75
        )
    } +
    # set axis options
    scale_y_continuous(labels = abs,
                       #limits = c(min(df$value), max(df$value)),
                       limits = c(val_min, val_max),
                       expand = expansion(mult = 0.15),
                       n.breaks = x_breaks) +
    scale_x_discrete(expand = expansion(add = c(1, 1)), drop = FALSE, limits=rev) +
    scale_fill_manual(values = mf_colours, drop = FALSE) +
    theme_minimal() +
    # customisations to minimal theme
    theme(
      panel.grid = element_blank(),
      axis.title.y = element_text(family="Arial", margin = margin(0, 20, 0, 0, "mm"), face="bold"),
      axis.title.x = element_text(face="bold"),
      legend.position = "bottom",
      legend.title = element_blank(),
      text = element_text(size = text_size, family="Arial"),
      plot.title = element_text(hjust = 0.5, family="Arial"),
      plot.margin = margin(1, 1.2, 1, 1.2, "cm"),
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black")

    ) +
    # No X label; only Y label  (which can be passed as an fn argument)
    labs(x = NULL, y = y_axis_title, fill = NULL)

  return(plot)
}
