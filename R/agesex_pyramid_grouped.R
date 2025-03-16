#' Age Sex Pyramid using grouped data
#'
#' @param df Data frame which has the following columns with names given below
#' \describe{
#'   \item{age_group}{Character type. Age group can be given as ranges (e.g. "0-4","5-18", "19-64", "65+")}
#'   \item{sex}{Character type. Must be coded as "Female" and "Male"}
#'   \item{value}{Numeric type. Numerical value used for pyramid (can be proportion or cases)}
#'   \item{lowercl}{Optional. Numeric type. Lower confidence limits for value (required if conf_limits=TRUE)}
#'   \item{uppercl}{Optional. Numeric type. Upper confidence limits for value (required if conf_limits=TRUE)}
#' }
#'
#' @param colours List of colours for the genders in HEX format in the order Female, Male.
#'                Default values: c("#003B5C", "#007C91")
#' @param x_breaks Number of ticks on X axis. Default is 20.
#' @param y_title Title that appears on the Y axis as a string.
#' @param text_size Text size can be modified by providing this number. Default is 15.
#' @param conf_limits Boolean variable. If TRUE, error bars are displayed.
#'                    If TRUE, the data frame should include the lowercl and uppercl columns.
#'
#' @return Returns an age-sex pyramid as a ggplot object
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
                                   colours,
                                   x_breaks,
                                   y_title,
                                   text_size,
                                   conf_limits)
{
  ##### Data wrangling before plotting the chart


  # Convert Female value to negative to flip columns
  df <- df |>
    dplyr::mutate(value = ifelse(sex == "Female",-value, value)) |>

    #order age bands
    # Remove "<" and "+" from agebands, then take strings starting with one or more digits and arrange them
    arrange(as.integer(sub("^(\\d+).*", "\\1", sub("[<+]", "", age_group)))) |>
    # Use the unique age groups in their original order (not reversed)
    mutate(age_group = factor(age_group, levels = unique(df$age_group))) |>
    na.omit()


  # Convert Female confidence limits to negative to flip columns
  if (conf_limits) {
    df <- df |>
      dplyr::mutate(lowercl = ifelse(sex == "Female",-lowercl, lowercl))  |>
      dplyr::mutate(uppercl = ifelse(sex == "Female",-uppercl, uppercl))
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
      if (conf_limits == TRUE)
        geom_errorbar(
          aes(x = age_group, ymin = lowercl, ymax = uppercl),
          colour = "black",
          width = 0.5,
          linewidth = 0.75
        )
    } +
    # set axis options
    scale_y_continuous(labels = abs,
                       limits = c(min(df$value), max(df$value)),
                       expand = expansion(mult = 0.15),
                       n.breaks = x_breaks) +
    scale_x_discrete(expand = expansion(add = c(1, 1)), drop = FALSE) +
    scale_fill_manual(values = colours, drop = FALSE) +
    theme_minimal() +
    # customisations to minimal theme
    theme(
      panel.grid = element_blank(),
      axis.title.y = element_text(family="Arial",margin = unit(c(0, 20, 0, 0), "mm"), face="bold"),
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
    labs(x = NULL, y = y_title, fill = NULL)

  return(plot)
}
