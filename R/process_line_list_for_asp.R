#' Process Line List for Age-Sex Pyramid
#'
#' This function processes a line list data frame, calculating the age from either the provided age column or date of birth.
#' It filters the data to include only males and females, groups the data into age groups, and summarizes the counts for each age group and sex.
#'
#' @noRd
#'
#' @param df A data frame containing the line list data.
#' @param var_map A list mapping variable names in the data frame to the expected names used in the function.
#' The list should contain elements:
#' \describe{
#'   \item{age_var}{Name of the variable in df containing age values (default is `'age'`).}
#'   \item{dob_var}{Name of the variable in df containing date of birth values (default is `'date_of_birth'`).}
#'   \item{sex_var}{Name of the variable in df containing sex values (default is `'sex'`).
#'   Permitted values for sex include M, F, Male, Female (not case sensitive)}
#' }
#' @param age_breakpoints A numeric vector specifying the breakpoints for age groups. The default is `c(0, 5, 19, 65, Inf)`.
#' @param age_calc_refdate A Date object specifying the reference date for calculating age from date of birth.
#' The default is `Sys.Date()`.
#'
#' @return A data frame that is aggregated by age group and sex, with columns for age group, sex, value (count),
#' lower confidence limit, and upper confidence limit.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   df <- epiviz::lab_data
#'   processed_df <- process_line_list_for_age_sex_pyramid(df)
#' }
process_line_list_for_age_sex_pyramid <- function(df,
                                                  var_map = list(age_var = 'age',
                                                                 dob_var = 'date_of_birth',
                                                                 sex_var = 'sex'),
                                                  age_breakpoints = c(0, 5, 19, 65, Inf),
                                                  age_calc_refdate = Sys.Date()) {
  # Ensure df is a data frame
  if (!is.data.frame(df)) {
    stop("The input df must be a data frame.")
  }

  # Ensure that either 'age_var' or 'dob_var' is provided
  if (is.null(var_map$age_var) & is.null(var_map$dob_var)) {
    stop("Either 'age_var' or 'dob_var' must be provided in the var_map list.")
  }

  if (is.null(var_map$sex_var)) {
    stop("'sex_var' must be provided in the var_map list.")
  }


  # Handle age calculation if 'age' is not provided
  if (!is.null(var_map$age_var) && var_map$age_var %in% names(df)) {
    df$age_var <- df[[var_map$age_var]]
  } else if (!is.null(var_map$dob_var) &&
             var_map$dob_var %in% names(df)) {
    df$age_var <- round(as.numeric(difftime(age_calc_refdate, df[[var_map$dob_var]], units = "days")) / 365.25)
  } else {
    stop("The specified 'age_var' or 'dob_var' variable was not found in the data frame.")
  }

  # Standardise the 'sex_var' variable
  df <- df |>
    mutate(!!var_map$sex_var := case_when(
      str_detect(get(var_map$sex_var), regex("^(M|Male)$", ignore_case = TRUE)) ~ "Male",
      str_detect(get(var_map$sex_var), regex("^(F|Female)$", ignore_case = TRUE)) ~ "Female",
      TRUE ~ NA_character_
    ))

  # Filter out rows where 'sex_var' column is not "Male" or "Female"
  df <- df[df[[var_map$sex_var]] %in% c("Male", "Female"), ]

  # Convert numeric age breakpoints to corresponding age group labels
  age_labels <- c()
  for (i in 1:(length(age_breakpoints) - 2)) {
    age_labels[i] <- paste0(age_breakpoints[i], "-", age_breakpoints[i + 1] - 1)
  }
  # Create the last age group label based on the last numeric breakpoint
  age_labels[length(age_labels) + 1] <- paste0(age_breakpoints[length(age_breakpoints) - 1], "+")

  # Create age groups
  df$age_group <- cut(df$age_var,
                      breaks = age_breakpoints,
                      labels = age_labels,
                      right = FALSE) # right = FALSE ensures that age_group includes the lower bound and excludes the upper bound

  # Remove rows with NA in the age_group column
  df <- df |> filter(!is.na(age_group))


  # Aggregate data by age group and sex
  df <- df |>
    dplyr::group_by(age_group, sex = df[[var_map$sex_var]]) |>
    dplyr::summarise(
      value = n(),
      ci_lower = -1.96 * sqrt(value),
      ci_upper =  1.96 * sqrt(value),
      .groups = "drop"
    ) |>
    ungroup() |>
    # Change +/- upper and lower limits into true upper and lower values needed for plotting
    mutate(ci_lower = value + ci_lower,
           ci_upper = value + ci_upper)

  # Return the processed data frame
  return(df)
}
