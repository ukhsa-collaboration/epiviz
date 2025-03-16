# test to check if the function returns a ggplot object

test_that("age_sex_pyramid works", {

  library(epiviz)



  # check that the output is a ggplot object
  expect_true(inherits(
    age_sex_pyramid(dynamic=FALSE,
                    params = list(
                      df=epiviz::lab_data,
                      var_map = list(
                        date_of_birth = 'date_of_birth',
                        sex = 'sex'
                      ),
                      grouped = FALSE,
                      colours = c("pink", "blue"))
    )
    , "ggplot"))

  expect_true(inherits(
    age_sex_pyramid(dynamic=FALSE,
                    params = list(
                      df=epiviz::lab_data,
                      var_map = list(
                        date_of_birth = 'date_of_birth',
                        sex = 'sex'
                      ),
                      grouped = FALSE,
                      colours = c("pink", "blue"),
                      x_breaks = 5)
    )
    , "ggplot"))

  expect_true(inherits(
    age_sex_pyramid(dynamic=FALSE,
                    params = list(
                      df=epiviz::lab_data,
                      var_map = list(
                        date_of_birth = 'date_of_birth',
                        sex = 'sex'
                      ),
                      grouped = FALSE,
                      colours = c("pink", "blue"),
                      x_breaks = 5,
                      conf_limits = TRUE)
    )
    , "ggplot"))


})

test_that("age_sex_pyramid works with dynamic output", {
  library(epiviz)
  
  # Test basic plotly output
  p <- age_sex_pyramid(
    dynamic=TRUE,
    params = list(
      df=epiviz::lab_data,
      var_map = list(
        date_of_birth = 'date_of_birth',
        sex = 'sex'
      ),
      grouped = FALSE,
      colours = c("pink", "blue")
    )
  )
  expect_true(inherits(p, "plotly"))
  
  # Test with x_breaks
  p <- age_sex_pyramid(
    dynamic=TRUE,
    params = list(
      df=epiviz::lab_data,
      var_map = list(
        date_of_birth = 'date_of_birth',
        sex = 'sex'
      ),
      grouped = FALSE,
      colours = c("pink", "blue"),
      x_breaks = 5
    )
  )
  expect_true(inherits(p, "plotly"))
  
  # Test with confidence limits
  p <- age_sex_pyramid(
    dynamic=TRUE,
    params = list(
      df=epiviz::lab_data,
      var_map = list(
        date_of_birth = 'date_of_birth',
        sex = 'sex'
      ),
      grouped = FALSE,
      colours = c("pink", "blue"),
      x_breaks = 5,
      conf_limits = TRUE
    )
  )
  expect_true(inherits(p, "plotly"))
  
  # Test with pre-grouped data
  grouped_df <- data.frame(
    age_group = rep(c("0-4", "5-18", "19-64", "65+"), 2),
    sex = rep(c("Male", "Female"), each = 4),
    value = c(100, 120, 150, 80, 90, 110, 140, 70),
    lowercl = c(90, 110, 140, 70, 80, 100, 130, 60),
    uppercl = c(110, 130, 160, 90, 100, 120, 150, 80)
  )
  
  p <- age_sex_pyramid(
    dynamic=TRUE,
    params = list(
      df = grouped_df,
      var_map = list(
        age_group = 'age_group', 
        sex = 'sex', 
        value = 'value',
        lowercl = 'lowercl',
        uppercl = 'uppercl'
      ),
      grouped = TRUE,
      conf_limits = TRUE
    )
  )
  expect_true(inherits(p, "plotly"))
  
  # Test with pre-grouped data without confidence limits
  p_no_ci <- age_sex_pyramid(
    dynamic=TRUE,
    params = list(
      df = grouped_df,
      var_map = list(age_group = 'age_group', sex = 'sex', value = 'value'),
      grouped = TRUE,
      conf_limits = FALSE
    )
  )
  expect_true(inherits(p_no_ci, "plotly"))
})


