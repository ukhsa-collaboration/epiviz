
# EXP_col_chart() reprex 1


library(devtools)
devtools::load_all()

detections_monthly2 <- lab_data |>
  group_by(specimen_month = lubridate::floor_date(specimen_date, 'month'), organism_species_name) |>
  summarise(detections = n()) |>
  ungroup() |>
  mutate(detections = round(detections/5)) |>
  rowwise() |>
  mutate(lower_limit = detections - sample(1:10,1),
         upper_limit = detections + sample(1:10,1)) |>
  ungroup()


params2 <- list(
  df = detections_monthly2,
  x = "specimen_month",
  y = "detections",
  x_limit_min = "2022-01-01",
  x_limit_max = "2022-12-31",
  x_axis_break_labels = seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by = "month"),
  x_time_series = TRUE,
  group_var = "organism_species_name",
  line_colours = c("KLEBSIELLA PNEUMONIAE" = "#007C92", "STAPHYLOCOCCUS AUREUS" = "#8A2B62", "PSEUDOMONAS AERUGINOSA" = "#FF7F32"),
  ci = "errorbar",
  ci_upper = "upper_limit",
  ci_lower = "lower_limit",
  ci_legend = TRUE,
  ci_legend_title = "Confidence interval",
  ci_colours = c("red", "red", "red"),
  errorbar_width = NULL,
  chart_title = "chart title test \nTEST Test",
  chart_footer = "chart footer test \nTEST Test",
  legend_title = "legend test title",
  legend_pos = "right",
  show_gridlines = TRUE,
  y_axis_title = "Number of detections",
  x_axis_label_angle = -45,
  y_axis_label_angle = -270,
  x_axis_title_font_size = 25,
  y_axis_title_font_size = 25,
  chart_title_colour = "#007C92",
  chart_footer_colour = "#007C92"
)

static <- EXP_line_chart(params = params2, dynamic = FALSE)
dynamic <- EXP_line_chart(params = params2, dynamic = TRUE)


### ISSUES:
# 1) The static chart ignores the values in ci_colours and uses the values in line_colours.
# 2) When only a single element vector is provided for ci_colours the dynamic chart fails.
#      Instead, when a single colour is provided, all error bars should be that colour
#      regardless of group.

