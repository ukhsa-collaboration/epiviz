## This file prevents a R CMD Check Note about
## no visible binding for global variable 'dfr'
## Undefined global functions or variables

utils::globalVariables(c(
  # For agesex_pyramid_grouped
  "na.omit", "sex", "value", "age_group", "lowercl", "uppercl",

  # For agesex_pyramid_linelist
  "sex", "na.omit", "age_group", "value",

  # For base_gg
  "st_theme", "chart_title", "chart_title_size", "chart_title_colour",
  "chart_font", "chart_footer", "chart_footer_size", "chart_footer_colour",
  "x_axis_title", "x", "y_axis_title", "y_sec_axis", "y",
  "x_axis_label_angle", "y_axis_label_angle", "show_gridlines",
  "show_axislines", "y_limit_max", "y_limit_min", "y_sec_axis_percent_full",
  "y_sec_axis_no_shift", "ci_lower", "ci_upper", "x_axis_reverse",
  "x_axis_break_labels", "y_axis_break_labels", "x_axis_n_breaks",
  "y_axis_n_breaks", "x_axis_date_breaks", "params", "hline_colour",
  "hline_width", "hline_type", "hline_label", "hline_label_colour",
  "y_percent",

  # For base_plotly
  "chart_font", "chart_title_size", "chart_title_colour", "chart_footer_size",
  "chart_footer_colour", "x_axis_label_angle", "x_axis_title", "x",
  "y_sec_axis", "y_axis_title", "y", "y_axis_label_angle", "show_gridlines",
  "show_axislines", "y_percent", "x_limit_min", "x_limit_max", "y_limit_min",
  "y_limit_max", "df", "x_axis_reverse", "x_axis_break_labels",
  "x_axis_date_breaks", "y_axis_break_labels", "x_axis_n_breaks",
  "y_axis_n_breaks", "y_sec_axis_percent_full", "hline", "hline_colour",
  "hline_type", "hline_width", "hline_label",

  # For epi_map
  "inc_shp", "data_areacode", "shp_name", "shp_areacode", "value_col",
  "area_labels", "area_labels_topn", "Value", "Area", "break_intervals",
  "value_cat", "break_labels", "fill_palette", "border_shape_name",
  "border_code_col", "border_areaname", "fill_opacity", "map_title",
  "map_footer", "chart_font", "centroid_long", "centroid_lat",
  "labels_static", "map_zoom", "map_title_size", "map_title_colour",
  "map_footer_size", "map_footer_colour", "legend_pos",

  # For line_chart
  "df", "x", "y", "group_var", "line_type", "st_theme", "line_colour",
  "width", "legend_title", "legend_position", "title", "x_label", "y_label",
  "x_label_angle", "y_label_angle", "show_gridlines", "show_axislines",
  "hline", "hline_colour", "hline_label", "y_percent", "add_points",
  "ci", "lower", "upper", "error_colour",

  # For point_chart
  "ci", "ci_lower", "ci_upper", "x", "ci_legend", "group_var",
  "ci_legend_title", "ci_colours", "point_size", "y", "point_colours",
  "point_shape", "legend_title", "legend_pos", "point_size_legend_title",
  "point_size_legend", "point_labels", "point_labels_size",
  "point_labels_hjust", "point_labels_vjust", "point_labels_nudge_x",
  "point_labels_nudge_y", "hline", "setNames", "y_percent",


  # For epi_curve
  "iso_year", "iso_week", "x_axis_title_font_size", "y_axis_title_font_size",
  "x_axis_label_font_size", "y_axis_label_font_size", "legend_font_size",
  "legend_title_font_size", "time_period", "date_factor", "x_limit",
  "rolling_average_line", "rolling_average_line_lookback", "cumulative_sum_line",
  "bar_border_colour", "case_boxes", "box", "case_boxes_colour",
  "rolling_average", "rolling_average_line_legend_label",
  "rolling_average_line_width", "cumulative_sum",
  "cumulative_sum_line_legend_label", "cumulative_sum_line_width",
  "cumulative_sum_line_axis_title", "rolling_average_line_colour",
  "cumulative_sum_line_colour", "hover_labels"
))
