# epiviz <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/harshanal/epiviz/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/harshanal/epiviz/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`epiviz` provides easy-to-use data visualisation functions for R data science products. The package includes functions to create a range of static and interactive visualisations. Each visualisation function can produce either a static (ggplot2) or dynamic (plotly/leaflet) output using the same set of parameters, allowing users to easily switch between visualisation types by simply changing the `dynamic` flag.

## Installation

epiviz can be installed from GitHub using the following code:
```r
# Install epiviz
remotes::install_github('harshanal/epiviz')

# To install the development version
remotes::install_github("harshanal/epiviz@dev")
``` 

## Features

The following visualisation functions are available in the current release of epiviz:

- `line_chart()`: Creates line charts with options for grouping, dual axes, threshold lines, and confidence limits (as ribbons or error bars).
- `point_chart()`: Creates scatter/point charts with options for grouping, sizing points by values, custom shapes, and confidence intervals.
- `col_chart()`: Creates column charts with options for grouping, dual axes, threshold lines, and confidence limits (as ribbons or error bars).
- `epi_curve()`: Creates epidemic curves with options for different time periods, rolling averages, cumulative sums, and grouping by categories.
- `epi_map()`: Creates choropleth maps with customisable colour scales, labels, and boundaries for different administrative levels.
- `age_sex_pyramid()`: Creates age-sex pyramids with customisable age bands and confidence intervals.

All visualisation functions follow a consistent interface:

```r
function_name(
  dynamic = FALSE,  # Set to TRUE for interactive plotly/leaflet output
  base = NULL,      # Optional base plot to add to
  params = list(    # List of parameters controlling the visualisation
    df = data,      # Data frame containing the data to visualise
    ...             # Function-specific parameters
  ),
  ...               # Additional arguments passed to underlying plotting functions
)
```

### Key Features

- **Consistent Interface**: All functions use the same parameter structure, making it easy to learn and use the package.
- **Static/Dynamic Flexibility**: Switch between static (ggplot2) and interactive (plotly/leaflet) visualisations by changing a single parameter.
- **Customisation Options**: Extensive parameters for customising colours, labels, axes, legends, and more.
- **Confidence Intervals**: Support for displaying confidence intervals across different visualisation types.
- **Grouping Support**: Easily create grouped visualisations with appropriate legends.
- **Dual Axes**: Support for secondary y-axes in applicable chart types.
- **Threshold Lines**: Add horizontal reference lines to applicable chart types.

## Experimental Features

### LLM Interpretation

The package includes an experimental function `llm_interpret()` that uses Large Language Models (LLMs) to automatically interpret epidemiological data or visualisations. This function can:

- Generate narrative interpretations of data frames
- Describe and analyse ggplot visualisations
- Provide epidemiologically relevant observations

```r
# Example: Interpret a data frame
interpretation <- llm_interpret(
  input = summarised_df,
  word_limit = 150
)

# Example: Interpret a ggplot visualisation
plot <- line_chart(
  dynamic = FALSE,
  params = list(
    df = summarised_df,
    x = "specimen_date",
    y = "count",
    group_var = "organism_species_name"
  )
)
plot_interpretation <- llm_interpret(plot)
```

#### Supported LLM Providers

The function supports multiple LLM providers:

- **OpenAI**: Models like `gpt-4o`, `gpt-4o-mini`, `o1-mini`
- **Google Gemini**: Models like `gemini-1.5-flash`
- **Anthropic Claude**: Models like `claude-1`

#### Environment Variable Setup

To use the `llm_interpret()` function, you need to set up the following environment variables:

```r
# In your .Renviron file or before calling the function:
Sys.setenv(LLM_PROVIDER = "openai")  # Choose from: "openai", "gemini", "claude"
Sys.setenv(LLM_API_KEY = "your-api-key-here")
Sys.setenv(LLM_MODEL = "gpt-4o")  # Use an appropriate model for your chosen provider
```

You can set these environment variables in your `.Renviron` file for persistent configuration:

```
# .Renviron file
LLM_PROVIDER=openai
LLM_API_KEY=your-api-key-here
LLM_MODEL=gpt-4o
```

## Package data

epiviz includes an anonymised sample dataset from the SGSS laboratory database: `lab_data()`

The Second Generation Surveillance System (SGSS) is a data asset held by the [UK Health Security Agency](https://www.gov.uk/government/publications/securing-our-health-the-uk-health-security-agency/securing-our-health-the-uk-health-security-agency) (formerly Public Health England) which routinely and automatically collects laboratory data from across England.

Laboratories return data on organisms isolated from samples such as the organism species, specimen type, sampling date and antimicrobial susceptibility testing results. These data are routinely used for public health surveillance and epidemiology in England.

## Examples

### Line Chart
```r
# Create a static line chart
line_chart(
  dynamic = FALSE,
  params = list(
    df = summarised_df,
    x = "specimen_date",
    y = "count",
    group_var = "organism_species_name",
    line_colour = c("blue", "green", "orange"),
    line_type = c("solid", "dotted", "dashed")
  )
)

# Convert to an interactive plotly chart by changing dynamic to TRUE
line_chart(
  dynamic = TRUE,
  params = list(
    df = summarised_df,
    x = "specimen_date",
    y = "count",
    group_var = "organism_species_name",
    line_colour = c("blue", "green", "orange"),
    line_type = c("solid", "dotted", "dashed")
  )
)
```

### Age-Sex Pyramid
```r
# Create a static age-sex pyramid
age_sex_pyramid(
  dynamic = FALSE,
  params = list(
    df = lab_data,
    var_map = list(
      date_of_birth = 'date_of_birth',
      sex = 'sex'
    ),
    grouped = FALSE,
    colours = c("pink", "blue")
  )
)

# Convert to an interactive plotly chart by changing dynamic to TRUE
age_sex_pyramid(
  dynamic = TRUE,
  params = list(
    df = lab_data,
    var_map = list(
      date_of_birth = 'date_of_birth',
      sex = 'sex'
    ),
    grouped = FALSE,
    colours = c("pink", "blue")
  )
)
``` 