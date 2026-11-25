---
name: epiviz-conventions
description: Use when creating, reviewing, or modifying epiviz R package functions (chart/map exports, internal helpers, tests, or docs) so new code follows the same naming, structure, parameter, documentation, and testing conventions as existing functions like epi_map, point_chart, col_chart, line_chart, age_sex_pyramid, and epi_curve.
---

# epiviz package conventions

epiviz is an R package providing consistent static (ggplot2) and dynamic
(plotly/leaflet) epidemiological charts and maps. Each exported function takes
a `params` list rather than individual arguments. Follow the patterns below so
new functions feel identical in style to the existing ones.

## File and function naming

- One exported function per file in `R/`, file name = function name
  (e.g. `R/point_chart.R` -> `point_chart()`, `R/epi_map.R` -> `epi_map()`).
- Naming pattern: `<thing>_chart` for axis-based ggplot/plotly charts
  (`col_chart`, `line_chart`, `point_chart`), `epi_<type>` or `<type>_pyramid`
  for specialised epi visuals (`epi_curve`, `epi_map`, `age_sex_pyramid`).
- Shared/internal (non-exported) helpers live in `R/utils.R`, `R/base_gg.R`,
  `R/base_plotly.R` and are tagged `@noRd` (not exported, no `@export`).
- `R/globals.R` holds a single `utils::globalVariables(c(...))` call used to
  silence "no visible binding for global variable" NOTEs — see below.
- `R/data.R` / `R/lab_data.R` document bundled datasets (`@docType data`,
  `@format`, `@source`, `"object_name"` as the last line).

## Function signature pattern

```r
my_chart <- function(dynamic = FALSE,
                      base = NULL,          # only for axis charts that can overlay a secondary axis
                      params = list(
                        df = NULL,
                        x = NULL,
                        y = NULL,
                        ...                 # every tunable option, with sensible defaults
                      )) {
  ...
}
```

- `dynamic = FALSE` always returns the static ggplot2 output; `dynamic = TRUE`
  returns plotly (charts) or leaflet (maps).
- Maps (`epi_map`) don't take a `base` argument; axis charts that support
  secondary-axis overlays (`point_chart`, `col_chart`, `line_chart`) do.
- All tunable options are passed as a single named `params` list, not as
  separate function arguments. Document every list element in `@param params`
  using a nested `\describe{ \item{name}{...} }` block (see Documentation).

## Standard function body structure

Every exported function follows this order:

1. **`set_Arial()`** — first line of the body, resolves font-family warnings
   and assigns `chart_font` into the function's environment.
2. **Default assignment block** — for every optional param, backfill a
   default if the caller didn't supply it:
   ```r
   if(!exists('legend_pos',where=params)) params$legend_pos <- "topright"
   ```
3. **Checks and warnings block** — validate required params and data with
   `stop()`, and flag questionable-but-recoverable combinations with
   `warning()`. Common checks seen across functions:
   - required arg missing: `if (!exists('df',where=params)) stop(...)`
   - wrong type: `if(!is.data.frame(params$df)) stop(...)`
   - named column not in `df`: `if (!params$x %in% colnames(params$df)) stop(...)`
   - mutually exclusive params (e.g. `break_intervals`/`break_labels` must be
     supplied together; `legend_pos` must be a leaflet-valid value when
     `dynamic = TRUE`)
   - vector-length mismatches, e.g. number of colours must equal number of
     groups in `group_var`
   - informational `warning()` for ignored-but-provided params (e.g. a value
     supplied that won't be used in the current code path)
4. **Parameter assignment** — call
   `param_assign(params, c("df","x","y",...))` to copy every list element
   into local variables (reference vector must list every param name used
   later in the function, so unused ones are set to `NULL`).
5. **Build the output** — construct the ggplot/plotly/leaflet object,
   typically inside `if (!dynamic) { ... return(map/chart) } else { ... return(map/chart) }`.
   Axis-based charts build on top of a shared base object; see below.

## Static vs dynamic construction

- Axis-based charts (`col_chart`, `line_chart`, `point_chart`,
  `age_sex_pyramid`) create a shared base object first:
  ```r
  environment(base_gg) <- environment()      # for dynamic = FALSE
  base <- base_gg()
  environment(base_plotly) <- environment()  # for dynamic = TRUE
  base <- base_plotly()
  ```
  `base_gg()`/`base_plotly()` read variables directly from the caller's
  environment (via `environment(...) <- environment()`) rather than
  receiving them as arguments — they apply title/footer/axis styling shared
  by every chart type.
- `epi_map()` (and any other spatial/map function) does **not** use
  `base_gg`/`base_plotly`. It builds the `ggplot()`/`leaflet()` object
  directly, since maps don't share the same axis/title scaffolding.
- Dynamic leaflet maps add a title/footer via `htmltools::tags$style` +
  `addControl(..., className = "map-title"/"map-footer")` — copy this block
  verbatim for new map functions rather than reinventing it.

## Shared helpers (`R/utils.R`)

Reuse these instead of duplicating logic:

- `set_Arial()` — sets `chart_font` in the parent frame.
- `param_assign(params, reference)` — list-to-local-variables with `NULL`
  fill-in for anything missing from `reference`.
- `palette_gen(x, n_pal)` — turns an RColorBrewer palette name, single colour,
  or colour vector into an `n_pal`-length palette (used for map fills and
  any colour-scale needs).
- `html_bold(x)` — wraps text in `<b></b>` for plotly font strings.
- `plotly_line_style(x)` — maps ggplot linetype names to plotly equivalents.
- `scale_function()` / `inv_scale_function()` — secondary y-axis scaling.
- `get_os()` — used by `set_Arial()` only.

All of these are tagged `@noRd` — don't `@export` new internal helpers.

## Registering new NSE variables (`R/globals.R`)

Because params are assigned into the function environment at runtime via
`param_assign()` / `mutate()`, static analysis (`R CMD check`) cannot see
where bare symbols like `legend_pos` or `Value` come from. Every bare
variable name used unquoted in `dplyr`/`ggplot2` calls (param names *and*
any new columns created with `mutate(NewCol = ...)`) must be added to the
relevant section of `utils::globalVariables(c(...))` in `R/globals.R` —
add a new `# For <function_name>` comment block and list its variables,
following the existing per-function sections.

## Documentation (roxygen2)

Every exported function has:

```r
#' Title Case Name
#'
#' @description One-paragraph description, noting static/dynamic behaviour.
#'
#' @param dynamic Logical indicating whether to produce a dynamic (plotly/leaflet)
#' output. Default is \code{FALSE}, which will return a static ggplot output.
#' @param params A named list containing arguements used to create the plot.
#' \describe{
#'    \item{df}{...}
#'    \item{x}{...}
#'  }
#'
#' @return A ggplot, plotly, or leaflet object.
#'
#' @import <only packages this file directly calls that aren't already
#'          imported elsewhere — check NAMESPACE first>
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: ...
#' }
my_chart <- function(...) { ... }
```

- `@export` only on the single public entry-point function per file.
- `@import`/`@importFrom` tags should reflect what the file uses, but check
  `NAMESPACE` first — if a package is already imported package-wide (e.g.
  `dplyr`, `ggplot2`), you don't strictly need to re-tag it, though several
  files do for self-documentation (`epi_map.R` lists `sf`, `leaflet`,
  `htmltools`, `classInt`, `RColorBrewer`, `grDevices`, `stringr`).
- `NAMESPACE` and `man/*.Rd` are generated by roxygen2
  (`devtools::document()`) — don't hand-edit `NAMESPACE` except as a
  temporary stand-in when roxygen2 isn't installed, and note that it needs
  regenerating.
- New dependencies go in `DESCRIPTION` under `Imports:`.

## Tests (`tests/testthat/`)

- One file per function: `tests/testthat/test-<function_name>.R`.
- Each `test_that()` builds a realistic `params` list (often derived from the
  bundled `lab_data` / boundary datasets), calls the function once for each
  relevant `dynamic` value, and asserts the output class:
  ```r
  expect_true(inherits(result, "ggplot"))
  expect_true(inherits(result, "plotly"))
  expect_true(inherits(result, "leaflet"))
  ```
- Add dedicated cases for argument validation (`expect_error(...)`) when a
  function introduces new required params or mutually-exclusive options.

## Checklist for adding a new exported function

1. Create `R/<name>.R` with the standard body structure above.
2. Reuse `set_Arial`, `param_assign`, `palette_gen`, `base_gg`/`base_plotly`
   (axis charts only) instead of re-implementing equivalents.
3. Add every bare NSE symbol (params + new `mutate()` columns) to
   `R/globals.R` under a new `# For <name>` section.
4. Write full roxygen docs with a `params` `\describe` block and `\dontrun`
   examples using the package's bundled datasets.
5. Add `tests/testthat/test-<name>.R` covering static and dynamic output,
   plus required-argument validation.
6. Run `devtools::document()` to regenerate `NAMESPACE`/`man/*.Rd`, then
   `devtools::test()` (or `devtools::check()`), before considering the
   function done.
