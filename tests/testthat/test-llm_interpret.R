# Helper functions for environment variable management
setup_env_vars <- function(provider = NULL, api_key = NULL, model = NULL) {
  if (!is.null(provider)) Sys.setenv(LLM_PROVIDER = provider)
  if (!is.null(api_key)) Sys.setenv(LLM_API_KEY = api_key)
  if (!is.null(model)) Sys.setenv(LLM_MODEL = model)
}

cleanup_env_vars <- function() {
  Sys.unsetenv("LLM_PROVIDER")
  Sys.unsetenv("LLM_API_KEY")
  Sys.unsetenv("LLM_MODEL")
}

# Set up test environment
withr::local_envvar(
  c(
    LLM_PROVIDER = NA,
    LLM_API_KEY = NA,
    LLM_MODEL = NA
  )
)

test_that("llm_interpret validates input parameters", {
  # Set up minimal environment variables for parameter validation tests
  setup_env_vars(provider = "openai", api_key = "fake-key", model = "gpt-4o")
  
  # Test invalid word_limit
  expect_error(
    llm_interpret(data.frame(x = 1), word_limit = -1),
    "word_limit must be a positive number"
  )
  
  expect_error(
    llm_interpret(data.frame(x = 1), word_limit = "invalid"),
    "word_limit must be a positive number"
  )
  
  # Test invalid prompt_extension
  expect_error(
    llm_interpret(data.frame(x = 1), prompt_extension = 123),
    "prompt_extension must be NULL or a character string"
  )
  
  # Note: Empty data frame test removed because environment variables are checked first
  
  # Test unsupported input type
  expect_error(
    llm_interpret("not a data frame or ggplot"),
    "Unsupported input type: character"
  )
})

test_that("llm_interpret validates environment variables", {
  # Test missing LLM_PROVIDER
  cleanup_env_vars()
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "LLM_PROVIDER environment variable is not set"
  )
  
  # Test missing LLM_API_KEY
  setup_env_vars(provider = "openai")
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "LLM_API_KEY environment variable is not set"
  )
  
  # Test missing LLM_MODEL
  setup_env_vars(provider = "openai", api_key = "fake-key")
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "LLM_MODEL environment variable is not set"
  )
  
  # Test unsupported provider
  setup_env_vars(provider = "unsupported", api_key = "fake-key", model = "fake-model")
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "Unsupported LLM provider: 'unsupported'"
  )
})

# =============================================================================
# OpenShift AI provider tests
# =============================================================================

test_that("openshiftai requires LLM_URL", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = NA
  ))
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "LLM_URL environment variable is not set"
  )
})

test_that("openshiftai rejects ggplot input", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  expect_error(
    llm_interpret(p),
    "ggplot image input is not supported for the 'openshiftai' provider"
  )
})

test_that("openshiftai sends correct httr2 request for data frame", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))

  # Fake a successful /v1/completions response
  fake_response_body <- list(
    choices = list(list(text = "assistantfinalDisease incidence peaked in week 10."))
  )

  # Stub httr2::req_perform so no real network call is made
  mockery::stub(
    llm_interpret,
    "httr2::req_perform",
    function(...) structure(list(), class = "httr2_response")
  )
  mockery::stub(
    llm_interpret,
    "httr2::resp_body_json",
    function(...) fake_response_body
  )

  result <- llm_interpret(data.frame(week = 10, cases = 42))
  expect_equal(result, "Disease incidence peaked in week 10.")
})

test_that(".parse_openshift_answer strips assistantfinal marker", {
  expect_equal(
    epiviz:::.parse_openshift_answer("some preambleassistantfinal The answer."),
    "The answer."
  )
})

test_that(".parse_openshift_answer strips Answer: prefix", {
  expect_equal(
    epiviz:::.parse_openshift_answer("Answer: The data shows a peak in March."),
    "The data shows a peak in March."
  )
})

test_that(".parse_openshift_answer returns clean text when no marker", {
  expect_equal(
    epiviz:::.parse_openshift_answer("The data shows a peak in March."),
    "The data shows a peak in March."
  )
})

# =============================================================================
# Markdown stripping (provider-agnostic)
# =============================================================================

test_that(".strip_markdown removes bold, code, links and headings", {
  expect_equal(epiviz:::.strip_markdown("The **peak** was in July."),
               "The peak was in July.")
  expect_equal(epiviz:::.strip_markdown("The __peak__ was in July."),
               "The peak was in July.")
  expect_equal(epiviz:::.strip_markdown("Column `cases` rose."),
               "Column cases rose.")
  expect_equal(epiviz:::.strip_markdown("See [the report](https://example.org)."),
               "See the report.")
  expect_equal(epiviz:::.strip_markdown("## Summary\nCases rose."),
               "Summary\nCases rose.")
})

test_that(".strip_markdown preserves snake_case and plain prose", {
  expect_equal(epiviz:::.strip_markdown("The specimen_date column is key."),
               "The specimen_date column is key.")
  expect_equal(epiviz:::.strip_markdown("Cases rose sharply."),
               "Cases rose sharply.")
})

test_that(".strip_markdown removes an unclosed bold marker from truncated text", {
  expect_equal(epiviz:::.strip_markdown("Cases rose, showing a **summer"),
               "Cases rose, showing a summer")
})

test_that(".trim_incomplete_sentence cuts back to the last full sentence", {
  expect_equal(
    epiviz:::.trim_incomplete_sentence("Cases peaked in July. The cause was"),
    "Cases peaked in July."
  )
  expect_equal(
    epiviz:::.trim_incomplete_sentence("Cases peaked in July."),
    "Cases peaked in July."
  )
})

test_that(".trim_incomplete_sentence returns text with no sentence break intact", {
  expect_equal(epiviz:::.trim_incomplete_sentence("Cases rose steadily in the"),
               "Cases rose steadily in the")
})

test_that("llm_interpret trims truncated openshiftai responses", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))

  fake_body <- list(choices = list(list(
    text = "assistantfinalCases peaked in July. The underlying cause was",
    finish_reason = "length"
  )))
  mockery::stub(llm_interpret, "httr2::req_perform",
                function(...) structure(list(), class = "httr2_response"))
  mockery::stub(llm_interpret, "httr2::resp_body_json", function(...) fake_body)

  expect_equal(llm_interpret(data.frame(month = "2023-07", cases = 1)),
               "Cases peaked in July.")
})

# =============================================================================
# Data governance guards
# =============================================================================

test_that(".detect_identifier_columns flags likely identifiers", {
  expect_equal(epiviz:::.detect_identifier_columns("date_of_birth"),
               "date_of_birth")
  expect_setequal(
    epiviz:::.detect_identifier_columns(c("nhs_number", "Postcode", "cases")),
    c("nhs_number", "Postcode")
  )
  expect_equal(epiviz:::.detect_identifier_columns("DateOfBirth"),
               "DateOfBirth")
})

test_that(".detect_identifier_columns ignores ordinary analytical columns", {
  expect_length(
    epiviz:::.detect_identifier_columns(
      c("specimen_date", "organism_species_name", "region", "cases", "month")
    ),
    0
  )
})

test_that("llm_interpret rejects data frames larger than max_rows", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))
  big <- data.frame(x = seq_len(501))
  expect_error(llm_interpret(big), "exceeds max_rows")
  expect_error(llm_interpret(big), "Aggregate the data first")
})

test_that("llm_interpret validates max_rows and timeout", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))
  expect_error(llm_interpret(data.frame(x = 1), max_rows = 0),
               "max_rows must be a single positive number")
  expect_error(llm_interpret(data.frame(x = 1), timeout = -1),
               "timeout must be a single positive number")
})

test_that("llm_interpret warns when identifier columns are present", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))

  fake_body <- list(choices = list(list(text = "assistantfinalDone.")))
  mockery::stub(llm_interpret, "httr2::req_perform",
                function(...) structure(list(), class = "httr2_response"))
  mockery::stub(llm_interpret, "httr2::resp_body_json", function(...) fake_body)

  expect_warning(
    llm_interpret(data.frame(date_of_birth = "1990-01-01", cases = 1)),
    "may hold personal identifiers"
  )
})

test_that("llm_interpret does not warn for aggregated data", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))

  fake_body <- list(choices = list(list(text = "assistantfinalDone.")))
  mockery::stub(llm_interpret, "httr2::req_perform",
                function(...) structure(list(), class = "httr2_response"))
  mockery::stub(llm_interpret, "httr2::resp_body_json", function(...) fake_body)

  expect_no_warning(llm_interpret(data.frame(month = "2023-01", cases = 42)))
})

test_that(".openshift_error_hint distinguishes failure modes", {
  url <- "https://example.internal/v1/completions"

  expect_match(
    epiviz:::.openshift_error_hint(simpleError("HTTP 401 Unauthorized."), url, 60),
    "truncated"
  )
  expect_match(
    epiviz:::.openshift_error_hint(simpleError("HTTP 404 Not Found."), url, 60),
    "not found"
  )
  expect_match(
    epiviz:::.openshift_error_hint(simpleError("Timeout was reached"), url, 60),
    "incorrect LLM_URL"
  )
  expect_match(
    epiviz:::.openshift_error_hint(simpleError("Could not resolve host"), url, 60),
    "could not be resolved"
  )
})

test_that("llm_interpret validates strip_markdown", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))
  expect_error(
    llm_interpret(data.frame(x = 1), strip_markdown = "yes"),
    "strip_markdown must be TRUE or FALSE"
  )
})

test_that("llm_interpret strips markdown from openshiftai responses by default", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))

  fake_body <- list(
    choices = list(list(text = "assistantfinalThe **peak** was in July."))
  )
  mockery::stub(llm_interpret, "httr2::req_perform",
                function(...) structure(list(), class = "httr2_response"))
  mockery::stub(llm_interpret, "httr2::resp_body_json", function(...) fake_body)

  expect_equal(llm_interpret(data.frame(week = 1)), "The peak was in July.")
})

test_that("llm_interpret keeps markdown when strip_markdown = FALSE", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "test-model",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))

  fake_body <- list(
    choices = list(list(text = "assistantfinalThe **peak** was in July."))
  )
  mockery::stub(llm_interpret, "httr2::req_perform",
                function(...) structure(list(), class = "httr2_response"))
  mockery::stub(llm_interpret, "httr2::resp_body_json", function(...) fake_body)

  expect_equal(llm_interpret(data.frame(week = 1), strip_markdown = FALSE),
               "The **peak** was in July.")
})
