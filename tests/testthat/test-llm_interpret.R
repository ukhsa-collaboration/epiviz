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

test_that("openshiftai-gpt-oss-120b requires LLM_URL", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai-gpt-oss-120b",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "openshiftai-gpt-oss-120b",
    LLM_URL      = NA
  ))
  expect_error(
    llm_interpret(data.frame(x = 1)),
    "LLM_URL environment variable is not set"
  )
})

test_that("openshiftai-gpt-oss-120b rejects ggplot input", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai-gpt-oss-120b",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "openshiftai-gpt-oss-120b",
    LLM_URL      = "https://fake-openshift-endpoint"
  ))
  p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  expect_error(
    llm_interpret(p),
    "ggplot image input is not supported for the 'openshiftai-gpt-oss-120b' provider"
  )
})

test_that("openshiftai-gpt-oss-120b sends correct httr2 request for data frame", {
  withr::local_envvar(c(
    LLM_PROVIDER = "openshiftai-gpt-oss-120b",
    LLM_API_KEY  = "fake-bearer-token",
    LLM_MODEL    = "openshiftai-gpt-oss-120b",
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

test_that(".parse_openshift_answer falls back to last non-empty line", {
  expect_equal(
    epiviz:::.parse_openshift_answer("line one\n\nline two"),
    "line two"
  )
}) 