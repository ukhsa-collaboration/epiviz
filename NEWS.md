# epiviz 0.0.0.9001

* Initial GitHub code transfer from internal repo.

# epiviz 0.1.0

* Initial version for internal production apps

# epiviz 0.1.1

* Initial CRAN submission

# epiviz (development version)

* `llm_interpret()` gains support for internally hosted, OpenAI-compatible
  completions endpoints via a new `"openshiftai"` provider. Set `LLM_PROVIDER`
  to `"openshiftai"` and supply the endpoint base URL in `LLM_URL`. Image input
  is not supported for this provider, so pass the underlying data frame rather
  than a ggplot object.
* `llm_interpret()` gains a `strip_markdown` argument, which removes Markdown
  emphasis from the response so it reads cleanly as plain text. This defaults
  to `TRUE` and applies to all providers, so responses that previously included
  Markdown formatting are now returned as plain prose. Set
  `strip_markdown = FALSE` to restore the previous behaviour, for example when
  embedding the result in an R Markdown or Quarto report.
* `llm_interpret()` gains a `max_rows` argument, defaulting to 500, which stops
  oversized inputs before any data is transmitted. The entire data frame is
  serialised into the prompt, so passing a line list by mistake would otherwise
  send every row to the language model. Aggregate the data first rather than
  raising this limit.
* `llm_interpret()` now warns when the input contains column names that commonly
  hold personal identifiers, such as `date_of_birth`, `nhs_number` or
  `postcode`, before the data is sent.
* `llm_interpret()` gains a `timeout` argument, defaulting to 60 seconds, for
  the `"openshiftai"` provider.
* Failures against the `"openshiftai"` provider now report what to check,
  distinguishing a rejected token from a missing endpoint from an unreachable
  host. An incorrect `LLM_URL` tends to stall rather than fail immediately, so
  the timeout message points at the URL before the network.
* `LLM_URL` is now accepted with or without a trailing `/v1`.
* Responses from the `"openshiftai"` provider that stop at the token limit are
  trimmed back to the last complete sentence.
* The `word_limit` argument of `llm_interpret()` is now documented as an
  approximate target rather than a hard limit, reflecting how providers vary in
  honouring it.
