# Private helper: strip the internal reasoning prefix that the OpenShift AI
# model sometimes prepends before a special "assistantfinal" marker.
# When the marker is absent, heuristically strip leading chain-of-thought
# lines (e.g. "We need to...", "Let's...", "So the...") and the "Answer:"
# prefix, then return whatever clean prose remains.
.parse_openshift_answer <- function(raw) {
  marker <- "assistantfinal"
  if (grepl(marker, raw, fixed = TRUE)) {
    return(trimws(strsplit(raw, marker, fixed = TRUE)[[1]][2]))
  }

  text <- trimws(raw)

  # Strip a leading "Answer:" prefix if present
  text <- sub("^Answer:\\s*", "", text)

  trimws(text)
}

# Private helper: remove common Markdown emphasis that LLM providers add, so
# the result reads cleanly when printed as plain text. Single-asterisk and
# underscore emphasis are deliberately left alone to avoid mangling snake_case
# identifiers and column names that appear in epidemiological data.
.strip_markdown <- function(text) {
  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    return(text)
  }

  text <- gsub("\\*\\*(.+?)\\*\\*", "\\1", text)
  text <- gsub("__(.+?)__", "\\1", text)
  text <- gsub("`([^`]+)`", "\\1", text)
  text <- gsub("\\[(.+?)\\]\\([^)]*\\)", "\\1", text)
  text <- gsub("(?m)^#{1,6}[ \t]+", "", text, perl = TRUE)
  # A truncated response can leave an unclosed bold marker behind.
  text <- gsub("\\*\\*", "", text)

  trimws(text)
}

# Private helper: when the model stops because it hit the token ceiling, the
# final sentence is left unfinished; cut back to the last completed one.
.trim_incomplete_sentence <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) {
    return(text)
  }

  ends <- gregexpr("[.!?]", text)[[1]]
  if (length(ends) == 1L && ends[1] == -1L) {
    return(text)
  }

  trimws(substring(text, 1L, ends[length(ends)]))
}

# Private helper: flag column names that commonly hold direct or indirect
# identifiers, so users are warned before patient-level data leaves the session.
.detect_identifier_columns <- function(nms) {
  tokens <- c("dob", "date_of_birth", "birth_date", "nhs_number", "nhs_no",
              "patient_id", "patient_no", "mrn", "hospital_number", "postcode",
              "post_code", "forename", "first_name", "surname", "last_name",
              "full_name", "address", "email", "telephone", "phone")
  joined <- c("dateofbirth", "nhsnumber", "patientid", "postcode", "surname",
              "forename", "email")

  underscored <- gsub("[^a-z0-9]+", "_", tolower(nms))
  stripped <- gsub("[^a-z0-9]", "", tolower(nms))

  bounded <- paste0("(^|_)(", paste(tokens, collapse = "|"), ")($|_)")
  substr_hit <- paste0("(", paste(joined, collapse = "|"), ")")

  unique(nms[grepl(bounded, underscored) | grepl(substr_hit, stripped)])
}

# Private helper: a wrong hostname usually stalls rather than failing fast, so
# distinguish that case from a bad token or a genuine network fault.
.openshift_error_hint <- function(e, url, timeout) {
  msg <- conditionMessage(e)
  status <- if (inherits(e, "httr2_http")) attr(e, "status") else NULL
  if (is.null(status) && grepl("HTTP [0-9]{3}", msg)) {
    status <- as.integer(sub(".*HTTP ([0-9]{3}).*", "\\1", msg))
  }

  hint <- if (!is.null(status) && status %in% c(401, 403)) {
    paste0("Authentication was rejected (HTTP ", status, "). Check that ",
           "LLM_API_KEY holds the complete bearer token. Long tokens are ",
           "easily truncated when pasted, and a truncated token fails this ",
           "way. Also confirm the token has not expired.")
  } else if (!is.null(status) && status == 404) {
    paste0("The endpoint was not found (HTTP 404) at ", url,
           ". Check that LLM_URL points at the right deployment.")
  } else if (!is.null(status) && status >= 500) {
    paste0("The service returned HTTP ", status,
           ". The deployment may be restarting or overloaded; retry shortly.")
  } else if (grepl("timed out|timeout", msg, ignore.case = TRUE)) {
    paste0("No response within ", timeout, " seconds from ", url,
           ". The usual causes are an incorrect LLM_URL, or no route to the ",
           "service because you are off the corporate network. An incorrect ",
           "hostname often stalls rather than failing immediately, so verify ",
           "the URL before assuming a network fault.")
  } else if (grepl("resolve", msg, ignore.case = TRUE)) {
    paste0("The hostname in LLM_URL could not be resolved (", url,
           "). Check it for typos and confirm you are on the corporate network.")
  } else {
    paste0("Request to ", url, " failed.")
  }

  paste0("OpenShift AI API request failed. ", hint,
         "\n  Underlying error: ", msg)
}

#' Interpret Epidemiological Data or Visualisations using LLMs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' This function interprets a given data frame or ggplot visualisation by sending it to a language model API. It supports multiple LLM providers, including an internal OpenShift AI deployment, allowing users to specify the desired provider and model through environment variables.
#'
#' @param input An input object, either a data frame or a ggplot object, representing the data or visualisation to be interpreted.
#' @param word_limit Integer. Approximate target length for the response, in words. Treated as a guide rather than a hard limit: providers vary in how closely they honour it, and the internal OpenShift AI provider steers length via token budget instead. Defaults to 100.
#' @param prompt_extension Character. Optional additional instructions to extend the standard prompt. Defaults to NULL.
#' @param strip_markdown Logical. Remove Markdown emphasis (bold, inline code, links, headings) from the response so it reads cleanly as plain text. Set to FALSE to keep the provider's original Markdown, for example when embedding the result in an R Markdown or Quarto report. Defaults to TRUE.
#' @param max_rows Integer. Maximum number of rows that may be sent to the language model. Guards against transmitting a line list by mistake, since the whole data frame is serialised into the prompt. Aggregate the data first rather than raising this. Defaults to 500.
#' @param timeout Numeric. Seconds to wait for a response from the `"openshiftai"` endpoint before giving up. Defaults to 60.
#' @return A character string containing the narrative or interpretation of the input object as generated by the LLM.
#' @details
#' **Supported LLM Providers and Models:**
#' - **OpenAI**: Utilises OpenAI's models via `chat_openai()`. Requires setting the `OPENAI_API_KEY` environment variable. Applicable models include:
#'   - `"gpt-4.1-nano"`
#' - **Google Gemini**: Utilises Google's Gemini models via `chat_gemini()`. Requires setting the `GOOGLE_API_KEY` environment variable. Applicable models include:
#'   - `"gemini-2.5-flash-lite"`
#' - **Anthropic Claude**: Utilises Anthropic's Claude models via `chat_anthropic()`. Requires setting the `CLAUDE_API_KEY` environment variable. Applicable models include:
#'   - `"claude-sonnet-4-20250514"`
#' - **OpenShift AI (internal)**: Utilises an internally hosted, OpenAI-compatible
#'   `/v1/completions` endpoint via a direct `httr2` call. Set `LLM_PROVIDER` to
#'   `"openshiftai"`, set `LLM_MODEL` to the model identifier served by your
#'   deployment, and set `LLM_URL` to its base URL. Endpoint details and credentials
#'   are issued by your organisation's platform administrators.
#'   Note: ggplot image input is **not** supported for this provider.
#'
#' **Environment Variables:**
#' - `LLM_PROVIDER`: Specifies the LLM provider ("openai", "gemini", "anthropic", or "openshiftai").
#' - `LLM_API_KEY`: The API key or Bearer token corresponding to the chosen provider.
#' - `LLM_MODEL`: The model identifier to use.
#' - `LLM_URL`: Base URL of the OpenShift AI endpoint. Required only when using the `"openshiftai"` provider.
#'
#' **Note:** Ensure that the appropriate environment variables are set before invoking this function. The function will throw an error if the specified provider is unsupported or if required environment variables are missing.
#'
#' @import ggplot2
#' @importFrom jsonlite toJSON
#' @import ellmer
#' @importFrom httr2 request req_headers req_body_json req_timeout req_perform resp_body_json
#' @importFrom lifecycle badge signal_stage
#' @export
#'
#' @section Tested Models:
#' As of April 2026, this function has been tested and verified to work with the following models:
#' - OpenAI: gpt-4.1-nano
#' - Anthropic: claude-sonnet-4-20250514  
#' - Google Gemini: gemini-2.5-flash-lite
#' - OpenShift AI (internal): verified against an internally hosted
#'   OpenAI-compatible completions deployment
#' 
#' Additional models may be tested in the future. Users can provide custom instructions
#' through the \code{prompt_extension} parameter for specialised analysis requirements.
llm_interpret <- function(input,
                          word_limit = 100,
                          prompt_extension = NULL,
                          strip_markdown = TRUE,
                          max_rows = 500,
                          timeout = 60) {
  lifecycle::signal_stage("experimental", "llm_interpret()")

  # Validate input parameters
  if (!is.numeric(word_limit) || word_limit <= 0) {
    stop("word_limit must be a positive number")
  }

  if (!is.null(prompt_extension) && !is.character(prompt_extension)) {
    stop("prompt_extension must be NULL or a character string")
  }

  if (!is.logical(strip_markdown) || length(strip_markdown) != 1 ||
      is.na(strip_markdown)) {
    stop("strip_markdown must be TRUE or FALSE")
  }

  if (!is.numeric(max_rows) || length(max_rows) != 1 || is.na(max_rows) ||
      max_rows <= 0) {
    stop("max_rows must be a single positive number")
  }

  if (!is.numeric(timeout) || length(timeout) != 1 || is.na(timeout) ||
      timeout <= 0) {
    stop("timeout must be a single positive number of seconds")
  }

  finalise <- function(x) {
    x <- as.character(x)
    if (strip_markdown) .strip_markdown(x) else x
  }

  # Check for required environment variables with informative messages
  tryCatch({
    provider <- Sys.getenv("LLM_PROVIDER")
    if (provider == "") {
      stop("LLM_PROVIDER environment variable is not set. ",
           "Please set it to one of: 'openai', 'gemini', 'anthropic', or ",
           "'openshiftai'")
    }

    api_key <- Sys.getenv("LLM_API_KEY")
    if (api_key == "") {
      stop("LLM_API_KEY environment variable is not set. ",
           sprintf("For %s, please set the appropriate API key.",
                   toupper(provider)))
    }

    model <- Sys.getenv("LLM_MODEL")
    if (model == "") {
      stop("LLM_MODEL environment variable is not set. ",
           sprintf("For %s, please set an appropriate model identifier.",
                   toupper(provider)))
    }

    # OpenShift AI requires the base URL of the hosted endpoint
    if (provider == "openshiftai") {
      llm_url <- Sys.getenv("LLM_URL")
      if (llm_url == "") {
        stop("LLM_URL environment variable is not set. ",
             "Set it to the base URL of your OpenShift AI endpoint.")
      }
    }
  }, error = function(e) {
    stop("Error checking environment variables: ", conditionMessage(e))
  })

  # For the OpenShift AI provider we bypass ellmer entirely and use httr2
  # directly (the endpoint exposes /v1/completions, not /v1/chat/completions).
  use_openshift <- provider == "openshiftai"

  # Initialize the chat client with error handling (ellmer providers only)
  chat <- if (!use_openshift) {
    tryCatch({
      switch(
        provider,
        "openai"    = chat_openai(model = model, api_key = api_key),
        "gemini"    = chat_google_gemini(model = model, api_key = api_key),
        "anthropic" = chat_anthropic(model = model, api_key = api_key),
        stop(sprintf("Unsupported LLM provider: '%s'", provider))
      )
    }, error = function(e) {
      stop("Failed to initialize chat client: ", conditionMessage(e))
    })
  } else {
    NULL
  }

  # Set the system prompt with error handling (ellmer providers only)
  if (!use_openshift) {
    tryCatch({
      chat$set_system_prompt(
        "You are a concise assistant focusing on epidemiologically relevant observations."
      )
    }, error = function(e) {
      stop("Failed to set system prompt: ", conditionMessage(e))
    })
  }

  # Define the standard prompt
  standard_prompt <- tryCatch({
    prompt <- paste(
      "Please provide an interpretation focusing on the most",
      "epidemiologically relevant observations in",
      word_limit,
      "words or fewer."
    )
    if (!is.null(prompt_extension)) {
      prompt <- paste(prompt, prompt_extension)
    }
    prompt
  }, error = function(e) {
    stop("Failed to construct prompt: ", conditionMessage(e))
  })

  # Process input based on type
  if (is.data.frame(input)) {
    if (nrow(input) == 0) {
      stop("Input data frame is empty")
    }

    if (nrow(input) > max_rows) {
      stop("Input has ", nrow(input), " rows, which exceeds max_rows (",
           max_rows, "). llm_interpret() is intended for aggregated summary ",
           "tables rather than line lists. Aggregate the data first, for ",
           "example with dplyr::count() or dplyr::summarise(). Raise ",
           "max_rows only if you are certain the full dataset should be sent ",
           "to the language model.")
    }

    flagged <- .detect_identifier_columns(names(input))
    if (length(flagged) > 0) {
      warning("The data being sent to the language model contains columns ",
              "that may hold personal identifiers: ",
              paste(flagged, collapse = ", "),
              ". Remove or aggregate these columns unless you are certain ",
              "the destination is approved for this data.",
              call. = FALSE)
    }

    # Convert data frame to JSON with error handling
    json_data <- tryCatch({
      jsonlite::toJSON(input, pretty = TRUE, auto_unbox = TRUE)
    }, error = function(e) {
      stop("Failed to convert data frame to JSON: ", conditionMessage(e))
    })

    # Send JSON data to the API with error handling
    if (use_openshift) {
      # Build prompt for the /v1/completions API. Do NOT mention word counts —
      # this model's chain-of-thought interprets any count target as an
      # instruction to literally count and annotate every word. Length is
      # controlled solely via max_tokens instead.
      # Build prompt for the /v1/completions API. This model works best with
      # the Question/Answer format (matching the proven Python RAG pattern).
      # The model emits chain-of-thought reasoning followed by an
      # "assistantfinal" marker; .parse_openshift_answer() strips that prefix.
      openshift_instruction <- paste0(
        "You are a concise epidemiological assistant.\n",
        "Answer the question using only the information in the data.\n",
        "Keep your answer short and helpful.\n",
        "Do not explain how you got the answer.\n",
        if (!is.null(prompt_extension)) paste0(prompt_extension, "\n") else "",
        "\nData:\n", json_data, "\n\n",
        "Question: Please provide a brief interpretation focusing on the ",
        "most epidemiologically relevant observations.\n\n",
        "Answer:"
      )
      full_prompt <- openshift_instruction
      # Accept a base URL with or without a trailing "/v1", since related
      # internal tooling documents it both ways.
      llm_url <- sub("/+$", "", Sys.getenv("LLM_URL"))
      llm_url <- sub("/v1$", "", llm_url)
      request_url <- paste0(llm_url, "/v1/completions")
      tryCatch({
        resp <- httr2::request(request_url) |>
          httr2::req_headers(
            Authorization = paste("Bearer", api_key)
          ) |>
          httr2::req_body_json(list(
            model       = model,
            prompt      = full_prompt,
            max_tokens  = max(300L, ceiling(word_limit * 3L)),
            temperature = 0.1
          )) |>
          httr2::req_timeout(timeout) |>
          httr2::req_perform()
        choice <- httr2::resp_body_json(resp)$choices[[1]]
        raw_text <- choice$text
        if (identical(choice$finish_reason, "length")) {
          raw_text <- .trim_incomplete_sentence(raw_text)
        }
        return(finalise(.parse_openshift_answer(raw_text)))
      }, error = function(e) {
        stop(.openshift_error_hint(e, request_url, timeout), call. = FALSE)
      })
    } else {
      tryCatch({
        response <- chat$chat(standard_prompt, json_data)
        return(finalise(response))
      }, error = function(e) {
        stop("API request failed for data frame input: ", conditionMessage(e))
      })
    }
  }
  else if (inherits(input, "ggplot")) {
    # The OpenShift AI endpoint (/v1/completions) is text-only and cannot
    # accept image data.
    if (use_openshift) {
      stop(
        "ggplot image input is not supported for the 'openshiftai' ",
        "provider. The internal endpoint only accepts text input. ",
        "Pass the underlying data frame instead."
      )
    }

    temp_file <- NULL

    # Use withCallingHandlers for cleanup even if error occurs
    withCallingHandlers({
      # Save the ggplot object as a temporary image
      temp_file <- tempfile(fileext = ".png")
      tryCatch({
        ggplot2::ggsave(
          temp_file,
          plot = input,
          width = 6,
          height = 4,
          dpi = 300
        )
      }, error = function(e) {
        stop("Failed to save plot as image: ", conditionMessage(e))
      })

      # Send image file to the API
      response <- tryCatch({
        chat$chat(content_image_file(temp_file), standard_prompt)
      }, error = function(e) {
        stop("API request failed for plot input: ", conditionMessage(e))
      })

      return(finalise(response))
    }, finally = {
      # Clean up temporary file
      if (!is.null(temp_file) && file.exists(temp_file)) {
        unlink(temp_file)
      }
    })
  }
  else {
    stop(
      sprintf(
        "Unsupported input type: %s. Input must be a data frame or ggplot object.",
        class(input)[1]
      )
    )
  }
}
