# Package-level mutable state for rate limiting (environment, not exported)
.llm_state <- new.env(parent = emptyenv())
.llm_state$gemini_last_call <- NULL

#' Extract and validate the text content of a chat-completions response
#'
#' A refusal or safety-blocked response carries a NULL \code{content};
#' passing that on silently yielded \code{character(0)}, which slipped past
#' the callers' tryCatch fallbacks and crashed report assembly much later
#' with "subscript out of bounds". Raising here instead lets the existing
#' placeholder-text fallbacks handle the failure.
#'
#' @param result Parsed JSON body of a chat-completions response.
#' @return Length-1 character string.
#' @keywords internal
extract_llm_content <- function(result) {
  if (length(result$choices) == 0) {
    stop("LLM returned no choices", call. = FALSE)
  }
  choice <- result$choices[[1]]
  text <- choice$message$content
  if (!is.character(text) || length(text) != 1 || is.na(text) ||
      !nzchar(text)) {
    stop("LLM returned empty content (finish_reason: ",
         choice$finish_reason %||% "unknown", ")", call. = FALSE)
  }
  text
}

#' Extract and validate the text content of a Claude Messages API response
#'
#' The Messages API returns a list of content blocks rather than
#' \code{choices}; with thinking enabled the first block can be an empty
#' \code{thinking} block, so the first \code{text} block is selected. A
#' refusal arrives as HTTP 200 with \code{stop_reason = "refusal"} and a
#' \code{max_tokens} stop means the text was truncated; both raise so the
#' callers' placeholder-text fallbacks handle them, as for
#' \code{extract_llm_content}.
#'
#' @param result Parsed JSON body of a Messages API response.
#' @return Length-1 character string.
#' @keywords internal
extract_claude_content <- function(result) {
  stop_reason <- result$stop_reason %||% "unknown"
  if (identical(stop_reason, "refusal")) {
    category <- result$stop_details$category %||% "unspecified"
    stop("LLM refused the request (category: ", category, ")", call. = FALSE)
  }
  if (identical(stop_reason, "max_tokens")) {
    stop("LLM output was truncated (stop_reason: max_tokens)", call. = FALSE)
  }
  texts <- Filter(function(block) identical(block$type, "text"),
                  result$content %||% list())
  text <- if (length(texts) > 0) texts[[1]]$text else NULL
  if (!is.character(text) || length(text) != 1 || is.na(text) ||
      !nzchar(text)) {
    stop("LLM returned empty content (stop_reason: ", stop_reason, ")",
         call. = FALSE)
  }
  text
}

#' Call the Claude API
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model Claude model name (default: \code{llm_model_name("claude")}).
#' @param temperature Ignored. Current Claude models reject sampling
#'   parameters; the argument is kept so all providers share one signature.
#' @return Character string with the generated text.
#' @keywords internal
call_claude <- function(system_prompt, user_prompt,
                        model = llm_model_name("claude"),
                        temperature = NULL) {
  resp <- httr2::req_perform(
    build_claude_request(system_prompt, user_prompt, model)
  )
  result <- httr2::resp_body_json(resp)
  strip_markdown(extract_claude_content(result))
}

#' Build a Claude Messages API request
#'
#' Shared by the single-call path (\code{call_claude}) and the parallel
#' batch path (\code{call_llm_batch}). The body is kept minimal so a model
#' override via \code{ANTHROPIC_MODEL} works across model generations: no
#' \code{thinking} block (current models run adaptive thinking by default)
#' and no sampling parameters (rejected with a 400 by current models).
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model Claude model name.
#' @param temperature Ignored; see \code{call_claude}.
#' @return An httr2 request object.
#' @keywords internal
build_claude_request <- function(system_prompt, user_prompt,
                                 model = llm_model_name("claude"),
                                 temperature = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for LLM API calls. ",
         "Install it with: install.packages(\"httr2\")", call. = FALSE)
  }
  api_key <- Sys.getenv("ANTHROPIC_API_KEY", "")
  if (!nzchar(api_key)) {
    stop("ANTHROPIC_API_KEY environment variable is not set.", call. = FALSE)
  }

  # max_tokens is required by the Messages API and also caps the (billed)
  # thinking tokens, so it is set well above the length of a report section.
  body <- list(
    model = model,
    max_tokens = 16000,
    system = system_prompt,
    messages = list(
      list(role = "user", content = user_prompt)
    )
  )

  # 180s timeout because adaptive thinking makes responses slower than the
  # chat-completions providers. 429 = rate-limited, 503/529 = overloaded;
  # all transient, so retry with backoff (httr2 honours Retry-After).
  httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `Content-Type` = "application/json",
      # httr2 only redacts Authorization by default; keep the key out of
      # printed request objects like the Bearer-token providers.
      .redact = "x-api-key"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429L, 503L, 529L),
      backoff = ~ 5
    ) |>
    httr2::req_error(body = function(resp) {
      tryCatch(httr2::resp_body_json(resp)$error$message,
               error = function(e) NULL)
    })
}

#' Call the OpenAI API
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model OpenAI model name (default: \code{llm_model_name("openai")}).
#' @param temperature Sampling temperature (default: 0.3). Lower values
#'   produce more deterministic, factual output; higher values are more
#'   creative. 0.3 is a good balance for scientific report text.
#' @return Character string with the generated text.
#' @keywords internal
call_openai <- function(system_prompt, user_prompt,
                        model = llm_model_name("openai"),
                        temperature = 0.3) {
  resp <- httr2::req_perform(
    build_openai_request(system_prompt, user_prompt, model, temperature)
  )
  result <- httr2::resp_body_json(resp)
  strip_markdown(extract_llm_content(result))
}

#' Build an OpenAI chat-completions request
#'
#' Shared by the single-call path (\code{call_openai}) and the parallel
#' batch path (\code{call_llm_batch}).
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model OpenAI model name.
#' @param temperature Sampling temperature.
#' @return An httr2 request object.
#' @keywords internal
build_openai_request <- function(system_prompt, user_prompt,
                                 model = llm_model_name("openai"),
                                 temperature = 0.3) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for LLM API calls. ",
         "Install it with: install.packages(\"httr2\")", call. = FALSE)
  }
  api_key <- Sys.getenv("OPENAI_API_KEY", "")
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY environment variable is not set.", call. = FALSE)
  }

  body <- list(
    model = model,
    temperature = temperature,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  # 120s timeout because report text generation can be slow for long prompts.
  # Retries twice with 5s backoff to handle transient API errors.
  httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 2, backoff = ~ 5)
}

#' Call the Gemini API (OpenAI-compatible endpoint)
#'
#' Uses Google's OpenAI-compatible chat completions endpoint. Includes a
#' rate-limit delay to stay within the free tier (5 RPM for Pro, 15 RPM
#' for Flash). On 429 responses, waits and retries up to 3 times with
#' increasing backoff.
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param model Gemini model name (default: "gemini-2.5-flash-lite").
#' @param temperature Sampling temperature (default: 0.3).
#' @return Character string with the generated text.
#' @keywords internal
call_gemini <- function(system_prompt, user_prompt,
                        model = llm_model_name("gemini"),
                        temperature = 0.3) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for LLM API calls. ",
         "Install it with: install.packages(\"httr2\")", call. = FALSE)
  }
  api_key <- Sys.getenv("GEMINI_API_KEY", "")
  if (!nzchar(api_key)) {
    stop("GEMINI_API_KEY environment variable is not set.", call. = FALSE)
  }

  body <- list(
    model = model,
    temperature = temperature,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = user_prompt)
    )
  )

  # Rate-limit delay: Gemini free tier allows 5 RPM for 2.5 Pro (one
  # request per 12s). Default 15s provides margin for variable response
  # times. Override with options(algaware.gemini_delay = <seconds>).
  gemini_delay <- getOption("algaware.gemini_delay", 15)
  last_call <- .llm_state$gemini_last_call
  if (!is.null(last_call)) {
    elapsed <- as.numeric(difftime(Sys.time(), last_call, units = "secs"))
    if (elapsed < gemini_delay) {
      Sys.sleep(gemini_delay - elapsed)
    }
  }

  .llm_state$gemini_last_call <- Sys.time()

  resp <- httr2::request("https://generativelanguage.googleapis.com/v1beta/openai/chat/completions") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(
      max_tries = 5,
      # 429 = rate-limited; 503 = service overload -- both are transient
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429L, 503L),
      # Exponential backoff: 15 s, 30 s, 60 s, 120 s (capped)
      backoff = \(attempt) min(gemini_delay * 2^(attempt - 1L), 120)
    ) |>
    httr2::req_perform()

  .llm_state$gemini_last_call <- Sys.time()

  result <- httr2::resp_body_json(resp)
  strip_markdown(extract_llm_content(result))
}

#' Call an LLM provider
#'
#' Dispatches to \code{call_openai}, \code{call_gemini} or
#' \code{call_claude}. When \code{provider} is NULL, auto-detects from
#' available API keys.
#'
#' @param system_prompt System prompt string.
#' @param user_prompt User prompt string.
#' @param provider Character string: \code{"openai"}, \code{"gemini"} or
#'   \code{"claude"}. NULL (default) auto-detects.
#' @param temperature Sampling temperature (default: 0.3). Not sent to
#'   Claude, which rejects sampling parameters.
#' @return Character string with the generated text.
#' @keywords internal
call_llm <- function(system_prompt, user_prompt, provider = NULL,
                     temperature = 0.3) {
  if (is.null(provider)) provider <- llm_provider()
  switch(provider,
    openai = call_openai(system_prompt, user_prompt,
                         temperature = temperature),
    gemini = call_gemini(system_prompt, user_prompt,
                         temperature = temperature),
    claude = call_claude(system_prompt, user_prompt),
    stop("No LLM API key configured. Set OPENAI_API_KEY, GEMINI_API_KEY ",
         "or ANTHROPIC_API_KEY.", call. = FALSE)
  )
}

#' Does a provider support parallel requests?
#'
#' OpenAI and Anthropic accounts have per-minute rate limits comfortably
#' above the handful of concurrent station descriptions a report needs.
#' The Gemini free tier is limited to a few requests per minute (see
#' \code{call_gemini}), so it must stay sequential.
#'
#' @param provider Provider name, or NULL to auto-detect.
#' @return TRUE when independent requests may be performed concurrently.
#' @keywords internal
llm_supports_parallel <- function(provider = NULL) {
  if (is.null(provider)) provider <- llm_provider()
  provider %in% c("openai", "claude")
}

#' Build a request for a provider that supports parallel requests
#'
#' @param provider \code{"openai"} or \code{"claude"}.
#' @inheritParams call_llm
#' @return An httr2 request object.
#' @keywords internal
build_llm_request <- function(provider, system_prompt, user_prompt,
                              temperature = 0.3) {
  switch(provider,
    openai = build_openai_request(system_prompt, user_prompt,
                                  temperature = temperature),
    claude = build_claude_request(system_prompt, user_prompt),
    stop("Provider '", provider, "' does not support parallel requests.",
         call. = FALSE)
  )
}

#' Extract the generated text from a parsed provider response
#'
#' @param provider Provider name.
#' @param result Parsed JSON response body.
#' @return Length-1 character string.
#' @keywords internal
extract_llm_text <- function(provider, result) {
  if (identical(provider, "claude")) {
    extract_claude_content(result)
  } else {
    extract_llm_content(result)
  }
}

#' Call an LLM provider for a batch of independent prompts
#'
#' For providers that support it (OpenAI, Claude), all requests are
#' performed concurrently with \code{httr2::req_perform_parallel()},
#' collapsing n sequential round trips into roughly the latency of the
#' slowest one. Other providers fall back to a sequential loop (Gemini
#' keeps its rate-limit delay). Each prompt gets an independent result: one
#' failed request never aborts the batch.
#'
#' @param prompts List of prompts, each a list with \code{system} and
#'   \code{user} strings.
#' @param provider Character string: \code{"openai"}, \code{"gemini"} or
#'   \code{"claude"}. NULL (default) auto-detects.
#' @param temperature Sampling temperature (default: 0.3).
#' @param on_progress Optional callback \code{function(i, n)} invoked
#'   before each request in the sequential fallback (not used in the
#'   parallel path, where requests run at once).
#' @return A list the same length as \code{prompts}; each element is a
#'   list with \code{text} (character or NULL) and \code{error}
#'   (character or NULL).
#' @keywords internal
call_llm_batch <- function(prompts, provider = NULL, temperature = 0.3,
                           on_progress = NULL) {
  if (length(prompts) == 0) return(list())
  if (is.null(provider)) provider <- llm_provider()

  as_success <- function(text) list(text = text, error = NULL)
  as_failure <- function(msg) list(text = NULL, error = msg)

  if (llm_supports_parallel(provider)) {
    reqs <- lapply(prompts, function(p) {
      build_llm_request(provider, p$system, p$user, temperature = temperature)
    })
    resps <- httr2::req_perform_parallel(reqs, on_error = "continue",
                                         progress = FALSE, max_active = 5)
    return(lapply(resps, function(resp) {
      if (inherits(resp, "condition")) {
        return(as_failure(conditionMessage(resp)))
      }
      tryCatch({
        result <- httr2::resp_body_json(resp)
        as_success(strip_markdown(extract_llm_text(provider, result)))
      }, error = function(e) as_failure(conditionMessage(e)))
    }))
  }

  lapply(seq_along(prompts), function(i) {
    if (is.function(on_progress)) on_progress(i, length(prompts))
    tryCatch(
      as_success(call_llm(prompts[[i]]$system, prompts[[i]]$user,
                          provider = provider, temperature = temperature)),
      error = function(e) as_failure(conditionMessage(e))
    )
  })
}

#' Strip markdown formatting from LLM output
#'
#' Removes markdown bold/italic markers while preserving HAB asterisks
#' (asterisk directly after a word with no space).
#'
#' @param text Character string.
#' @return Cleaned character string.
#' @keywords internal
strip_markdown <- function(text) {
  # Remove **bold** markers
  text <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", text)
  # Remove *italic* markers (asterisk-word-asterisk with no adjacent letter)
  # But preserve HAB markers like "species_name*" (no closing asterisk)
  text <- gsub("(?<![\\w])\\*([^*]+)\\*(?![\\w])", "\\1", text, perl = TRUE)
  # Remove any leading/trailing whitespace
  trimws(text)
}
