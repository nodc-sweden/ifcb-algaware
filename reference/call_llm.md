# Call an LLM provider

Dispatches to `call_openai` or `call_gemini`. When `provider` is NULL,
auto-detects from available API keys.

## Usage

``` r
call_llm(system_prompt, user_prompt, provider = NULL, temperature = 0.3)
```

## Arguments

- system_prompt:

  System prompt string.

- user_prompt:

  User prompt string.

- provider:

  Character string: `"openai"` or `"gemini"`. NULL (default)
  auto-detects.

- temperature:

  Sampling temperature (default: 0.3).

## Value

Character string with the generated text.
