# Call an LLM provider

Dispatches to `call_openai`, `call_gemini` or `call_claude`. When
`provider` is NULL, auto-detects from available API keys.

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

  Character string: `"openai"`, `"gemini"` or `"claude"`. NULL (default)
  auto-detects.

- temperature:

  Sampling temperature (default: 0.3). Not sent to Claude, which rejects
  sampling parameters.

## Value

Character string with the generated text.
