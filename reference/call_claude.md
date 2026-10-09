# Call the Claude API

Call the Claude API

## Usage

``` r
call_claude(
  system_prompt,
  user_prompt,
  model = llm_model_name("claude"),
  temperature = NULL
)
```

## Arguments

- system_prompt:

  System prompt string.

- user_prompt:

  User prompt string.

- model:

  Claude model name (default: `llm_model_name("claude")`).

- temperature:

  Ignored. Current Claude models reject sampling parameters; the
  argument is kept so all providers share one signature.

## Value

Character string with the generated text.
