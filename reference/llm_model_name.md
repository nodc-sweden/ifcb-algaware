# Get the model name for a provider

Uses the environment variable `OPENAI_MODEL`, `GEMINI_MODEL` or
`ANTHROPIC_MODEL` if set, otherwise falls back to built-in defaults.

## Usage

``` r
llm_model_name(provider = llm_provider())
```

## Arguments

- provider:

  Character string: `"openai"`, `"gemini"` or `"claude"`. Defaults to
  the active provider.

## Value

Character string with the model name.
