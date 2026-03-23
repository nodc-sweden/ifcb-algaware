# Get the model name for a provider

Uses the environment variable `OPENAI_MODEL` or `GEMINI_MODEL` if set,
otherwise falls back to built-in defaults.

## Usage

``` r
llm_model_name(provider = llm_provider())
```

## Arguments

- provider:

  Character string: `"openai"` or `"gemini"`. Defaults to the active
  provider.

## Value

Character string with the model name.
