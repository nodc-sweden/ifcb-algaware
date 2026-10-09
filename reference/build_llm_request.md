# Build a request for a provider that supports parallel requests

Build a request for a provider that supports parallel requests

## Usage

``` r
build_llm_request(provider, system_prompt, user_prompt, temperature = 0.3)
```

## Arguments

- provider:

  `"openai"` or `"claude"`.

- system_prompt:

  System prompt string.

- user_prompt:

  User prompt string.

- temperature:

  Sampling temperature (default: 0.3). Not sent to Claude, which rejects
  sampling parameters.

## Value

An httr2 request object.
