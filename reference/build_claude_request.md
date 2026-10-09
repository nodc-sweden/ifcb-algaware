# Build a Claude Messages API request

Shared by the single-call path (`call_claude`) and the parallel batch
path (`call_llm_batch`). The body is kept minimal so a model override
via `ANTHROPIC_MODEL` works across model generations: no `thinking`
block (current models run adaptive thinking by default) and no sampling
parameters (rejected with a 400 by current models).

## Usage

``` r
build_claude_request(
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

  Claude model name.

- temperature:

  Ignored; see `call_claude`.

## Value

An httr2 request object.
