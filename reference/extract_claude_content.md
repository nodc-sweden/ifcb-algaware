# Extract and validate the text content of a Claude Messages API response

The Messages API returns a list of content blocks rather than `choices`;
with thinking enabled the first block can be an empty `thinking` block,
so the first `text` block is selected. A refusal arrives as HTTP 200
with `stop_reason = "refusal"` and a `max_tokens` stop means the text
was truncated; both raise so the callers' placeholder-text fallbacks
handle them, as for `extract_llm_content`.

## Usage

``` r
extract_claude_content(result)
```

## Arguments

- result:

  Parsed JSON body of a Messages API response.

## Value

Length-1 character string.
