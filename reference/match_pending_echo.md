# Match a selectize change event against the pending programmatic-update queue

The gallery keeps the class selectize in sync with the current class
index by calling `updateSelectizeInput()`. Each such call echoes back as
an `input$class_select` change event. To stop those echoes from feeding
back into the index (which could ping-pong between two classes), every
pushed value is recorded in a queue and the matching echo is swallowed
here.

## Usage

``` r
match_pending_echo(queue, sel)
```

## Arguments

- queue:

  Character vector of values pushed to the widget, oldest first.

- sel:

  The incoming `input$class_select` value (length-1 character, possibly
  `NA`).

## Value

A list with `is_echo` (logical) and `queue` (the queue with consumed
entries removed when `is_echo` is `TRUE`).

## Details

The queue is drained through the *last* matching entry, not just the
head: rapid navigation can make Shiny coalesce several programmatic
updates and only echo the final value, so intermediate entries never
return.
