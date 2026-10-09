# Point the current class index back at a class after the class list changed

The gallery addresses its class by position in the region's sorted class
list, so a class appearing or disappearing ahead of it (e.g. a class
emptied by a threshold being reset) would otherwise switch the gallery
to a neighbouring class.

## Usage

``` r
restore_current_class(rv, class_name)
```

## Arguments

- rv:

  Reactive values used by
  [`get_region_context()`](https://nodc-sweden.github.io/ifcb-algaware/reference/get_region_context.md).

- class_name:

  The class shown before the change, or `NULL`.

## Value

`NULL`, invisibly. When the class is gone, the index is only kept within
the class list.
