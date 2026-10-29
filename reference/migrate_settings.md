# Bring settings saved by an older algaware version up to date

Each step runs once: the migrated settings carry the current
`settings_version`, which is persisted on the next save, so later user
edits (e.g. removing a class again) are kept.

## Usage

``` r
migrate_settings(settings, saved_version)
```

## Arguments

- settings:

  Settings list with saved values merged over the defaults.

- saved_version:

  `settings_version` of the saved file (1 for files written before the
  field existed).

## Value

The migrated settings list.
