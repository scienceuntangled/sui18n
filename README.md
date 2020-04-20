
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sui18n

<!-- badges: start -->

<!-- badges: end -->

## Installation

``` r
remotes::install_github("scienceuntangled/sui18n")
```

## Example usage

``` r
library(sui18n)

tr <- sui_translator()
tr$set_target("fr")
tr$t("hello")
#> [1] "bonjour"
```

Note that the default translation data is probably of no value to you,
and you will probably want to create your own:

``` r
library(tibble)
my_data <- tribble(~en, ~fr, ~it,
                   "goodbye", "au revior", "ciao")
tr <- sui_translator(to = "fr", csv_path = my_data)

tr$t("goodbye")
#> [1] "au revior"

tr$set_target("it")
tr$t("goodbye")
#> [1] "ciao"
```

(Or point `csv_path` to a CSV file with similar structure.)

## Using in Shiny apps

In the UI function, call

``` r
sui_js()
```

somewhere to inject the required javascript and

``` r
sui_shinymod_ui(id = "something")
```

to insert the language selector UI element at an appropriate place in
the UI.

Then in your server function:

``` r
my_i18n <- callModule(sui_shinymod_server, id = "something", csv_path = "/path/to/csv")
```

With that scaffolding in place, you can then use elements in the UI with
a `lang_key` attribute:

``` r
tags$p(lang_key = "hello")
```

And the content of that tag will be populated with the translation of
`lang_key` value. You can also use the `my_i18n` object directly in the
server:

``` r
output$some_element <- renderText({
    blah <- mi18n$i18n_lang() ## make this expression reactive to the selected language
    my_i18n$i18n$t("hello")
})
```

-----

`sui18n` uses:

  - the [i18njs](https://github.com/roddeh/i18njs) internationalisation
    library for JS projects. MIT License. Copyright (c) 2013-2018 Simon
    Rodwell
  - flag images from
    [flag-icons](https://github.com/yammadev/flag-icons). A beautiful
    svg + png + sass + css collection of 261 flags. MIT License.
    Copyright (c) 2017 Yefferson