
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sui18n

<!-- badges: start -->

[![R-CMD-check](https://github.com/scienceuntangled/sui18n/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/scienceuntangled/sui18n/actions/workflows/R-CMD-check.yaml)
[![R build
status](https://github.com/scienceuntangled/sui18n/workflows/test-coverage/badge.svg)](https://github.com/scienceuntangled/sui18n/actions)
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

Note that the default translation data is probably of no value to you
(unless you have an unhealthy interest in volleyball), and you will
probably want to create your own:

``` r
library(tibble)
my_data <- tribble(~key, ~en, ~fr, ~es, ~cat,
                   "greeting", "hello", "bonjour", "hola", "miaow")
tr <- sui_translator(to = "en", csv_path = my_data)

tr$t("greeting")
#> [1] "hello"

tr$set_target("cat")
tr$t("greeting")
#> [1] "miaow"
```

(Or point `csv_path` to a CSV file with similar structure.)

## Using in Shiny apps

In the UI function, include

``` r
sui_js()
```

somewhere to inject the required javascript and

``` r
sui_shinymod_ui(id = "something")
```

to insert the language selector UI element at an appropriate spot in the
UI.

Then in your server function:

``` r
my_i18n <- callModule(sui_shinymod_server, id = "something", csv_path = "/path/to/csv")
```

With those pieces in place, you can then use elements in the UI with a
`lang_key` attribute:

``` r
tags$p(lang_key = "hello")
```

The content of that `p` tag will be automatically populated with the
translation of `lang_key` value, and it will change whenever the user
changes their selected language. This approach doesn’t require any extra
reactive code, so it keeps your apps uncluttered. However, it is limited
to simple tags (the translated text is inserted into the
`element.innerHTML` attribute of the tag). This is actually more
flexible than you might think — for example, you can do:

``` r
tabPanel(title = tags$span(lang_key = "My tab title"), ...)
```

or

``` r
selectInput("select_id", label = tags$span(lang_key = "My selector title"), ...)
```

However, if you need more complex UI structures, you can also use the
`my_i18n` object directly. In your UI:

``` r
uiOutput("some_element")
```

And in the server:

``` r
output$some_element <- renderUI({
    my_i18n$i18n_lang() ## make this expression reactive to the selected language
    tags$p(my_i18n$i18n$t("hello"))
})
```

-----

`sui18n` makes use of:

  - the [i18njs](https://github.com/roddeh/i18njs) internationalisation
    library for JS projects. MIT License. Copyright (c) 2013-2018 Simon
    Rodwell
  - flag images from
    [flag-icons](https://github.com/yammadev/flag-icons). A beautiful
    svg + png + sass + css collection of 261 flags. MIT License.
    Copyright (c) 2017 Yefferson
