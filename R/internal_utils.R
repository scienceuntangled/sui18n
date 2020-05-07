is_uppercase <- function(z) {
    if (!nzchar(z)) return(FALSE)
    z <- strsplit(z, "")[[1]]
    all(grepl("[[:upper:]]", z) | grepl("[^[:alpha:]]", z))
}

is_titlecase <- function(z) {
    if (!nzchar(z)) return(FALSE)
    !is_uppercase(z) && !any(grepl("\\<[[:lower:]]", z)) && !any(grepl("\\<[[:upper:]][[:upper:]]", z)) && grepl("[[:space:]]", str_trim(z)) ## needs to be more than one word, too
}

is_firstupper <- function(z) {
    if (!nzchar(z)) return(FALSE)
    grepl("[[:upper:]]", substr(z, 1, 1)) && !grepl("[[:upper:]]", substr(z, 2, nchar(z)))
}


punct_str_to_upper <- function(x, locale) {
    leading_punct <- str_match(x, "^([^[:alnum:]]*)")[1, 1]
    paste0(leading_punct, str_to_upper(sub("^([^[:alnum:]]*)", "", x), locale = locale))
}

punct_str_to_title <- function(x, locale) {
    leading_punct <- str_match(x, "^([^[:alnum:]]*)")[1, 1]
    paste0(leading_punct, str_to_title(sub("^([^[:alnum:]]*)", "", x), locale = locale))
}

punct_str_to_firstupper <- function(x, locale) {
    leading_punct <- str_match(x, "^([^[:alnum:]]*)")[1, 1]
    x <- sub("^([^[:alnum:]]*)", "", x)
    paste0(leading_punct, str_to_upper(substr(x, 1, 1), locale = locale), str_to_lower(substr(x, 2, nchar(x)), locale = locale))
}
