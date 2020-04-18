#' Construct SU i18n translator
#'
#' @param to string: language to translate to, defaults to "en" (i.e. no translation)
#' @param csv_path string: path to the translation CSV file
#' @return A list
#'
#' @examples
#'
#' tr <- sui_translator()
#' tr$t("hello")
#' tr$set_target("fr")
#' tr$t("hello")
#' tr$t("Hello")
#' tr$t("Hello!")
#'
#' @export
sui_translator <- function(to, csv_path = system.file("extdata/su_translations.csv", package = "sui18n")) {
    tdata <- read.csv(csv_path, stringsAsFactors = FALSE)
    for (ci in seq_len(ncol(tdata))) tdata[[ci]] <- str_trim(tdata[[ci]])
    lng <- colnames(tdata)
    opts <- list(languages = lng, to = lng[1], from = lng[1]) ## always from en
    if (!missing(to)) {
        if (to %in% lng) {
            opts$to <- to
        } else {
            stop("language '", to, "' not available")
        }
    }

    list(
        set_target = function(to) {
            if (to %in% opts$languages) {
                opts$to <<- to
            } else {
                stop("language '", to, "' not available")
            }
        },

        target = function() opts$to,

        t = function(txt) {
            if (opts$to == opts$from) return(txt)
            txt_parts <- str_match_all(txt, "^([[:punct:]]*)([^[:punct:]]*)([[:punct:]]*)$")
            trx <- lapply(txt_parts, function(this) {
                if (!nzchar(this)) return(this)
                idx <- grep(tolower(this[3]), tolower(tdata[[opts$from]]), fixed = TRUE)
                if (length(idx) == 1) {
                    ## match capitalization
                    wout <- tdata[[opts$to]][idx]
                    if (is_uppercase(this[3])) {
                        paste0(this[2], str_to_upper(wout, locale = opts$to), this[4])
                    } else if (is_titlecase(this[3])) {
                        paste0(this[2], str_to_title(wout, locale = opts$to), this[4])
                    } else if (is_firstupper(this[3])) {
                        paste0(this[2], str_to_upper(substr(wout, 1, 1), locale = opts$to),
                               str_to_lower(substr(wout, 2, nchar(wout)), locale = opts$to), this[4])
                    } else {
                        paste0(this[2], wout, this[4])
                    }
                } else {
                    this[3]
                }
            })
            unlist(trx)
        }
    )
}

is_uppercase <- function(z) {
    if (!nzchar(z)) return(FALSE)
    z <- strsplit(z, "")[[1]]
    all(grepl("[[:upper:]]", z) | grepl("[^[:alpha:]]", z))
}

is_titlecase <- function(z) {
    if (!nzchar(z)) return(FALSE)
    !is_uppercase(z) && !any(grepl("\\<[[:lower:]]", z))
}

is_firstupper <- function(z) {
    if (!nzchar(z)) return(FALSE)
    grepl("[[:upper:]]", substr(z, 1, 1)) && !grepl("[[:upper:]]", substr(z, 2, nchar(z)))
}
