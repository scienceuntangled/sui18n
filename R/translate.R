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
    tdata <- read.csv(csv_path, stringsAsFactors = FALSE, comment.char = "#")
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
            ## first try for exact (but case-insensitive) match
            out <- vapply(txt, function(z) {
                idx <- which(tolower(z) == tolower(tdata[[opts$from]]))
                if (length(idx) == 1) {
                    match_case(tdata[[opts$to]][idx], match_to = z, locale = opts$to)
                } else {
                    NA_character_
                }
            }, FUN.VALUE = "", USE.NAMES = FALSE)
            naidx <- which(is.na(out))
            if (length(naidx)) {
                ## now check for matches but discarding punctuation at the start or end of the text
                txt_parts <- str_match_all(txt[naidx], "^([[:punct:]]*)([^[:punct:]]*)([[:punct:]]*)$")
                trx <- vapply(txt_parts, function(this) {
                    if (!any(nzchar(this))) return("")
                    idx <- which(tolower(this[3]) == tolower(tdata[[opts$from]]))
                    if (length(idx) == 1) {
                        ## match capitalization
                        paste0(this[2], match_case(tdata[[opts$to]][idx], match_to = this[3], locale = opts$to), this[4])
                    } else {
                        paste0(this[2], this[3], this[4])
                    }
                }, FUN.VALUE = "", USE.NAMES = FALSE)
                out[naidx] <- trx
            }
            out
        },

        get_table = function() tdata
    )
}

match_case <- function(txt, match_to, locale) {
    if (is_uppercase(match_to)) {
        str_to_upper(txt, locale = locale)
    } else if (is_titlecase(match_to)) {
        str_to_title(txt, locale = locale)
    } else if (is_firstupper(match_to)) {
        paste0(str_to_upper(substr(txt, 1, 1), locale = locale), str_to_lower(substr(txt, 2, nchar(txt)), locale = locale))
    } else {
        txt
    }
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
