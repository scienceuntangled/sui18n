#' Construct SU i18n translator
#'
#' The CSV data should contain one column per language.
#'
#' @param to string: language to translate to, defaults to "en" (i.e. no translation)
#' @param csv_path string: path to the translation CSV file. \code{csv_path} can also be provided directly as a data.frame
#'
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
    if (is.data.frame(csv_path)) {
        tdata <- csv_path
    } else {
        tdata <- read.csv(csv_path, stringsAsFactors = FALSE, comment.char = "@")
        tdata <- tdata[, !grepl("^X", colnames(tdata))] ## drop unnamed cols
    }
    for (ci in seq_len(ncol(tdata))) tdata[[ci]] <- str_trim(gsub("\\\\n", "\n", tdata[[ci]]))
    tdata <- tdata[!apply(tdata, 1, function(z) all(is.na(z) | !nzchar(z))), ] ## drop empty rows
    lng <- colnames(tdata)
    opts <- list(languages = lng, to = if (length(lng) > 1) setdiff(lng, "key")[1] else lng[1],
                 from = if ("key" %in% lng) "key" else lng[1], warn_unmatched = FALSE)
    if (!missing(to)) {
        if (to %in% lng) {
            opts$to <- to
        } else {
            stop("language '", to, "' not available")
        }
    }

    list(
        languages = function() opts$languages,
        set_target = function(to) {
            if (to %in% opts$languages) {
                opts$to <<- to
                invisible(to)
            } else {
                stop("language '", to, "' not available")
            }
        },
        target = function() opts$to,

        set_from = function(from) {
            if (from %in% opts$languages) {
                opts$from <<- from
                invisible(from)
            } else {
                stop("language '", from, "' not available")
            }
        },
        from = function() opts$from,

        warn_unmatched = function(x) {
            if (!missing(x)) {
                stopifnot(is.logical(x) && length(x) == 1 & !is.na(x))
                opts$warn_unmatched <<- x
            }
            invisible(opts$warn_unmatched)
        },

        t = function(txt) {
            if (opts$to == opts$from) return(txt)
            txt0 <- txt
            ## first try for exact match
            out <- match_to_table(txt, tdata = tdata, from = opts$from, to = opts$to, ignore_case = FALSE)
            naidx <- is.na(out)
            if (any(naidx)) {
                ## then exact (but case-insensitive) match
                out[naidx] <- match_to_table(txt[naidx], tdata = tdata, from = opts$from, to = opts$to)
            }
            naidx <- is.na(out)
            if (any(naidx)) {
                ## allow for underscores instead of spaces
                txt <- gsub("_", " ", txt)
                ## check for match but ignoring any of "%#+!-/=" at start or end of string
                pcodes <- "[%#+!/=-][[:space:]]*"
                idx2 <- naidx & grepl(paste0("^", pcodes), txt)
                if (any(idx2)) {
                    idx2 <- which(idx2)
                    temp <- match_to_table(sub(paste0("^", pcodes), "", txt[idx2]), tdata = tdata, from = opts$from, to = opts$to) ## matched text
                    ## discard NA's in temp, which are non-matches
                    idx2 <- idx2[!is.na(temp)]
                    temp <- temp[!is.na(temp)]
                    temp1 <- str_match(txt[idx2], paste0("^(", pcodes, ")"))[, 2]
                    if (length(temp1) == length(temp)) out[idx2] <- paste0(temp1, temp)
                }
                pcodes <- "[[:space:]]*[%#+!/=-]"
                idx2 <- naidx & grepl(paste0(pcodes, "$"), txt)
                if (any(idx2)) {
                    idx2 <- which(idx2)
                    temp <- match_to_table(sub(paste0(pcodes, "$"), "", txt[idx2]), tdata = tdata, from = opts$from, to = opts$to) ## matched text
                    idx2 <- idx2[!is.na(temp)]
                    temp <- temp[!is.na(temp)]
                    temp1 <- str_match(txt[idx2], paste0("(", pcodes, ")$"))[, 2]
                    if (length(temp1) == length(temp)) out[idx2] <- paste0(temp, temp1)
                }
            }
            ## now check for matches but discarding punctuation and spaces at the start or end of the text
            naidx <- is.na(out)
            if (any(naidx)) {
                txt_parts <- str_match_all(txt[naidx], "^([[:punct:][:space:]]*)([^[:punct:]]*)([[:punct:][:space:]]*)$")
                trx <- vapply(txt_parts, function(this) {
                    if (!any(nzchar(this))) return("")
                    idx <- which(tolower(this[3]) == tolower(tdata[[opts$from]]))
                    if (length(idx) == 1) {
                        ## match capitalization
                        paste0(this[2], match_case(tdata[[opts$to]][idx], match_to = this[3], locale = opts$to), this[4])
                    } else {
                        NA_character_
                    }
                }, FUN.VALUE = "", USE.NAMES = FALSE)
                out[naidx] <- trx
            }
            ## and catch anything that did not match above and replace with input
            idx <- (!nzchar(out) | is.na(out)) & nzchar(txt0) & !is.na(txt0)
            if (any(idx) && opts$warn_unmatched) {
                warning("inputs without matching entries in the i18n data:\n", paste(txt[idx], collapse = "\n", sep = "\n"))
            }
            out[idx] <- txt0[idx]
            out
        },

        get_table = function() tdata
    )
}

match_to_table <- function(txt, tdata, from, to, ignore_case = TRUE) {
    this_tdata <- if (ignore_case) tolower(tdata[[from]]) else tdata[[from]]
    vapply(txt, function(z) {
        idx <- if (ignore_case) which(tolower(z) == this_tdata) else which(z == this_tdata)
        if (length(idx) == 1) {
            out <- tdata[[to]][idx]
            if (ignore_case) out <- match_case(out, match_to = z, locale = to)
            out
        } else {
            NA_character_
        }
    }, FUN.VALUE = "", USE.NAMES = FALSE)
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
    !is_uppercase(z) && !any(grepl("\\<[[:lower:]]", z)) && grepl("[[:space:]]", str_trim(z)) ## needs to be more than one word, too
}

is_firstupper <- function(z) {
    if (!nzchar(z)) return(FALSE)
    grepl("[[:upper:]]", substr(z, 1, 1)) && !grepl("[[:upper:]]", substr(z, 2, nchar(z)))
}
