#' Construct i18n translator
#'
#' The CSV data should contain one column per language.
#'
#' @param to string: language to translate to, defaults to "en" (i.e. no translation)
#' @param csv_path string: path to the translation CSV file. \code{csv_path} can also be provided directly as a data.frame
#'
#' @return A list object. The translation functionality is provided by the \code{t} component. The \code{t} function will first look for an exact match between the input text and the reference data. If no exact match is found, further matching behaviour is controlled by the following options, which are applied in order:
#' \itemize{
#'  \item \code{underscores_as_spaces} (default = \code{TRUE}) - underscores in the input will be converted to spaces
#'  \item \code{as_regexp} (default = \code{FALSE}) - the input is treated as a regular expression. Note that the regular expression must match the entire input string
#'  \item \code{ignore_case} (default = \code{TRUE}) - allow case-insensitive matching
#'  \item \code{allow_punct} (default = \code{TRUE}) - allow additional punctuation and space characters at the beginning and end of the input
#' }
#' The \code{t} function also takes an \code{output_match_case} parameter. If \code{TRUE} an attempt will be made to match the case style of the output to the input (i.e. all upper case, titlecase, first-letter-only uppercase).
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
    for (ci in seq_len(ncol(tdata))) {
        tdata[[ci]] <- str_trim(gsub("\\\\n", "\n", tdata[[ci]]))
        tdata[[ci]] <- gsub("\\\\\\\\", "\\\\", tdata[[ci]])
    }
    tdata <- tdata[!apply(tdata, 1, function(z) all(is.na(z) | !nzchar(z))), ] ## drop empty rows
    tdata <- unique(tdata) ## drop duplicate rows
    lng <- colnames(tdata)
    opts <- list(languages = lng, to = if (length(lng) > 1) setdiff(lng, "key")[1] else lng[1],
                 from = if ("key" %in% lng) "key" else lng[1], warn_unmatched = FALSE)
    if (!missing(to) && !is.null(to)) {
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

        t = function(txt, underscores_as_spaces = TRUE, as_regexp = FALSE, ignore_case = TRUE, allow_punct = TRUE, output_match_case = TRUE) {
            if (opts$to == opts$from) return(txt)
            assert_that(is.flag(ignore_case), !is.na(ignore_case))
            assert_that(is.flag(as_regexp), !is.na(as_regexp))
            assert_that(is.flag(underscores_as_spaces), !is.na(underscores_as_spaces))
            assert_that(is.flag(allow_punct), !is.na(allow_punct))
            assert_that(is.flag(output_match_case), !is.na(output_match_case))
            txt0 <- txt
            ## first try for exact match
            out <- match_to_table(txt, tdata = tdata, from = opts$from, to = opts$to, ignore_case = FALSE, output_match_case = output_match_case)
            if (underscores_as_spaces) txt <- gsub("_", " ", txt) ## treat underscores as spaces
            naidx <- is.na(out)
            if (any(naidx) && as_regexp) {
                out[naidx] <- match_to_table(txt[naidx], tdata = tdata, from = opts$from, to = opts$to, as_regexp = TRUE, output_match_case = output_match_case)
                naidx <- is.na(out)
            }
            if (any(naidx) && ignore_case) {
                ## then exact (but case-insensitive) match
                out[naidx] <- match_to_table(txt[naidx], tdata = tdata, from = opts$from, to = opts$to, ignore_case = TRUE, output_match_case = output_match_case)
                naidx <- is.na(out)
            }
            ## now check for matches but discarding punctuation and spaces at the start or end of the text
            if (any(naidx) && allow_punct) {
                txt_parts <- str_match_all(txt[naidx], "^([[:punct:]=[:space:]]*)(.+?)([[:punct:]=[:space:]]*)$") ## why isn't = considered part of punct here?
                trx <- vapply(txt_parts, function(this) {
                    if (!any(nzchar(this))) return(NA_character_)
                    idx <- if (ignore_case) which(tolower(this[3]) == tolower(tdata[[opts$from]])) else which(this[3] == tdata[[opts$from]])
                    if (length(idx) == 1 && !is.na(tdata[[opts$to]][idx])) {
                        ## match capitalization
                        paste0(this[2], if (output_match_case) match_case(tdata[[opts$to]][idx], match_to = this[3], locale = opts$to) else tdata[[opts$to]][idx], this[4])
                    } else {
                        NA_character_
                    }
                }, FUN.VALUE = "", USE.NAMES = FALSE)
                out[naidx] <- trx
            }
            ## and catch anything that did not match above and replace with input
            out[!nzchar(txt0)] <- ""
            idx <- is.na(out) & nzchar(txt0) & !is.na(txt0)
            if (any(idx) && opts$warn_unmatched) {
                warning("inputs without matching entries in the i18n data:\n", paste(txt[idx], collapse = "\n", sep = "\n"))
            }
            out[idx] <- txt0[idx]
            out
        },

        get_table = function() tdata
    )
}

match_to_table <- function(txt, tdata, from, to, ignore_case = TRUE, as_regexp = FALSE, output_match_case = TRUE) {
    this_tdata <- if (ignore_case && !as_regexp) tolower(tdata[[from]]) else tdata[[from]]
    vapply(txt, function(z) {
        if (as_regexp) {
            idx <- which(vapply(this_tdata, function(re) tryCatch(grepl(paste0("^", re, "$"), z, ignore.case = ignore_case), error = function(e) FALSE), FUN.VALUE = TRUE, USE.NAMES = FALSE))
        } else {
            idx <- if (ignore_case) which(tolower(z) == this_tdata) else which(z == this_tdata)
        }
        if (length(idx) == 1) {
            if (as_regexp) {
                out <- sub(this_tdata[idx], tdata[[to]][idx], z)
                ##print(this_tdata[idx])
                ##print(tdata[[to]][idx])
                ##print(out)
            } else {
                out <- tdata[[to]][idx]
            }
            if (output_match_case) out <- match_case(out, match_to = z, locale = to)
            out
        } else {
            NA_character_
        }
    }, FUN.VALUE = "", USE.NAMES = FALSE)
}

## operates on single string input txt
match_case <- function(txt, match_to, locale) {
    if (is.na(txt) || !nzchar(txt)) return(txt)
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
