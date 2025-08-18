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
#' The \code{t} function also takes an \code{output_match_case} parameter. If \code{TRUE} an attempt will be made to match the case style of the output to the input (i.e. all upper case, titlecase, first-letter-only uppercase). This is currently \code{TRUE} by default except when translating to German, but this behaviour might change.
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
        tdata <- read.csv(csv_path, stringsAsFactors = FALSE, comment.char = "@", encoding = "UTF-8", check.names = FALSE)
        tdata <- tdata[, !grepl("^X", colnames(tdata))] ## drop unnamed cols
    }
    for (ci in seq_len(ncol(tdata))) {
        tdata[[ci]] <- str_trim(gsub("\\\\n", "\n", tdata[[ci]]))
        tdata[[ci]] <- gsub("\\\\\\\\", "\\\\", tdata[[ci]])
    }
    tdata <- tdata[!apply(tdata, 1, function(z) all(is.na(z) | !nzchar(z))), ] ## drop empty rows
    tdata <- unique(tdata) ## drop duplicate rows
    lng <- c(if ("key" %in% colnames(tdata)) "key", if ("en" %in% colnames(tdata)) "en", sort(setdiff(colnames(tdata), c("key", "en"))))
    opts <- list(languages = lng, to = if (length(lng) > 1) setdiff(lng, "key")[1] else lng[1],
                 from = if ("key" %in% lng) "key" else lng[1], warn_unmatched = FALSE, warn_untranslated = FALSE, log_unmatched = NULL, log_untranslated = NULL)
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
            ## if set to TRUE, issue a warning for text that does not appear in our dictionary
            if (!missing(x)) {
                stopifnot(is.logical(x) && length(x) == 1 & !is.na(x))
                opts$warn_unmatched <<- x
            }
            invisible(opts$warn_unmatched)
        },

        warn_untranslated = function(x) {
            ## if set to TRUE, issue a warning for text that appears in our dictionary but does not have a translation for the target language
            if (!missing(x)) {
                stopifnot(is.logical(x) && length(x) == 1 & !is.na(x))
                opts$warn_untranslated <<- x
            }
            invisible(opts$warn_untranslated)
        },

        log_unmatched = function(x) {
            ## if non-NULL, log to that file any text that does not appear in our dictionary
            if (!missing(x)) opts$log_unmatched <<- x
            invisible(opts$log_unmatched)
        },

        log_untranslated = function(x) {
            ## if non-NULL, log to that file any text that appears in our dictionary but does not have a translation for the target language
            if (!missing(x)) opts$log_untranslated <<- x
            invisible(opts$log_untranslated)
        },

        ## output_match_case defaults to TRUE for all languages other than "de"
        ## TODO add a way to set these parm defaults for the instantiated object, so that if we want non-default behaviour they don't have to be specified in every object$t call
        t = function(txt, underscores_as_spaces = TRUE, as_regexp = FALSE, ignore_case = TRUE, allow_punct = TRUE, output_match_case) {
            if (opts$to == opts$from) return(txt)
            if (missing(output_match_case)) output_match_case <- !opts$to %in% c("de")
            assert_that(is.flag(ignore_case), !is.na(ignore_case))
            assert_that(is.flag(as_regexp), !is.na(as_regexp))
            assert_that(is.flag(underscores_as_spaces), !is.na(underscores_as_spaces))
            assert_that(is.flag(allow_punct), !is.na(allow_punct))
            assert_that(is.flag(output_match_case), !is.na(output_match_case))
            txt0 <- txt
            ## first try for exact match
            temp <- match_to_table2(txt, tdata = tdata, from = opts$from, to = opts$to, ignore_case = FALSE, output_match_case = output_match_case)
            out <- temp$text
            entry_found <- temp$entry_found ## so we can differentiate text that could not be matched to an entry, from text that had an entry but no translation
            if (underscores_as_spaces) txt <- gsub("_", " ", txt) ## treat underscores as spaces
            naidx <- is.na(out) & !entry_found
            if (any(naidx) && as_regexp) {
                temp <- match_to_table2(txt[naidx], tdata = tdata, from = opts$from, to = opts$to, as_regexp = TRUE, output_match_case = output_match_case)
                out[naidx] <- temp$text
                entry_found[naidx] <- entry_found[naidx] | temp$entry_found
                naidx <- is.na(out) & !entry_found
            }
            if (any(naidx) && ignore_case) {
                ## then exact (but case-insensitive) match
                temp <- match_to_table2(txt[naidx], tdata = tdata, from = opts$from, to = opts$to, ignore_case = TRUE, output_match_case = output_match_case)
                out[naidx] <- temp$text
                entry_found[naidx] <- entry_found[naidx] | temp$entry_found
                naidx <- is.na(out) & !entry_found
            }
            ## now check for matches but discarding punctuation and spaces at the start or end of the text
            ## note!
            ## str_detect("+", "[[:punct:]]")
            ## [1] FALSE
            ## > grepl("[[:punct:]]", "+")
            ## [1] TRUE
            ## punct according to `? regexp` is ‘ ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~ ’
            ## but these are not detected as punct by stringr: "$" "+" "<" "=" ">" "^" "`" "|" "~"
            if (any(naidx) && allow_punct) {
                txt_parts <- str_match_all(txt[naidx], "^([[:punct:]\\$\\+<=>\\^`\\|~[:space:]]*)(.+?)([[:punct:]\\$\\+<=>\\^`\\|~[:space:]]*)$")
                ## TODO if translating to Spanish and the input ends with ?, add ¿ to the start. But need to search backwards from each ? to find either the start of that sentence (which might be the start of the string)
                tempidx <- vapply(txt_parts, function(this) {
                    if (!any(nzchar(this))) return(NA_integer_)
                    idx <- if (ignore_case) which(tolower(this[3]) == tolower(tdata[[opts$from]])) else which(this[3] == tdata[[opts$from]])
                    if (length(idx) == 1) idx else NA_integer_
                }, FUN.VALUE = 1L, USE.NAMES = FALSE)
                trx <- vapply(seq_along(txt_parts), function(i) {
                    idx <- tempidx[i]
#                    if (is.na(tempidx[i])) return(NA_character_)^^^
#                    idx <- if (ignore_case) which(tolower(this[3]) == tolower(tdata[[opts$from]])) else which(this[3] == tdata[[opts$from]])
                    if (!is.na(idx) && !is.na(tdata[[opts$to]][idx])) {
                        ## match capitalization
                        this <- txt_parts[[i]]
                        paste0(this[2], if (output_match_case) match_case(tdata[[opts$to]][idx], match_to = this[3], locale = opts$to) else tdata[[opts$to]][idx], this[4])
                    } else {
                        NA_character_
                    }
                }, FUN.VALUE = "", USE.NAMES = FALSE)
                out[naidx] <- trx
                entry_found[naidx] <- entry_found[naidx] | !is.na(tempidx)
            }
            ## and catch anything that did not match above and replace with input
            out[!nzchar(txt0)] <- ""
            idx <- is.na(out) & nzchar(txt0) & !is.na(txt0)
            idxw <- is.na(out) & nzchar(txt0) & !is.na(txt0) & !grepl("^[[:digit:]\\.\\+\\-]+$", txt0) ## don't warn about purely numeric inputs
            if (any(idxw & !entry_found)) {
                if (opts$warn_unmatched) warning("inputs without matching entries in the i18n data: ", paste(txt[idx & !entry_found], collapse = "\n", sep = "\n"))
                if (!is.null(opts$log_unmatched)) try(cat(paste(txt[idx & !entry_found], collapse = "\n", sep = "\n"), "\n", sep = "", file = opts$log_unmatched, append = TRUE))
            }
            if (any(idxw & entry_found)) {
                if (opts$warn_untranslated) warning("inputs with matching entries but no translations in the i18n data: ", paste(txt[idx & entry_found], collapse = "\n", sep = "\n"))
                if (!is.null(opts$log_untranslated)) try(cat(paste(txt[idx & entry_found], collapse = "\n", sep = "\n"), "\n", sep = "", file = opts$log_untranslated, append = TRUE))
            }
            out[idx] <- txt0[idx]
            out
        },

        get_table = function() tdata
    )
}

match_to_table2 <- function(txt, tdata, from, to, ignore_case = TRUE, as_regexp = FALSE, output_match_case = TRUE) {
    this_tdata <- if (ignore_case && !as_regexp) tolower(tdata[[from]]) else tdata[[from]]
    iidx <- vapply(txt, function(z) {
        if (as_regexp) {
            suppressWarnings(idx <- which(vapply(this_tdata, function(re) tryCatch(grepl(paste0("^[[:space:]]*", re, "[[:space:]]*$"), z, ignore.case = ignore_case), error = function(e) FALSE), FUN.VALUE = TRUE, USE.NAMES = FALSE)))
        } else {
            idx <- if (ignore_case) which(tolower(z) == this_tdata) else which(z == this_tdata)
        }
        if (length(idx) == 1) idx else NA_integer_
    }, FUN.VALUE = 1L, USE.NAMES = FALSE)
    entry_found <- !is.na(iidx)
    out <- vapply(seq_along(txt), function(i) {
        idx <- iidx[i]
        z <- txt[i]
        if (!is.na(idx)) {
            if (as_regexp) {
                temp <- sub(this_tdata[idx], tdata[[to]][idx], z)
                ##print(this_tdata[idx])
                ##print(tdata[[to]][idx])
                ##print(temp)
            } else {
                temp <- tdata[[to]][idx]
            }
            if (output_match_case) temp <- match_case(temp, match_to = z, locale = to)
            temp
        } else {
            NA_character_
        }
    }, FUN.VALUE = "", USE.NAMES = FALSE)
    list(text = out, entry_found = entry_found)
}

## operates on single string input txt (the translated text) and match_to (the template we are matching to)
## 2025-08-14: we don't modify the case of txt to upper/firstupper/title if it is not all lower case. But convert txt to lowercase if it is not and if match_to is
match_case <- function(txt, match_to, locale) {
    if (is.na(txt) || !nzchar(txt)) return(txt)
    txt_lc <- is_lowercase(txt)
    if (is_uppercase(match_to) && txt_lc) {
        punct_str_to_upper(txt, locale = locale)
    } else if (is_firstupper(match_to) && txt_lc) { ## check for firstupper before title, because a single word is TRUE for both
        punct_str_to_firstupper(txt, locale = locale)
    } else if (is_titlecase(match_to) && txt_lc) {
        punct_str_to_title(txt, locale = locale)
    } else if (is_lowercase(match_to) && !txt_lc) {
        punct_str_to_lower(txt, locale = locale)
    } else {
        txt
    }
}
