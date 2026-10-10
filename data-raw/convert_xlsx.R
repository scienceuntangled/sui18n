x <- readxl::read_excel("su_translations.xlsx", col_names = FALSE)
## write the file long-hand, so that we retain control over comment lines and so on
csv_file <- "../inst/extdata/su_translations.csv"
unlink(csv_file)

## map full-width characters (used in Japanese source, esp punctuation) to standard
w2std <- function(z) {
    out <- sapply(z, function(zz) {
        zz <- utf8ToInt(zz)
        idx <- zz %in% (65281 + (0:93))
        zz[idx] <- zz[idx] - 65281 + 33
        intToUtf8(zz)
    })
    gsub("\u301c", "~", gsub("\u3002", ".", unname(out), fixed = TRUE), fixed = TRUE)
}

## quote if necessary, and convert " to '. Convert multiple spaces to single (but NOT for the wacky "n  srv" entry that needs it) and trim leading/trailing spaces
qifn <- function(z, no_collapse_spaces = FALSE, remove_trailing_colon = FALSE) {
    cs <- if (no_collapse_spaces) identity else function(w) gsub("[[:space:]]+", " ", w)
    out <- sapply(z, function(txt) {
        ## replace alternative quote chars, collapse spaces
        y <- cs(if (grepl(",", txt)) paste0("\"", gsub("[\"\u201c\u201d\u2018\u2019]", "'", txt), "\"") else gsub("[\"\u201c\u201d\u2018\u2019]", "'", txt))
        y <- stringr::str_trim(w2std(y)) ## replace wide chars, trim whitespace
        if (remove_trailing_colon) y <- sub("[[:space:]]*:$", "", y)
        y
    })
    unname(out)
}
con  <- file(csv_file, open = "wt", encoding = "UTF-8")
for (l in seq_len(nrow(x))) {
    if (all(is.na(x[l, ]))) {
        ## blank line
        writeLines("", con = con)
    } else if (grepl("^@", x[[l, 1]])) {
        ## comment line
        writeLines(x[[l, 1]], con = con)
        if (!all(is.na(x[l, -1]))) {
            close(con)
            stop("non-empty columns in comment row")
        }
    } else {
        rem_tr_c <- x[l, 1] %in% c("crunch time is defined as", "matches selected from a total of", "no_app_access", "converting to", "plots show", "but not")
        ## to check: "matches selected from a total of"
        ## not: "minimum number of sets", "attack chart style", see below
        ## also: "no_app_access" ("You do not have access to this application ") - is not actually used? see common_server_sc
        writeLines(paste(qifn(x[l, ], no_collapse_spaces = isTRUE(x[[l, 1]] == "n  srv"), remove_trailing_colon = rem_tr_c), collapse = ","), con = con)
        ## some lines need duplication with/without case and punctuation matching because they are used directly by the js translator (must match exactly) and the R translator (but used here without the punctuation)
        if (x[[l, 1]] %in% c("Minimum number of sets:", "Attack chart style:")) {
            writeLines(paste(qifn(tolower(sub("[:\uff1a]$", "", x[l, ]))), collapse = ","), con = con)
        }
        if (x[[l, 1]] %in% c("BP_won", "indiv_B%")) {
            writeLines(paste(qifn(tolower(gsub("_", " ", x[l, ]))), collapse = ","), con = con)
        }
    }
}
close(con)

## quotes " now changed to ' in the regexp above
##for (cl in seq_len(ncol(x))[-1]) {
##    chk <- grep("\"", x[[cl]])
##    if (length(chk) > 0) stop("Embedded quotes in ", x[[cl]][2], " on rows ", paste(chk, collapse = ", "))
##}

## checks
source("../R/internal_utils.R")
library(stringr)
x <- read.csv(csv_file, stringsAsFactors = FALSE, comment.char = "@", encoding = "UTF-8")
stopifnot(!any(grepl("^X", colnames(x)))) ## all cols named
if (any(duplicated(x$key))) {
    print(x[x$key %in% x$key[which(duplicated(x$key))], 1, drop = FALSE])
    stop("duplicated key entries")
}
if (any(duplicated(tolower(x$key)))) {
    print(x[tolower(x$key) %in% tolower(x$key)[which(duplicated(tolower(x$key)))], ])
    warning("duplicated (ignoring case) key entries - may be OK?")
}
en_uppercase <- unlist(lapply(x$en, is_uppercase))
en_titlecase <- unlist(lapply(x$en, is_titlecase))
en_firstupper <- unlist(lapply(x$en, is_firstupper))
##for (tgt in setdiff(colnames(x), c("en", "key"))) {
for (tgt in "vn") {##setdiff(colnames(x), c("en", "key"))) {
    if (!tgt %in% "jp") {
        check_uppercase <- unlist(lapply(x[[tgt]], is_uppercase))
        check_titlecase <- unlist(lapply(x[[tgt]], is_titlecase))
        check_firstupper <- unlist(lapply(x[[tgt]], function(z) is_firstupper(sub("^¿", "", z))))
        chk <- check_uppercase != en_uppercase | check_titlecase != en_titlecase | check_firstupper != en_firstupper
        chk <- chk | (grepl("[A-Z]", substr(x$en, 1, 1)) & grepl("[a-z]", substr(x[[tgt]], 1, 1))) | (grepl("[a-z]", substr(x$en, 1, 1)) & grepl("[A-Z]", substr(x[[tgt]], 1, 1)))
        if (any(chk)) {
            cat("Language ", tgt, " case comparisons need checking:\n", sep = "")
            for (ii in which(chk)) {
                cat("[", ii, "] en: ", x$en[ii], "\n[", ii, "] ", tgt, ": ", x[[tgt]][ii], "\n\n", sep = "")
            }
        } else {
            cat("Language ", tgt, " case comparisons all passed.\n", sep = "")
        }
    } else {
        cat("Language ", tgt, " case comparisons skipped.\n", sep = "")
    }
    chk <- is.na(x[[tgt]]) | x$en==x[[tgt]]
    if (any(chk)) {
        cat("Language ", tgt, " identical/missing entries need checking:\n", sep = "")
        for (ii in which(chk)) {
            cat("[", ii, "] en: ", x$en[ii], "\n[", ii, "] ", tgt, ": ", x[[tgt]][ii], "\n\n", sep = "")
        }
    }

    chk <- grepl("\"", x[[tgt]], fixed = TRUE)
    ## this doesn't work, the double quotes get removed by xlread?
    if (any(chk)) {
        cat("Language ", tgt, " has embedded double quotes:\n", sep = "")
        for (ii in which(chk)) {
            cat("[", ii, "] en: ", x$en[ii], "\n[", ii, "] ", tgt, ": ", x[[tgt]][ii], "\n\n", sep = "")
        }
    }

    for (punct in c(":", "\\?", "\\.", "\\)", "\\]", "%")) {
        chk <- grepl(paste0(punct, "$"), x[[tgt]]) & !grepl(paste0(punct, "$"), x$en)
        if (any(chk)) {
            cat("Language ", tgt, " has ending '", gsub("\\\\", "", punct), "' where English does not:\n", sep = "")
            for (ii in which(chk)) {
                cat("[", ii, "] key: ", x$key[ii], "\n[", ii, "] en: ", x$en[ii], "\n[", ii, "] ", tgt, ": ", x[[tgt]][ii], "\n\n", sep = "")
            }
        }
        chk <- !grepl(paste0(punct, "$"), x[[tgt]]) & grepl(paste0(punct, "$"), x$en)
        if (any(chk)) {
            cat("Language ", tgt, " does not have an ending '", gsub("\\\\", "", punct), "' where English does:\n", sep = "")
            for (ii in which(chk)) {
                cat("[", ii, "] key: ", x$key[ii], "\n[", ii, "] en: ", x$en[ii], "\n[", ii, "] ", tgt, ": ", x[[tgt]][ii], "\n\n", sep = "")
            }
        }
    }

    chk <- grepl("[\n\r]", x[[tgt]])
    if (any(chk)) {
        cat("Language", tgt, "has line breaks:\n")
        for (ii in which(chk)) {
            cat("[", ii, "] key: ", x$key[ii], "\n[", ii, "] en: ", x$en[ii], "\n[", ii, "] ", tgt, ": ", x[[tgt]][ii], "\n\n", sep = "")
        }
    }
}

