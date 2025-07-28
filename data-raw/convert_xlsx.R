x <- readxl::read_excel("su_translations.xlsx", col_names = FALSE)
## write the file long-hand, so that we retain control over comment lines and so on
csv_file <- "../inst/extdata/su_translations.csv"
unlink(csv_file)
## quote if necessary, and convert " to '
qifn <- function(z) unname(sapply(z, function(txt) if (grepl(",", txt)) paste0("\"", gsub("[\"“”]", "'", txt), "\"") else gsub("[\"“”]", "'", txt)))
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
        writeLines(paste(qifn(x[l, ]), collapse = ","), con = con)
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
x <- read.csv("../inst/extdata/su_translations.csv", stringsAsFactors = FALSE, comment.char = "@", encoding = "UTF-8")
stopifnot(!any(grepl("^X", colnames(x)))) ## all cols named
if (any(duplicated(x$key))) {
    print(x[x$key %in% x$key[which(duplicated(x$key))], ])
    stop("duplicated key entries")
}
if (any(duplicated(tolower(x$key)))) {
    print(x[tolower(x$key) %in% tolower(x$key)[which(duplicated(tolower(x$key)))], ])
    warning("duplicated (ignoring case) key entries - may be OK?")
}
en_uppercase <- unlist(lapply(x$en, is_uppercase))
en_titlecase <- unlist(lapply(x$en, is_titlecase))
en_firstupper <- unlist(lapply(x$en, is_firstupper))
for (tgt in setdiff(colnames(x), c("en", "key"))) {
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
                cat("[", ii, "] en: ", x$en[ii], "\n[", ii, "] ", tgt, ": ", x[[tgt]][ii], "\n\n", sep = "")
            }
        }
        chk <- !grepl(paste0(punct, "$"), x[[tgt]]) & grepl(paste0(punct, "$"), x$en)
        if (any(chk)) {
            cat("Language ", tgt, " does not have an ending '", gsub("\\\\", "", punct), "' where English does:\n", sep = "")
            for (ii in which(chk)) {
                cat("[", ii, "] en: ", x$en[ii], "\n[", ii, "] ", tgt, ": ", x[[tgt]][ii], "\n\n", sep = "")
            }
        }
    }
}

