x <- readxl::read_excel("su_translations.xlsx", col_names = FALSE)
## write the file long-hand, so that we retain control over comment lines and so on
csv_file <- "../inst/extdata/su_translations.csv"
unlink(csv_file)
## quote if necessary
qifn <- function(z) sapply(z, function(txt) if (grepl("[,\"]", txt)) paste0("\"", txt, "\"") else txt)
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
