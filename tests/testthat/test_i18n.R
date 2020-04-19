context("general tests")

test_that("general tests", {
    tr <- sui_translator()
    expect_equal(tr$t(c("hi", "hello!", "HELLO", "Hello")),
                 c("hi", "hello!", "HELLO", "Hello"))
    expect_error(tr$set_target("pl"), "not available")
    tr$set_target("fr")
    expect_equal(tr$t(c("hi", "hello!", "HELLO", "Hello")),
                 c("hi", "bonjour!", "BONJOUR", "Bonjour"))

    ## no entry should give the text back unchanged
    expect_equal(tr$t(c("bilbo baggins!", "BILBO Baggins", "Bilbo Baggins")),
                 c("bilbo baggins!", "BILBO Baggins", "Bilbo Baggins"))

    expect_error(tr <- sui_translator('pl'), "not available")

    ## initialize with the "to" language
    tr <- sui_translator("fr")
    expect_equal(tr$t(c("hi", "hello!", "HELLO", "Hello")),
                 c("hi", "bonjour!", "BONJOUR", "Bonjour"))

    ## warnings
    tr$warn_unmatched(TRUE)
    expect_warning(tr$t("unknownword"), "inputs without matching entries")

    ## cope with % and other symbols
    test_tdata <- data.frame(en = c("err"), fr = c("grr"), stringsAsFactors = FALSE)
    assign("tdata", test_tdata, envir = environment(tr$t))
    expect_equal(tr$t(c("%err", "% err", "%  ERR", "err%", "ERR %", "err  %", "=err", "eRr =")),
                 c("%grr", "% grr", "%  GRR", "grr%", "GRR %", "grr  %", "=grr", "grr ="))
})
