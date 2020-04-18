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
})
