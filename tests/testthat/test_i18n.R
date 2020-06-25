context("general tests")

test_that("text encoding in translations file looks OK", {
    tr <- sui_translator()
    for (lng in tr$languages()) {
        invalid_chars <- !validUTF8(tr$get_table()[[lng]])
        expect_equal(sum(invalid_chars), 0)
    }
})

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
    tr <- sui_translator("fr", csv_path = data.frame(en = c("err", "cat dog"), fr = c("grr", "chat chien"), stringsAsFactors = FALSE))
    expect_equal(tr$t(c("%err", "% err", "%  ERR", "err%", "ERR %", "err  %", "=err", "eRr =")),
                 c("%grr", "% grr", "%  GRR", "grr%", "GRR %", "grr  %", "=grr", "grr ="))

    ## test other options
    expect_equal(tr$t("cat dog"), "chat chien") ## default options
    expect_equal(tr$t("Cat dog"), "Chat chien") ## first upper
    expect_equal(tr$t("Cat Dog"), "Chat Chien") ## titlecase
    expect_equal(tr$t("CAT DOG"), "CHAT CHIEN") ## all upper
    expect_equal(tr$t("CAT DOG", output_match_case = FALSE), "chat chien") ## no output case matching
    expect_equal(tr$t("cAt dog"), "chat chien") ## can't match case, so return as data entry value which is all lower case
    expect_equal(tr$t("cAt dog", ignore_case = FALSE), "cAt dog") ## no match

    ## to test
    ## as_regexp = TRUE
    ## allow_punct = FALSE
})

test_that("operation with 'key' column is ok", {
    tr <- sui_translator(csv_path = data.frame(en = "cat", key = "miaow", fr = "chat", stringsAsFactors = FALSE))
    expect_equal(tr$languages(), c("key", "en", "fr"))
    expect_equal(tr$from(), "key") ## should be chosen by default
    expect_equal(tr$target(), "en") ## should be chosen by default
    expect_equal(tr$t("miaow"), "cat")
    tr$set_from("fr")
    tr$set_target("key")
    expect_equal(tr$t("chat"), "miaow")
})
