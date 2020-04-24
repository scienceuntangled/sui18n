#' Inject javascript needed by the sui_shinymod modules
#'
#' @return A script tag
#'
#' @seealso \code{\link{sui_shinymod_ui}}
#'
#' @export
sui_js <- function() {
    js <- readLines(system.file("extdata/js/i18n.js", package = "sui18n"))
    tags$head(tags$script(HTML(paste(js, collapse = "\n"))))
}

#' A Shiny module for internationalization
#'
#' @param id string: the id of the shiny element
#' @param input : see \code{\link[shiny]{callModule}}
#' @param output : see \code{\link[shiny]{callModule}}
#' @param session : see \code{\link[shiny]{callModule}}
#' @param to string: as for \code{link{sui_translator}}
#' @param csv_path string: as for \code{\link{sui_translator}}
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'   app <- shinyApp(
#'       ui = fluidPage(
#'           sui_js(),
#'           sui_shinymod_ui(id = "lang"),
#'           tags$p(lang_key = "hello"), ## simple tags can be done like this
#'           uiOutput("another")
#'       ),
#'       server = function(input, output) {
#'           ms <- callModule(sui_shinymod_server, id = "lang")
#'           ## more complex UI elements can be done like this
#'           output$another <- renderUI({
#'               ms$i18n_lang()
#'               tags$p(ms$i18n$t("Please wait"))
#'           })
#'       }
#'   )
#'   runApp(app)
#' }
#' @rdname sui_shinymod
#' @export
sui_shinymod_ui <- function(id) {
    ns <- NS(id)
    shinyWidgets::pickerInput(ns("select_lang"), label = NULL, choices = NULL, width = "80px")
}

#' @rdname sui_shinymod
#' @export
sui_shinymod_server <- function(input, output, session, csv_path = NULL, to = NULL) {
    this_i18n <- if (is.null(csv_path)) sui_translator(to) else sui_translator(to, csv_path = csv_path)
    this_i18n_lang <- reactiveVal(this_i18n$target())
    shiny::addResourcePath("sui_flags", system.file("extdata/flags", package = "sui18n"))
    this_update_selection <- reactiveVal(NULL) ## manually force the selection to change

    ## update choices
    observe({
        langs <- tryCatch(setdiff(this_i18n$languages(), "key"), error = function(e) NULL)
        if (!is.null(langs)) {
            if (!is.null(this_update_selection())) {
                sel <- this_update_selection()
            } else {
                isolate(sel <- input$select_lang)
                if (is.null(sel) || !sel %in% langs) sel <- langs[1]
            }
            flgs <- langs
            flgs[flgs == "en"] <- "GB"
            flgs <- setNames(lapply(seq_along(flgs), function(fi) paste0("<img src=\"sui_flags/", toupper(flgs[fi]), ".svg\" />", toupper(langs[fi]))), langs)
            shinyWidgets::updatePickerInput(session = session, inputId = "select_lang", choices = langs, choicesOpt = list(content = unlist(flgs)), selected = sel)
        }
    })

    observeEvent(input$select_lang, {
        if (!is.null(input$select_lang) && input$select_lang %in% this_i18n$languages()) {
            this_i18n$set_target(input$select_lang)
            this_i18n_lang(input$select_lang)
            ## construct js-side translator for this language
            dict <- this_i18n$get_table()
            idx <- is.na(dict[[input$select_lang]])
            if (!is.null(this_i18n$warn_unmatched) && this_i18n$warn_unmatched()) dict[[input$select_lang]][idx] <- paste0('<span style="border:1px solid red;">', dict$key[idx], '</span>')
            myscr <- paste0('mytr = i18n.create({ values : ',
                            jsonlite::toJSON(setNames(as.list(dict[[input$select_lang]]), dict$en), auto_unbox = TRUE),
                            '});')
            evaljs(myscr)
            ## run it
            do_translate()
        }
    })

    do_translate <- function() evaljs("translate_all()")

    list(i18n = this_i18n, i18n_lang = this_i18n_lang, update_selection = this_update_selection, do_translate = do_translate)
}

evaljs <- function(expr) {
    shiny::getDefaultReactiveDomain()$sendCustomMessage("evaljs", expr)
}
