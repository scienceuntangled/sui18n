#' Inject javascript needed by the sui_shinymod modules
#'
#' @return A script tag
#'
#' @seealso \code{\link{sui_shinymod_ui}}
#'
#' @export
sui_js <- function() {
    js <- readLines(system.file("extdata/js/i18n.js", package = "sui18n"))
    js <- paste(js, collapse = "\n")
    tags$head(tags$script(HTML(js)))
}

#' A Shiny module for internationalization
#'
#' @param id string: the id of the shiny element
#' @param input : see \code{\link[shiny]{callModule}}
#' @param output : see \code{\link[shiny]{callModule}}
#' @param session : see \code{\link[shiny]{callModule}}
#' @param csv_path string: path to the languages CSV file. See \code{\link{sui_translator}} for details
#'
#' @examples
#' \dontrun{
#'   ## in ui function
#'   sui_js()
#'   sui_shinymod_ui(id = "lang")
#'
#'   ## in server function
#'   callModule(sui_shinymod_server, id = "lang", csv_path = "optional/path/to/your/file")
#' }
#' @rdname sui_shinymod
#' @export
sui_shinymod_ui <- function(id) {
    ns <- NS(id)
    shinyWidgets::pickerInput(ns("select_lang"), label = NULL, choices = NULL, width = "80px")
}

#' @rdname sui_shinymod
#' @export
sui_shinymod_server <- function(input, output, session, csv_path = NULL) {
    this_i18n <- if (is.null(csv_path)) sui_translator() else sui_translator(csv_path = csv_path)
    this_i18n_lang <- reactiveVal(this_i18n$target())
    shiny::addResourcePath("sui_flags", system.file("extdata/flags", package = "sui18n"))

    ## update choices
    observe({
        langs <- tryCatch(setdiff(this_i18n$languages(), "key"), error = function(e) NULL)
        if (!is.null(langs)) {
            isolate(sel <- input$select_lang)
            if (is.null(sel) || !sel %in% langs) sel <- langs[1]
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
            myscr <- paste0('mytr = i18n.create({ values : ',
                            jsonlite::toJSON(setNames(as.list(dict[[input$select_lang]]), dict$en), auto_unbox = TRUE),
                            '});')
            shinyjs::runjs(myscr)
            ## run it
            shinyjs::runjs("translate_all()")
        }
    })

    list(i18n = this_i18n, i18n_lang = this_i18n_lang)
}
