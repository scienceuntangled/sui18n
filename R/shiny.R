##i18n <- reactive({
##    selected <- input$selected_language
##    if (length(selected) > 0 && selected %in% translator$languages) {
##      translator$set_translation_language(selected)
##    }
##    translator
##  })
