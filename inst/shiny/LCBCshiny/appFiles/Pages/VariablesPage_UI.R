output$VariablesPage = shiny::renderUI({
  shiny::fluidPage(
    shiny::fluidRow(shiny::HTML("<br>")),
    shiny::fluidRow(DT::dataTableOutput("table_Variables") %>%
                      shinycssloaders::withSpinner(type = 5, color = "#66bfdd"))
  )
})


# --------- Variables variables ----------
Variables = shiny::reactive({
  rbind(ConversionTab,
        cbind(MOAS=names(subDATA())[(!names(subDATA()) %in% ConversionTab$MOAS)],
              Label=NA, Type=NA, Class=NA, Values=NA)) %>%
    dplyr::rename(Column=MOAS) %>%
    dplyr::mutate(Number=seq(1, nrow(.))) %>%
    dplyr::select(Number, dplyr::everything())
})

output$table_Variables = DT::renderDataTable({
  Variables() %>%
    DT::datatable(options = list(scrollX = TRUE, pageLength = nrow(Variables())))
})
