output$VariablesPage = shiny::renderUI({
  shiny::fluidPage(
    shiny::fluidRow(shiny::HTML("<br>")),
    shiny::fluidRow(DT::dataTableOutput("table_Variables") %>% shinycssloaders::withSpinner(type = 5, color = "#66bfdd") )
  )
})


# --------- Variables variables ----------
Variables = shiny::reactive({
  tmp = rbind(ConversionTab,
              cbind(MOAS=names(DATA())[(!names(DATA()) %in% ConversionTab$MOAS)],
                    Label=NA, Type=NA, Class=NA, Values=NA)) %>%
    plotly::rename(Column=MOAS) %>% dplyr::mutate(Number=seq(1, nrow(.))) %>%
    dplyr::select(Number, dplyr::everything())
  return(tmp)
})

output$table_Variables = shiny::renderDataTable(options = list(pageLength = nrow(Variables())), {
  Variables()
})
