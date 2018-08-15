output$VariablesPage = renderUI({
  fluidPage(
    fluidRow(HTML("<br>")),
    fluidRow(dataTableOutput("table_Variables") %>% withSpinner(type = 5, color = "#66bfdd") )
  )
})


# --------- Variables variables ----------
Variables = reactive({
  tmp = rbind(ConversionTab, 
              cbind(MOAS=names(DATA())[(!names(DATA()) %in% ConversionTab$MOAS)], 
                    Label=NA, Type=NA, Class=NA, Values=NA)) %>% 
    rename(Column=MOAS) %>% mutate(Number=seq(1, nrow(.))) %>% 
    select(Number, everything())
  return(tmp)
})

output$table_Variables = renderDataTable(options = list(pageLength = nrow(Variables())), {
  Variables()
})
