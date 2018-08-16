# Subset DATA variables ----------

output$ColSelect_SubData = shiny::renderUI({
  # The options are dynamically generated on the server
  opts = names(DATA())[!(names(DATA()) %in% BaseCols)]
  shiny::selectizeInput('ExtraColsDATA', 'Add columns to your file', opts, selected = character(0),
                        multiple = T, options = list(placeholder = 'Type to start selecting'), width="100%")
})

output$widenTable = shiny::renderUI({
  if(input$actionDoubles == "asis"){
    shiny::radioButtons("actionTable", label="Table format",
                      choices=c("Keep as is (long)"="none", "Widen by site"="Site_Name", "Widen by site then by wave"="s2w"))
  }else{
    shiny::radioButtons("actionTable", label="Table format",
                        choices=c("Keep as is (long)"="none", "Widen by site"="Site_Name", "Widen by wave"="Project_Wave", "Widen by time point"="Subject_Timepoint"))
  }
})

output$actionTableHelp = shiny::renderUI({

  help = switch(input$actionTable,
                "none" = "",
                "Site_Name"         = "one row per scanning and project wave (prefix 'S[site]')",
                "Project_Wave"      = "one row per participant (prefix 'W[x]')",
                "Subject_Timepoint" = "one row per participant (prefix 'tp[x]')",
                "s2w"               = "one row per participant (prefix 'S[site]_W')",
                "t2w"               = "one row per participant (prefix 'tp[x]_W')"
  )

  if(input$actionDoubles != "asis" & input$actionTable == "Site_Name"){
    help = "one row per participant (prefix 'S[site]')"
  }

  shiny::helpText(help)
})


ExCols =  shiny::eventReactive(input$goClick_DATA, {
  Cols=character()
  if(nchar(input$ColSearch_SubData)>0){
    tmp = gsub(",", "|", input$ColSearch_SubData)
    tmp = gsub(" ","", tmp)
    Cols = DATA() %>%
      dplyr::select(tmp) %>%
      names()
  }
  if(!purrr::is_empty(input$ExtraColsDATA)) Cols=c(Cols,input$ExtraColsDATA)
  return(Cols)
})

subDATA = shiny::eventReactive({input$goClick_DATA; input$actionTable},{
  tmp=DATA()
  print(unique(c(BaseCols , ExCols() )))
  if(input$search_DATA!="") tmp = tmp %>% dplyr::filter_(input$search_DATA)
  if(!purrr::is_empty(ExCols())) tmp = tmp %>% dplyr::select(dplyr::one_of(unique(c(BaseCols , ExCols() ))))

  for(i in names(which(sapply(DATA(), class) == "list"))){
    tmp = list2df(tmp, i)
  }

  if(input$tickSelect) tmp = tmp %>% dplyr::filter(tmp %>% dplyr::select(dplyr::one_of(input$ExtraColsDATA)) %>% stats::complete.cases())

  if(input$actionDoubles != "asis") tmp = tmp %>% MOAS::site_keeper(input$actionDoubles)

  return(tmp %>% MOAS::na.col.rm())
})

subDATA_structured = shiny::eventReactive({input$goClick_DATA; input$actionTable},{
  tmp=subDATA()

  if(input$actionTable %in% "w2s"){
    tmp = tmp %>% MOAS::widen("Project_Wave", ConversionTab[,1]) %>%
      MOAS::widen("Site_Name", ConversionTab[,1])
  }else if(input$actionTable %in% "s2w"){
    tmp = tmp %>% MOAS::widen("Site_Name", ConversionTab[,1]) %>%
      MOAS::widen("Project_Wave", ConversionTab[,1])
  }else if(input$actionTable %in% "t2w"){
    tmp = tmp %>% MOAS::widen(tmp, "Site_Tesla", ConversionTab[,1]) %>%
      MOAS::widen("Project_Wave", ConversionTab[,1])
  }else{
    tmp = MOAS::widen(tmp, input$actionTable, ConversionTab[,1])
  }

  return(tmp %>% MOAS::na.col.rm())
})


