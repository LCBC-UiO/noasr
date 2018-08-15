# Subset DATA variables ----------

output$ColSelect_SubData = renderUI({
  # The options are dynamically generated on the server
  opts = names(DATA())[!(names(DATA()) %in% BaseCols)]
  selectizeInput('ExtraColsDATA', 'Add columns to your file', opts, selected = character(0), 
                 multiple = T, options = list(placeholder = 'Type to start selecting'), width="100%")
})

ExCols =  eventReactive(input$goClick_DATA, {
  Cols=character()
  if(nchar(input$ColSearch_SubData)>0){
    tmp = gsub(",", "|", input$ColSearch_SubData)
    tmp = gsub(" ","", tmp)
    Cols = DATA() %>% 
      select(matches(tmp)) %>% 
      names()
  }
  if(!is_empty(input$ExtraColsDATA)) Cols=c(Cols,input$ExtraColsDATA)
  return(Cols)
})

subDATA = eventReactive({input$goClick_DATA; input$actionTable},{
  tmp=DATA()
  print(unique(c(BaseCols , ExCols() )))    
  if(input$search_DATA!="") tmp = tmp %>% filter_(input$search_DATA)
  if(!is_empty(ExCols())) tmp = tmp %>% select(one_of(unique(c(BaseCols , ExCols() ))))
  
  for(i in names(which(sapply(DATA(), class) == "list"))){
    tmp = list2df(tmp, i) 
  }
  
  if(input$tickSelect) tmp = tmp %>% filter(tmp %>% select(one_of(input$ExtraColsDATA)) %>% complete.cases())
  
  return(tmp %>% na.col.rm())
})

subDATA_structured = eventReactive({input$goClick_DATA; input$actionTable},{
  tmp=subDATA()
  
  if(input$actionTable %in% "w2s"){
    tmp = tmp %>% widen("Project_Wave", ConversionTab[,1]) %>% 
      widen("Site_Name", ConversionTab[,1])
  }else if(input$actionTable %in% "s2w"){
    tmp = tmp %>% widen("Site_Name", ConversionTab[,1]) %>% 
      widen("Project_Wave", ConversionTab[,1])
  }else if(input$actionTable %in% "t2w"){
    tmp = tmp %>% widen(tmp, "Site_Tesla", ConversionTab[,1]) %>% 
      widen("Project_Wave", ConversionTab[,1])
  }else{
    tmp = widen(tmp, input$actionTable, ConversionTab[,1])
  }
  
  return(tmp %>% na.col.rm())
})


