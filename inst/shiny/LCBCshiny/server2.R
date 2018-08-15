### Shiny server specification -------------
server=function(input,output){
  
  #Filesize allowance increase (30MB)
  options(shiny.maxRequestSize=100*1024^2)
  
  # PAGES code --------------
  
  output$SubsetPage = renderUI({
    fluidPage(
      
      
      fluidRow(
        HTML("<br><p style='color: #626262; font-size: 12px;'> Use the 'Search' field to provide expressions that subsets the rows of 
             the table with a logical expression ['COLUMN' 'expression' 'value'], ex. Age>20 for Age over 20. See 'Help' tab for more information <br>"),
        HTML("<br><b>Base columns are: </b>"), paste(BaseCols , collapse = "  ")
      ),
      
      fluidRow(
        column(6,wellPanel(
          textInput(inputId="search_DATA", label="Type an expression to reduce rows"),
          HTML("<p style='color: #626262; font-size: 12px;'> See the 'Help' tab for extra help in searching"),
          style="background-color: #e6efc9; border-color: #c5da84;")
        ),
        
        column(6,wellPanel( 
          HTML("<br>"),
          uiOutput('ColSelect_SubData', inline=T),
          checkboxInput("tickSelect", label="Remove rows with empty observations in the above columns", value = FALSE, width = NULL),
          textInput('ColSearch_SubData', label = "Add all columns containing:", value = "", placeholder = "Comma separated list of strings"),
          style="background-color: #c6e7f2; border-color: #66bfdd;")
        ),
        style="margin: 2% 0px 0px 0px"),
      
      fluidRow(
        column(12,  
               wellPanel(actionButton(inputId = "goClick_DATA", label="Go!", width='80%',
                                      style="color: #626262; background-color: #dcdcdc; border-color: #626262; margin: 0px 20px 2px 20px"),
                         HTML("<br><br><br>"),
                         style="background-color: transparent; border-color: transparent;"))
      ),
      
      fluidRow(
        h4("Data exerpt"),
        # uiOutput('SumTable_DATA', inline=T),
        fluidRow(wellPanel(
          column(4,
                 radioButtons("actionTable", label="Table format",
                              choices=WidenChoices),
                 uiOutput("actionTableHelp")),
          column(4,
                 selectInput(inputId="downloadType_subDATA", label="Choose file type", choices=DownloadOptionsTable, width='40%')),
          column(4,
                 downloadButton("download_subDATA", label="Download table")),
          style="background-color: transparent; border-color: transparent; margin-bottom: 2%;")),
        HTML("<p align='center' style='color: #626262; font-size: 12px;'> Random sample of the subsetted data. Scroll sideways to see all columns"),
        wellPanel(p(textOutput("N_DATA")),
                  fluidRow(dataTableOutput("table_DATA")))
      )
    )
  })
  
  output$FS_LMMPage = renderUI({
    fluidPage(          
      fluidRow(
        HTML("<br><p style='color: #626262; font-size: 12px;'> Use the 'Search' field to provide expressions that subsets the rows of 
             the table with a logical expression ['COLUMN' 'expression' 'value'], ex. Age>20 for Age over 20. See 'Help' tab for more information. <br>"),
        HTML("<br><b>Base columns are: </b> fsid fsid-base time [FS_LMM specific columns created by the script, necessary for FS_LMM to run]")),
      
      fluidRow(
        column(6,wellPanel(
          textInput(inputId="search_FS_LMM", label="Type an expression to reduce rows"),
          HTML("<p style='color: #626262; font-size: 12px;'> See the 'Help' tab for extra help in searching. <br></p>"),
          style="background-color: #e6efc9; border-color: #c5da84;")),
        
        column(6,wellPanel( 
          HTML("<br>"),
          uiOutput('chooseGrouping', inline=T),
          HTML("<p style='color: #626262; font-size: 12px;'> (i.e. non-numeric covariates, comma separated)-</p><br><br>"),
          
          uiOutput('chooseNumeric', inline=T),
          HTML("<p style='color: #626262; font-size: 12px;'> (i.e. numeric variables of interest, comma separated).</p>"),
          style="background-color: #c6e7f2; border-color: #66bfdd;"))
      ),
      
      fluidRow(
        column(12,wellPanel(
          radioButtons("actionFS", label="Action to take for values in 'Numeric' columns",
                       choices=c("Mean","All","First","Delete"), inline=T),
          uiOutput("actionFSHelp")
        ))
      ),
      
      fluidRow(
        column(12,wellPanel(
          actionButton(inputId = "goClick_FS_LMM", label="Go!", width='80%',
                       style="color: #626262; background-color: #dcdcdc; border-color: #626262; margin: 0px 20px 2px 20px"),
          style="background-color: transparent; border-color: transparent;"))
      ),
      
      fluidRow(
        h4("Table"),
        fluidRow(wellPanel(
          downloadButton("download_FS_LMM", label="Download table"),
          style="background-color: transparent; border-color: transparent; margin-bottom: 2%;")),
        wellPanel(p(textOutput("N_FS_LMM")),
                  HTML("<p align='center' style='color: #626262; font-size: 12px;'> Random sample of the subsetted data. Scroll sideways to see all columns"),
                  fluidRow(dataTableOutput("table_FS_LMM")))
      )
    )
  })
  
  output$PlotPage = renderUI({
    fluidPage(
      
      fluidRow(
        column(4, wellPanel(
          radioButtons(inputId = "PlotType",label = "Choose plot type", 
                       choices =c("Histogram","Scatter","Violin","Line"), inline = T),
          uiOutput("radioPlotHelp",inline=T)
        )),
        column(4,wellPanel( 
          HTML("<br>"),
          uiOutput('chooseXPlot', inline=T),
          uiOutput('chooseYPlot', inline=T),
          style="background-color: #e6efc9; border-color: #c5da84;")
        ),
        column(4,wellPanel( 
          HTML("<br>Optional"),
          uiOutput('chooseGroupPlot', inline=T),
          HTML("<p align='center ' style='color: #626262; font-size: 12px;'> Will color/fill by the 
               grouped variable, and may cluster them together.</p>"),
          style="background-color: #c6e7f2; border-color: #66bfdd;")
        ),
        style="margin: 2% 0px 0px 0px"),
      
      fluidRow(
        column(12,  
               wellPanel(actionButton(inputId = "goClick_Plot", label="Plot it!", width='80%',
                                      style="color: #626262; background-color: #dcdcdc; border-color: #626262; margin: 0px 20px 2px 20px"),
                         HTML("<br><br>"),
                         style="background-color: transparent; border-color: transparent;"))
      ),
      
      fluidRow(
        h4("Plot"),
        fluidRow(
          wellPanel(
            column(6,      
                   selectInput("downloadType_Plot", "Choose file type", choices=c("png","tiff","jpeg","pdf","svg"), selected = "png", multiple = FALSE, selectize = TRUE, width='40%')),
            column(6,
                   downloadButton("download_Plot", label="Download plot")),
            style="background-color: transparent; border-color: transparent; margin-bottom: 2%;")),
        wellPanel(fluidRow(plotlyOutput("GG_plot"), style="margin: 2% 2% 2% 2%, height: 100%;"))
      )
    )
  })
  
  output$HelpPage = renderUI({
    fluidPage(
      fluidRow(HTML("<br>")),
      fluidRow(
        column(1),
        column(10,wellPanel(
          h3("'Search'"),
          HTML("<div align='left'>Allows you to search for observations matching certain characteristics
               (like Age above 30: <i>Age>30</i>). Search is case-sensitive, meaning capital and small letters must be correct.<br> <br>
               You can create complex searches to subset the data, but each expression must consist of
               [Column_name Expression Value] (search for empty or not empty values are the only expetion to this, see below).
               Different search parameters can be specified with OR-sign (|) and AND-sign (&)
               (like 'Age >= 20 | Age <= 50' to get all between the ages of 20 and 50)<br><br>"),
          HTML("<p><h4>Between queries</h4>
               <table width='60%'>
                 <tr> <th>Expression</th> <th align='center'>Meaning</th> </tr>
                 <tr> <td>x & y</td> <td>x and y</td> </tr>
                 <tr> <td>x | y</td> <td>x or y</td> </tr>
               </table><br><br>
               <figure align='center' style='color: #626262; font-size: 10px;'>
                 <img src='Logicals.png' class='img-responsive' align='center' width='100%' height='auto' style='max-width:600px;'/>
                 <figcaption>image from http://r4ds.had.co.nz/transform.html</figcaption>
               </figure>"),
          HTML("<br><br>
               <p><h4>Query expression</h4>
               <table width='100%'>
                 <tr> <th>Expression</th> <th align='center'>Meaning</th> </tr>
                 <tr> <td>x == y</td> <td>x equal to y </tr>
                 <tr> <td>x != y</td> <td>x not equal to y</td> </tr>
                 <tr> <td>x > y</td> <td>x larger than y</td> </tr>
                 <tr> <td>x >= y</td> <td>x larger or equal to y</td> </tr>
                 <tr> <td>x < y</td> <td>x smaller than y</td> </tr>
                 <tr> <td>is.na(x)</td> <td>x has no value (NA=not applicable/not a number) </td> </tr>
                 <tr> <td>![expr]</td> <td>Negation (will inverts statements above, like '!=' to '==')</td> </tr>
               </table><br><br></p>"),
          HTML("<p><h4>Examples:</h4>
                <ul>
                 <li><b>Age >= 20 - </b> all observations with Age above or equal to 20.<br></li>
                 <li><b>!is.na(CVLT_A_Total) - </b>gives all observations where there IS a CVLT_A_Total value <br></li>
                 <li><b>Age >= 20 & !is.na(CVLT_A_Total) - </b> all observations that have Age over or equal to 20 AND have CVLT_A_Total scores. <br></li>
                 <li><b>Age >= 20 | !is.na(CVLT_A_Total) - </b> all observations that have either Age over or equal to 20 or have CVLT_A_Total scores. <br></li>
                 <li><b>Age >= 20 | Age <= 50' - </b>all observations between the ages of 20 and 50.<br></li>
                 <li><b>(Age<10 & WISC_Digitspan_FWD > 10) | (Age>20 & WASI_DigitSpan_FWD > 20) - </b></li>
                 complex search. all observations where those aged below 10 must have above 12 in digit span,
                 and adults aobve 20 who have more than 20 on the DigitSpan.</li>
               </ul></p></div>")
        )))
    )
  })
  
  output$VariablesPage = renderUI({
    fluidPage(
      fluidRow(HTML("<br>")),
      fluidRow(dataTableOutput("table_Variables"))
    )
  })
  
  
  
  # VARIABLES code --------------
  
  #In case something goes wrong with loading the inDATA, prompt manual upload
  if(!exists("inDATA")) { 
    InFILE = eventReactive(input$inDATA,{
      if ( is.null(input$inDATA)) return(NULL)
      inFile = isolate({input$inDATA })
      tmp = get_file(inFile$datapath)
      return( tmp)
    })
    
    output$uploadFILE = renderUI({
      fileInput("inDATA", "Upload the MOAS.RData file",
                multiple = FALSE,
                accept = c(".RData", ".rda"))
    })
    
  }else{
    InFILE=function(){return(inDATA)}
  }
  
  DATA = reactive({
    tmp = InFILE()[["Imaging"]][["SuperLong"]]
    return( tmp)
  })
  
  # --------- Subset DATA variables ----------
  
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
  
  output$actionTableHelp = renderUI({
    help = switch(input$actionTable,
                  "none" = "",
                  "Site_Name"     = "one row per scanning and project wave",
                  "s2w"           = "one row per participant",
                  "t2w"           = "one row per participant"
    )
    helpText(help) 
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
  
  
  output$table_DATA = renderDataTable({
    n=sample(1:(nrow(subDATA_structured())-24),1)
    NUMS = apply(subDATA_structured(),2,
                 function(x) (as.numeric(x)%%1 > 0 ) %>% any)
    NUMS = (NUMS %in% TRUE) %>% which()
    
    subDATA_structured()[n:(n+24),] %>% 
      datatable(options = list(scrollX = TRUE,pageLength = 25)) %>% 
      formatStyle(columns = c(1:ncol(subDATA_structured())), 'text-align' = 'center') %>% 
      formatRound(columns=NUMS, digits=2)
  })
  
  output$N_DATA = renderText({
    paste("Number of observations: ", nrow(subDATA_structured()))
  })
  
  output$download_subDATA = downloadHandler(
    
    filename = function(){
      EXT=ifelse(input$downloadType_subDATA =="csv2","csv",input$downloadType_subDATA)
      
      paste("Table", input$search_DATA, ExCols(),format(Sys.Date(),"%d.%m.%Y"),
            EXT, sep=".")
    },
    
    content = function(file) {
      rio::export(x = subDATA_structured(),
                  file = file,
                  format = input$downloadType_subDATA)
    }
  )
  
  
  # --------- FS_LMM variables ----------
  
  output$chooseGrouping = renderUI({
    CLASS = unlist(lapply(DATA(),class))
    opts = names(DATA())[grep("factor|character", CLASS)]
    selectizeInput('choiceGrouping', 'Select grouping variables', opts, selected = character(0), 
                   multiple = T, options = list(placeholder = 'Type to start selecting'), width="100%")
  })
  
  output$chooseNumeric = renderUI({
    CLASS = unlist(lapply(DATA(),class))
    opts = names(DATA())[grep("numeric|integer", CLASS)]
    # The options are dynamically generated on the server
    selectizeInput('choiceNumeric', 'Select numeric covariates', opts, selected = character(0), 
                   multiple = T, options = list(placeholder = 'Type to start selecting'), width="100%")
  })
  
  FS_Table = eventReactive(input$goClick_FS_LMM, {
    if(is_empty(input$choiceGrouping) | is_empty(input$choiceNumeric)){
      tmp = FS_LMM(DATA(), "Sex", "Age", input$actionFS)
    }else{
      tmp = FS_LMM(DATA(),input$choiceGrouping, input$choiceNumeric, input$actionFS )
    }
    return(tmp) 
  })
  
  output$actionFSHelp = renderUI({
    help = switch(input$actionFS,
                  Mean   = "Replace missing values with the mean of the participant. Keeps as much available data as possible.",
                  Delete = "Delete all missing values from the dataset.",
                  First  = "Replace all values with the first recorded instance for the participant. Circumvent change-change relationship.",
                  All    = "Replace all values with the mean of the participant. Assumes a stable trait."
    )
    helpText(help) 
  })
  
  output$table_FS_LMM = renderDataTable(options = list(scrollX = TRUE), {
    n=sample(1:(nrow(FS_Table())-24),1)
    FS_Table()[n:(n+24),]
    #FS_Table()
  })
  
  output$N_FS_LMM = renderText({
    N=nrow(FS_Table())
    if(is_empty(N)){
      paste("Please select both Grouping and Numeric variables for output.")
    }else{
      paste("Number of observations: ", N)
    }
  })
  
  output$download_FS_LMM = downloadHandler(
    
    filename = function(){
      paste("FS_LMM", Grouping_FS(), Numeric_FS(),format(Sys.Date(),"%d.%m.%Y"),
            "txt", sep=".")
    },
    
    content = function(file) {
      write.table(x = FS_Table(),
                  file = file,
                  sep=" ", row.names = F, quote = F)
    }
  )
  
  # --------- Plot variables ----------
  
  output$chooseGroupPlot = renderUI({
    opts = c("",names(DATA()))
    # The options are dynamically generated on the server
    selectizeInput('groupPlot', 'Choose grouping variable (optional)', opts, selected = character(0), 
                   options = list(placeholder = 'Type to start selecting'), width="100%")
  })
  
  output$chooseXPlot = renderUI({
    opts = names(DATA())
    # The options are dynamically generated on the server
    selectizeInput('x_axisPlot', 'Choose X-axis', opts, selected = "", 
                   options = list(placeholder = 'Type to start selecting',
                                  onInitialize = I('function() { this.setValue(""); }')), width="100%")  })
  
  output$chooseYPlot = renderUI({
    opts = names(DATA())
    # The options are dynamically generated on the server
    if(input$PlotType!="Histogram"){
      selectizeInput('y_axisPlot', 'Choose Y-axis', opts, selected = "", 
                     options = list(placeholder = 'Type to start selecting',
                                    onInitialize = I('function() { this.setValue(""); }')), width="100%")
    }
  })
  
  output$radioPlotHelp = renderUI({
    help = switch(input$PlotType,
                  "Scatter"          = "Creates a scatterplot with a fit curve with 95% CI",
                  "ScatterLine"      = "Creates a scatterplot with lines connecting repeated measurements",
                  "Violin"           = "Creates a violin density plot. Recommended to have a category/factor as x-axis and grouping",
                  "Histogram"        = "Creates a simple frequency histogram, setting grouping here will create a grid of small plots"
    )
    helpText(help) 
  })
  
  Plot = eventReactive(input$goClick_Plot, {
    if(input$groupPlot!=""){
      if(input$PlotType=="Histogram"){
        GG = ggplot(DATA(), aes_string(x=input$x_axisPlot, group=input$groupPlot, 
                                       color=input$groupPlot, fill=input$groupPlot)) + 
          facet_wrap(as.formula(paste("~", input$groupPlot)),scales='free',  drop=T)     
      }else{
        GG = ggplot(DATA(), aes_string(x=input$x_axisPlot, y=input$y_axisPlot, group=input$groupPlot, 
                                       color=input$groupPlot, fill=input$groupPlot))        
      }
    }else{
      if(input$PlotType=="Histogram"){
        GG = ggplot(DATA(), aes_string(x=input$x_axisPlot)) 
      }else{
        GG = ggplot(DATA(), aes_string(x=input$x_axisPlot, y=input$y_axisPlot)) 
      }
    }
    
    
    switch(input$PlotType,
           "Scatter" = assign("GG", GG + geom_smooth() +  geom_point(alpha=.2)),
           "Line" = assign("GG", GG + geom_line(aes(group=CrossProject_ID), alpha=.3) +  geom_point(alpha=.2)),
           "Violin" = assign("GG", GG + geom_violin(alpha=.6) + geom_boxplot(width=.05, alpha=.1)),
           "Histogram" = assign("GG", GG + geom_histogram(alpha=.7, bins=50))
    )
    
    GG = GG + 
      labs(x=input$x_axisPlot, y=input$y_axisPlot, fill="", color="") + 
      theme_minimal()
    
    return(GG)
  })
  
  output$GG_plot = renderPlotly({
    ggplotly(Plot()) 
  })
  
  
  output$download_Plot = downloadHandler(
    
    filename = function(){ 
      paste("DATA_plot", format(Sys.Date(), "%d.%m.%Y"), input$downloadType_Plot, sep="." )
    },
    
    content = function(file) {
      fheight = round(105*0.0393701,2); fwidth = round(148*0.0393701,2); fres = 300
      
      switch(input$downloadType_Plot,
             "png"  = png(file, height=fheight, width=fwidth, res=fres, units="in"),
             "tiff" = tiff(file, height=fheight, width=fwidth, res=fres, units="in",compression="lzw"),
             "jpeg" = jpeg(file, height=fheight, width=fwidth, res=fres, units="in",quality=100),
             "pdf"  = pdf(file, height=fheight, width=fwidth),
             #"eps"  = ggsave(file, device=cairo_ps, width = fwidth, height = fheight, fallback_resolution=fres),
             "svg"  = ggsave(file, width = fwidth, height = fheight))
      plot(Plot())
      dev.off()
    }
  )
  
  
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
  
}
