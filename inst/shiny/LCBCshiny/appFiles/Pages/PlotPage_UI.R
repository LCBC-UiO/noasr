
#### --------- Plot UI ---------- ####
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
      wellPanel(fluidRow(plotlyOutput("GG_plot") %>% withSpinner(type = 5, color = "#66bfdd"), 
                         style="margin: 2% 2% 2% 2%, height: 100%;"))
    )
    )
})



#### --------- Plot variables ---------- ####

output$chooseGroupPlot = renderUI({
  opts = c("",names(subDATA_structured()))
  # The options are dynamically generated on the server
  selectizeInput('groupPlot', 'Choose grouping variable (optional)', opts, selected = character(0), 
                 options = list(placeholder = 'Type to start selecting'), width="100%")
})

output$chooseXPlot = renderUI({
  opts = names(subDATA_structured())
  # The options are dynamically generated on the server
  selectizeInput('x_axisPlot', 'Choose X-axis', opts, selected = "", 
                 options = list(placeholder = 'Type to start selecting',
                                onInitialize = I('function() { this.setValue(""); }')), width="100%")  })

output$chooseYPlot = renderUI({
  opts = names(subDATA_structured())
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
      GG = ggplot(subDATA_structured(), aes_string(x=input$x_axisPlot, group=input$groupPlot, 
                                     color=input$groupPlot, fill=input$groupPlot)) + 
        facet_wrap(as.formula(paste("~", input$groupPlot)),scales='free',  drop=T)     
    }else{
      GG = ggplot(subDATA_structured(), aes_string(x=input$x_axisPlot, y=input$y_axisPlot, group=input$groupPlot, 
                                     color=input$groupPlot, fill=input$groupPlot))        
    }
  }else{
    if(input$PlotType=="Histogram"){
      GG = ggplot(subDATA_structured(), aes_string(x=input$x_axisPlot)) 
    }else{
      GG = ggplot(subDATA_structured(), aes_string(x=input$x_axisPlot, y=input$y_axisPlot)) 
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
    paste("Data_plot", format(Sys.Date(), "%d.%m.%Y"), input$downloadType_Plot, sep="." )
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



