
#### --------- Plot UI ---------- ####
output$PlotPage = shiny::renderUI({
  shiny::fluidPage(

    shiny::fluidRow(
      shiny::column(4, shiny::wellPanel(
        shiny::radioButtons(inputId = "PlotType",label = "Choose plot type",
                     choices =c("Histogram","Scatter","Violin","Line"), inline = T),
        shiny::uiOutput("radioPlotHelp",inline=T)
      )),
      shiny::column(4,shiny::wellPanel(
        shiny::HTML("<br>"),
        shiny::uiOutput('chooseXPlot', inline=T),
        shiny::uiOutput('chooseYPlot', inline=T),
        style="background-color: #e6efc9; border-color: #c5da84;")
      ),
      shiny::column(4,shiny::wellPanel(
        shiny::HTML("<br>Optional"),
        shiny::uiOutput('chooseGroupPlot', inline=T),
        shiny::HTML("<shiny::p align='center ' style='color: #626262; font-size: 12px;'> Will color/fill by the
             grouped variable, and may cluster them together.</p>"),
        style="background-color: #c6e7f2; border-color: #66bfdd;")
      ),
      style="margin: 2% 0px 0px 0px"),

    shiny::fluidRow(
      shiny::column(12,
             shiny::wellPanel(shiny::actionButton(inputId = "goClick_Plot", label="Plot it!", width='80%',
                                    style="color: #626262; background-color: #dcdcdc; border-color: #626262; margin: 0px 20px 2px 20px"),
                       shiny::HTML("<br><br>"),
                       style="background-color: transparent; border-color: transparent;"))
    ),

    shiny::fluidRow(
      shiny::h4("Plot"),
      shiny::fluidRow(
        shiny::wellPanel(
          shiny::column(6,
                 shiny::selectInput("downloadType_Plot", "Choose file type", choices=c("png","tiff","jpeg","pdf","svg"), selected = "png", multiple = FALSE, selectize = TRUE, width='40%')),
          shiny::column(6,
                 shiny::downloadButton("download_Plot", label="Download plot")),
          style="background-color: transparent; border-color: transparent; margin-bottom: 2%;")),
      shiny::wellPanel(shiny::fluidRow(plotly::plotlyOutput("GG_plot") %>% shinycssloaders::withSpinner(type = 5, color = "#66bfdd"),
                         style="margin: 2% 2% 2% 2%, height: 100%;"))
    )
    )
})



#### --------- Plot variables ---------- ####

output$chooseGroupPlot = shiny::renderUI({
  opts = c("",names(subDATA_structured()))
  # The options are dynamically generated on the server
  shiny::selectizeInput('groupPlot', 'Choose grouping variable (optional)', opts, selected = character(0),
                 options = list(placeholder = 'Type to start selecting'), width="100%")
})

output$chooseXPlot = shiny::renderUI({
  opts = names(subDATA_structured())
  # The options are dynamically generated on the server
  shiny::selectizeInput('x_axisPlot', 'Choose X-axis', opts, selected = "",
                 options = list(placeholder = 'Type to start selecting',
                                onInitialize = I('function() { this.setValue(""); }')), width="100%")  })

output$chooseYPlot = shiny::renderUI({
  opts = names(subDATA_structured())
  # The options are dynamically generated on the server
  if(input$PlotType!="Histogram"){
    shiny::selectizeInput('y_axisPlot', 'Choose Y-axis', opts, selected = "",
                   options = list(placeholder = 'Type to start selecting',
                                  onInitialize = I('function() { this.setValue(""); }')), width="100%")
  }
})

output$radioPlotHelp = shiny::renderUI({
  help = switch(input$PlotType,
                "Scatter"          = "Creates a scatterplot with a fit curve with 95% CI",
                "ScatterLine"      = "Creates a scatterplot with lines connecting repeated measurements",
                "Violin"           = "Creates a violin density plot. Recommended to have a category/factor as x-axis and grouping",
                "Histogram"        = "Creates a simple frequency histogram, setting grouping here will create a grid of small plots"
  )
  shiny::helpText(help)
})

Plot = shiny::eventReactive(input$goClick_Plot, {
  if(input$groupPlot!=""){
    if(input$PlotType=="Histogram"){
      GG = ggplot2::ggplot(subDATA_structured(), ggplot2::aes_string(x=input$x_axisPlot, group=input$groupPlot,
                                     color=input$groupPlot, fill=input$groupPlot)) +
        ggplot2::facet_wrap(stats::as.formula(paste("~", input$groupPlot)),scales='free',  drop=T)
    }else{
      GG = ggplot2::ggplot(subDATA_structured(), ggplot2::aes_string(x=input$x_axisPlot, y=input$y_axisPlot, group=input$groupPlot,
                                     color=input$groupPlot, fill=input$groupPlot))
    }
  }else{
    if(input$PlotType=="Histogram"){
      GG = ggplot2::ggplot(subDATA_structured(), ggplot2::aes_string(x=input$x_axisPlot))
    }else{
      GG = ggplot2::ggplot(subDATA_structured(), ggplot2::aes_string(x=input$x_axisPlot, y=input$y_axisPlot))
    }
  }


  switch(input$PlotType,
         "Scatter" = assign("GG", GG + ggplot2::geom_smooth() +  ggplot2::geom_point(alpha=.2)),
         "Line" = assign("GG", GG + ggplot2::geom_line(ggplot2::aes(group=CrossProject_ID), alpha=.3) +  ggplot2::geom_point(alpha=.2)),
         "Violin" = assign("GG", GG + ggplot2::geom_violin(alpha=.6) + ggplot2::geom_boxplot(width=.05, alpha=.1)),
         "Histogram" = assign("GG", GG + ggplot2::geom_histogram(alpha=.7, bins=50))
  )

  GG = GG +
    ggplot2::labs(x=input$x_axisPlot, y=input$y_axisPlot, fill="", color="") +
    ggplot2::theme_minimal()

  return(GG)
})

output$GG_plot = plotly::renderPlotly({
  plotly::ggplotly(Plot())
})


output$download_Plot = shiny::downloadHandler(

  filename = function(){
    paste("Data_plot", format(Sys.Date(), "%d.%m.%Y"), input$downloadType_Plot, sep="." )
  },

  content = function(file) {
    fheight = round(105*0.0393701,2); fwidth = round(148*0.0393701,2); fres = 300

    switch(input$downloadType_Plot,
           "png"  = grDevices::png(file, height=fheight, width=fwidth, res=fres, units="in"),
           "tiff" = grDevices::tiff(file, height=fheight, width=fwidth, res=fres, units="in",compression="lzw"),
           "jpeg" = grDevices::jpeg(file, height=fheight, width=fwidth, res=fres, units="in",quality=100),
           "pdf"  = grDevices::pdf(file, height=fheight, width=fwidth),
           #"eps"  = ggsave(file, device=cairo_ps, width = fwidth, height = fheight, fallback_resolution=fres),
           "svg"  = ggplot2::ggsave(file, width = fwidth, height = fheight))
    graphics::plot(Plot())
    grDevices::dev.off()
  }
)



