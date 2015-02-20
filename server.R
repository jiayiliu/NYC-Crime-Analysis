library(shiny)
library(leaflet)
source("analyze.R")
source("plot.helper.R")
load("data/precinct.RData")
load("data/record.RData")
crime.id <- c("rape","robbery","burglary","murder","falony","GL","GLMV")
mdata <- get_monthly_data(data, -1, mean=TRUE)
old_change_flag = FALSE

shinyServer(function(input, output, session) {
  get_weight <- reactive({
    for (cname in crime.id)
      updateSliderInput(session, paste0(cname,"2"),value=input[[cname]])
    c(input$rape,input$robbery, input$burglary, input$murder,
      input$felony, input$GL, input$GLMV)
  })
  get_weight2 <- reactive({
    for (cname in crime.id){
      updateSliderInput(session, cname,value=input[[paste0(cname,"2")]])
    } 
    c(input$rape2,input$robbery2, input$burglary2, input$murder2,
      input$felony2, input$GL2, input$GLMV2)
  })
  
  get_scale <- reactive({
    change_flag = input$ncrime=="change"
    month_flag = input$month == "all"
    if (change_flag&(!old_change_flag)){# change from not change to change
      updateSelectInput(session,"month",selected="all")
      old_change_flag <<- TRUE
      s <- get_crime_monthly_change_all(data, get_weight2())
    } else {
      updateCheckboxInput(session,"ncrime",value="tot")
      old_change_flag <<- FALSE
      if(month_flag)
        s <- get_crime_month(data, get_weight2(), month=NULL)
      else
        s <- get_crime_month(data, get_weight2(), month=as.integer(input$month))
    }
    switch (input$unit,
            raw=s,
            pop={
              s[,2] <- s[,2]/precinct.info$population*1000
              s[s[,1]==22,2] <- 0
              s
            },
            area={
              s[,2] <- s[,2]/precinct.info$area
              s
            })
    })
  
  get_crime_data_averaged <- reactive({
    get_monthly_data(data, input$precinct)
  })
  
  get_crime_data_full <- reactive({
    get_monthly_data(data, input$precinct, mean=FALSE)
  })
  
  get_crime_comp <- reactive({
    if (input$month=="all"){
      s <- as.data.frame(colSums(mdata)[-1])
      s$variable <- rownames(s)
      names(s) <- c("value","variable")
      return(s)
    } else {
      return(melt(mdata[mdata$MONTH==as.integer(input$month),], 
                          id="MONTH"))
    }
  })
  
  is_ncrime_change <- reactive(input$ncrime=="change")
  is_legend <- reactive(input$legend)
  ## Interactive Map ###########################################
  
  # Create the map
  map <- createLeafletMap(session, "map")
  session$onFlushed(once=TRUE, function(){
    paintObs <- observe({
      map$clearShapes()
      scale <- get_scale()
      addPrecinct_color(map, precinct, scale)
    })
    session$onSessionEnded(paintObs$suspend)
  })
  
  # Show a popup of precinct
  showPrecinctPopup <- function(precinct_id){
    p <- get_precinct_detail(precinct, precinct.info, precinct_id)
    content <- as.character(tagList(
      tags$h4("Precinct:", p$id),
      tags$strong(HTML(sprintf("Population: %d",
                               p$population))), 
      tags$br(),
      tags$strong(HTML(sprintf("Area: %f mi^2",p$area)))
      ))
    updateNumericInput(session,inputId="precinct",
                       value=as.integer(strsplit(precinct_id,"-")[[1]][1]))
    map$showPopup(p$lat, p$lng, content,precinct_id)
  }
  
  # observer for popup
  clickObs <- observe({
    map$clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showPrecinctPopup(event$id)
    })
  })
  session$onSessionEnded(clickObs$suspend)
    
  # create <- get_monthly_data(data, -1, mean=TRUE)e chart
  output$piechart <- renderPlot({
    if (!is_legend()) return()
    weight <- get_weight()
    plot_crime_pie(get_crime_comp(), weight=get_weight2())
  })
  
  output$colorbar <- renderPlot({
    scale <- get_scale()[,2]
    title <- ifelse(is_ncrime_change(),"Change #","Amount")
    color.bar(colorRampPalette(brewer.pal(11,"Spectral"))(256), 
              min(scale),max(scale),nticks=5,title=title)
  })
  ## Exploratory Data ##########################################
  output$tsPlot <- renderPlot({
    weight <- get_weight()
    if(input$averaged){
      data <- get_crime_data_averaged()
      if(is.null(data)) return()
      plot_monthly_trend(data, weight, input$overlay)
    } else {
      data <- get_crime_data_full()
      if(is.null(data)) return()
      plot_full_trend(data, weight, input$overlay)
    }
  })
})