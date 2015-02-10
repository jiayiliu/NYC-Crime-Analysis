################################################################################
# This is a shiny R app to visualize NYC crime report
################################################################################

library(shiny)
library(leaflet)

sdate <- c("all",1:12)
names(sdate) <- c("All","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
shinyUI(
  navbarPage("Crime Analysis", id="nav",
             tabPanel("Interactive Crime Map",
                      div(class="outer",
                          tags$head(
                            includeCSS("styles.css")
                          ),
                          leafletMap("map", width="100%", height="100%",
                                     initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                                     initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                                     options=list(
                                       center = c(40.75,-73.88), # note this is lat/lng 
                                       zoom = 11,
                                       maxBounds = list(list(40,-74.66), list(41.5,-73)) # Show US only
                                     )
                          ),
                          absolutePanel(id="controls", class="panel panel-default",
                                        fixed=TRUE, draggable=FALSE,
                                        top=50, left="auto",right=20,bottom="auto",
                                        width=330,height="auto",
                                        h3("Crime Indicator"),
                                        selectInput("ncrime","Choose:",
                                                     c("Total Amount"="tot",
                                                       "Change Rate"="change"),
                                                    selected="tot"),
                                        selectInput("unit","Unit:",
                                                     c("Raw"="raw",
                                                       "Per 1000 residents"="pop",
                                                       "Per 1 Mi^2"="area"),
                                                     selected="raw"),
                                        selectInput("month","Month:", sdate),
                                        h3("Crime Weight"),
                                        sliderInput("rape2", "Rape", 
                                                    min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                                        sliderInput("robbery2", "Robbery", 
                                                    min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                                        sliderInput("burglary2", "Burglary", 
                                                    min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                                        sliderInput("murder2", "Murder", 
                                                    min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                                        sliderInput("felony2", "Felony Assault", 
                                                    min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                                        sliderInput("GL2", "Grand Larceny", min = 0, 
                                                    max = 1, value=1, step=0.25, ticks=FALSE),
                                        sliderInput("GLMV2", "Grand Larceny\nof Motor Vehicle", 
                                                    min = 0, max = 1, value=1, step=0.25, ticks=FALSE)
                          ),
                          absolutePanel(id="colorbar", class="panel panel-default",
                                        fixed=TRUE, draggable=TRUE,
                                        top=100,left=20, right="auto",bottom="auto",
                                        width=100, height=250,
                                        plotOutput("colorbar")),
                          absolutePanel(id="legendplot", class="panel panel-default",
                                        fixed=TRUE, draggable=FALSE,
                                        top="auto", left=20, right="auto",bottom=20,
                                        width="auto",height="auto",
                                        checkboxInput("legend","Weight Chart",FALSE),
                                        conditionalPanel(condition="input.legend == true",
                                                         width="800px", height="600px",fixed=TRUE,
                                                         bottom=20,left=20,
                                                         h2("Crime Chart"),
                                                         plotOutput("piechart")
                                        )
                          )
                      )
             ),
                      
             #############################################################
             tabPanel("Crime Analysis",
                      div(class="outer",
                          tags$head(
                            includeCSS("styles.css")
                          ),
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("precinct","Precinct No. (select from Map, or -1 for all)",
                                           -1, min=-1, max=200, step=1),
                              checkboxInput("averaged","Monthly Average?",value=TRUE),
                              checkboxInput("overlay","Overlay Individual Crime",value=FALSE),
                              helpText("Choose the weights for crimes:"),
                              sliderInput("rape", "Rape", 
                                          min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                              sliderInput("robbery", "Robbery", 
                                          min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                              sliderInput("burglary", "Burglary", 
                                          min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                              sliderInput("murder", "Murder", 
                                          min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                              sliderInput("felony", "Felony Assault", 
                                          min = 0, max = 1, value=1, step=0.25, ticks=FALSE),
                              sliderInput("GL", "Grand Larceny", min = 0, 
                                          max = 1, value=1, step=0.25, ticks=FALSE),
                              sliderInput("GLMV", "Grand Larceny\nof Motor Vehicle", 
                                          min = 0, max = 1, value=1, step=0.25, ticks=FALSE)
                            ),
                            mainPanel(plotOutput("tsPlot"))
                          )
                      )
             ),
             tabPanel("Help",
                      div(class="outer",
                          HTML("
                               <br>
                               <h1> How to use this website </h1>
                               <br>
                               <p> This is an interactive website to show you the crime history in NYC.
                               The control features are following:</p>
                               <ul>
                               <li>Check the total crime number, crime change rate based on total number, per area or per capita</li>
                               <li>Assign personal crime weights to emphasize the crimes you most care</li>
                               <li>Check the seasonal crime pattern</li>
                               </ul>
                               <h1> Manual </h1>
                               <h2> Interactive Crime Map </h2>
                               <ul>
                               <li>The right panel is the control panel.  You can choose to present total amount/change rate; total/per area/per capita; and individual month.</li>
                               <li>Below that, you can choose your personal weights on different crimes.  At the left bottom, you will find the weight charts before and after applying the weights by clicking weight chart.</li>
                               <li>The left panel shows the color code for the map</li>
                               </ul>
                               <h2> Crime Analysis </h2>
                               <ul>
                               <li>If you clicked a precinct in the interactive map, that precinct result is shown on the right figure. Otherwise it is defaulted to show the whole records.</li>
                               <li>You can choose to show the monthly data over two years or the monthly averaged data.</li>
                               <li>You can also choose to show the individual crime types to see their trends over the history.</li>
                               <li>The weights for crimes are same as used in the interactive crime map.</li>
                               </ul>
                               <br>
                               <p> The data is collected from <a href=\"http://maps.nyc.gov/crime/\">nyc.gov</a>.</p>
                               <footnote> The website is copyleft @ Jiayi (Jason) Liu 
                               </footnote>
                               ")
                      ))
  ))