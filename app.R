if (!require("pacman")) install.packages("pacman")
# for shiny app
pacman::p_load(dplyr, # always
               ggplot2,
               lemon, #for function facet_rep_wrap()
               lubridate, #time stuff
               viridis, #viridis color scale
               shiny, #shiny app
               shinythemes, #ggmap, #if using maps with download tiles in shiny output
               #tidyr, #for using gather(), rearranging data
               move, #movebank
               leaflet, #map making
               shinyjs, #previous/next button
               dygraphs,
               beepr,
               xts, #to make the convertion data-frame / xts format
               cowplot) #arrange ggplots
detach("package:xts", unload = TRUE) #otherwise error "Error in get: Objekt '.xts_chob' nicht gefunden"

Sys.setlocale("LC_TIME", "C")  #set English hours for correct x-axis

curl <- movebankLogin(username=MOVEBANK_USERNAME,  password=MOVEBANK_PASSWORD)
s <- Sys.time(); attr(s,"tzone") <- "UTC"

# always 1 month worth of data
timestamp_end <- paste0(format(Sys.time(), format="%Y%m%d%H%M%S"), "000")
timestamp_start <- paste0(format(as.Date(Sys.time())  %m+%  days(-as.numeric(31)) , format="%Y%m%d%H%M%S"), "000")


########### 0 - Download data MILSAR ############
milsar0 <- getMovebankData(study="Milvusmilvus_Milsar_SOI_final", login=curl, moveObject=TRUE, 
                           timestamp_start=timestamp_start, timestamp_end=timestamp_end)
milsar <- as.data.frame(milsar0) #different number of columns than Ecotone
#milsar$acceleration <- sqrt(milsar$acceleration_x^2+milsar$acceleration_y^2+milsar$acceleration_z^2) #magnitude of acceleration 
#doesn't work as well as just acceleration_x
milsar.gps <- milsar %>% 
  dplyr::select(bird_id='local_identifier', 
                TransmGSM='tag_local_identifier',
                timestamp, 
                temperature='external_temperature',
                acceleration_x,
                tag_voltage, 
                longitude='location_long', latitude='location_lat') %>% 
  droplevels() %>% unique() %>% 
  mutate(battery = round(tag_voltage/1000, 2), 
         temperature = round(temperature, 1),
         acceleration_x = round(acceleration_x, 2),
         bird_id = as.factor(bird_id),
         date = as.Date(timestamp, format="%Y-%m-%d"),
         num_time = as.numeric(timestamp, origin=as.POSIXct("2015-01-01", tz="GMT"))) %>% #as workaround for color legend
  arrange(TransmGSM, timestamp)



########### 0 - Download data Ecotone ############
myDF <- getMovebankLocationData(study=230545451, sensorID="GPS", login=curl, 
                                timestamp_start=timestamp_start, timestamp_end=timestamp_end)
myDF <- myDF[!duplicated(paste0(myDF$timestamp,myDF$individual.local.identifier)),] ## this is to exclude duplicated timestamps (if present)
myMSk <- move(myDF)
ecotone <- as.data.frame(myMSk) #different number of columns than Ecotone
ecotone.gps <- ecotone %>% 
  dplyr::select(bird_id='individual.local.identifier', 
                TransmGSM='comments',
                timestamp, 
                temperature='external.temperature',
                activity='activity.count',
                tag_voltage='tag.voltage', 
                longitude='location.long', latitude='location.lat') %>% 
  droplevels() %>% unique() %>% 
  mutate(battery = round(tag_voltage/1000, 2), 
         temperature = round(temperature, 1),
         bird_id = as.factor(bird_id),
         date = as.Date(timestamp, format="%Y-%m-%d"),
         num_time = as.numeric(timestamp, origin=as.POSIXct("2015-01-01", tz="GMT"))) %>% #as workaround for color legend
  arrange(TransmGSM, timestamp)



########### 1 - Pre-shiny Preparation ###########
# code to make actionButtons also work by hitting Enter
# @Eli Berkow, https://stackoverflow.com/questions/53642006/proxy-click-not-functioning-in-modal
jscode <- '$(document).keyup(function(event) {
    if ((event.keyCode == 13)) {
        $("#button").click();}});'

# code formatting mouseover legend in dygraphs
valueFormatter <- "function(x) {
          var options = {weekday: 'short', year: 'numeric', month: '2-digit', day: '2-digit', hour: '2-digit', minute: '2-digit', 
          hour12: false, timeZone: 'UTC'};
          var dayX = new Date(x);
          return dayX.toLocaleString('en-SE', options);
        }"

# Shading of background depending on risk zones in Acceleration, Temperature, and Battery
TempLwrLimDanger <- -1000 #-Inf doesn't work
TempUprLimDanger <- if (is.na(SPECIFY_OUTSIDE_TEMPERATURE)) 10 else SPECIFY_OUTSIDE_TEMPERATURE
TempUprLimRisk <- if (is.na(SPECIFY_OUTSIDE_TEMPERATURE)) 20 else SPECIFY_OUTSIDE_TEMPERATURE+10
TempUprLimSafe <- 1000 #Inf doesn't work

AccLwrLimSafe <- -1000 #-Inf doesn't work
AccLwrLimRisk <- -5
AccLwrLimDanger <- -3
AccUprLimDanger <- 3
AccUprLimRisk <- 5
AccUprLimSafe <- 1000 #Inf doesn't work

ActLwrLimDanger <- ifelse(min(ecotone.gps$activity, na.rm=T)>-3000, -3000, -abs(min(ecotone.gps$activity, na.rm=T))*2) #-Inf doesn't work
ActUprLimDanger <- 10
ActUprLimRisk <- 50
ActUprLimSafe <- max(ecotone.gps$activity, na.rm=T)*2

rgb.red <- "rgba(255, 0, 0, 0.1)"
rgb.yellow <- "rgba(255, 200, 0, 0.15)"
rgb.green <- "rgba(0, 160, 0, 0.1)"

# List of Milsar / Ecotone choices, sorted alphabetically
Milsar.List <- as.factor(sort(as.character(unique(milsar.gps$TransmGSM))))
Ecotone.List <- as.factor(sort(as.character(unique(ecotone.gps$TransmGSM))))



########### 2 - user interface ###########

ui <- fluidPage(    
  #tags$head(
  #  tags$style(HTML('.dygraph-legend {color: black; background-color: transparent !important;} .highlight {display: inline;background-color: #B0B0B0;font-size: 15px;}'))), #left: 50px !important; 
  navbarPage("Bird Checking",
             #theme = shinytheme("slate"), # or darkly
             
             ### MILSAR ###
             tabPanel("Milsar",
                      
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          
                          selectInput(inputId = "ID.m", label = "Red Kite", choices = Milsar.List, multiple = F),
                          column(6, actionButton("prevBtn.m", "<<"), align = "right"),
                          column(6, actionButton("nextBtn.m", ">>"), align = "left"), #style='padding:4px; font-size:80%')
                          tags$head(tags$script(HTML(jscode))), #an insert to use actionButtons by hitting Enter as well
                          
                          br(), br(), br(), hr(),
                          
                          radioButtons(inputId = "PointsToDisplay.m",
                                       label = "Data",
                                       choices = c("last 5 points" = 1,
                                                   "last 10 points" = 2,
                                                   "last 2 days" = 3,
                                                   "last 5 days" = 4,
                                                   "last 10 days" = 5,
                                                   "last 1 month" = 6),
                                       selected = 1),
                          
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  #some empty rows to align sidebarPanel with mainPanel
                          br(), br(), br(), br(), br(),
                        ),
                        
                        mainPanel(
                          
                          # Display last 5 points
                          tableOutput("five.points.m"),
                          
                          hr(),
                          
                          # Plot points on map
                          leafletOutput("zoomplot.m", height = 250),
                          
                          br(),
                          
                          # Dygraph for life signs
                          dygraphOutput("dygraph.acc.m", height = 90),
                          dygraphOutput("dygraph.temp.m", height = 83), 
                          dygraphOutput("dygraph.batt.m", height = 83)
                        )
                      )
             ),
             
             ### ECOTONE ###
             tabPanel("Ecotone",
                      
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          
                          selectInput(inputId = "ID.e", label = "Red Kite", 
                                      choices = Ecotone.List, multiple = F),
                          column(6, actionButton("prevBtn.e", "<<"), align = "right"),
                          column(6, actionButton("nextBtn.e", ">>"), align = "left"),#style='padding:4px; font-size:80%')
                          
                          br(), br(), br(), hr(),
                          
                          radioButtons(inputId = "PointsToDisplay.e",
                                       label = "Data",
                                       choices = c("last 5 points" = 1,
                                                   "last 10 points" = 2,
                                                   "last 2 days" = 3,
                                                   "last 5 days" = 4,
                                                   "last 10 days" = 5,
                                                   "last 1 month" = 6),
                                       selected = 1),
                          
                          br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),  #some empty rows to align sidebarPanel with mainPanel
                          br(), br(), br(), br(), br(),
                        ),
                        
                        mainPanel(
                          
                          # Display last 5 points
                          tableOutput("five.points.e"),
                          
                          hr(),
                          
                          # Plot points on map
                          leafletOutput("zoomplot.e", height = 250),
                          
                          br(),
                          
                          # Dygraph for life signs
                          dygraphOutput("dygraph.act.e", height=90), #125
                          dygraphOutput("dygraph.temp.e", height=83),
                          dygraphOutput("dygraph.batt.e", height=83)
                        )
                      )
             )
  )
)


############### 3 - server ###############

server <- function(input, output, session){
  
  dataPerID.m <- reactive({  milsar.gps[milsar.gps$TransmGSM == input$ID.m,] })
  dataPerID.e <- reactive({ecotone.gps[ecotone.gps$TransmGSM == input$ID.e,] })
  
  # site updates when clicking on Previous / Next Red Kite
  observeEvent(input$prevBtn.m, {
    listPlacement.m <- which(Milsar.List == input$ID.m)
    if (listPlacement.m > 1) { 
      newSelection <- Milsar.List[listPlacement.m-1]
      updateSelectInput(session, inputId = "ID.m", selected = newSelection)
    }
  })  
  observeEvent(input$nextBtn.m, {
    listPlacement.m <- which(Milsar.List == input$ID.m)
    if (listPlacement.m < length(Milsar.List)) { 
      newSelection <- Milsar.List[listPlacement.m+1]
      updateSelectInput(session, inputId = "ID.m", selected = newSelection)
    }
  })  
  observeEvent(input$prevBtn.e, {
    listPlacement.e <- which(Ecotone.List == input$ID.e)
    if (listPlacement.e > 1) { 
      newSelection <- Ecotone.List[listPlacement.e-1]
      updateSelectInput(session, inputId = "ID.e", selected = newSelection)
    }
  })  
  observeEvent(input$nextBtn.e, {
    listPlacement.e <- which(Ecotone.List == input$ID.e)
    if (listPlacement.e < length(Ecotone.List)) { 
      newSelection <- Ecotone.List[listPlacement.e+1]
      updateSelectInput(session, inputId = "ID.e", selected = newSelection)
    }
  })  
  
  # determining subset based on Data to Display 
  dataInd.m <- reactive({
    if(input$PointsToDisplay.m == 1) {tail(dataPerID.m(), n=5)} #last 5 points
    else if(input$PointsToDisplay.m == 2) {tail(dataPerID.m(), n=10)} #last 10 points
    else if(input$PointsToDisplay.m == 3) {subset(dataPerID.m(), timestamp >= Sys.Date()-1)} #last 2 days
    else if(input$PointsToDisplay.m == 4) {subset(dataPerID.m(), timestamp >= Sys.Date()-4)} #last 5 days
    else if(input$PointsToDisplay.m == 5) {subset(dataPerID.m(), timestamp >= Sys.Date()-9)} #last 10 days
    else if(input$PointsToDisplay.m == 6) {dataPerID.m()} #last month
  })
  dataInd.e <- reactive({
    if(input$PointsToDisplay.e == 1) {tail(dataPerID.e(), n=5)} #last 5 points
    else if(input$PointsToDisplay.e == 2) {tail(dataPerID.e(), n=10)} #last 10 points
    else if(input$PointsToDisplay.e == 3) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-1))} #last 2 days
    else if(input$PointsToDisplay.e == 4) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-4))} #last 5 days
    else if(input$PointsToDisplay.e == 5) {subset(dataPerID.e(), timestamp >= as.POSIXct(Sys.Date()-9))} #last 10 days
    else if(input$PointsToDisplay.e == 6) {dataPerID.e()} #last month
  })
  
  # Display information of the last 5 points
  output$five.points.m <- renderTable({
    tail(dataPerID.m(), n = 6) %>% 
      mutate(dist_m = round(distHaversine(cbind(longitude, latitude),
                                          cbind(lag(longitude), lag(latitude))),0)) %>% 
      tail(n = 5) %>% 
      dplyr::mutate(time=as.character(timestamp),
                    battery = formatC(battery, digits = 2, format = "f"),
                    temperature = formatC(temperature, digits = 1, format = "f"),
                    acceleration_x = formatC(acceleration_x, digits = 2, format = "f"),
                    dist_m = formatC(dist_m, digits = 0, format = "f")) %>% 
      dplyr::select(time, bird_id, battery, temperature, acceleration_x, dist_m)
  }, spacing = "xs", align = "lrrrrr")
  output$five.points.e <- renderTable({
    tail(dataPerID.e(), n = 6) %>% 
      mutate(dist_m = round(distHaversine(cbind(longitude, latitude),
                                          cbind(lag(longitude), lag(latitude))),0)) %>% 
      tail(n = 5) %>% 
      dplyr::mutate(time=as.character(timestamp),
                    battery = formatC(battery, digits = 2, format = "f"),
                    temperature = formatC(temperature, digits = 1, format = "f"),
                    dist_m = formatC(dist_m, digits = 0, format = "f")) %>% 
      dplyr::select(time, bird_id, battery, activity, temperature, dist_m)
  }, spacing = "xs", align = "lrrrrr")
  
  # Plot GPS points on map
  output$zoomplot.m <- renderLeaflet({
    
    ### make colour palette for Date
    pal.date <- colorNumeric(palette = viridis(200), domain = NULL, reverse=T)
    
    ### legend for Date coloration
    myLabelFormat = function(...,dates=FALSE){ 
      if(dates){ 
        function(type = "numeric", cuts){
          as <- as.POSIXct(cuts, origin="1970-01-01", tz="GMT")
          format(as,"%y-%m-%d %H:%M")
        } 
      }else{
        labelFormat(...)
      }
    }
    
    l1.m <- leaflet(options = leafletOptions(zoomControl = F) #zoom Snap controls padding of points to map border, but then
                    #zoom symbols (+,-) don't work
    ) %>% #changes position of zoom symbol
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ 
                           position: 'bottomright' }).addTo(this)}"
      ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
      addProviderTiles("Esri.WorldImagery", group = "Satellite",
                       options = providerTileOptions(opacity = 0.6, attribution = F)) %>%
      addProviderTiles("OpenTopoMap", group = "Roadmap", options = providerTileOptions(attribution = F)) %>%  
      addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%  
      addCircleMarkers(
        data=dataInd.m(), lng=dataInd.m()$longitude, lat=dataInd.m()$latitude,
        radius = 5,
        stroke = TRUE, color = "black", weight = 0.5,
        fillColor = ~pal.date(num_time), fillOpacity = 1,
        popup = ~ paste0("bird ID: ", bird_id, "<br>", timestamp, "<br>batt.: ", battery, 
                         " V<br>temp.: ", temperature, " °C<br>acc. X: ", acceleration_x)
      ) %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F)) %>% 
      addLegend(     # legend for date (viridis scale)
        data = dataInd.m(),
        position = "topleft", 
        pal = pal.date,
        values = ~num_time,
        opacity = 1,
        bins = 4,
        labFormat = myLabelFormat(dates=T),
        title = NULL
      )
  })      
  output$zoomplot.e <- renderLeaflet({
    
    ### make colour palette for Date
    pal.date <- colorNumeric(palette = viridis(200), domain = NULL, reverse=T)
    
    ### legend for Date coloration
    myLabelFormat = function(...,dates=FALSE){ 
      if(dates){ 
        function(type = "numeric", cuts){
          as <- as.POSIXct(cuts, origin="1970-01-01", tz="GMT")
          format(as,"%y-%m-%d %H:%M")
        } 
      }else{
        labelFormat(...)
      }
    }
    
    l1.e <- leaflet(options = leafletOptions(zoomControl = FALSE) #zoom Snap controls padding of points to map border, but then
                    #zoom symbols (+,-) don't work
    ) %>% #changes position of zoom symbol
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this)}"
      ) %>% #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
      addProviderTiles("Esri.WorldImagery", group = "Satellite",
                       options = providerTileOptions(opacity = 0.6, attribution = F)) %>%
      addProviderTiles("CartoDB.Voyager", group = "Roadmap", options = providerTileOptions(attribution = F)) %>%  
      addLayersControl(baseGroups = c("Satellite", "Roadmap")) %>%  
      addCircleMarkers(
        data=dataInd.e(), lng=dataInd.e()$longitude, lat=dataInd.e()$latitude,
        radius = 5,
        stroke = TRUE, color = "black", weight = 0.5,
        fillColor = ~pal.date(num_time), fillOpacity = 1,
        popup = ~ paste0("bird ID: ", bird_id, "<br>", timestamp, "<br>batt.: ", battery, 
                         " V<br>act.: ", activity, "<br>temp.: ", temperature, " °C")
      ) %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F)) %>% 
      #addMeasure(  ## doesn''t work for some reason
      #  position = "bottomleft",
      #  primaryLengthUnit = "kilometers",
      #  activeColor = "#3D535D",
      #  completedColor = "#7D4479"
      #) %>% 
      addLegend(     # legend for date (viridis scale)
        data = dataInd.e(),
        position = "topleft", 
        pal = pal.date,
        values = ~num_time,
        opacity = 1,
        bins = 4,
        labFormat = myLabelFormat(dates=T),
        title = NULL
      )
  })
  
  # min-max values, fixing issue for when there is only 1 datapoint
  valueRange.m.acc <- reactive({
    if (min(dataPerID.m()$acceleration_x, na.rm=T) == max(dataPerID.m()$acceleration_x, na.rm=T)) {
      c(min(dataPerID.m()$acceleration_x, na.rm=T) - 1, max(dataPerID.m()$acceleration_x, na.rm=T) + 1)}
    else {
      c(min(dataPerID.m()$acceleration_x, na.rm=T), max(dataPerID.m()$acceleration_x, na.rm=T))}
  })
  valueRange.m.temp <- reactive({
    if (min(dataPerID.m()$temperature, na.rm=T) == max(dataPerID.m()$temperature, na.rm=T)) {
      c(min(dataPerID.m()$temperature, na.rm=T) - 1, max(dataPerID.m()$temperature, na.rm=T) + 1)}
    else {
      c(min(dataPerID.m()$temperature, na.rm=T), max(dataPerID.m()$temperature, na.rm=T))}
  })
  valueRange.m.batt <- reactive({
    if (min(dataPerID.m()$battery, na.rm=T) == max(dataPerID.m()$battery, na.rm=T)) {
      c(min(dataPerID.m()$battery, na.rm=T) - 1, max(dataPerID.m()$battery, na.rm=T) + 1)}
    else {
      c(min(dataPerID.m()$battery, na.rm=T), max(dataPerID.m()$battery, na.rm=T))}
  })
  valueRange.e.act <- reactive({
    if (min(dataPerID.e()$activity, na.rm=T) == max(dataPerID.e()$activity, na.rm=T)) {
      c(min(dataPerID.e()$activity, na.rm=T) - 1, max(dataPerID.e()$activity, na.rm=T) + 1)}
    else {
      c(min(dataPerID.e()$activity, na.rm=T), max(dataPerID.e()$activity, na.rm=T))}
  })
  valueRange.e.temp <- reactive({
    if (min(dataPerID.e()$temperature, na.rm=T) == max(dataPerID.e()$temperature, na.rm=T)) {
      c(min(dataPerID.e()$temperature, na.rm=T) - 1, max(dataPerID.e()$temperature, na.rm=T) + 1)}
    else {
      c(min(dataPerID.e()$temperature, na.rm=T), max(dataPerID.e()$temperature, na.rm=T))}
  })
  valueRange.e.batt <- reactive({
    if (min(dataPerID.e()$battery, na.rm=T) == max(dataPerID.e()$battery, na.rm=T)) {
      c(min(dataPerID.e()$battery, na.rm=T) - 1, max(dataPerID.e()$battery, na.rm=T) + 1)}
    else {
      c(min(dataPerID.e()$battery, na.rm=T), max(dataPerID.e()$battery, na.rm=T))}
  })
  
  # Plot life signs as dygraph MILSAR
  output$dygraph.acc.m <- renderDygraph({
    if(all(is.na(dataInd.m()[c('acceleration_x')]))){
      return(NULL)} else {
        dygraph(data = xts::as.xts(x = subset(dataInd.m(), select=acceleration_x), order.by = dataInd.m()$timestamp), 
                group = "life signs Milsar") %>% 
          dyShading(axis = "x", from = min(dataInd.m()$timestamp), to = max(dataInd.m()$timestamp), 
                    color = "white") %>% #background
          dyShading(axis = "y", from = AccLwrLimDanger, to = AccUprLimDanger, color = rgb.red) %>% #danger - red
          dyShading(axis = "y", from = AccLwrLimRisk, to = AccLwrLimDanger, color = rgb.yellow) %>% 
          dyShading(axis = "y", from = AccUprLimDanger, to = AccUprLimRisk, color = rgb.yellow) %>% #risk - yellow
          dyShading(axis = "y", from = AccLwrLimSafe, to = AccLwrLimRisk, color = rgb.green) %>% 
          dyShading(axis = "y", from = AccUprLimRisk, to = AccUprLimSafe, color = rgb.green) %>% #safe - green
          dySeries(label = "Acceleration", color="blue") %>%
          dyAxis("x", axisLabelFontSize=0, valueFormatter=JS(valueFormatter)) %>%
          dyAxis("y", label = "Acceleration", valueRange = valueRange.m.acc,
                 pixelsPerLabel=10, labelWidth=15, rangePad=5, axisLabelFontSize=10,
                 axisLabelWidth=45) %>% #controls width between label and plot
          dyOptions(useDataTimezone = TRUE, #enable for original time zone UTC, disable for automatic switch to client's tz
                    axisLabelColor = "white", axisLineColor="lightgrey", gridLineColor="white") %>% 
          dyLegend(width=120) %>% #show="follow": for the mouseover legend to follow the mouse, but that also means it will go over the plot border
          dyCSS(textConnection("
          .dygraph-legend {
              font-size: 10px !important;
              color: black; 
              background-color: transparent !important;} 
              ") #also interesting:  "text-align: right !important;"
          )
      }
  })
  output$dygraph.temp.m <- renderDygraph({
    if(all(is.na(dataInd.m()[c('temperature')]))){
      return(NULL)} else {
        dygraph(data = xts::as.xts(x = subset(dataInd.m(), select=temperature), order.by = dataInd.m()$timestamp), 
                group = "life signs Milsar") %>% 
          dyShading(axis = "x", from = min(dataInd.m()$timestamp), to = max(dataInd.m()$timestamp), 
                    color = "white") %>% #background
          dyShading(axis = "y", from = TempLwrLimDanger, to = TempUprLimDanger, color = rgb.red) %>% #danger - red
          dyShading(axis = "y", from = TempUprLimDanger, to = TempUprLimRisk, color = rgb.yellow) %>% #risk - yellow
          dyShading(axis = "y", from = TempUprLimRisk, to = TempUprLimSafe, color = rgb.green) %>% #safe - green
          dySeries(label = "Temp. (°C)", color="red") %>%
          dyAxis("x", axisLabelFontSize=0, valueFormatter=JS(valueFormatter)) %>%
          dyAxis("y", label = "Temp. (°C)", valueRange = valueRange.m.temp, 
                 pixelsPerLabel=15, labelWidth=15, rangePad=5, axisLabelFontSize=10,
                 axisLabelWidth=45) %>% #controls width between label and plot
          dyOptions(useDataTimezone = TRUE, #enable for original time zone UTC, disable for automatic switch to client's tz
                    axisLabelColor = "white", axisLineColor="lightgrey", gridLineColor="white") %>% 
          dyLegend(width=120) %>% 
          dyCSS(textConnection("
          .dygraph-legend {
              font-size: 10px !important;
              color: black; 
              background-color: transparent !important;} 
              "))
      }
  })
  output$dygraph.batt.m <- renderDygraph({
    if(all(is.na(dataInd.m()[c('battery')]))){
      return(NULL)} else {
        dygraph(data = xts::as.xts(x = subset(dataInd.m(), select=battery), order.by = dataInd.m()$timestamp), 
                group = "life signs Milsar") %>% 
          dyShading(axis = "x", from = min(dataInd.m()$timestamp), to = max(dataInd.m()$timestamp), 
                    color = "white") %>% #background
          dyShading(axis = "y", from = -100, to = 3.8, color = rgb.red) %>% #danger - red
          dyShading(axis = "y", from = 3.8, to = 3.9, color = rgb.yellow) %>% #risk - yellow
          dyShading(axis = "y", from = 3.9, to = 100, color = rgb.green) %>% #safe - green
          dySeries(label = "Battery (V)", color="green") %>%
          dyAxis("x", valueFormatter=JS(valueFormatter)) %>%
          dyAxis("y", label = "Battery (V)", valueRange = valueRange.m.batt,
                 axisLabelFontSize=10, labelWidth=15, rangePad=5, 
                 pixelsPerLabel=15, axisLabelWidth=45) %>% #controls width between label and plot
          dyOptions(useDataTimezone = TRUE, #enable for original time zone UTC, disable for automatic switch to client's tz
                    axisLabelColor = "white", axisLineColor="lightgrey", gridLineColor="white",
                    sigFigs=2, axisLabelFontSize=10) %>%   #gridLineWidth=0.1
          dyLegend(width=120) %>% 
          dyCSS(textConnection("
          .dygraph-legend {
              font-size: 10px !important;
              color: black; 
              background-color: transparent !important;} 
              "))
      }
  })
  # Plot life signs as dygraph ECOTONE
  output$dygraph.act.e <- renderDygraph({
    if(all(is.na(dataInd.e()[c('activity')]))){
      return(NULL)} else {
        dygraph(data = xts::as.xts(x = subset(dataInd.e(), select=activity), order.by = dataInd.e()$timestamp), 
                group = "life signs Milsar") %>% 
          dyShading(axis = "x", from = min(dataInd.e()$timestamp), to = max(dataInd.e()$timestamp), 
                    color = "white") %>% #background
          dyShading(axis = "y", from = ActLwrLimDanger, to = ActUprLimDanger, color = rgb.red) %>% #danger - red
          dyShading(axis = "y", from = ActUprLimDanger, to = ActUprLimRisk, color = rgb.yellow) %>% #risk - yellow
          dyShading(axis = "y", from = ActUprLimRisk, to = ActUprLimSafe, color = rgb.green) %>% #safe - green
          dySeries(label = "Activity", color="blue") %>%
          dyAxis("x", axisLabelFontSize=0, valueFormatter=JS(valueFormatter)) %>%
          dyAxis("y", label = "Activity", valueRange = valueRange.e.act, 
                 pixelsPerLabel=15, labelWidth=15, rangePad=5, axisLabelFontSize=10,
                 axisLabelWidth=45) %>% #controls width between label and plot
          dyOptions(useDataTimezone = TRUE, #enable for original time zone UTC, disable for automatic switch to client's tz
                    axisLabelColor = "white", axisLineColor="lightgrey", gridLineColor="white") %>% 
          dyLegend(width=120) %>% 
          dyCSS(textConnection("
          .dygraph-legend {
              font-size: 10px !important;
              color: black; 
              background-color: transparent !important;} 
              "))
      }
  })
  output$dygraph.temp.e <- renderDygraph({
    if(all(is.na(dataInd.e()[c('temperature')]))){
      return(NULL)} else {
        dygraph(data = xts::as.xts(x = subset(dataInd.e(), select=temperature), order.by = dataInd.e()$timestamp), 
                group = "life signs Milsar") %>% 
          dyShading(axis = "x", from = min(dataInd.e()$timestamp), to = max(dataInd.e()$timestamp), 
                    color = "white") %>% #background
          dyShading(axis = "y", from = TempLwrLimDanger, to = TempUprLimDanger, color = rgb.red) %>% #danger - red
          dyShading(axis = "y", from = TempUprLimDanger, to = TempUprLimRisk, color = rgb.yellow) %>% #risk - yellow
          dyShading(axis = "y", from = TempUprLimRisk, to = TempUprLimSafe, color = rgb.green) %>% #safe - green
          dySeries(label = "Temp. (°C)", color="red") %>%
          dyAxis("x", axisLabelFontSize=0, valueFormatter=JS(valueFormatter)) %>%
          dyAxis("y", label = "Temp. (°C)", valueRange = valueRange.e.temp, 
                 pixelsPerLabel=15, labelWidth=15, rangePad=5, axisLabelFontSize=10,
                 axisLabelWidth=45) %>% #controls width between label and plot
          dyOptions(useDataTimezone = TRUE, #enable for original time zone UTC, disable for automatic switch to client's tz
                    axisLabelColor = "white", axisLineColor="lightgrey", gridLineColor="white") %>% 
          dyLegend(width=120) %>% 
          dyCSS(textConnection("
          .dygraph-legend {
              font-size: 10px !important;
              color: black; 
              background-color: transparent !important;} 
              "))
      }
  })
  output$dygraph.batt.e <- renderDygraph({
    if(all(is.na(dataInd.e()[c('battery')]))){
      return(NULL)} else {
        dygraph(data = xts::as.xts(x = subset(dataInd.e(), select=battery), order.by = dataInd.e()$timestamp), 
                group = "life signs Milsar") %>% 
          dyShading(axis = "x", from = min(dataInd.e()$timestamp), to = max(dataInd.e()$timestamp), 
                    color = "white") %>% #background
          dyShading(axis = "y", from = -100, to = 3.8, color = rgb.red) %>% #danger - red
          dyShading(axis = "y", from = 3.8, to = 3.9, color = rgb.yellow) %>% #risk - yellow
          dyShading(axis = "y", from = 3.9, to = 100, color = rgb.green) %>% #safe - green
          dySeries(label = "Battery (V)", color="green") %>%
          dyAxis("x", valueFormatter=JS(valueFormatter)) %>%
          dyAxis("y", label = "Battery (V)", valueRange = valueRange.e.batt, 
                 axisLabelFontSize=10, labelWidth=15, rangePad=5, 
                 pixelsPerLabel=15, axisLabelWidth=45) %>% #controls width between label and plot
          dyOptions(useDataTimezone = TRUE, #enable for original time zone UTC, disable for automatic switch to client's tz
                    axisLabelColor = "white", axisLineColor="lightgrey", gridLineColor="white",
                    sigFigs=2, axisLabelFontSize=10) %>%   #gridLineWidth=0.1
          dyLegend(width=120) %>% 
          dyCSS(textConnection("
          .dygraph-legend {
              font-size: 10px !important;
              color: black; 
              background-color: transparent !important;} 
              "))
      }
  })
}

beep("coin")


############### 4 - start shinyApp ##############

shinyApp(ui = ui, server = server)