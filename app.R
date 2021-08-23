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
               cowplot) #arrange ggplots

options(scipen = 999) #R avoids scientific style of numbers (options(scipen=0) reset to default)

Sys.setlocale("LC_TIME", "C")  #set English hours for correct x-axis

curl <- movebankLogin(username=MOVEBANK_USERNAME,  password=MOVEBANK_PASSWORD)
s <- Sys.time(); attr(s,"tzone") <- "UTC"


########### 0 - Download data MILSAR ############
# always 1 month
timestamp_end <- paste0(format(Sys.time(), format="%Y%m%d%H%M%S"), "000")
timestamp_start <- paste0(format(as.Date(Sys.time())  %m+%  days(-as.numeric(31)) , format="%Y%m%d%H%M%S"), "000")

milsar0 <- getMovebankData(study="Milvusmilvus_Milsar_SOI_final", login=curl, moveObject=TRUE, timestamp_start=timestamp_start, timestamp_end=timestamp_end)
milsar <- as.data.frame(milsar0) #different number of columns than Ecotone
milsar$acceleration <- sqrt(milsar$acceleration_x^2+milsar$acceleration_y^2+milsar$acceleration_z^2) #magnitude of acceleration
milsar.gps <- milsar %>% 
  dplyr::select(bird_id='local_identifier', 
                TransmGSM='tag_local_identifier',
                timestamp, 
                temperature='external_temperature',
                acceleration, #made of 3 vectors, see above
                tag_voltage, 
                longitude='location_long', latitude='location_lat') %>% 
  droplevels() %>% unique() %>% 
  mutate(battery = round(tag_voltage/1000, 2), 
         temperature = round(temperature, 1),
         acceleration = round(acceleration, 2),
         bird_id = as.factor(bird_id),
         date = as.Date(timestamp, format="%Y-%m-%d"),
         num_time = as.numeric(timestamp, origin=as.POSIXct("2015-01-01", tz="GMT"))) %>% #as workaround for color legend
  arrange(TransmGSM, timestamp)


########### 0 - Download data Ecotone ############
# always 1 month
timestamp_end <- paste0(format(Sys.time(), format="%Y%m%d%H%M%S"), "000")
timestamp_start <- paste0(format(as.Date(Sys.time())  %m+%  days(-as.numeric(31)) , format="%Y%m%d%H%M%S"), "000")

ecotone0 <- getMovebankData(study="Milvusmilvus_GSM_SOI", login=curl, moveObject=TRUE, timestamp_start=timestamp_start, timestamp_end=timestamp_end)
ecotone <- as.data.frame(ecotone0) #different number of columns than Ecotone
#ecotone$acceleration <- sqrt(ecotone$acceleration_x^2+ecotone$acceleration_y^2+ecotone$acceleration_z^2) #all NA in movebank
ecotone.gps <- ecotone %>% 
  dplyr::select(bird_id='local_identifier', 
                TransmGSM='comments',
                timestamp, 
                temperature='external_temperature',
                #acceleration, #made of 3 vectors, see above
                tag_voltage, 
                longitude='location_long', latitude='location_lat') %>% 
  droplevels() %>% unique() %>% 
  mutate(battery = round(tag_voltage/1000, 2), 
         temperature = round(temperature, 1),
         #acceleration = round(acceleration, 2),
         bird_id = as.factor(bird_id),
         date = as.Date(timestamp, format="%Y-%m-%d"),
         num_time = as.numeric(timestamp, origin=as.POSIXct("2015-01-01", tz="GMT"))) %>% #as workaround for color legend
  arrange(TransmGSM, timestamp)



########### 1 - Pre-shiny Preparation ###########
# code to make actionButtons also work by hitting Enter
# @Eli Berkow, https://stackoverflow.com/questions/53642006/proxy-click-not-functioning-in-modal
jscode <- '
$(document).keyup(function(event) {
    if ((event.keyCode == 13)) {
        $("#button").click();
    }
});
'

########### 2 - user interface ###########

ui <- fluidPage(
  navbarPage(
    "Bird Checking",
    theme = shinytheme("darkly"),
    
    ### MILSAR ###
    tabPanel("Milsar",
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 selectInput(inputId = "ID.m", label = "Red Kite", choices = unique(milsar.gps$TransmGSM), multiple = F),
                 column(6, actionButton("prevBtn.m", "<<"), align = "right"),
                 column(6, actionButton("nextBtn.m", ">>"), align = "left"),#style='padding:4px; font-size:80%')
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
                 
                 # Plot life signs
                 plotOutput(outputId = "life.signs.m",
                            width = "auto", height = 250,
                            hover = "plot_hover")
               )
             )
    ),
    
    ### ECOTONE ###
    tabPanel("Ecotone",
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 selectInput(inputId = "ID.e", label = "Red Kite", choices = unique(ecotone.gps$TransmGSM), multiple = F),
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
                 
                 # Plot life signs
                 plotOutput(outputId = "life.signs.e",
                            width = "auto", height = 250,
                            hover = "plot_hover")
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
    listPlacement.m <- which(unique(milsar.gps$TransmGSM) == input$ID.m)
    if (listPlacement.m > 1) { 
      newSelection <- unique(milsar.gps$TransmGSM)[listPlacement.m-1]
      updateSelectInput(session, inputId = "ID.m", selected = newSelection)
   }
  })  
  observeEvent(input$nextBtn.m, {
    listPlacement.m <- which(unique(milsar.gps$TransmGSM) == input$ID.m)
    if (listPlacement.m < length(unique(milsar.gps$TransmGSM))) { 
      newSelection <- unique(milsar.gps$TransmGSM)[listPlacement.m+1]
      updateSelectInput(session, inputId = "ID.m", selected = newSelection)
    }
  })  
  observeEvent(input$prevBtn.e, {
    listPlacement.e <- which(unique(ecotone.gps$TransmGSM) == input$ID.e)
    if (listPlacement.e > 1) { 
      newSelection <- unique(ecotone.gps$TransmGSM)[listPlacement.e-1]
      updateSelectInput(session, inputId = "ID.e", selected = newSelection)
    }
  })  
  observeEvent(input$nextBtn.e, {
    listPlacement.e <- which(unique(ecotone.gps$TransmGSM) == input$ID.e)
    if (listPlacement.e < length(unique(ecotone.gps$TransmGSM))) { 
      newSelection <- unique(ecotone.gps$TransmGSM)[listPlacement.e+1]
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
    else if(input$PointsToDisplay.e == 3) {subset(dataPerID.e(), timestamp >= Sys.Date()-1)} #last 2 days
    else if(input$PointsToDisplay.e == 4) {subset(dataPerID.e(), timestamp >= Sys.Date()-4)} #last 5 days
    else if(input$PointsToDisplay.e == 5) {subset(dataPerID.e(), timestamp >= Sys.Date()-9)} #last 10 days
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
                    acceleration = formatC(acceleration, digits = 2, format = "f"),
                    dist_m = formatC(dist_m, digits = 0, format = "f")) %>% 
      dplyr::select(time, battery, temperature, acceleration, dist_m)
    }, spacing = "xs", align = "lrrrr")
  output$five.points.e <- renderTable({
    tail(dataPerID.e(), n = 6) %>% 
      mutate(dist_m = round(distHaversine(cbind(longitude, latitude),
                                          cbind(lag(longitude), lag(latitude))),0)) %>% 
      tail(n = 5) %>% 
      dplyr::mutate(time=as.character(timestamp),
                    battery = formatC(battery, digits = 2, format = "f"),
                    temperature = formatC(temperature, digits = 1, format = "f"),
                    dist_m = formatC(dist_m, digits = 0, format = "f")) %>% 
      dplyr::select(time, battery, temperature, dist_m) #, acceleration)
  }, spacing = "xs", align = "lrrr")
  
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
        popup = ~ paste0("bird ID: ", bird_id, "<br>", timestamp, "<br>batt.: ", battery, " V<br>temp.: ", temperature, " °C<br>acc.: ", acceleration)
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
        popup = ~ paste0("bird ID: ", bird_id,"<br>", timestamp, "<br>batt.: ", battery, " V<br>temp.: ", temperature, " °C") #, "<br>acc.: ", acceleration)
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
  
  # Plot life signs as graph
  output$life.signs.m = renderPlot({
    if(all(is.na(dataInd.m()[c('battery', 'temperature', 'acceleration')]))){
      return(NULL)} else {
        alpha <- 0.2
        colDanger <- "red"; colRisk <- "yellow"; colSafe <- "green"
        if(input$PointsToDisplay.m == 6){
          minTemp <- as.POSIXct(s)  %m+%  days(-as.numeric(31))
          maxTemp <- s
        } else {
          minTemp <- min(dataInd.m()$timestamp, na.rm =T)
          maxTemp <- max(dataInd.m()$timestamp, na.rm =T)
        }
        
        minAcc <- min(dataInd.m()$acceleration, na.rm=T)
        maxAcc <- max(dataInd.m()$acceleration, na.rm=T)
        AccDangerLwrLimit <- 9.7; AccDangerUprLimit <- 10.1
        AccRiskLwrLimit <- 8; AccRiskUprLimit <- 12

        globalMinTemp <- min(dataPerID.m()$temperature, na.rm=T)
        globalMaxTemp <- max(dataPerID.m()$temperature, na.rm=T)
        TempDangerUprLimit <- 10; TempRiskUprLimit <- 20
        
        
        # Battery plot
        p.batt.m <- dataInd.m()[!is.na(dataInd.m()$battery),] %>%
          dplyr::select(timestamp, battery) %>%
          ggplot(aes(x = timestamp, y = battery)) + #, 
          geom_line(col="springgreen3") +
          geom_point(col="springgreen3", size=0.1) +
          scale_y_continuous(limits=c(min(dataPerID.m()$battery, na.rm=T), max(dataPerID.m()$battery, na.rm=T))) +
          ylab("Battery (V)") + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.y = element_text(size=10),
                axis.text.x = element_blank())
        
        # Acceleration plot
        p.acti.m <- dataInd.m()[!is.na(dataInd.m()$acceleration),] %>%
          dplyr::select(timestamp, acceleration) %>%
          ggplot(aes(x = timestamp, y = acceleration)) + #, 
          scale_y_continuous(limits=c(min(dataPerID.m()$acceleration, na.rm=T), max(dataPerID.m()$acceleration, na.rm=T)))
        #reactive rectangles
        if(minAcc>=AccDangerLwrLimit & maxAcc<AccDangerUprLimit) {
          p.acti.m2 <- p.acti.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=Inf, fill=colDanger, alpha=alpha)
        } else if(minAcc >= AccRiskLwrLimit & maxAcc < AccRiskUprLimit) {
          p.acti.m2 <- p.acti.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=AccDangerLwrLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccDangerLwrLimit, ymax=AccDangerUprLimit, fill=colDanger, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccDangerUprLimit, ymax=Inf, fill=colRisk, alpha=alpha)
        } else if(minAcc >= AccRiskLwrLimit & maxAcc >= AccRiskUprLimit) {
          p.acti.m2 <- p.acti.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=AccRiskLwrLimit, fill=colSafe, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccRiskLwrLimit, ymax=AccDangerLwrLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccDangerLwrLimit, ymax=AccDangerUprLimit, fill=colDanger, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccDangerUprLimit, ymax=AccRiskUprLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccRiskUprLimit, ymax=Inf, fill=colSafe, alpha=alpha)
        } else if(minAcc < AccRiskLwrLimit & maxAcc < AccRiskUprLimit) {
          p.acti.m2 <- p.acti.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=AccRiskLwrLimit, fill=colSafe, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccRiskLwrLimit, ymax=AccDangerLwrLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccDangerLwrLimit, ymax=AccDangerUprLimit, fill=colDanger, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccDangerUprLimit, ymax=AccRiskUprLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccRiskUprLimit, ymax=Inf, fill=colSafe, alpha=alpha)
        } else if(minAcc <AccRiskLwrLimit & maxAcc >= AccRiskUprLimit) {
          p.acti.m2 <- p.acti.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=AccRiskLwrLimit, fill=colSafe, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccRiskLwrLimit, ymax=AccDangerLwrLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccDangerLwrLimit, ymax=AccDangerUprLimit, fill=colDanger, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccDangerUprLimit, ymax=AccRiskUprLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=AccRiskUprLimit, ymax=Inf, fill=colSafe, alpha=alpha)
        }        
        p.acti.m3 <- p.acti.m2 + 
          geom_line(col="blue") +
          geom_point(col="blue", size=0.1) +
          ylab("Acceleration") + 
          theme(axis.title.y = element_text(size=10),
                axis.title.x = element_blank())
        
        # Temperature plot
        p.temp.m <- dataInd.m()[!is.na(dataInd.m()$temperature),] %>%
          dplyr::select(timestamp, temperature) %>%
          ggplot(aes(x = timestamp, y = temperature)) + 
          scale_y_continuous(limits=c(globalMinTemp, globalMaxTemp))
        #reactive rectangles
        if(globalMinTemp >= TempRiskUprLimit) {
          p.temp.m2 <- p.temp.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=Inf, fill=colSafe, alpha=alpha)
        } else if(globalMinTemp >= TempDangerUprLimit & globalMinTemp < TempRiskUprLimit &
                  globalMaxTemp >= TempRiskUprLimit) {
          p.temp.m2 <- p.temp.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=TempRiskUprLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=TempRiskUprLimit, ymax=Inf, fill=colSafe, alpha=alpha)
        } else if(globalMinTemp >= TempDangerUprLimit & globalMinTemp < TempRiskUprLimit &
                  globalMaxTemp < TempRiskUprLimit) {
          p.temp.m2 <- p.temp.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=Inf, fill=colRisk, alpha=alpha)
        } else if(globalMinTemp < TempDangerUprLimit &
                  globalMaxTemp >= TempRiskUprLimit) {
          p.temp.m2 <- p.temp.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=TempDangerUprLimit, fill=colDanger, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=TempDangerUprLimit, ymax=TempRiskUprLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=TempRiskUprLimit, ymax=Inf, fill=colSafe, alpha=alpha)
        } else if(globalMinTemp < TempDangerUprLimit &
                  globalMaxTemp < TempRiskUprLimit) {
          p.temp.m2 <- p.temp.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=TempDangerUprLimit, fill=colDanger, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=TempDangerUprLimit, ymax=Inf, fill=colRisk, alpha=alpha)
        } else if(globalMaxTemp < TempRiskUprLimit) {
          p.temp.m2 <- p.temp.m + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=Inf, fill=colDanger, alpha=alpha)
        }
        p.temp.m3 <- p.temp.m2 + 
          geom_line(col="red") +
          geom_point(col="red", size=0.1) +
          ylab("Temp. (°C)") + 
          theme(axis.title.y = element_text(size=TempDangerUprLimit),
                axis.title.x = element_blank())
        
        cowplot::plot_grid(p.batt.m, p.temp.m3, p.acti.m3, align = "v", ncol = 1) #, rel_heights = c(0.25, 0.75))
      }
  })
  output$life.signs.e = renderPlot({
    
    if(all(is.na(dataInd.e()[c('battery', 'temperature')]))){
      return(NULL)} else {
        alpha <- 0.2
        colDanger <- "red"; colRisk <- "yellow"; colSafe <- "green"
        if(input$PointsToDisplay.e == 6){
          minTemp <- as.POSIXct(s)  %m+%  days(-as.numeric(31))
          maxTemp <- s
        } else {
          minTemp <- min(dataInd.e()$timestamp, na.rm =T)
          maxTemp <- max(dataInd.e()$timestamp, na.rm =T)
        }
        
        globalMinTemp <- min(dataPerID.e()$temperature, na.rm=T)
        globalMaxTemp <- max(dataPerID.e()$temperature, na.rm=T)
        TempDangerUprLimit <- 10; TempRiskUprLimit <- 20

        # Battery plot
        p.batt.e <- dataInd.e() %>%
          dplyr::select(timestamp, battery) %>%
          ggplot(aes(x = timestamp, y = battery)) +
          geom_line(col="springgreen3") +
          scale_y_continuous(limits=c(min(dataPerID.e()$battery, na.rm=T), max(dataPerID.e()$battery, na.rm=T))) +
          ylab("Battery (V)") + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.y = element_text(size=10),
                axis.text.x = element_blank())
        
        # Temperature plot
        p.temp.e <- dataInd.e() %>%
          dplyr::select(timestamp, temperature) %>%
          ggplot(aes(x = timestamp, y = temperature)) +
          scale_y_continuous(limits=c(min(dataPerID.e()$temperature, na.rm=T), max(dataPerID.e()$temperature, na.rm=T)))
        #reactive rectangles
        if(globalMinTemp >= TempRiskUprLimit) {
          p.temp.e2 <- p.temp.e + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=Inf, fill=colSafe, alpha=alpha)
        } else if(globalMinTemp >= TempDangerUprLimit & globalMinTemp < TempRiskUprLimit &
                  globalMaxTemp >= TempRiskUprLimit) {
          p.temp.e2 <- p.temp.e + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=TempRiskUprLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=TempRiskUprLimit, ymax=Inf, fill=colSafe, alpha=alpha)
        } else if(globalMinTemp >= TempDangerUprLimit & globalMinTemp < TempRiskUprLimit &
                  globalMaxTemp < TempRiskUprLimit) {
          p.temp.e2 <- p.temp.e + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=Inf, fill=colRisk, alpha=alpha)
        } else if(globalMinTemp < TempDangerUprLimit &
                  globalMaxTemp >= TempRiskUprLimit) {
          p.temp.e2 <- p.temp.e + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=TempDangerUprLimit, fill=colDanger, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=TempDangerUprLimit, ymax=TempRiskUprLimit, fill=colRisk, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=TempRiskUprLimit, ymax=Inf, fill=colSafe, alpha=alpha)
        } else if(globalMinTemp < TempDangerUprLimit &
                  globalMaxTemp < TempRiskUprLimit) {
          p.temp.e2 <- p.temp.e + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=TempDangerUprLimit, fill=colDanger, alpha=alpha) +
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=TempDangerUprLimit, ymax=Inf, fill=colRisk, alpha=alpha)
        } else if(globalMaxTemp < TempDangerUprLimit) {
          p.temp.e2 <- p.temp.e + 
            annotate("rect", xmin=minTemp, xmax=maxTemp, 
                     ymin=-Inf, ymax=Inf, fill=colDanger, alpha=alpha)
        }
        p.temp.e3 <- p.temp.e2 + geom_line(col="red") +
          ylab("Temp. (°C)") + 
          theme(axis.title.y = element_text(size=10),
                axis.title.x = element_blank())
        
        cowplot::plot_grid(p.batt.e, p.temp.e3, align = "v", ncol = 1) #, rel_heights = c(0.25, 0.75))
      }
  })
}



############### 4 - start shinyApp ##############

shinyApp(ui = ui, server = server)
