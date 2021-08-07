if (!require("pacman")) install.packages("pacman")
# for shiny app
pacman::p_load(dplyr, # always
               ggplot2,
               lemon, #for function facet_rep_wrap()
               lubridate, #time stuff
               viridis, #viridis color scale
               shiny, #shiny app
               shinythemes, #ggmap, #if using maps with download tiles in shiny output
               tidyr, #for using gather(), rearranging data
               move, #movebank
               leaflet, #map making
               cowplot) #arrange ggplots

options(scipen = 999) #R avoids scientific style of numbers (options(scipen=0) reset to default)

Sys.setlocale("LC_TIME", "C")  #set English hours for correct x-axis

curl <- movebankLogin(username=MOVEBANK_USERNAME,  password=MOVEBANK_PASSWORD)



########### 0 - Download data MILSAR ############
# always 1 month
timestamp_end <- paste0(format(Sys.Date(), format="%Y%m%d%H%M%S"), "000")
timestamp_start <- paste0(format(as.Date(Sys.Date())  %m+%  days(-as.numeric(31)) , format="%Y%m%d%H%M%S"), "000")

milsar0 <- getMovebankData(study="Milvusmilvus_Milsar_SOI_final", login=curl, moveObject=TRUE, timestamp_start=timestamp_start, timestamp_end=timestamp_end)
milsar <- as.data.frame(milsar0) #different number of columns than Ecotone
milsar$acceleration <- sqrt(milsar$acceleration_x^2+milsar$acceleration_y^2+milsar$acceleration_z^2) #magnitude of acceleration
milsar.gps <- milsar %>% 
  dplyr::select(bird_id='local_identifier', 
                TransmGSM='tag_local_identifier',
                timestamp, 
                temperature='cpu_temperature', #probably rather the cpu_temperature chosen here, rather than external temperature
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
  arrange(bird_id, timestamp)


########### 0 - Download data Ecotone ############
# always 1 month
timestamp_end <- paste0(format(Sys.Date(), format="%Y%m%d%H%M%S"), "000")
timestamp_start <- paste0(format(as.Date(Sys.Date())  %m+%  days(-as.numeric(31)) , format="%Y%m%d%H%M%S"), "000")

ecotone0 <- getMovebankData(study="Milvusmilvus_GSM_SOI", login=curl, moveObject=TRUE, timestamp_start=timestamp_start, timestamp_end=timestamp_end)
ecotone <- as.data.frame(ecotone0) #different number of columns than Ecotone
#ecotone$acceleration <- sqrt(ecotone$acceleration_x^2+ecotone$acceleration_y^2+ecotone$acceleration_z^2) #all NA in movebank
ecotone.gps <- ecotone %>% 
  dplyr::select(bird_id='local_identifier', 
                TransmGSM='comments',
                timestamp, 
                temperature='external_temperature', #cpu_temperature not available here
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



########### 1 - user interface ###########

ui <- fluidPage(
  navbarPage(
    "Bird Checking",
    theme = shinytheme("darkly"),
    
    ### MILSAR ###
    tabPanel("Milsar",
             
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 selectInput(inputId = "ID.m", label = "Red Kite", choices = unique(milsar.gps$bird_id), multiple = F),
                 
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
                 #h4("Last 5 points"),
                 tableOutput("five.points.m"),
                 
                 hr(),
                 
                 # Plot points on map
                 #h4("Mapped data"),
                 leafletOutput("zoomplot.m", height = 300),
                 
                 br(),
                 
                 # Plot life signs
                 #h4("Life signs"),
                 plotOutput(outputId = "life.signs.m",
                            width = "auto", height = 200,
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
                 #h4("Last 5 points"),
                 tableOutput("five.points.e"),
                 
                 hr(),
                 
                 # Plot points on map
                 #h4("Mapped data"),
                 leafletOutput("zoomplot.e", height = 300),
                 
                 br(),
                 
                 # Plot life signs
                 #h4("Life signs"),
                 plotOutput(outputId = "life.signs.e",
                            width = "auto", height = 200,
                            hover = "plot_hover")
               )
             )
    )
  )
)    
  

    

############### 2 - server ###############

server <- function(input, output, session){

  dataPerID.m <- reactive({  milsar.gps[milsar.gps$bird_id == input$ID.m,] })
  dataPerID.e <- reactive({ecotone.gps[ecotone.gps$TransmGSM == input$ID.e,] })
  
  # determining subset based on Data to Display 
  dataInd.m <- reactive({
    if(input$PointsToDisplay.m == 1) {tail(dataPerID.m(), n=5)} #last 5 points
    else if(input$PointsToDisplay.m == 2) {tail(dataPerID.m(), n=10)} #last 10 points
    else if(input$PointsToDisplay.m == 3) {subset(dataPerID.m(), timestamp >= Sys.Date()-1)} #last 2 days
    else if(input$PointsToDisplay.m == 4) {subset(dataPerID.m(), timestamp >= Sys.Date()-4)} #last 5 days
    else if(input$PointsToDisplay.m == 5) {subset(dataPerID.m(), timestamp >= Sys.Date()-9)} #last 10 days
    else if(input$PointsToDisplay.m == 6) {dataPerID.m()} #last month
    })
  # determining subset based on Data to Display 
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
    tail(dataPerID.m(), n = 5) %>% 
      dplyr::mutate(time=as.character(timestamp),
                    battery = formatC(battery, digits = 2, format = "f"),
                    temperature = formatC(temperature, digits = 1, format = "f"),
                    acceleration = formatC(acceleration, digits = 2, format = "f")) %>% 
      dplyr::select(time, battery, temperature, acceleration)
    }, spacing = "xs", align = "lrrr")
  # Display information of the last 5 points
  output$five.points.e <- renderTable({
    tail(dataPerID.e(), n = 5) %>% 
      dplyr::mutate(time=as.character(timestamp),
                    battery = formatC(battery, digits = 2, format = "f"),
                    temperature = formatC(temperature, digits = 1, format = "f")) %>% 
      dplyr::select(time, battery, temperature) #, acceleration)
  }, spacing = "xs", align = "lrr")
  
  
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
    
    l1.m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% #changes position of zoom symbol
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this)}"
      ) %>%
      addProviderTiles(providers$CartoDB.Voyager,# Esri.WorldStreetMap,  #providers$OpenTopoMap
                       options = providerTileOptions(opacity = 0.7) #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
      ) %>%  
      addCircleMarkers(
        data=dataInd.m(), lng=dataInd.m()$longitude, lat=dataInd.m()$latitude,
        radius = 5,
        stroke = TRUE, color = "black", weight = 0.5,
        fillColor = ~pal.date(num_time), fillOpacity = 1,
        popup = ~ paste0("TransmGSM: ", TransmGSM, "<br>", timestamp, "<br>batt.: ", battery, " V<br>temp.: ", temperature, " °C<br>acc.: ", acceleration)
      ) %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F)) %>% 
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      ) %>% 
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
  # Plot GPS points on map
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
    
    l1.e <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% #changes position of zoom symbol
      htmlwidgets::onRender("function(el, x) {L.control.zoom({ position: 'topright' }).addTo(this)}"
      ) %>%
      addProviderTiles(providers$CartoDB.Voyager,  #OpenTopoMap,
                       options = providerTileOptions(opacity = 0.7) #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
      ) %>%  
      addCircleMarkers(
        data=dataInd.e(), lng=dataInd.e()$longitude, lat=dataInd.e()$latitude,
        radius = 5,
        stroke = TRUE, color = "black", weight = 0.5,
        fillColor = ~pal.date(num_time), fillOpacity = 1,
        popup = ~ paste0("bird ID: ", bird_id,"<br>", timestamp, "<br>batt.: ", battery, " V<br>temp.: ", temperature, " °C") #, "<br>acc.: ", acceleration)
      ) %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = F)) %>% 
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      ) %>% 
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
        p.batt.m <- dataInd.m() %>%
          dplyr::select(timestamp, battery) %>%
          ggplot(aes(x = timestamp, y = battery)) + #, 
          geom_line(col="red") +
          scale_y_continuous(limits=c(min(dataPerID.m()$battery), max(dataPerID.m()$battery))) +
          ylab("Battery (V)") + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.y = element_text(size=10),
                axis.text.x = element_blank())
        p.temp.m <- dataInd.m() %>%
          dplyr::select(timestamp, temperature) %>%
          ggplot(aes(x = timestamp, y = temperature)) + #, 
          geom_line(col="springgreen3") +
          scale_y_continuous(limits=c(min(dataPerID.m()$temperature), max(dataPerID.m()$temperature))) +
          ylab("Temp. (°C)") + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.y = element_text(size=10),
                axis.text.x = element_blank())
        p.acti.m <- dataInd.m() %>%
          dplyr::select(timestamp, acceleration) %>%
          ggplot(aes(x = timestamp, y = acceleration)) + #, 
          geom_line(col="blue") +
          scale_y_continuous(limits=c(min(dataPerID.m()$acceleration), max(dataPerID.m()$acceleration))) +
          ylab("Acceleration") + 
          theme(axis.title.y = element_text(size=10),
                axis.title.x = element_blank())
        
        cowplot::plot_grid(p.batt.m, p.temp.m, p.acti.m, align = "v", ncol = 1) #, rel_heights = c(0.25, 0.75))
      }
  })
  # Plot life signs as graph
  output$life.signs.e = renderPlot({
    if(all(is.na(dataInd.e()[c('battery', 'temperature')]))){
      return(NULL)} else {
        p.batt.e <- dataInd.e() %>%
          dplyr::select(timestamp, battery) %>%
          ggplot(aes(x = timestamp, y = battery)) + #, 
          geom_line(col="red") +
          scale_y_continuous(limits=c(min(dataPerID.e()$battery), max(dataPerID.e()$battery))) +
          ylab("Battery (V)") + 
          theme(axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.y = element_text(size=10),
                axis.text.x = element_blank())
        p.temp.e <- dataInd.e() %>%
          dplyr::select(timestamp, temperature) %>%
          ggplot(aes(x = timestamp, y = temperature)) + #, 
          geom_line(col="springgreen3") +
          scale_y_continuous(limits=c(min(dataPerID.e()$temperature), max(dataPerID.e()$temperature))) +
          ylab("Temp. (°C)") + 
          theme(axis.title.y = element_text(size=10),
                axis.title.x = element_blank())
        
        cowplot::plot_grid(p.batt.e, p.temp.e, align = "v", ncol = 1) #, rel_heights = c(0.25, 0.75))
      }
  })
}



############### 3 - start shinyApp ##############

shinyApp(ui = ui, server = server)
