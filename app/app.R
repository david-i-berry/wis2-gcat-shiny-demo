#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(htmltools)
library(httr)
library(jsonlite)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinyjs)

pal <- c(

)


protectedKeywords = c( '"', ' AND ', ' OR ', '*')

time_window <- 86400

logmsg <- function(message){
  if( is.list(message) ){
    for(item in message){
      logmsg(item)
    }
  }else{
    cat( file=stderr(), paste0(message,"\n") )
  }
}


ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      tags$li(class = "treeview-divider"),
      menuItem("Settings", tabName = "settings", icon = icon("gear", verify_fa = FALSE),
               selectInput("globalCatalogue", "Select a catalogue", choices = c("Canada" = "https://api.weather.gc.ca/collections/wis2-discovery-metadata/items")),
               selectInput("globalBroker", "Select a broker", choices = c("France" = "globalbroker.meteo.fr"))
      ),
      menuItem("Search", tabName = "search", icon = icon("search"), startExpanded = TRUE,
               numericInput("minx", "Min-x", min=-180, max = 180, step = 1, value = -180),
               numericInput("maxx", "Max-x", min=-180, max = 180, step = 1, value =  180),
               numericInput("miny", "Min-y", min=-180, max = 180, step = 1, value = -90),
               numericInput("maxy", "Max-y", min=-180, max = 180, step = 1, value = 90),
               textAreaInput("keywords", "Keywords",placeholder="Enter semi-colon delimited keywords"),
               actionButton("search","Search")),
      menuItem("Results/Subscribe", tabName = "results", icon = icon("download"),
               uiOutput("checkboxes")
      ),
      menuItem("Display options", tabName = "options", icon = icon(""),
               checkboxInput("displayBBOX","Display bounding boxes", value = TRUE),
               numericInput("maxAge", "Max age (minutes)", value=60)
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(type = "text/css", "#map {height: calc(100vh - 40px) !important;}"),
      tags$style(HTML(".leaflet-container { background: #e6f2ff; }")),
      tags$style(type="text/css", "#map.recalculating { opacity: 1.0 !important; }"),
      tags$style(type="text/css", "#connection_status.recalculating { opacity: 1.0!important; }")# ,
      # tags$style(type="text/css", ".main-sidebar {width: 300px;}")
    ),
    leafletOutput("map")
  )
)


makePopup <- function(feature, layer){
  layer$bindPopup(paste0(
    "<strong>Name: </strong>", feature$properties$name, "<br/>",
    "<strong>Identifier: </strong>", feature$id, "<br/>",
    "<strong>Description: </strong>", feature$properties$description)
    )
}

server <- function(input, output, session) {

  connection <- redux::hiredis(host="redis",port=6379)

  # setup map
  map_status <- reactiveVal(FALSE)
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("Esri.WorldTerrain")
    m <- m %>% setView(lat = 0, lng = 0, zoom = 2)
    m
  })


  # observer to detect when map ready
  observeEvent(input$map_zoom, {
    map_status(TRUE)
  })


  subscribed_topics = reactiveVal(NULL)

  observe({
    updateNumericInput(session, "minx", value = max(-180,input$map_bounds$west))
    updateNumericInput(session, "maxx", value = min(180,input$map_bounds$east))
    updateNumericInput(session, "miny", value = max(-90,input$map_bounds$south))
    updateNumericInput(session, "maxy", value = min(90,input$map_bounds$north))
  })

  collections <- reactiveVal(NULL)


  observe({
    # parse and add topic to item
    for( collection in collections()$features ){
      for( link in collection$links ){
        if( toupper(link$type) == "MQTT"){
          collection$topic = link$channel
        }
      }
    }
    # now update check box
    choices <- sapply( collections()$features, FUN = function(X){
      res <- NULL
      for( link in X$links ){
        if( toupper(link$type) == "MQTT"){
          collection$topic = link$channel
          res <- link$channel
          if( is.null(res) ){
            res <- link$`wmo:topic`
          }
          break
        }
      }
      if(! is.null(res) ){
        names(res) <- X$properties$title
      }
      return(res)
    })
    output$checkboxes <- renderUI({
      checkboxGroupInput("results_checkboxes", "Select collection to subscribe", choices = choices)
    })
  })


  observeEvent(input$search, {
    # build search url / query
    baseURL <-  input$globalCatalogue
    query <- "?q=mqtt"
    keywords <- trimws(input$keywords)
    keywords <- gsub(" ","%20", keywords)
    if( nchar(keywords)>0){
      for( pk in protectedKeywords){
        keywords <- gsub(pk,'', keywords)
      }
      keywords <- paste0('', trimws(unlist(strsplit(keywords,';'))),'')
      keywords <- paste0(keywords, collapse = "%20AND%20")
      keywords <- paste0("%20AND%20", keywords,"")
    }else{
      keywords <- ''
    }
    bbox <- paste0("&bbox=", paste(input$minx,input$miny,input$maxx,input$maxy,sep="%2C"))
    query <- paste0(query, keywords, bbox, sep= "")
    logmsg(paste0(baseURL,query,"&f=json"))
    response <- GET(paste0(baseURL,query,"&f=json"), timeout(10))
    if( response$status_code == 200){
      collections(fromJSON(content(response, as = "text"), simplifyDataFrame = FALSE))
      if(map_status()){
        map <- leafletProxy("map")
        map <- map %>% removeGeoJSON("collections")
        if( input$displayBBOX){
          map <- map %>% addGeoJSON( collections(), layerId = "collections")
        }
      }
    }else{
      logmsg(response$status_code)
      collections(NULL)
    }
  })

  observe({
    map <- leafletProxy("map")
    if(input$displayBBOX){
      map <- map %>% removeGeoJSON("collections") %>% addGeoJSON( collections(),  layerId = "collections" )
    }else{
      map <- map %>% removeGeoJSON("collections")
    }
  })


  selected <- reactive({
    input$results_checkboxes
  })

  observe({
    selected_options <- selected()
    logmsg(paste(selected_options, collapse="\n"))
    previous <- subscribed_topics()
    deleted_topics <- previous[! (previous %in% selected_options)]
    new_topics <- selected_options[ !(selected_options %in% previous)]
    for(topic in new_topics){
      connection$PUBLISH("topic_add", topic)
    }
    for(topic in deleted_topics){
      connection$PUBLISH("topic_remove", topic)
    }
    subscribed_topics(selected_options)
  })

  values <- reactiveValues(obs = data.frame() )
  observe({
    invalidateLater(1000*5, session) # update every 30 seconds
    if( map_status() ){
      isolate({
        logmsg(paste0(Sys.time(), ": Fetching data ...\n"))
        min_time <- Sys.time() - input$maxAge*60
        min_score <- as.numeric(format(min_time, "%s"))
        topics <- selected()
        ids <- list()
        for( topic in topics){
          logmsg(topic)
          ids <- append(ids, connection$ZRANGEBYSCORE(topic,min_score,"+inf"))
        }#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(htmltools)
library(httr)
library(jsonlite)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(viridis)

pal_range <- list(
  blue = c(min=NA, max=NA),
  pressure = c(min=75000, max =  90000),
  mslp     = c(min=101000, max = 103000),
  air_temperature      = c(min = 280.0, max = 300.0),
  dewpoint_temperature = c(min = 275.0, max = 295.0),
  wind_speed = c(min = 0, max = 10),
  wind_direction = c(min = 0, max = 360)
)


pal_options <- c(
  "blue" = "blue",
  "cyan" = "cyan",
  "green" = "green",
  "Station level pressure" = "pressure",
  "Pressure reduced to mean sea level" = "mslp",
  "Air temperature" = "air_temperature",
  "Dewpoint temperature" = "dewpoint_temperature",
  "Wind speed" = "wind_speed",
  "Wind direction" = "wind_direction"
)

pal <- viridis(22)


calc_cind <- function(data, range_){
  topbin <- which(data >= range_[2])
  bottombin <- which(data < range_[1])
  valid <- which(!is.na(data))
  cind <- rep(NA, length(data))
  if(length(valid)){
    cind[valid] <- cut(data[valid], breaks = seq(range_[1], range_[2], length.out=21), labels=FALSE) + 1
    cind[bottombin] <- 1
    cind[topbin] <- 22
  }
  return(cind)
}

protectedKeywords = c( '"', ' AND ', ' OR ', '*')


logmsg <- function(message){
  if( is.list(message) ){
    for(item in message){
      logmsg(item)
    }
  }else{
    cat( file=stderr(), paste0(message,"\n") )
  }
}


ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      tags$li(class = "treeview-divider"),
      menuItem("Settings", tabName = "settings", icon = icon("gear", verify_fa = FALSE),
               selectInput("globalCatalogue", "Select a catalogue", choices = c("Canada" = "https://api.weather.gc.ca/collections/wis2-discovery-metadata/items")),
               selectInput("globalBroker", "Select a broker", choices = c("France" = "globalbroker.meteo.fr"))
      ),
      menuItem("Search", tabName = "search", icon = icon("search"), startExpanded = TRUE,
               numericInput("minx", "Min-x", min=-180, max = 180, step = 1, value = -180),
               numericInput("maxx", "Max-x", min=-180, max = 180, step = 1, value =  180),
               numericInput("miny", "Min-y", min=-180, max = 180, step = 1, value = -90),
               numericInput("maxy", "Max-y", min=-180, max = 180, step = 1, value = 90),
               textAreaInput("keywords", "Keywords",placeholder="Enter semi-colon delimited keywords"),
               actionButton("search","Search")),
      menuItem("Results/Subscribe", tabName = "results", icon = icon("download"),
               uiOutput("checkboxes")
      ),
      menuItem("Display options", tabName = "options", icon = icon(""),
               checkboxInput("displayBBOX","Display bounding boxes", value = TRUE),
               numericInput("maxAge", "Max age (minutes)", value=60),
               selectInput("plotColour", "Point colour", choices = pal_options, selected = "blue")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(type = "text/css", "#map {height: calc(100vh - 40px) !important;}"),
      tags$style(HTML(".leaflet-container { background: #e6f2ff; }")),
      tags$style(type="text/css", "#map.recalculating { opacity: 1.0 !important; }"),
      tags$style(type="text/css", "#connection_status.recalculating { opacity: 1.0!important; }")# ,
      # tags$style(type="text/css", ".main-sidebar {width: 300px;}")
    ),
    leafletOutput("map")
  )
)


makePopup <- function(feature, layer){
  layer$bindPopup(paste0(
    "<strong>Name: </strong>", feature$properties$name, "<br/>",
    "<strong>Identifier: </strong>", feature$id, "<br/>",
    "<strong>Description: </strong>", feature$properties$description)
  )
}

server <- function(input, output, session) {

  connection <- redux::hiredis(host="redis",port=6379)

  # setup map
  map_status <- reactiveVal(FALSE)
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("Esri.WorldTerrain")
    m <- m %>% setView(lat = 0, lng = 0, zoom = 2)
    m
  })


  # observer to detect when map ready
  observeEvent(input$map_zoom, {
    map_status(TRUE)
  })


  subscribed_topics = reactiveVal(NULL)

  observe({
    updateNumericInput(session, "minx", value = max(-180,input$map_bounds$west))
    updateNumericInput(session, "maxx", value = min(180,input$map_bounds$east))
    updateNumericInput(session, "miny", value = max(-90,input$map_bounds$south))
    updateNumericInput(session, "maxy", value = min(90,input$map_bounds$north))
  })

  collections <- reactiveVal(NULL)


  observe({
    # parse and add topic to item
    for( collection in collections()$features ){
      for( link in collection$links ){
        if( toupper(link$type) == "MQTT"){
          collection$topic = link$channel
        }
      }
    }
    # now update check box
    choices <- sapply( collections()$features, FUN = function(X){
      res <- NULL
      for( link in X$links ){
        if( toupper(link$type) == "MQTT"){
          collection$topic = link$channel
          res <- link$channel
          if( is.null(res) ){
            res <- link$`wmo:topic`
          }
          break
        }
      }
      if(! is.null(res) ){
        names(res) <- X$properties$title
      }
      return(res)
    })
    output$checkboxes <- renderUI({
      checkboxGroupInput("results_checkboxes", "Select collection to subscribe", choices = choices)
    })
  })


  observeEvent(input$search, {
    # build search url / query
    baseURL <-  input$globalCatalogue
    query <- "?q=mqtt"
    keywords <- trimws(input$keywords)
    keywords <- gsub(" ","%20", keywords)
    if( nchar(keywords)>0){
      for( pk in protectedKeywords){
        keywords <- gsub(pk,'', keywords)
      }
      keywords <- paste0('', trimws(unlist(strsplit(keywords,';'))),'')
      keywords <- paste0(keywords, collapse = "%20AND%20")
      keywords <- paste0("%20AND%20", keywords,"")
    }else{
      keywords <- ''
    }
    bbox <- paste0("&bbox=", paste(input$minx,input$miny,input$maxx,input$maxy,sep="%2C"))
    query <- paste0(query, keywords, bbox, sep= "")
    logmsg(paste0(baseURL,query,"&f=json"))
    response <- GET(paste0(baseURL,query,"&f=json"), timeout(10))
    if( response$status_code == 200){
      collections(fromJSON(content(response, as = "text"), simplifyDataFrame = FALSE))
      if(map_status()){
        map <- leafletProxy("map")
        map <- map %>% removeGeoJSON("collections")
        if( input$displayBBOX){
          map <- map %>% addGeoJSON( collections(), layerId = "collections")
        }
      }
    }else{
      logmsg(response$status_code)
      collections(NULL)
    }
  })

  observe({
    map <- leafletProxy("map")
    if(input$displayBBOX){
      map <- map %>% removeGeoJSON("collections") %>% addGeoJSON( collections(),  layerId = "collections" )
    }else{
      map <- map %>% removeGeoJSON("collections")
    }
  })


  selected <- reactive({
    input$results_checkboxes
  })

  observe({
    selected_options <- selected()
    logmsg(paste(selected_options, collapse="\n"))
    previous <- subscribed_topics()
    deleted_topics <- previous[! (previous %in% selected_options)]
    new_topics <- selected_options[ !(selected_options %in% previous)]
    for(topic in new_topics){
      connection$PUBLISH("topic_add", topic)
    }
    for(topic in deleted_topics){
      connection$PUBLISH("topic_remove", topic)
    }
    subscribed_topics(selected_options)
  })

  values <- reactiveValues(obs = data.frame() )
  observe({
    invalidateLater(1000*30, session) # update every 30 seconds
    if( map_status() ){
      isolate({
        logmsg(paste0(Sys.time(), ": Fetching data ...\n"))
        min_time <- Sys.time() - max(input$maxAge*60, 5*60)
        min_score <- as.numeric(format(min_time, "%s"))
        topics <- selected()
        ids <- list()
        for( topic in topics){
          logmsg(topic)
          ids <- append(ids, connection$ZRANGEBYSCORE(topic,min_score,"+inf"))
        }
        if(length(ids) > 0){
          values$obs <- do.call('rbind',lapply( ids, FUN = function(X) { as.data.frame(fromJSON(connection$GET(X), simplifyVector=FALSE))}))
          if( ! is.null(values$obs) ){
            if( nrow(values$obs) > 0 ){
              values$obs <- subset(values$obs, abs(longitude) < 180 & abs(latitude) < 90)
              # apply some basic QC and set missing to NA
              values$obs$pressure <- ifelse(values$obs$pressure < -1E90, NA, values$obs$pressure)
              values$obs$mslp <- ifelse(values$obs$mslp < -1E90, NA, values$obs$mslp)
              values$obs$wind_speed <- ifelse(values$obs$wind_speed < -1E90, NA, values$obs$wind_speed)
              values$obs$wind_direction <- ifelse(values$obs$wind_direction < -1E90, NA, values$obs$wind_direction)
              values$obs$air_temperature <- ifelse(values$obs$air_temperature < -1E90, NA, values$obs$air_temperature)
              values$obs$dewpoint_temperature <- ifelse(values$obs$dewpoint_temperature < -1E90, NA, values$obs$dewpoint_temperature)
            }
          }
        }else{
          values$obs <- NULL
        }
      })
    }
  })

  observe({
    obs <- values$obs
    if( !is.null(obs)){
      if( nrow(obs) > 0){
        obstime <- sprintf("%04d-%02d-%02d %02d:%02dZ", obs$year, obs$month, obs$day, obs$hour, obs$minute)
        obsid <- paste(obs$wsi_series, obs$wsi_issuer, obs$wsi_issue_number, obs$wsi_local_identifier, sep="-")
        if( input$plotColour %in% c("blue", "cyan", "green")){
          fillColor <- input$plotColour
          label <- paste0("(",obstime,") ", obsid)
        }else{
          fillColor <- pal[ calc_cind(as.vector(obs[,input$plotColour]), pal_range[[input$plotColour]]) ]
          label <- paste0("(",obstime,") ", obsid, ": ",input$plotColour," = ", as.vector(obs[,input$plotColour]))
        }
        m <- leafletProxy("map") %>% clearGroup("obs") %>%
          addCircles(lat = obs$latitude, lng = obs$longitude, radius = 50000, stroke=TRUE,
                           weight=1, color="black", fillColor = fillColor, fillOpacity = 0.5, group="obs",
                           label = (label), labelOptions = labelOptions(textsize = "15px"))
      }else{
        m <- leafletProxy("map") %>% clearGroup("obs")
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

        if(length(ids) > 0){
          logmsg(ids)
          values$obs <- do.call('rbind',lapply( ids, FUN = function(X) { as.data.frame(fromJSON(connection$GET(X), simplifyVector=FALSE))}))
          if( ! is.null(values$obs) ){
            if( nrow(values$obs) > 0 ){
              values$obs <- subset(values$obs, abs(longitude) < 180 & abs(latitude) < 90)
              logmsg(paste0("pressure: ", paste(range(values$obs$pressure), collapse=" -> ")))
              logmsg(paste0("mslp: ", paste(range(values$obs$mslp), collapse=" -> ")))
              logmsg(paste0("wind_speed: ", paste(range(values$obs$wind_speed), collapse=" -> ")))
              logmsg(paste0("wind_direction: ", paste(range(values$obs$wind_direction), collapse=" -> ")))
              logmsg(paste0("air_temperature: ", paste(range(values$obs$air_temperature), collapse=" -> ")))
              logmsg(paste0("dewpoint_temperature: ", paste(range(values$obs$dewpoint_temperature), collapse=" -> ")))
            }
          }
        }else{
          values$obs <- NULL
        }
      })
    }
  })

  observe({
    obs <- values$obs
    if( !is.null(obs)){
      if( nrow(obs) > 0){
        m <- leafletProxy("map") %>% clearGroup("obs") %>%
          addCircleMarkers(lat = obs$latitude, lng = obs$longitude, radius = 5, stroke=TRUE,
                           weight=1, color="black", fillColor = "blue", fillOpacity = 0.5, group="obs")
      }else{
        m <- leafletProxy("map") %>% clearGroup("obs")
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
