
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(session, input, output) {
  
  reactive.values <- reactiveValues()
  reactive.values$selectedCust <- NULL
  reactive.values$detailsdata <- customers
  reactive.values$pivot <- NULL
  reactive.values$selectedSet <- NULL
  
  data_of_click <- reactiveValues(clickedMarker = list())
  
  
  coordinates_df <- NULL
  
  
  ############################## - Start Customer Map - #############################
  #' Map
  #'
  output$map <- renderLeaflet({
    
    #' create df with complete coodinates and radius variable that
    #' can be used to plot data
    has.cood.account <- maputils::getMapPlotDF(customers, UNIQUE_ID_COL = CUSTOMER_UNIQUE_ID_COL, 
                                               CUTOMER_SIZE_COL = CUTOMER_SIZE_COL, 
                                               LONGITUDE_COL = LONGITUDE_COL, LATITUDE_COL = LATITUDE_COL)
    
    
    #' ###
    coordinates_df <<- SpatialPointsDataFrame(has.cood.account[,c(LONGITUDE_COL, LATITUDE_COL)] , has.cood.account)
    
    
    #' sales reps coordinates
    salesreps <- readRDS(SALES_REP_DATA)
    
    pal <- colorFactor(tol21rainbow, domain = has.cood.account[[input$colorBy]])
    
    map <- leaflet(has.cood.account) %>%
      addTiles() %>%
      setView(lng = -3, lat = 52.3, zoom = 7) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal(has.cood.account[[input$colorBy]]),
        stroke = TRUE, weight = 2,
        opacity = 0.6,
        radius = ~RADIUS,
        label = has.cood.account[[CUSTOMER_NAME_COL]],
        layerId = has.cood.account[[CUSTOMER_UNIQUE_ID_COL]],
        options = list(riseOnHover = TRUE)
        #clusterOptions = markerClusterOptions()
      ) %>%
      addLegend("bottomleft",pal=pal, values=has.cood.account[[input$colorBy]], layerId = "colorLegend") %>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                          ,color = 'white'
                                                                          ,weight = 3)),
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
        circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                          ,color = 'white'
                                                                          ,weight = 3)),
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%
        addMarkers(data = salesreps, 
                   lat = salesreps[[SALES_REP_LATITUDE_COL]],
                   lng = salesreps[[SALES_REP_LONGITUDE_COL]],
                   label = salesreps[[SALES_REP_NAME_COL]],
                   icon = salesIcons[salesreps[[SALES_REP_ICON_COL]]],
                   layerId = salesreps[[SALES_REP_NAME_COL]])
    
    
    return(map)
    
  })
  # END MAP
  
  # When map is clicked, show a popup with tweet info
  observe({
    
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showAccountSummaryPopup(data=reactive.values$detailsdata, 
                              unique_id_col = CUSTOMER_UNIQUE_ID_COL,
                              filter_value = event$id, event$lat, event$lng)
    })
  })
  
  
  #'##############################################################
  #' Observe event to select multiple points on map
  #'
  observeEvent(input$map_draw_new_feature,{
    #Only add new layers for bounded locations
    found_in_bounds <- maputils::findLocations(shape = input$map_draw_new_feature
                                     , location_coordinates = coordinates_df
                                     , location_id_colname = CUSTOMER_UNIQUE_ID_COL)
    
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    
    #' subset data to selected points
    #selected <- subset(coordinates_df, CUSTOMER_UNIQUE_ID_COL %in% data_of_click$clickedMarker)
    selected <- coordinates_df[coordinates_df[[CUSTOMER_UNIQUE_ID_COL]] %in% data_of_click$clickedMarker, ]
    reactive.values$selectedSet <- selected 
    
    proxy <- leafletProxy("map")
    proxy %>% addCircleMarkers(data = selected,
                         radius = selected$RADIUS,
                         lat = selected$latitude,
                         lng = selected$longitude,
                         fillColor = "wheat",
                         fillOpacity = 1,
                         color = "hotpink",
                         weight = 3,
                         stroke = T,
                         layerId = as.character(selected$secondLocationID)
                         # highlightOptions = highlightOptions(color = "hotpink",
                         #                                     opacity = 1.0,
                         #                                     weight = 2,
                         #                                     bringToFront = TRUE)
                         )
    
  })
  
  
  
  
  #'##############################################################
  #' Observe event to deselect multiple points on map
  #'
  observeEvent(input$map_draw_deleted_features,{
    # loop through list of one or more deleted features/ polygons
    for(feature in input$map_draw_deleted_features$features){
      
      # get ids for locations within the bounding shape
      bounded_layer_ids <- maputils::findLocations(shape = feature
                                         , location_coordinates = coordinates_df
                                         , location_id_colname = "secondLocationID")
      
      
      # remove second layer representing selected locations
      proxy <- leafletProxy("map")
      proxy %>% removeMarker(layerId = as.character(bounded_layer_ids))
      
      first_layer_ids <- subset(coordinates_df, secondLocationID %in% bounded_layer_ids)[[CUSTOMER_UNIQUE_ID_COL]]
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
    }
  })
  
  #'##############################################################
  #' action button listener for create selection from selected accounts
  #'
  observeEvent(input$createSelection, {
    
    detailsdata <- reactive.values$detailsdata
    selectionName <- input$selectedAccountsSelectionName
    
    #' subset data to selected points
    selectedData <- detailsdata[detailsdata[[CUSTOMER_UNIQUE_ID_COL]] %in% data_of_click$clickedMarker, ]
    
    
    saveRDS(selectedData, paste0("selections/",selectionName,".RDS"))
    
    #' update load selection select dropdown
    updateSelectInput(session, "selectionsToLoad", choices = list.files("./selections", pattern = ".RDS"))
    
    #' update venn diagram drop downs
    updateSelectInput(session, "selection1", choices = list.files("./selections", pattern = ".RDS"))
    updateSelectInput(session, "selection2", choices = list.files("./selections", pattern = ".RDS"))
    
  }) 
  
  
  #'##############################################################
  #' table showing selected data set from map
  #'
  output$selectedAccountsTable <- DT::renderDataTable({
    as.data.frame(reactive.values$selectedSet[, SELECTED_CUSTOMERS_DISPLAY_COLUMS])
  })
  
  
  #'##############################################################
  #' observe event for button to update value for selected set
  #' 
  observeEvent(input$updateValueButton,{
    
    coltoupdate <- input$columnToUpdate
    valuetoupdate<- input$valueToUpdate
    
    newcoltocreate <- input$newColumnToCreate
    newvaluetoupdate <- input$newValueToUpdate
    
    selectedids <- reactive.values$selectedSet[[CUSTOMER_UNIQUE_ID_COL]]
    
    if(!is.empty(valuetoupdate)){
      
      customers[customers[[CUSTOMER_UNIQUE_ID_COL]] %in% selectedids, ][[coltoupdate]] <<- valuetoupdate
      
    }
    
    
    
    reactive.values$detailsdata <- customers
    
    #' #' keep only accounts with cood for plotting on map
    has.cood.account <- maputils::getMapPlotDF(reactive.values$detailsdata, UNIQUE_ID_COL = CUSTOMER_UNIQUE_ID_COL, 
                                               CUTOMER_SIZE_COL = CUTOMER_SIZE_COL, 
                                               LONGITUDE_COL = LONGITUDE_COL, LATITUDE_COL = LATITUDE_COL)
    
    #' # update global variable for coordinates df
    coordinates_df <<- SpatialPointsDataFrame(has.cood.account[,c('longitude', 'latitude')] , has.cood.account)
    
    #reactive.values$selectedSet <- subset(coordinates_df, CONTRACT_SOLDTOID %in% selectedids)
    reactive.values$selectedSet <- coordinates_df[coordinates_df[[CUSTOMER_UNIQUE_ID_COL]] %in% selectedids, ]
    
    saveRDS(customers,"data/customersMAR2018.RDS")
    
  })
  
  
  #'##############################################################
  #'Function to generate popup content when user clicks on map
  #'
  showAccountSummaryPopup <- function(data, unique_id_col, filter_value, lat, lng) {
    
    selectedAccount <- data[data[[unique_id_col]] == filter_value, ]
    
    tags_list <- get_tag_list(selectedAccount)
    
    content <- as.character(tags_list)
    
    leafletProxy("map") %>% addPopups(lng, lat, content)
    
    reactive.values$selectedCust <- selectedAccount
  }
  
  
  #'#################################################################################################
  #'Observe event for button to get user summary
  #'
  observeEvent(input$getAccountSummary, {
    
    selectedAccount <- reactive.values$selectedCust
    
    if (is.empty(selectedAccount)){
      output$accountSummary <- renderUI(tags$h4('Select an account from the map and click Account Details to view more details of the account'))
    }
    
    tags_list <- get_tag_list(selectedAccount)
    
    output$accountSummary <- renderUI(tags_list)
    
    # output$accountSummary <- renderUI( list(tags$h1( selectedAccount[[CUTOMER_NAME_COL]][1] ),
    #                                         tags$b( paste( CUSTOMER_UNIQUE_ID_COL + " :", selectedAccount[[CUSTOMER_UNIQUE_ID_COL]][1])),
    #                                         tags$br(),
    #                                         #tags$b( paste( "SAP Id:", paste(unique(selectedAccount$CONTRACT_SOLDTOID),collapse = ", ")) ),
    #                                         tags$h3( paste("CCH Info. Value: £",selectedAccount$CCH_INFO_VALUE[1]) ),
    #                                         tags$h3( paste("Croner Info. Value: £",selectedAccount$CRONER_INFO_VALUE[1]) ),
    #                                         tags$h4( paste("Earliest Renewal: ",selectedAccount$EARLIEST_RENEWAL[1])),
    #                                         tags$h4( paste("Latest Renewal: ",selectedAccount$LATEST_RENEWAL[1]))
    #                                         ) )
    
    accountContracts <- selectedAccount[!duplicated(selectedAccount[[CONTRACT_UNIQUE_ID_COL]]) ,
                                        SUMMARY_CONTRACTS_PREVIEW_COLS]
    
    accountProducts <- selectedAccount[!duplicated(selectedAccount[[PRODUCT_UNIQUE_ID_COL]]) ,
                                       SUMMARY_PRODUCT_PREVIEW_COLS]
    
    accountContacts <- selectedAccount[!duplicated(selectedAccount[[CONTRACT_UNIQUE_ID_COL]]) ,
                                       SUMMARY_CONTACT_PREVIEW_COLS]
    
    
    output$contractsAtAccount <- DT::renderDataTable(accountContracts)
    
    output$productsAtAccount <- DT::renderDataTable(accountProducts)
    
    output$contactsAtAccount <- DT::renderDataTable(accountContacts)
    
  })
  
  
  
  #'################################################################
  #' side panel of overall summary
  
  observe({
    
    selectedCustomers <- reactive.values$detailsdata
    
    #' one per account
    accounts <- selectedCustomers[ !duplicated(selectedCustomers[[CUSTOMER_UNIQUE_ID_COL]]) , ]
    
    #' one per contract
    contracts <- selectedCustomers[ !duplicated(selectedCustomers[[CONTRACT_UNIQUE_ID_COL]]) , ]
    
    ################################- Value Boxes -###############################
    
    #' Value box showing number of users
    output$customersValueBox <- renderValueBox({
      valueBox(
        length(unique(selectedCustomers[[CUSTOMER_UNIQUE_ID_COL]])), "Accounts", icon = icon("building"),
        color = "teal"
      )
    })
    
    #' Value box showing number of users
    output$contractsValueBox <- renderValueBox({
      valueBox(
        length(unique(selectedCustomers[[CONTACT_UNIQUE_ID_COL]])), "Contacts", icon = icon("users"),
        color = "teal"
      )
    })
    
    #' Value box showing total value
    output$totalValueBox <- renderValueBox({
      
      valueBox(
        paste(CURRENCY, 
              format(round(sum(accounts[[CONTRACT_VALUE_COL]], na.rm = TRUE)/VALUE_DENOMINATION,1), big.mark=","),VALUE_IN, sep=" "), 
        "Total Value", icon = icon(CURRENCY_ICON),
        color = "teal"
      )
    })
    
    #' Value box showing APVC
    output$APVCValueBox <- renderValueBox({
      
      valueBox(
        paste(CURRENCY,ceiling(sum(accounts[[CONTRACT_VALUE_COL]], na.rm = TRUE)/nrow(accounts))), "APVC", icon = icon("money"),
        color = "teal"
      )
    })
    
    if (!is.empty(CONTACT_PHONE_COL)) {
      #' Value box showing APVC
      output$phoneValueBox <- renderValueBox({
        
        valueBox(
          length(unique(selectedCustomers[[CONTACT_PHONE_COL]])), "Phone Contacts", icon = icon("phone"),
          color = "teal"
        )
      })
      
    }
    
    if (!is.empty(CONTACT_EMAIL_COL)){
      #' Value box showing APVC
      output$emailValueBox <- renderValueBox({
        
        valueBox(
          length(unique(selectedCustomers[[CONTACT_EMAIL_COL]])), "Email Contacts", icon = icon("envelope"),
          color = "teal"
        )
      })
      
    }
    
    
    
    
    ############################### - pie charts - ###############################
    
    output$donutChart1 <- googleVis::renderGvis({
      
      sizeTable <- as.data.frame(table(accounts[[DONUT_ONE_COL]]))
      
      doughnut <- gvisPieChart(sizeTable, 
                               options=list(
                                 #width=250,
                                 #height=250,
                                 title=DONUT_ONE_COL,
                                 legend='none',
                                 #colors="['black','orange', 'blue', 
                                 #'red', 'purple', 'green']",
                                 pieSliceText='label',
                                 pieHole=0.2),
                               chartid="size_doughnut")
      
      doughnut$html$footer <- doughnut$html$caption <- NULL
      
      return(doughnut)
      
    })
    
    
    output$donutChart2 <- googleVis::renderGvis({
      
      statusTable <- as.data.frame(table(accounts[[DONUT_TWO_COL]]))
      
      status_doughnut <- gvisPieChart(statusTable, 
                               options=list(
                                 # width=250,
                                 # height=250,
                                  title=DONUT_TWO_COL,
                                  legend='none',
                                 #colors="['black','orange', 'blue', 
                                 #'red', 'purple', 'green']",
                                 pieSliceText='label',
                                 pieHole=0.2),
                               chartid="status_doughnut")
      
      status_doughnut$html$footer <- status_doughnut$html$caption <- NULL
      
      return(status_doughnut)
      
    })
    
    
    output$barChart <- googleVis::renderGvis({
      
        proddf <- as.data.frame(table(contracts$PRODUCT_CODE))
        
        proddf <- proddf[order(proddf$Freq, decreasing = T), ]
        
        proddf <- proddf[1:10, ]
        
        googleVis::gvisColumnChart(proddf,
                                options = list(colors="['blue']",
                                               title = paste("Top 10",BAR_CHART_COL)))
        
    })
    
    
  })
  
  
  
  
  #'################ - Pie charts - ##############
  
  
  #'############# - end value boxes - ##########################
  
  
  #### End- Customer Map Code - #####
  
  
  observeEvent(input$applyFilter,{
    # Whenever we apply filters we apply to global customers df
    # to avoid narrowing to smaller and samller df that can confuse user
    thiscustomers <- customers
    
    withProgress(message="Replotting Map...",{
      
      for (filter_col in FILTER_CATEGORY_COLS) {
        selectedCategories = input[[paste0(filter_col, "Checkbox")]]
        thiscustomers <- thiscustomers[thiscustomers[[filter_col]] %in% selectedCategories, ]
      }
      
      for (filter_col in FILTER_FREETEXT_COLS) {
        selectedCategories = input[[paste0(filter_col, "ToMatch")]]
        thiscustomers <- thiscustomers[thiscustomers[[filter_col]] %containswithspace% selectedCategories, ]
      }
      
      
      #' set filtered data to reactive df
      reactive.values$detailsdata <- thiscustomers
      
      #' #' keep only accounts with cood for plotting on map
      has.cood.account <- maputils::getMapPlotDF(thiscustomers, UNIQUE_ID_COL = CUSTOMER_UNIQUE_ID_COL, 
                                                 CUTOMER_SIZE_COL = CUTOMER_SIZE_COL, 
                                                 LONGITUDE_COL = LONGITUDE_COL, LATITUDE_COL = LATITUDE_COL)
      
      
      #' # update global variable for coordinates df
      if (nrow(has.cood.account) > 0){
        coordinates_df <<- SpatialPointsDataFrame(has.cood.account[,c('longitude', 'latitude')] , has.cood.account)
      }
      
      redrawMap(has.cood.account, input$colorBy)
      
    })
    
  })
  
  
  #' ##########################################################
  #' Filter data by company name
  #' 
  observeEvent(input$filterByName,{
    
    withProgress(message="Finding accounts by name", {
      
      searchterm <- input$searchTerm
      this_customers <- reactive.values$detailsdata
      
      this_customers <- thiscustomers[thiscustomers[[CUSTOMER_NAME_COL]] %containswithspace% searchterm, ]
      
      if(nrow(customers) < 1) {
        return(NULL)
      }
      
      reactive.values$detailsdata <- this_customers
      
      #' #' keep only accounts with cood for plotting on map
      has.cood.account <- maputils::getMapPlotDF(this_customers, UNIQUE_ID_COL = CUSTOMER_UNIQUE_ID_COL, 
                                                 CUTOMER_SIZE_COL = CUTOMER_SIZE_COL, 
                                                 LONGITUDE_COL = LONGITUDE_COL, LATITUDE_COL = LATITUDE_COL)
      
      #' # update global variable for coordinates df
      coordinates_df <<- SpatialPointsDataFrame(has.cood.account[,c('longitude', 'latitude')] , has.cood.account)
      
      redrawMap(has.cood.account)
    })
    
  })
  
  #' ##########################################################
  #' Data table for full details data
  output$selectedDataSet <- DT::renderDataTable({
    
    return(reactive.values$detailsdata)
    
  })
  
  
  output$custPivot <- DT::renderDataTable({
    
    pivot <- withProgress(message = "in progress", {
      
      #' get filtered customer data
      customers <- reactive.values$detailsdata
      
      #' reshape and cast data table
      xx <- customers %>% 
        group_by(CONTRACT_SOLDTOID, SOLDTO_GUO_NAME, BRAND) %>% 
        summarise(PRODUCTS = paste(unique(CONTRACT_DESCRIPTION), collapse=","), 
                  NUMCONTRACTS = n_distinct(CONTRACT_SAP_ID))
      
      yy <- dcast(xx, CONTRACT_SOLDTOID + SOLDTO_GUO_NAME ~ BRAND, value.var="PRODUCTS")
      zz <- dcast(xx, CONTRACT_SOLDTOID + SOLDTO_GUO_NAME ~ BRAND, value.var="NUMCONTRACTS")                                                                  
      
      pp <- merge(zz,yy, by=c("CONTRACT_SOLDTOID","SOLDTO_GUO_NAME") )
      
      cols <- colnames(pp)
      
      cols <- cols %>%
                gsub(pattern =".x", replacement = "_Contracts", fixed = TRUE) %>%
                gsub(pattern =".y", replacement = "_Products", fixed = TRUE)
      
      colnames(pp) <- cols
      
      pp
      
    })
    
    reactive.values$pivot <- pivot
    return(pivot)
    
  })
  
  getAggData <- function (aggBy, data)   {
    agg.by <- lapply(aggBy, as.symbol)
    agg.data <- data %>% group_by_(.dots = agg.by) %>% select(one_of(SEGMENT_AGG_COLS)) %>% summarise(Customers = n_distinct(as.symbol(CUSTOMER_UNIQUE_ID_COL)),
                                                                                                      Contracts = n_distinct(as.symbol(CONTRACT_UNIQUE_ID_COL)),
                                                                                                      Products = n_distinct(as.symbol(PRODUCT_UNIQUE_ID_COL)),
                                                                                                      TOV = sum(CONTRACT_VALUE,  na.rm = TRUE),
                                                                                                      APVC = sum(CONTRACT_VALUE,  na.rm = TRUE) / Customers
                                                                                                      )
    
    return(agg.data)
  }
  
  
  #' #########################################################
  #' Customer Segment Pivot
  #' 
  observeEvent(input$drawTable, {
    
    output$custSegPivot <- DT::renderDataTable({
      
      xaxis <- isolate(input$xaxis)
      yaxis <- isolate(input$yaxis)
      valuevar <- isolate(input$valuevar)
      
      customers <- reactive.values$detailsdata
      #customers <- customers[ !duplicated(customers$CONTRACT_SFDC_ID) , ]
      
      crosstabutils::getAggData()
      
      display.table <<- withProgress(message = "in progress",
                                     detail = "This may take a while...",{
                                       crosstabutils::getSummaryData(customers, xaxis, yaxis, valuevar)
                                     })
      
      return(display.table)
      
    })
    
    updateTabItems(session, "tabsmenu", selected = "viewSegments")
    
  })
  
  
  #' #########################################################
  #' bar chart
  #' 
  output$barChartPlot <- plotly::renderPlotly({
    
    #@ TODO
    
    #' # tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", 
    #' #                 "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", 
    #' #                 "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
    #' # 
    #' # par(las=2, # make label text perpendicular to axis
    #' #     mar=c(5,10,2,1), # increase y-axis margin
    #' #     mfrow = c(1:2), # plot side by side
    #' #     ps = 8, cex = 1, cex.main = 1) 
    #' 
    #' #' get filtered customer data
    #' customers <- reactive.values$detailsdata
    #' 
    #' #' deduplicate
    #' plot_data <- customers[ !duplicated(customers[[CUSTOMER_UNIQUE_ID_COL]]) , ]
    #' 
    #' bar_char_col = BAR_CHART_COL[1]
    #' 
    #' plot_data[[bar_char_col]][is.na(plot_data[[bar_char_col]])] <- "Unknown"
    #' plot_data <- plot_data %>% dplyr::filter(INDUSTRY != "Unknown") %>% dplyr::filter(EMP_SIZE != "Unknown")
    #' 
    #' counts <- table(plot_data$EMP_SIZE, plot_data$INDUSTRY)
    #' counts <- table(plot_data$INDUSTRY, plot_data$EMP_SIZE)
    #' counts <- as.data.frame(counts)
    #' 
    #' p <- counts %>% plot_ly(x = ~Var1, y = ~Freq, color = ~Var2)
    
  })
  
  
  #' #########################################################
  #' Heat Map
  #' 
  output$heatMapPlot <- renderPlotly({
    
    withProgress({
      input$drawHeatMap
      
      #' get filtered customer data
      customers <- reactive.values$detailsdata
      
      xvar <- isolate(input$heatmapXVar)
      yvar <- isolate(input$heatmapYVar)
      
      #' deduplicate
      plot_data <- customers[ !duplicated(customers[[CUSTOMER_UNIQUE_ID_COL]]) , ]
      
      plot_data <- plot_data[,c(xvar, yvar)]
      plot_data[] <- lapply(plot_data, as.character)
      
      plot_data[[xvar]][is.na(plot_data[[xvar]])] <- "Unknown"
      plot_data[[yvar]][is.na(plot_data[[yvar]])] <- "Unknown"
      
      plot_data <- plot_data[(plot_data[[xvar]] != "Unknown" & plot_data[[yvar]] != "Unknown"), ]
      
      counts <- table(plot_data[[xvar]], plot_data[[yvar]])
      
      
      p <- heatmaply(as.data.frame.matrix(counts), 
                     draw_cellnote = TRUE, colors = Reds(10),
                     dendrogram = "none")
      
      p
      
    })
    
  })
  
 
  
  #' #########################################################
  #' Export details 
  #' 
  output$exportDetails <- downloadHandler(
    filename = function() {
      paste('details-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      data <- reactive.values$detailsdata
      
      data <- data[ !duplicated(data[[CONTRACT_UNIQUE_ID_COL]]), ]
      
      data <- sapply(data,as.character)
      data[is.na(data)] <- ""
      write.csv(data, con, sep = ",", row.names = FALSE)
    }
  )
  
  
  #' #########################################################
  #' Export Pivot 
  #' 
  output$exportPivot <- downloadHandler(
    filename = function() {
      paste('pivot-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      data <- display.table$x$data
      data <- sapply(data,as.character)
      data[is.na(data)] <- ""
      write.table(data, con, sep = ",", row.names = FALSE)
    }
  )
  
  
  #'############################################################################
  #' observerEvent for the loadSelection action button
  #' 
  observeEvent(input$loadSelection,{
    
    withProgress(message="Loading data and redrawing...", {
      
      #' get the files selected to be loaded
      files.selected <- isolate(input$selectionsToLoad)
      
      #'path to directory
      path <- "selections/"
      
      files.to.load <- paste0(path,files.selected)
      
      #load for the first file
      selections.df <- readRDS(files.to.load[1])
      
      #' if more than one file selected load data for other files
      i <- 2
      while(i <= length(files.to.load)){
        selections.df <- rbind(selections.df,
                               readRDS(files.to.load[i]))
        i <- i+1
      }
      
      reactive.values$detailsdata <- selections.df
      
      #' #' keep only accounts with cood for plotting on map
      has.cood.account <- maputils::getMapPlotDF(customers, UNIQUE_ID_COL = CUSTOMER_UNIQUE_ID_COL, 
                                                 CUTOMER_SIZE_COL = CUTOMER_SIZE_COL, 
                                                 LONGITUDE_COL = LONGITUDE_COL, LATITUDE_COL = LATITUDE_COL)
      
      #' # update global variable for coordinates df
      coordinates_df <<- SpatialPointsDataFrame(has.cood.account[,c('longitude', 'latitude')] , has.cood.account)
      
      redrawMap(has.cood.account)
      
    })
    
  })
  
  
  #'############################################################################
  #' observerEvent for the loadSelection action button
  #' 
  observeEvent(input$saveSelection,{
    
    selectiondata <- reactive.values$detailsdata
    selectionName <- input$selectionName
    
    saveRDS(selectiondata, paste0("selections/",selectionName,".RDS"))
    
    #' update load selection select dropdown
    updateSelectInput(session, "selectionsToLoad", choices = list.files("./selections", pattern = ".RDS"))
    
    #' update venn diagram drop downs
    updateSelectInput(session, "selection1", choices = list.files("./selections", pattern = ".RDS"))
    updateSelectInput(session, "selection2", choices = list.files("./selections", pattern = ".RDS"))
    
    #' do something to show that selection is saved
    
  })
  
  
  #############################################################################
  #' observeEvent for drawVenn action button
  #' 
  observeEvent(input$drawVenn,{
    
    withProgress(message = "Drawing Venn diagram...",{
      
      selection1 <- input$selection1
      selection2 <- input$selection2
      
      selection1 <- readRDS(paste0("selections/",selection1))
      selection2 <- readRDS(paste0("selections/",selection2))
      
      #' set the environment df to selected selections
      temp_selection <<- rbind(selection1,selection2)
      
      unique_sel1 <- unique(selection1[[CONTRACT_UNIQUE_ID_COL]])
      unique_sel2 <- unique(selection2[[CONTRACT_UNIQUE_ID_COL]])
      
      only_sel1 <<- setdiff(unique_sel1, unique_sel2)
      only_sel2 <<- setdiff(unique_sel2, unique_sel1)
      
      sel1_and_sel2 <<- intersect(unique_sel1, unique_sel2)
      
      sel1_union_sel2 <- union(unique_sel1, unique_sel2)
      
      
      print(paste(" only selection 1:", length(only_sel1)))
      print(paste(" only selection 2:", length(only_sel2)))
      print(paste(" intersection :", length(sel1_and_sel2)))
      print(paste(" union :", length(sel1_union_sel2)))
      
      
      ##########################################################################
      #' Render Function  for Venn diagrams
      #' 
      output$venn <- renderPlot({
        
        draw.pairwise.venn(area1 = length(only_sel1) + length(sel1_and_sel2),
                           area2 = length(only_sel2) + length(sel1_and_sel2),
                           cross.area = length(sel1_and_sel2),
                           category = c(input$selection1, input$selection2), 
                           lty = rep("blank", 2), 
                           fill = c("light blue", "pink"), 
                           alpha = rep(0.5, 2), 
                           cat.pos = c(0,0), 
                           cat.dist = rep(0.025, 2), 
                           scaled = FALSE,
                           print.mode = c("percent","raw"))
        
        # draw.pairwise.venn(area1 = length(only_sel1) + length(sel1_and_sel2),
        #                    area2 = length(only_sel2) + length(sel1_and_sel2),
        #                    cross.area = length(sel1_and_sel2),
        #                    category = c("Google Analytics","Adobe Analytics"),
        #                    fill = c("#F29B05","#A1D490"),
        #                    ext.text = TRUE,
        #                    ext.percent = c(0.1,0.1,0.1),
        #                    ext.length = 0.6,
        #                    label.col = rep("gray10",3),
        #                    lwd = 0,
        #                    cex = 2,
        #                    fontface = rep("bold",3),
        #                    fontfamily = rep("sans",3),
        #                    cat.cex = 1.5,
        #                    cat.fontface = rep("plain",2),
        #                    cat.fontfamily = rep("sans",2),
        #                    cat.pos = c(0, 0),
        #                    print.mode = c("percent","raw"))
        
      })
      
      output$downloadSetsButton <- shiny::renderUI({
        
        buttonname1 <- gsub(paste("Only",input$selection1,"(",length(only_sel1),")"),pattern=".RDS",replacement ="",fixed = TRUE)
        buttonname2 <- gsub(paste(input$selection2,"And",input$selection1, "(",length(sel1_and_sel2),")"),pattern=".RDS",replacement ="",fixed = TRUE)
        buttonname3 <- gsub(paste("Only",input$selection2, "(",length(only_sel2),")"),pattern=".RDS",replacement ="",fixed = TRUE)
        buttonname4 <- gsub(paste(input$selection1,"Union",input$selection2, "(",length(sel1_union_sel2),")"),pattern=".RDS",replacement ="",fixed = TRUE)
        
        
        # list(downloadButton("onlySetA", buttonname1),
        #      downloadButton("setAandsetB", buttonname2),
        #      downloadButton("onlySetB", buttonname3),
        #      downloadButton("setAorsetB", buttonname4))
        
        subsetchoices = c(buttonname1, buttonname2, buttonname3)
        
        list(
             checkboxGroupInput("selectSubsets", "Select Subsets", choiceNames = subsetchoices, choiceValues = c("onlyA","AandB","onlyB")),
             tags$br(),
             actionButton("setSubsetAsSelection","Set Selection")
             )
        
      }) #' end download buttons
      
    })
    
  })
  
  observeEvent(input$setSubsetAsSelection, {
    
    idstoset <- NULL
    
    if("onlyA" %in% input$selectSubsets ) {
      idstoset <- c(idstoset, only_sel1)
    }
    
    if("AandB" %in% input$selectSubsets ) {
      idstoset <- c(idstoset, sel1_and_sel2)
    }
    
    if("onlyB" %in% input$selectSubsets ) {
      idstoset <- c(idstoset, only_sel2)
    }
    
    
    print(length(idstoset))
    
    this_customers <- customers
    
    reactive.values$detailsdata <- this_customers[this_customers[[CUSTOMER_UNIQUE_ID_COL]] %in% idstoset, ]
    
    updateTabItems(session, "tabsmenu", selected = "customermap")
    
    
  })
  
  
})
