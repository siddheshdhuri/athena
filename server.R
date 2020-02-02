
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
        label = has.cood.account[[CUTOMER_NAME_COL]],
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
      showAccountSummaryPopup(reactive.values$detailsdata, event$id, event$lat, event$lng)
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
      
      first_layer_ids <- subset(coordinates_df, secondLocationID %in% bounded_layer_ids)$CONTRACT_SOLDTOID
      
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
    selectedData <- subset(detailsdata, CONTRACT_SOLDTOID %in% data_of_click$clickedMarker)
    
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
    as.data.frame(reactive.values$selectedSet[, c(CUSTOMER_UNIQUE_ID_COL, CUSTOMER_NAME_COL, "Region", "EMP_SIZE","TURNOVER_SIZE","INFO_VALUE_SIZE","SOLDTO_POSTCODE")])
  })
  
  
  #'##############################################################
  #' observe event for button to update value for selected set
  #' 
  observeEvent(input$updateValueButton,{
    
    coltoupdate <- input$columnToUpdate
    valuetoupdate<- input$valueToUpdate
    
    newcoltocreate <- input$newColumnToCreate
    newvaluetoupdate <- input$newValueToUpdate
    
    selectedids <- reactive.values$selectedSet$CONTRACT_SOLDTOID
    
    if(!is.empty(valuetoupdate)){
      
      customers[customers$CONTRACT_SOLDTOID %in% selectedids, ][[coltoupdate]] <<- valuetoupdate
      
    }
    
    # if(!is.empty(newcoltocreate)){
    #   
    #   customers[[newcoltocreate]] <<- NA
    #   customers[customers$CONTRACT_SOLDTOID %in% selectedids, ][[coltoupdate]] <<- newvaluetoupdate
    #   
    # }
    
    
    reactive.values$detailsdata <- customers
    
    #' #' keep only accounts with cood for plotting on map
    has.cood.account <- maputils::getMapPlotDF(reactive.values$detailsdata)
    
    #' # update global variable for coordinates df
    coordinates_df <<- SpatialPointsDataFrame(has.cood.account[,c('longitude', 'latitude')] , has.cood.account)
    
    reactive.values$selectedSet <- subset(coordinates_df, CONTRACT_SOLDTOID %in% selectedids)
    
    saveRDS(customers,"data/customersMAR2018.RDS")
    
  })
  
  
  #'##############################################################
  #'Function to generate popup content when user clicks on map
  #'
  showAccountSummaryPopup <- function(data, soldtoid, lat, lng) {
    
    selectedAccount <- data %>%
                        filter(CONTRACT_SOLDTOID == soldtoid) #%>%
                        #filter(CONTRACT_STATUS %in% c("Active", "Draft"))
    
    
    unq_shipto <- unique(selectedAccount$CONTRACT_SHIPTOID)
    unq_contracts <- unique(selectedAccount$CONTRACT_SAP_ID)
    unq_products <- unique(selectedAccount$PRODUCT_CODE)
    
    contract_val <- sum(selectedAccount$CONTRACT_VALUE, na.rm = TRUE)
    
    content <- as.character(tagList(
      
      tags$strong(HTML(sprintf("%s",
                               selectedAccount$SOLDTO_GUO_NAME[1]
      ))), tags$br(),
      sprintf("Ship To Contacts: %s", length(unq_shipto)),tags$br(),
      sprintf("Contracts: %s", length(unq_contracts)),tags$br(),
      sprintf("Products: %s", length(unq_products)),tags$br(),
      sprintf("Contracts Value: %s", contract_val),tags$br(),
      sprintf("Account Info Value: %s", selectedAccount$ACCOUNT_INFO_VALUE[1]),tags$br(),
      sprintf("Address: %s", paste(selectedAccount$SOLDTO_ADDRESS[1], selectedAccount$SOLDTO_TOWN[1], selectedAccount$SOLDTO_POSTCODE[1])),
              tags$br(),
      sprintf("Sold to Tel: %s", selectedAccount$SOLDTO_PHONE[1]),tags$br(),
      
      tags$br()
    ))
    
    leafletProxy("map") %>% addPopups(lng, lat, content)
    
    reactive.values$selectedCust <- selectedAccount
  }
  
  
  #'#################################################################################################
  #'Observe event for button to get user summary
  #'
  observeEvent(input$getAccountSummary, {
    
    selectedAccount <- reactive.values$selectedCust
    
    output$accountSummary <- renderUI( list(tags$h1( selectedAccount$SOLDTO_GUO_NAME[1] ),
                                            tags$b( paste( "SalesForce Id:", selectedAccount$ACCOUNT_SFDC_ID[1])),
                                            tags$br(),
                                            tags$b( paste( "SAP Id:", paste(unique(selectedAccount$CONTRACT_SOLDTOID),collapse = ", ")) ),
                                            tags$h3( paste("CCH Info. Value: £",selectedAccount$CCH_INFO_VALUE[1]) ),
                                            tags$h3( paste("Croner Info. Value: £",selectedAccount$CRONER_INFO_VALUE[1]) ),
                                            tags$h4( paste("Earliest Renewal: ",selectedAccount$EARLIEST_RENEWAL[1])),
                                            tags$h4( paste("Latest Renewal: ",selectedAccount$LATEST_RENEWAL[1]))
                                            ) )
    
    accountContracts <- selectedAccount[!duplicated(selectedAccount$CONTRACT_SAP_ID) ,
                                        c("CONTRACT_SAP_ID","CONTRACT_DESCRIPTION","CONTRACT_STATUS",
                                          "CONTRACT_VALUE","CONTRACT_STARTDATE","CONTRACT_ENDDATE")]
    
    accountProducts <- selectedAccount[!duplicated(selectedAccount$PRODUCT_CODE) ,
                                       c("PRODUCT_CODE","PRODUCT_FAMILY")]
    
    accountContacts <- selectedAccount[!duplicated(selectedAccount$CONTRACT_SHIPTOID) ,
                                       c("SHIPTO_FIRST_NAME","SHIPTO_LAST_NAME", "SHIPTO_EMAIL", "SHIPTO_PHONE")]
    
    
    output$contractsAtAccount <- DT::renderDataTable(accountContracts)
    
    output$productsAtAccount <- DT::renderDataTable(accountProducts)
    
    output$contactsAtAccount <- DT::renderDataTable(accountContacts)
    
  })
  
  
  
  #'################################################################
  #' side panel of overall summary
  
  observe({
    
    selectedCustomers <- reactive.values$detailsdata
    
    #' one per account
    accounts <- selectedCustomers[ !duplicated(selectedCustomers$CONTRACT_SOLDTOID) , ]
    
    #' one per contract
    contracts <- selectedCustomers[ !duplicated(selectedCustomers$CONTRACT_SFDC_ID) , ]
    
    ################################- Value Boxes -###############################
    
    #' Value box showing number of users
    output$customersValueBox <- renderValueBox({
      valueBox(
        length(unique(selectedCustomers$ACCOUNT_SFDC_ID)), "Accounts", icon = icon("building"),
        color = "teal"
      )
    })
    
    #' Value box showing number of users
    output$contractsValueBox <- renderValueBox({
      valueBox(
        length(unique(selectedCustomers$CONTRACT_SHIPTOID)), "Contacts", icon = icon("users"),
        color = "teal"
      )
    })
    
    #' Value box showing total value
    output$totalValueBox <- renderValueBox({
      
      valueBox(
        paste("£ ",format(round(sum(accounts$ACCOUNT_INFO_VALUE, na.rm = TRUE)/1000000,1), big.mark=",")," M",sep=""), "Total Value", icon = icon("gbp"),
        color = "teal"
      )
    })
    
    #' Value box showing APVC
    output$APVCValueBox <- renderValueBox({
      
      valueBox(
        paste("£",ceiling(sum(accounts$ACCOUNT_INFO_VALUE, na.rm = TRUE)/nrow(accounts))), "APVC", icon = icon("money"),
        color = "teal"
      )
    })
    
    #' Value box showing APVC
    output$emailValueBox <- renderValueBox({
      
      valueBox(
        length(unique(selectedCustomers$SOLDTO_PHONE)), "Account Phone", icon = icon("phone"),
        color = "teal"
      )
    })
    
    #' Value box showing APVC
    output$phoneValueBox <- renderValueBox({
      
      valueBox(
        length(unique(selectedCustomers$SHIPTO_EMAIL)), "Contact Emails", icon = icon("envelope"),
        color = "teal"
      )
    })
    
    
    ############################### - pie charts - ###############################
    
    # output$empSizeDonut <- plotly::renderPlotly({
    #   
    #   sizeTable <- as.data.frame(table(accounts$EMP_SIZE))
    #   
    #   p <- sizeTable %>%
    #     plot_ly(labels = ~Var1, values = ~Freq) %>%
    #     add_pie(hole = 0.3)
    # })
    
    output$empSizeDonut <- googleVis::renderGvis({
      
      sizeTable <- as.data.frame(table(accounts$EMP_SIZE))
      
      doughnut <- gvisPieChart(sizeTable, 
                               options=list(
                                 #width=250,
                                 #height=250,
                                 title='Employee Sizes',
                                 legend='none',
                                 #colors="['black','orange', 'blue', 
                                 #'red', 'purple', 'green']",
                                 pieSliceText='label',
                                 pieHole=0.2),
                               chartid="size_doughnut")
      
      doughnut$html$footer <- doughnut$html$caption <- NULL
      
      return(doughnut)
      
    })
    
    
    # output$statusDonut <- plotly::renderPlotly({
    #   
    #   sizeTable <- as.data.frame(table(accounts$STATUS))
    #   
    #   p <- sizeTable %>%
    #     plot_ly(labels = ~Var1, values = ~Freq) %>%
    #     add_pie(hole = 0.3)
    # })
    
    output$statusDonut <- googleVis::renderGvis({
      
      statusTable <- as.data.frame(table(accounts$STATUS))
      
      status_doughnut <- gvisPieChart(statusTable, 
                               options=list(
                                 # width=250,
                                 # height=250,
                                  title='Account Status',
                                  legend='none',
                                 #colors="['black','orange', 'blue', 
                                 #'red', 'purple', 'green']",
                                 pieSliceText='label',
                                 pieHole=0.2),
                               chartid="status_doughnut")
      
      status_doughnut$html$footer <- status_doughnut$html$caption <- NULL
      
      return(status_doughnut)
      
    })
    
    
    output$prodBar <- googleVis::renderGvis({
      
        proddf <- as.data.frame(table(contracts$PRODUCT_CODE))
        
        proddf <- proddf[order(proddf$Freq, decreasing = T), ]
        
        proddf <- proddf[1:10, ]
        
        googleVis::gvisColumnChart(proddf,
                                options = list(colors="['blue']",
                                               title = "Top 10 Products"))
        
    })
    
    
  })
  
  
  
  
  #'################ - Pie charts - ##############
  
  
  #'############# - end value boxes - ##########################
  
  
  #### End- Customer Map Code - #####
  
  
  observeEvent(input$applyFilter,{
    
    withProgress(message="Replotting Map...",{
      
      empSizes <- input$empsizeCheckbox
      turnoverSizes <- input$turnoversizeCheckbox
      infovalsizes <- input$infovalsizeCheckbox
      businessTypes <- input$businessTypeCheckbox
      custStatus <- input$custStatusCheckbox
      contractStatus <- isolate(input$contractStatusCheckbox)
      prodBrands <- isolate(input$productBrandCheckbox)
      prodCodes <- isolate(input$productsToMatch)
      jobTitles <- isolate(input$jobsToMatch)
      regions <- isolate(input$regionCheckbox)
      
      customers <- customers %>%
        dplyr::filter( EMP_SIZE %in% empSizes & 
                         TURNOVER_SIZE %in% turnoverSizes &
                         INFO_VALUE_SIZE %in% infovalsizes & 
                         CCH_BUSINESS_TYPE %in% businessTypes &
                         STATUS %in% custStatus & 
                         CONTRACT_STATUS %in% contractStatus &
                         BRAND %in% prodBrands &
                         Region %in% regions &
                         PRODUCT_CODE %contains% prodCodes &
                         SHIPTO_JOB_TITLE %containswithspace% jobTitles )
      
      #' apply filter to global customers df
      # customers <- customers %>%
      #   dplyr::filter( EMP_SIZE %in% empSizes ) %>%
      #   dplyr::filter( TURNOVER_SIZE %in% turnoverSizes ) %>%
      #   dplyr::filter( INFO_VALUE_SIZE %in% infovalsizes ) %>%
      #   dplyr::filter( CCH_BUSINESS_TYPE %in% businessTypes ) %>%
      #   dplyr::filter( STATUS %in% custStatus ) %>%
      #   dplyr::filter( CONTRACT_STATUS %in% contractStatus ) %>%
      #   dplyr::filter( PRODUCT_CODE %contains% prodCodes) %>%
      #   dplyr::filter( SHIPTO_JOB_TITLE %containswithspace% jobTitles )
      
      #accounts <- customers[ !duplicated(customers$CONTRACT_SOLDTOID) , ]
      
      #' set filtered data to reactive df
      reactive.values$detailsdata <- customers
      
      #' #' keep only accounts with cood for plotting on map
      has.cood.account <- maputils::getMapPlotDF(reactive.values$detailsdata)
      
      #' # update global variable for coordinates df
      coordinates_df <<- SpatialPointsDataFrame(has.cood.account[,c('longitude', 'latitude')] , has.cood.account)
      
      
      redrawMap(has.cood.account, input$colorBy)
      
    })
    
  })
  
  
  #' ##########################################################
  #' Filter data by company name
  #' 
  observeEvent(input$filterByName,{
    
    withProgress(message="Findinng accounts by name", {
      
      searchterm <- input$searchTerm
      customers <- reactive.values$detailsdata
      print(searchterm)
      customers <- customers %>%
        dplyr::filter( SOLDTO_GUO_NAME %containswithspace% searchterm )
      
      #dplyr::filter( grepl(pattern = searchterm, x = customers$SOLDTO_GUO_NAME, ignore.case = T) )
      
      if(nrow(customers) < 1) {
        return(NULL)
      }
      
      reactive.values$detailsdata <- customers
      
      #' #' keep only accounts with cood for plotting on map
      has.cood.account <- maputils::getMapPlotDF(reactive.values$detailsdata)
      
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
    
    # tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", 
    #                 "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", 
    #                 "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
    # 
    # par(las=2, # make label text perpendicular to axis
    #     mar=c(5,10,2,1), # increase y-axis margin
    #     mfrow = c(1:2), # plot side by side
    #     ps = 8, cex = 1, cex.main = 1) 
    
    #' get filtered customer data
    customers <- reactive.values$detailsdata
    
    #' deduplicate
    plot_data <- customers[ !duplicated(customers$CONTRACT_SFDC_ID) , ]
    plot_data$INDUSTRY[is.na(plot_data$INDUSTRY)] <- "Unknown"
    plot_data <- plot_data %>% dplyr::filter(INDUSTRY != "Unknown") %>% dplyr::filter(EMP_SIZE != "Unknown")
    
    counts <- table(plot_data$EMP_SIZE, plot_data$INDUSTRY)
    counts <- table(plot_data$INDUSTRY, plot_data$EMP_SIZE)
    counts <- as.data.frame(counts)
    
    p <- counts %>% plot_ly(x = ~Var1, y = ~Freq, color = ~Var2)
    
    # counts <- as.data.frame.matrix(counts)
    # counts <- setDT(counts, keep.rownames = TRUE)[]
    # colnames(counts)[1] <- "EMP_SIZE"
    # p <- plot_ly(counts, x = ~EMP_SIZE, y = ~as.symbol(colnames(counts)[2]), type = 'bar', name = colnames(counts)[2])
    # 
    # for(i in 3:6){
    #   p <- p %>% add_trace(y = ~as.symbol(colnames(counts)[i]), name = colnames(counts)[i])
    # }
    # 
    # p <- p %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    
    # Add 5 trace to this graphic with a loop
      #add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
      
    # #' Basic stacked bar plot
    # barplot(counts, main="Barchart",
    #         xlab="Number of Accounts", col=tol21rainbow, horiz = TRUE)
    # legend("topright", rownames(counts), bg = "transparent", fill = tol21rainbow)
    
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
      plot_data <- customers[ !duplicated(customers$CONTRACT_SFDC_ID) , ]
      
      plot_data <- plot_data[,c(xvar, yvar)]
      plot_data[] <- lapply(plot_data, as.character)
      
      plot_data[[xvar]][is.na(plot_data[[xvar]])] <- "Unknown"
      plot_data[[yvar]][is.na(plot_data[[yvar]])] <- "Unknown"
      
      plot_data <- plot_data[(plot_data[[xvar]] != "Unknown" & plot_data[[yvar]] != "Unknown"), ]
      
      # plot_data <- plot_data %>% 
      #   dplyr::filter(as.symbol(xvar) != "Unknown") %>% 
      #   dplyr::filter(as.symbol(yvar) != "Unknown")
      
      counts <- table(plot_data[[xvar]], plot_data[[yvar]])
      
      # vals <- unique(scales::rescale(c(counts)))
      # o <- order(vals, decreasing = FALSE)
      # cols <- scales::col_numeric("Reds", domain = NULL)(vals)
      # colz <- setNames(data.frame(vals[o], cols[o]), NULL)
      
      # p <- plot_ly(
      #   x = colnames(counts), y = rownames(counts),
      #   z = counts, type = "heatmap", colorscale = colz
      # )
      
      p <- heatmaply(as.data.frame.matrix(counts), 
                     draw_cellnote = TRUE, colors = Reds(10),
                     dendrogram = "none")
      
      p
      
    })
    
  })
  
  #' output$heatMapPlot <- renderPlot({
  #'   
  #'   #' get filtered customer data
  #'   customers <- reactive.values$detailsdata
  #'   
  #'   #' deduplicate
  #'   plot_data <- customers[ !duplicated(customers$CONTRACT_SFDC_ID) , ]
  #'   plot_data$INDUSTRY[is.na(plot_data$INDUSTRY)] <- "Unknown"
  #'   plot_data <- plot_data %>% dplyr::filter(INDUSTRY != "Unknown") %>% dplyr::filter(EMP_SIZE != "Unknown")
  #'   
  #'   #counts <- table(plot_data$INDUSTRY, plot_data$EMP_SIZE)
  #'   
  #'   plotheatmap(plot_data, NULL, var1="EMP_SIZE", var2="STATUS")
  #'   
  #'   
  #' })
  
  
  
  
  #' #########################################################
  #' Export details 
  #' 
  output$exportDetails <- downloadHandler(
    filename = function() {
      paste('details-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      
      data <- reactive.values$detailsdata
      
      data <- data[ !duplicated(data$CONTRACT_SHIPTOID), ]
      
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
  
  #' #########################################################
  #' ui summary
  #' 
  # output$countsSummary <- shiny::renderUI({
  #   
  #   numcust <- length(unique(reactive.values$detailsdata$CONTRACT_SOLDTOID))
  #   numcontacts <- length(unique(reactive.values$detailsdata$CONTRACT_SHIPTOID))
  #   
  #   uilist <- list(tags$h3(paste("Customers:", numcust)),
  #                  tags$h4(paste("Contacts:", numcontacts)))
  #   
  #   return(uilist)
  # })
  
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
      has.cood.account <- maputils::getMapPlotDF(reactive.values$detailsdata)
      
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
      
      
      only_sel1 <<- setdiff(unique(selection1$ACCOUNT_SFDC_ID), unique(selection2$ACCOUNT_SFDC_ID))
      only_sel2 <<- setdiff(unique(selection2$ACCOUNT_SFDC_ID), unique(selection1$ACCOUNT_SFDC_ID))
      
      sel1_and_sel2 <<- intersect(unique(selection1$ACCOUNT_SFDC_ID), unique(selection2$ACCOUNT_SFDC_ID))
      
      sel1_union_sel2 <- union(unique(selection1$ACCOUNT_SFDC_ID), unique(selection2$ACCOUNT_SFDC_ID))
      
      
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
        
        # list(
        # 
        #   column(width=3,
        #          downloadButton("onlySetA", paste("Only",selection1))
        #   ),
        #   column(width=3,
        #          downloadButton("setAandsetB", paste(selection1,"And",selection2))
        #   ),
        #   column(width=3,
        #          downloadButton("onlySetB", paste("Only",selection2))
        #   ),
        #   column(width=3,
        #          downloadButton("setAorsetB", paste(selection1,"Union",selection2))
        #   )
        # )
        
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
    
    
    reactive.values$detailsdata <- temp_selection %>%
                                      dplyr::filter( ACCOUNT_SFDC_ID %in% idstoset )
    
    updateTabItems(session, "tabsmenu", selected = "customermap")
    
    
  })
  
  
})
