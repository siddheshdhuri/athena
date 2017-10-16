
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
  
  ############################## - Start Tweet Map - #############################
  #' Map
  #'
  output$map <- renderLeaflet({
    
    # map <- leaflet() %>%
    #   addTiles() %>%
    #   setView(lng = -0.0, lat = 51.5, zoom = 4) %>%
    #   addMarkers(lng = ~lon, lat = ~lat, layerId = ~tweetid)
    
    accounts <- customers[ !duplicated(customers$CONTRACT_SOLDTOID) , ]
    
    #' set NA account_values to zero
    accounts$ACCOUNT_INFO_VALUE[is.na(accounts$ACCOUNT_INFO_VALUE)] <- 0
    
    has.cood.index <-  complete.cases(accounts[,c("longitude","latitude")])
    
    has.cood.account <- accounts[has.cood.index,]
    has.cood.account$RADIUS <- scales::rescale(has.cood.account$ACCOUNT_INFO_VALUE, to = c(8,60))
    
    #' keep filtered
    empSizes <- isolate(input$empsizeCheckbox)
    infovalsizes <- isolate(input$infovalsizeCheckbox)
    businessTypes <- isolate(input$businessTypeCheckbox)
    
    has.cood.account <- has.cood.account %>%
      dplyr::filter( EMP_SIZE %in% empSizes ) %>%
      dplyr::filter( INFO_VALUE_SIZE %in% infovalsizes ) %>%
      dplyr::filter( CCH_BUSINESS_TYPE %in% businessTypes ) %>%
      dplyr::filter( STATUS %in% "LIVE" )
    
    pal <- colorFactor(c("blue","black"), domain = has.cood.account$STATUS)
    
    map <- leaflet(has.cood.account) %>%
      addTiles() %>%
      setView(lng = -4, lat = 52.3, zoom = 7) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal(STATUS),
        stroke = TRUE, weight = 2,
        opacity = 0.6,
        radius = ~RADIUS,
        label = ~SOLDTO_GUO_NAME,
        layerId = ~CONTRACT_SOLDTOID,
        options = list(riseOnHover = TRUE)
        #clusterOptions = markerClusterOptions()
      ) %>%
      addLegend("bottomleft",pal=pal, values=~STATUS, layerId = "colorLegend")
    
    
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
                                            tags$b( selectedAccount$CONTRACT_SOLDTOID[1] ),
                                            tags$h2( paste("Information Value: £",selectedAccount$ACCOUNT_INFO_VALUE[1]) ),
                                            tags$h2( paste("Software Value: £",selectedAccount$ACCOUNT_SW_VALUE[1]) ),
                                            tags$h3( paste("Earliest Renewal: ",selectedAccount$EARLIEST_RENEWAL[1])),
                                            tags$h3( paste("Latest Renewal: ",selectedAccount$LATEST_RENEWAL[1]))
                                            ) )
    
    accountContracts <- selectedAccount[!duplicated(selectedAccount$CONTRACT_SAP_ID) ,
                                        c("CONTRACT_SAP_ID","CONTRACT_DESCRIPTION","CONTRACT_STATUS",
                                          "CONTRACT_VALUE","CONTRACT_STARTDATE","CONTRACT_ENDDATE")]
    
    accountProducts <- selectedAccount[!duplicated(selectedAccount$PRODUCT_CODE) ,
                                       c("PRODUCT_CODE","PRODUCT_DESCRIPTION")]
    
    accountContacts <- selectedAccount[!duplicated(selectedAccount$CONTRACT_SHIPTOID) ,
                                       c("SHIPTO_FIRST_NAME","SHIPTO_LAST_NAME", "SHIPTO_EMAIL", "SHIPTO_PHONE")]
    
    
    output$contractsAtAccount <- DT::renderDataTable(accountContracts)
    
    output$productsAtAccount <- DT::renderDataTable(accountProducts)
    
    output$contactsAtAccount <- DT::renderDataTable(accountContacts)
    
  })
  
  
  
  #### End- user Map Code - #####
  
  
  observeEvent(input$applyFilter,{
    
    empSizes <- input$empsizeCheckbox
    infovalsizes <- input$infovalsizeCheckbox
    businessTypes <- input$businessTypeCheckbox
    custStatus <- input$custStatusCheckbox
    contractStatus <- isolate(input$contractStatusCheckbox)
    
    #' apply filter to global customers df
    customers <- customers %>%
      dplyr::filter( EMP_SIZE %in% empSizes ) %>%
      dplyr::filter( INFO_VALUE_SIZE %in% infovalsizes ) %>%
      dplyr::filter( CCH_BUSINESS_TYPE %in% businessTypes ) %>%
      dplyr::filter( STATUS %in% custStatus ) %>%
      dplyr::filter( CONTRACT_STATUS %in% contractStatus )
    
    accounts <- customers[ !duplicated(customers$CONTRACT_SOLDTOID) , ]
    
    #' set filtered data to reactive df
    reactive.values$detailsdata <- customers
    
    
    redrawMap(accounts)
    
  })
  
  
  #' ##########################################################
  #' Filter data by company name
  #' 
  observeEvent(input$filterByName,{
    
    searchterm <- input$searchTerm
    customers <- reactive.values$detailsdata
    
    customers <- customers %>%
      dplyr::filter( grepl(pattern = searchterm, x = customers$SOLDTO_GUO_NAME, ignore.case = T) )
    
    reactive.values$detailsdata <- customers
    
    accounts <- customers[ !duplicated(customers$CONTRACT_SOLDTOID) , ]
    
    redrawMap(accounts)
    
    
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
      
      # if(length(colnames(pp)) > 6){
      #   colnames(pp) <- c("CONTRACT_SOLDTOID","SOLDTO_GUO_NAME",
      #                     "CCH_Contracts","Croner_Contracts","Unknown_Contracts",
      #                     "CCH_Products", "Croner_Products", "Unknown_Products")
      #   
      # }else{
      #   colnames(pp) <- c("CONTRACT_SOLDTOID","SOLDTO_GUO_NAME",
      #                     "CCH_Contracts","Croner_Contracts",
      #                     "CCH_Products", "Croner_Products")
      # }
      
      pp
      
    })
    
    reactive.values$pivot <- pivot
    return(pivot)
    
  })
  
  
  #' #########################################################
  #' Customer Segment Pivot
  #' 
  output$custSegPivot <- DT::renderDataTable({
    
    xaxis <- input$xaxis
    yaxis <- input$yaxis
    valuevar <- input$valuevar
    
    customers <- reactive.values$detailsdata
    customers <- customers[ !duplicated(customers$CONTRACT_SFDC_ID) , ]
    
    display.table <<- withProgress(message = "in progress",
                                   detail = "This may take a while...",{
                                     getSummaryData(customers, xaxis, yaxis, valuevar)
                                   })
    
    return(display.table)
    
  })
  
  
  
  #' #########################################################
  #' Export details 
  #' 
  output$exportDetails <- downloadHandler(
    filename = function() {
      paste('details-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      #' apply filters
      empSizes <- isolate(input$empsizeCheckbox)
      infovalsizes <- isolate(input$infovalsizeCheckbox)
      businessTypes <- isolate(input$businessTypeCheckbox)
      custStatus <- isolate(input$custStatusCheckbox)
      contractStatus <- isolate(input$contractStatusCheckbox)
      
      customers <- customers %>%
        dplyr::filter( EMP_SIZE %in% empSizes ) %>%
        dplyr::filter( INFO_VALUE_SIZE %in% infovalsizes ) %>%
        dplyr::filter( CCH_BUSINESS_TYPE %in% businessTypes ) %>%
        dplyr::filter( STATUS %in% custStatus ) %>%
        dplyr::filter( CONTRACT_STATUS %in% contractStatus )
      
      
      customers <- sapply(customers,as.character)
      customers[is.na(customers)] <- ""
      write.csv(customers, con, sep = ",", row.names = FALSE)
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
      write.csv(data, con, sep = ",", row.names = FALSE)
    }
  )
  
  #' #########################################################
  #' ui summary
  #' 
  output$countsSummary <- shiny::renderUI({
    
    numcust <- length(unique(reactive.values$detailsdata$CONTRACT_SOLDTOID))
    numcontacts <- length(unique(reactive.values$detailsdata$CONTRACT_SHIPTOID))
    
    uilist <- list(tags$h3(paste("Customers:", numcust)),
                   tags$h4(paste("Contacts:", numcontacts)))
    
    return(uilist)
  })
  
  #############################################################################
  #' observerEvent for the loadSelection action button
  observeEvent(input$loadSelection,{
    
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
    
    accounts <- selections.df[ !duplicated(selections.df$CONTRACT_SOLDTOID) , ]
    
    redrawMap(accounts)
    
  })
  
  
  #############################################################################
  #' observerEvent for the loadSelection action button
  observeEvent(input$saveSelection,{
    
    selectiondata <- reactive.values$detailsdata
    selectionName <- input$selectionName
    
    saveRDS(selectiondata, paste0("selections/",selectionName,".RDS"))
    
    #' do something to show that selection is saved
    
  })
  
})
