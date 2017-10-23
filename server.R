
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
  
  ############################## - Start Customer Map - #############################
  #' Map
  #'
  output$map <- renderLeaflet({
    
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
      setView(lng = -3, lat = 52.3, zoom = 7) %>%
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
        length(unique(selectedCustomers$CONTRACT_SOLDTOID)), "Accounts", icon = icon("building"),
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
        paste("£ ",format(round(sum(accounts$ACCOUNT_INFO_VALUE)/1000000,1), big.mark=",")," M",sep=""), "Total Value", icon = icon("gbp"),
        color = "teal"
      )
    })
    
    #' Value box showing APVC
    output$APVCValueBox <- renderValueBox({
      
      valueBox(
        paste("£",ceiling(sum(accounts$ACCOUNT_INFO_VALUE)/nrow(accounts))), "APVC", icon = icon("money"),
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
    
    # output$prodBar <- plotly::renderPlotly({
    # 
    #   proddf <- as.data.frame(table(contracts$PRODUCT_CODE))
    #   print(head(proddf))
    #   proddf <- proddf[order(proddf$Freq, decreasing = T), ]
    #   print(head(proddf))
    #   proddf <- proddf[1:5, ]
    #   
    #   p <- plot_ly(
    #     x = proddf$Var1,
    #     y = proddf$Freq,
    #     name = "Top 5 Products",
    #     type = "bar"
    #   )
    #   
    # 
    # })
    
    
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
      prodCodes <- isolate(input$productsToMatch)
      jobTitles <- isolate(input$jobsToMatch)
      
      #' apply filter to global customers df
      customers <- customers %>%
        dplyr::filter( EMP_SIZE %in% empSizes ) %>%
        dplyr::filter( TURNOVER_SIZE %in% turnoverSizes ) %>%
        dplyr::filter( INFO_VALUE_SIZE %in% infovalsizes ) %>%
        dplyr::filter( CCH_BUSINESS_TYPE %in% businessTypes ) %>%
        dplyr::filter( STATUS %in% custStatus ) %>%
        dplyr::filter( CONTRACT_STATUS %in% contractStatus ) %>%
        dplyr::filter( PRODUCT_CODE %contains% prodCodes) %>%
        dplyr::filter( JOB_TITLE %contains% jobTitles )
      
      
      accounts <- customers[ !duplicated(customers$CONTRACT_SOLDTOID) , ]
      
      #' set filtered data to reactive df
      reactive.values$detailsdata <- customers
      
      
      redrawMap(accounts)
      
    })
    
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
      
      data <- reactive.values$detailsdata
      
      data <- data[ !duplicated(x$CONTRACT_SHIPTOID), ]
      
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
      
      accounts <- selections.df[ !duplicated(selections.df$CONTRACT_SOLDTOID) , ]
      
      redrawMap(accounts)
      
    })
    
  })
  
  
  #'############################################################################
  #' observerEvent for the loadSelection action button
  #' 
  observeEvent(input$saveSelection,{
    
    selectiondata <- reactive.values$detailsdata
    selectionName <- input$selectionName
    
    saveRDS(selectiondata, paste0("selections/",selectionName,".RDS"))
    
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
      
      only_sel1 <- setdiff(unique(selection1$CONTRACT_SOLDTOID), unique(selection2$CONTRACT_SOLDTOID))
      only_sel2 <- setdiff(unique(selection2$CONTRACT_SOLDTOID), unique(selection1$CONTRACT_SOLDTOID))
      
      sel1_and_sel2 <- intersect(unique(selection1$CONTRACT_SOLDTOID), unique(selection2$CONTRACT_SOLDTOID))
      
      sel1_union_sel2 <- union(unique(selection1$CONTRACT_SOLDTOID), unique(selection2$CONTRACT_SOLDTOID))
      
      
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
      
    })
    
  })
  
  
})
