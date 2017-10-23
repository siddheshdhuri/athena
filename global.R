library(data.table)
library(ggmap)
library(dplyr)
library(reshape2)
library(DT)
library(stringr)
library(plotly)
library(VennDiagram)
library(googleVis)

source("helpe.R")

if(FALSE){

customers <- data.table::fread("CRONERI_DATA.csv")
coordinates <- data.table::fread("ukpostcodes.csv")

customers <- readRDS("data/customers.RDS")
coordinates <- readRDS("data/ukpostcodes.RDS")

customers <- merge(customers,coordinates,by.x = "SOLDTO_POSTCODE", by.y ="postcode",all.x = TRUE)

customers$CONTRACT_STARTDATE <- as.Date(as.character(customers$CONTRACT_STARTDATE), "%Y%m%d") 
customers$CONTRACT_ENDDATE <- as.Date(as.character(customers$CONTRACT_ENDDATE), "%Y%m%d") 

customer_renewals <- customers %>% 
                      group_by(CONTRACT_SOLDTOID) %>% 
                      summarise(EARLIEST_RENEWAL = min(CONTRACT_ENDDATE),
                                LATEST_RENEWAL = max(CONTRACT_ENDDATE))

customer_renewals$STATUS <- ifelse(customer_renewals$LATEST_RENEWAL >= Sys.Date(), "LIVE", "LAPSED")

customers <- merge(customers,customer_renewals,by = "CONTRACT_SOLDTOID", all.x = TRUE)




#' compute emp size categories
max_emp <- max(customers$NUMBEROFEMPLOYEES, na.rm = T)
customers$EMP_SIZE = cut(customers$NUMBEROFEMPLOYEES,c(0,5,10,50,250,500,1000,max_emp))
levels(customers$EMP_SIZE) = c("0-5","6-10","11-50","51-250","251-500","501-1000","1000+")
#customers$EMP_SIZE <- as.character(customers$EMP_SIZE)

#' Add NA as a level in the factor
levels_without_na <- levels(customers$EMP_SIZE)
customers$EMP_SIZE <- addNA(customers$EMP_SIZE)
levels(customers$EMP_SIZE) <- c(levels_without_na, "Unknown")


#' compute annual revenue categories
max_turnover <- max(customers$ANNUALREVENUE, na.rm = T)
customers$TURNOVER_SIZE = cut(customers$ANNUALREVENUE, c(0,100000,500000,1000000,5000000,10000000,max_turnover))
levels(customers$TURNOVER_SIZE) = c("0-100K","100K-500L","500K-1M","1M-5M","5M-10M","10M+")


levels_without_na <- levels(customers$TURNOVER_SIZE)
customers$TURNOVER_SIZE <- addNA(customers$TURNOVER_SIZE)
levels(customers$TURNOVER_SIZE) <- c(levels_without_na, "Unknown")




#' compute info value size categories
max_info_val <- max(customers$ACCOUNT_INFO_VALUE, na.rm = T)
min_info_val <- min(customers$ACCOUNT_INFO_VALUE, na.rm = T)-1
customers$INFO_VALUE_SIZE = cut(customers$ACCOUNT_INFO_VALUE,c(min_info_val,500,1000,2500,5000,10000,max_info_val))
levels(customers$INFO_VALUE_SIZE) = c("< 500","501-1000","1001-2500","2501-5000","5001-10000","10000+")
#customers$INFO_VALUE_SIZE <- as.character(customers$INFO_VALUE_SIZE)



unknown.addr <- customers[is.na(customers$latitude), c("SOLDTO_POSTCODE","longitude","latitude")]

#unknown.addr.str <- paste(unknown.addr$SOLDTO_ADDRESS, unknown.addr$SOLDTO_TOWN, sep = ", ")
unknown.addr <- unknown.addr[!duplicated(unknown.addr$SOLDTO_POSTCODE),]

unknown.addr <- unknown.addr[1:2000,]

coods <- ggmap::geocode(unknown.addr$SOLDTO_POSTCODE)

unknown.addr$longitude <- unknown.addr$latitude <- NULL
unknown.addr$longitude <- coods$lon
unknown.addr$latitude <- coods$lat

colnames(unknown.addr) <- c("postcode","longitude","latitude")
unknown.addr <- unknown.addr[!is.na(unknown.addr$longitude), c("postcode","latitude","longitude")]

#' save to uk postcodes

# for( i in 1:100){
#   
#   location <- unknown.addr$SOLDTO_TOWN[i]
#   
#   withTimeout({
#     repeat {
#       coods <- try(ggmap::geocode(location))
#       unknown.addr$longitude[i] <- coods$lon
#       unknown.addr$latitude[i] <- coods$lat
#       # Handle error from try if necessary:
#       if (class(coods) != "try-error") {
#         Sys.sleep(60)
#       }
#     }
#   }, timeout = 5*60, onTimeout = "warning")
#   
# }


unkown.coods.df <- data.frame(location = unknown.addr,
                              longitude = coods$lon,
                              latitude = coods$lat)

}


customers <- readRDS("data/customers.RDS")

#' accounts one record per sold to id for plotting on map
# accounts <- customers[ !duplicated(customers$CONTRACT_SOLDTOID) , ]


#' function to redraw map
redrawMap <- function(accounts, colorBy = NULL){
  
  withProgress(message = "Redrawing Map",{
    
    has.cood.index <-  complete.cases(accounts[,c("longitude","latitude")])
    
    has.cood.account <- accounts[has.cood.index,]
    has.cood.account$RADIUS <- scales::rescale(has.cood.account$ACCOUNT_INFO_VALUE, to = c(8,60))
    
    pal <- colorFactor(c("blue","black"), domain = has.cood.account$STATUS)
    
    leafletProxy("map", data = has.cood.account) %>% 
      clearMarkers() %>%
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
    
  })
  
}