library(data.table)
library(ggmap)
library(dplyr)
library(reshape2)
library(DT)
library(stringr)
library(plotly)
library(VennDiagram)
library(googleVis)
library(rJava)
library(RJDBC)


# initial_seg_cols = c("CCH_BUSINESS_TYPE","ACCOUNT_SFDC_ID","SOLDTO_GUO_NAME","SOLDTO_ADDRESS","SOLDTO_TOWN","SOLDTO_POSTCODE",
#                      "INDUSTRY","SOLDTO_EMAIL","SOLDTO_PHONE", "CONTRACT_SHIPTOID","SHIPTO_FIRST_NAME","SHIPTO_LAST_NAME","SHIPTO_JOB_TITLE",
#                      "SHIPTO_EMAIL","SHIPTO_PHONE","LATEST_RENEWAL","EARLIEST_RENEWAL","STATUS","CCH_INFO_VALUE","CRONER_INFO_VALUE")

initial_seg_cols = c("ACCOUNT_SFDC_ID","SOLDTO_GUO_NAME","SOLDTO_POSTCODE",
                     "LATEST_RENEWAL","EARLIEST_RENEWAL","STATUS","CCH_INFO_VALUE","CRONER_INFO_VALUE")


if (FALSE) {
  
  #'#####################################################################################
  #' intitialise settings
  Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-8-openjdk-amd64')
  options(java.parameters="-Xmx2g")
  
  .jinit()
  
  jdbcDriver <- JDBC(driverClass = "oracle.jdbc.OracleDriver",
                     classPath = "/usr/lib/oracle/12.1/client64/lib/ojdbc6.jar")
  
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//ukkinORA03x.wkuk.net:1521/WKUKDW",
                              "REPO", "INTERVENTION5")
  
  #'#####################################################################################
  
  
  #' current customers query
  cust_query <- "WITH LIVE_CUST_DETAILS AS (
  SELECT 
  CT.SOLDTOID__C AS CONTRACT_SOLDTOID, 
  CT.SHIPTOID__C AS CONTRACT_SHIPTOID,
  CT.BILL_TO_ID__C AS CONTRACT_BILLTOID,
  CT.PAYER_ID__C AS CONTRACT_PAYERID, 
  CT.ID AS CONTRACT_SFDC_ID, CT.CONTRACTNUMBER__C AS CONTRACT_SAP_ID,
  CT.AMOUNT__C AS CONTRACT_VALUE, CT.STATUS__C AS CONTRACT_STATUS,
  CT.DESCRIPTION__C AS CONTRACT_DESCRIPTION,
  TO_CHAR(CT.STARTDATE__C,'YYYYMMDD') AS CONTRACT_STARTDATE, 
  TO_CHAR(CT.ENDDATE__C,'YYYYMMDD') AS CONTRACT_ENDDATE,
  P.PRODUCT_CODE, P.PRODUCT_FAMILY, P.BRAND, P.BUSINESS_UNIT, P.\"Subs_One-Off\" AS SUBS_ONE_OFF,
  A.ID AS ACCOUNT_SFDC_ID, A.SAP_CLIENT_NUMBER__C AS ACCOUNT_SAP_ID, 
  (A.TOTALCCHINFORMATIONVALUE + A.TOTALCRONERINFORMATIONVALUE) AS ACCOUNT_INFO_VALUE,
  soldto.CCH_BUSINESS_TYPE, soldto.BUSINESS_TYPE, A.NUMBEROFEMPLOYEES, A.ANNUALREVENUE, A.INDUSTRY,
  REPLACE(soldto.GUO_NAME,',',';') AS SOLDTO_GUO_NAME,
  REPLACE(soldto.ADDRESS,',',';') AS SOLDTO_ADDRESS, 
  REPLACE(soldto.TOWN,',',';') AS SOLDTO_TOWN, 
  REPLACE(soldto.POST_CODE,',',';') AS SOLDTO_POSTCODE,
  soldto.EMAIL AS SOLDTO_EMAIL, soldto.TELEPHONE SOLDTO_PHONE,
  REPLACE(shipto.FIRST_NAME,',',';') AS SHIPTO_FIRST_NAME,
  REPLACE(shipto.LAST_NAME,',',';') AS SHIPTO_LAST_NAME,
  REPLACE(shipto.JOB_TITLE,',',';') AS JOB_TITLE,
  REPLACE(shipto.ADDRESS,',',';') AS SHIPTO_ADDRESS, 
  REPLACE(shipto.TOWN,',',';') AS SHIPTO_TOWN, 
  REPLACE(shipto.POST_CODE,',',';') AS SHIPTO_POSTCODE,
  shipto.EMAIL AS SHIPTO_EMAIL, shipto.TELEPHONE SHIPTO_PHONE,
  A.TOTALCCHINFORMATIONVALUE AS CCH_INFO_VALUE,
  A.TOTALCRONERINFORMATIONVALUE AS CRONER_INFO_VALUE
  FROM ODS.SFDC_CONTRACT CT, ODS.SFDC_ACCOUNT A, ODS.SFDC_CONTRACT_LINEITEM LI,
  BUSINESS_ANALYSIS.CUSTOMER_D SOLDTO, 
  BUSINESS_ANALYSIS.CUSTOMER_D SHIPTO,
  BUSINESS_ANALYSIS.PRODUCT_D P
  WHERE UPPER(CT.CATEGORY__C) = 'INFORMATION'
  AND CT.ACCOUNTID__C = A.ID
  AND CT.ID = LI.CONTRACTID__C
  AND P.PRODUCT_CODE = LI.PRODUCTCODE__C
  AND CT.ENDDATE__C > (SYSDATE - 365*2)
  /*AND UPPER(CT.STATUS__C) = 'ACTIVE'
  AND (A.TOTALCCHINFORMATIONVALUE + A.TOTALCRONERINFORMATIONVALUE) > 0*/
  AND SOLDTO.BP_ID = CT.SOLDTOID__C
  AND SHIPTO.BP_ID = CT.SHIPTOID__C
  )
  SELECT *
  FROM LIVE_CUST_DETAILS LCD"


cust_query <- gsub("\n|\t"," ",cust_query)


t1 <- Sys.time()
customers <- RJDBC::dbGetQuery(jdbcConnection, cust_query)
t2 <- Sys.time()

print(paste("time taken : ",t2-t1))
  
}


source("helpe.R")

if(FALSE){

  x <- readLines("data/CUST_DATA2.csv")
  
customers <- data.table::fread("data/CUST_DATA2.csv")




coordinates <- data.table::fread("ukpostcodes.csv")

customers <- readRDS("data/customers.RDS")
coordinates <- readRDS("data/ukpostcodes.RDS")

customers <- merge(customers,coordinates,by.x = "SOLDTO_POSTCODE", by.y ="postcode",all.x = TRUE)

customers$CONTRACT_STARTDATE <- as.Date(as.character(customers$CONTRACT_STARTDATE), "%Y-%m-%d") 
customers$CONTRACT_ENDDATE <- as.Date(as.character(customers$CONTRACT_ENDDATE), "%Y-%m-%d") 

customer_renewals <- customers %>% 
                      group_by(ACCOUNT_SFDC_ID) %>% 
                      summarise(EARLIEST_RENEWAL = min(CONTRACT_ENDDATE),
                                LATEST_RENEWAL = max(CONTRACT_ENDDATE))

customer_renewals$STATUS <- ifelse(customer_renewals$LATEST_RENEWAL >= Sys.Date(), "LIVE", "LAPSED")

customers <- merge(customers,customer_renewals,by = "ACCOUNT_SFDC_ID", all.x = TRUE)



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


customers <- readRDS("data/customers2.RDS")

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