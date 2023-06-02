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
#library(RJDBC)
library(leaflet.extras)
library(sp)
library(heatmaply)
library(maputils)
library(crosstabutils)

library(officer)
library(knitr)
library(flextable)


tol21rainbow= c("#771155", "#CC99BB", "#AA4488", "#771122", "#AA4455", "#774411", "#DD7788", "#AA7744", "#DDAA77", 
                "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", 
                "#77CCCC", "#117744", "#44AA77" )


############# - NA Patters to ignore while reding text file - ################################
na.patterns <- "#REF!|#N/A|\\(blank\\)"

source("utils/helpe.R")
source("utils/config.R")



is.empty <- function(x){
  
  if(is.null(x)) return(TRUE)
  
  if(is.na(x)) return(TRUE)
  
  if(nchar(gsub("[^[:alnum:]]","",x)) < 1) return(TRUE)
  
  return(FALSE)
  
}

#' subset global variables (as i am lazy to create reactive variables)
only_sel1 <- only_sel2 <- sel1_and_sel2 <- NULL
temp_selection <- NULL


if (FALSE) {
  
  #'#####################################################################################
  #' connect to database
  jdbcConnection <- dbconnect::getDBConnection(dbserver = "ukkinORA03x.wkuk.net",
                                           portnumber = "1521", 
                                           dbname = "WKUKDW", 
                                           username = "DHURIS", 
                                           password = "Approach$204Fill")
  
  #'#####################################################################################
  
  cust_query <- "WITH LIVE_CUST_DETAILS AS (
  SELECT 
  CT.SOLDTOID__C AS CONTRACT_SOLDTOID, 
  CT.SHIPTOID__C AS CONTRACT_SHIPTOID,
  CT.BILL_TO_ID__C AS CONTRACT_BILLTOID,
  CT.PAYER_ID__C AS CONTRACT_PAYERID, 
  CT.ID AS CONTRACT_SFDC_ID, CT.CONTRACTNUMBER__C AS CONTRACT_SAP_ID,
  CT.STATUS__C AS CONTRACT_STATUS, CT.DESCRIPTION__C AS CONTRACT_DESCRIPTION,
  TO_CHAR(CT.STARTDATE__C,'YYYYMMDD') AS CONTRACT_STARTDATE, 
  TO_CHAR(CT.ENDDATE__C,'YYYYMMDD') AS CONTRACT_ENDDATE,
  CT.AMOUNT__C AS CONTRACT_VALUE, CT.CONTRACTTERM__C/12 AS CONTRACT_TERM_YEARS, 
  LI.CONTRACTLINEITEMNUMBER__C AS LINEITEM_SAP_ID, LI.AMOUNT__C AS LINEITEM_VALUE,
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
  A.TOTALCRONERINFORMATIONVALUE AS CRONER_INFO_VALUE,
  ROW_NUMBER() OVER (PARTITION BY LI.CONTRACTLINEITEMNUMBER__C ORDER BY LI.CONTRACTLINEITEMNUMBER__C) AS RN
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
  FROM LIVE_CUST_DETAILS LCD
  WHERE RN = 1"


cust_query <- gsub("\n|\t"," ",cust_query)


t1 <- Sys.time()
customers <- RJDBC::dbGetQuery(jdbcConnection, cust_query)
t2 <- Sys.time()

print(paste("time taken : ",t2-t1))


customers$CONTRACT_TERM_YEARS <- ifelse(customers$CONTRACT_TERM_YEARS < 1,1,customers$CONTRACT_TERM_YEARS)

customers$LINEITEM_ANNUAL_VALUE <- customers$LINEITEM_VALUE / customers$CONTRACT_TERM_YEARS
customers$CONTRACT_ANNUAL_VALUE <- customers$CONTRACT_VALUE / customers$CONTRACT_TERM_YEARS

  
}


if(FALSE){
  
  coordinates <- readRDS("data/ukpostcodes.RDS")
  
  customers <- merge(customers,coordinates,by.x = "SOLDTO_POSTCODE", by.y ="postcode",all.x = TRUE)
  remove(coordinates)
  
  customers$CONTRACT_STARTDATE <- as.Date(as.character(customers$CONTRACT_STARTDATE), "%Y%m%d") 
  customers$CONTRACT_ENDDATE <- as.Date(as.character(customers$CONTRACT_ENDDATE), "%Y%m%d") 
  
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
  levels(customers$TURNOVER_SIZE) = c("0-100K","100K-500K","500K-1M","1M-5M","5M-10M","10M+")
  
  
  levels_without_na <- levels(customers$TURNOVER_SIZE)
  customers$TURNOVER_SIZE <- addNA(customers$TURNOVER_SIZE)
  levels(customers$TURNOVER_SIZE) <- c(levels_without_na, "Unknown")
  
  
  #' compute info value size categories
  max_info_val <- max(customers$ACCOUNT_INFO_VALUE, na.rm = T)
  min_info_val <- min(customers$ACCOUNT_INFO_VALUE, na.rm = T)-1
  customers$INFO_VALUE_SIZE = cut(customers$ACCOUNT_INFO_VALUE,c(min_info_val,500,1000,2500,5000,10000,max_info_val))
  levels(customers$INFO_VALUE_SIZE) = c("< £ 500","£ 501-1000","£ 1001-2500","£ 2501-5000","£ 5001-10000","£ 10000+")
  
  ## add region
  customers$SOLDTO_POSTCODE <- gsub("[^[:alnum:][:space:]]","",customers$SOLDTO_POSTCODE)
  customers$PostcodeArea <- substr(customers$SOLDTO_POSTCODE,
                                   start = 1,
                                   stop = as.numeric(regexpr("[0-9]",customers$SOLDTO_POSTCODE))-1)
  
  postregion <- readRDS("data/postarea_region_mapping.RDS")
  
  customers <- merge(customers, postregion, by="PostcodeArea", all.x = TRUE)
  customers$Region[unlist(lapply(customers$Region, function(x) is.empty(x)))] <- "Unknown"
  
  
  saveRDS(customers, "data/archive/customersMAR2018.RDS")
  
  if(FALSE) {
    
    #' find adresses for unknown postcode
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
    
    coordinates <- rbind(coordinates, unknown.addr)
    
    #' save to uk postcodes
    saveRDS(coordinates, "data/ukpostcodes.RDS")
    
  }

}


customers <- readRDS("data/customers_anonymized.RDS")

#' ###############################################################################
#' function to redraw map
#' 
redrawMap <- function(has.cood.account, colorBy = "Region"){
  
  withProgress(message = "Redrawing Map",{
    
    pal <- colorFactor(tol21rainbow, domain = has.cood.account[[colorBy]])
    
    leafletProxy("map", data = has.cood.account) %>% 
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal(has.cood.account[[colorBy]]),
        stroke = TRUE, weight = 2,
        opacity = 0.6,
        radius = ~RADIUS,
        label = ~SOLDTO_GUO_NAME,
        layerId = ~CONTRACT_SOLDTOID,
        options = list(riseOnHover = TRUE)
        #clusterOptions = markerClusterOptions()
      ) %>%
      addLegend("bottomleft",pal=pal, values=has.cood.account[[colorBy]], layerId = "colorLegend")
    
  })
  
}

#'#######################################################################################################
#' Code to anonymize customers data
#'
if(FALSE) {
  customers$CONTRACT_DESCRIPTION <- ""
  customers$CONTRACT_DESCRIPTION <- paste0(customers$CONTRACT_DESCRIPTION, c("Outsourced Services contract", "Software Development contract", "Protein Database License contract",
                                                                             "Medical Information Subscription contract", "Clinical Research contract", "Consultancy services contract",
                                                                             "Drugs Database Subscription contract ", "Adverse Event DB contract", "Protein interaction network contract",
                                                                             "Metabolic Pathways Map contract", "Genomics Sequence Service contract", "Protein Network Database contract"))
  customers$PRODUCT_CODE <- ""
  customers$PRODUCT_CODE <- paste0(customers$PRODUCT_CODE, c("OS101","SD201", "PR301", "MIS001", "CR301", "CS201", "DRDB001","AEDB201",
                                                             "PIN001","MPM201","GSS101", "PNDB101"))
  
  customers$PRODUCT_FAMILY <- ""
  customers$PRODUCT_FAMILY <- paste0(customers$PRODUCT_FAMILY, c("Service", "Consultancy","Software", "Information" , "Service", "Consultancy", "Information", "Information",
                                                                 "Software", "Software", "Service", "Information"))
  
  customers$BRAND  <- ""
  customers$BRAND  <- paste0(customers$BRAND, c("InfoServ", "InfoSoft", "InfoSoft", "InfoServ","InfoServ", "InfoSoft", "InfoServ"))
  
  customers$BUSINESS_UNIT <- ""
  customers$BUSINESS_UNIT <- paste0(customers$BUSINESS_UNIT, c("Information", "Software", "Consultancy"))
  
  
  customers$SOLDTO_GUO_NAME <- ""
  customers$SOLDTO_GUO_NAME <- paste0(customers$SOLDTO_GUO_NAME, c("Big Pharma Company", "Medical Device Company", "Clinical Research Org", "Medical Research Company",
                                                                   "Stemcell research company", "Pharma Manufacturer", "Drug API Company", "Generic Drug Manufacturer", 
                                                                   "Biotech company", "Pharma marketing company", "Oncology Researcg Institute", "Medicine University",
                                                                   "Vaccines company", "Cosmetics research company"))
  
  
  
  customers$SOLDTO_EMAIL <- ""
  customers$SOLDTO_EMAIL <- paste0(customers$SOLDTO_EMAIL, c("info@BigPharma.com", "info@MedicalDevice.com", "info@ClinicalResearch.Org", "info@MedicalResearch.Com",
                                                             "info@Stemcellresearch.com", "info@PharmaManufacturer.com", "info@DrugAPI.Com", "info@GenericDrugManufacturer.com", 
                                                             "info@Biotech.com", "info@Pharmamarketing.com", "info@OncologyResearcg.org", "info@MedicineUniversity.edu",
                                                             "info@Vaccines.com", "info@Cosmeticsresearch.com"))
  
  
  customers$SHIPTO_FIRST_NAME <- ""
  customers$SHIPTO_FIRST_NAME <- paste0(customers$SHIPTO_FIRST_NAME, c("Jake","Jay","Joy","Justin", "Jenny","John", "Jeffery", "Jack","Jamie","Jane","Jim"))
  
  customers$SHIPTO_LAST_NAME <- ""
  customers$SHIPTO_LAST_NAME <- paste0(customers$SHIPTO_LAST_NAME, c("Doe", "Smith", "Jackson", "Wilson", "Sanders","Williams", "North","Cole", "Cooper"))
  
  customers$SHIPTO_EMAIL <- ""
  customers$SHIPTO_EMAIL <- paste0(customers$SHIPTO_FIRST_NAME,".",customers$SHIPTO_FIRST_NAME,"@mail.com")
  
  
  customers <- customers[!customers$Region == "Unknown", ]
  #' randomize rows
  customers <- customers[sample(nrow(customers)), ]
  
  customers <- customers[1:100000, ]
  
  saveRDS(customers, "data/customers_anonymized.RDS")
  
}



###############################################################
salesIcons <- iconList(
  sdIcon = makeIcon(
    iconUrl = "www/img/sr1.jpeg",
    iconWidth = 50, iconHeight = 50
  ),
  svIcon = makeIcon(
    iconUrl = "www/img/sr2.jpeg",
    iconWidth = 50, iconHeight = 50
  ),
  hmIcon = makeIcon(
    iconUrl = "www/img/sr3.jpeg",
    iconWidth = 50, iconHeight = 50
  ),
  skIcon = makeIcon(
    iconUrl = "www/img/sr4.jpeg",
    iconWidth = 50, iconHeight = 50
  ),
  lpIcon = makeIcon(
    iconUrl = "www/img/sr5.jpeg",
    iconWidth = 50, iconHeight = 50
  ),
  jbIcon = makeIcon(
    iconUrl = "www/img/sr6.jpeg",
    iconWidth = 50, iconHeight = 50
  )
)


#' ################################################################
#' function to get df to plot on map
#' 
#' getMapPlotDF <- function(df){
#'   
#'   accounts <- df[ !duplicated(df$CONTRACT_SOLDTOID) , ]
#'   
#'   #' set NA account_values to zero
#'   accounts$ACCOUNT_INFO_VALUE[is.na(accounts$ACCOUNT_INFO_VALUE)] <- 0
#'   
#'   has.cood.index <- complete.cases(accounts[,c("longitude","latitude")])
#'   
#'   has.cood.account <- accounts[has.cood.index,]
#'   
#'   if(nrow(has.cood.account) <= 0) return(NULL)
#'   
#'   has.cood.account$RADIUS <- scales::rescale(has.cood.account$ACCOUNT_INFO_VALUE, to = c(8,60))
#'   
#'   # generate second set of unique location IDs for second layer of selected locations
#'   has.cood.account$secondLocationID <- paste0(as.character(has.cood.account$CONTRACT_SOLDTOID), "_selectedLayer")
#'   
#'   return(has.cood.account)
#'   
#' }




