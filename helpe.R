
# '########################################################
#' Function to aggregate data frame given aggregate columns
getAggData <- function(aggBy, data){
  
  agg.by <- lapply(aggBy, as.symbol)
  
  agg.data <- data %>%
    group_by_(.dots = agg.by) %>%
    select(SOLDTO_GUO_NAME, CONTRACT_SOLDTOID,  CONTRACT_SAP_ID, PRODUCT_CODE, PRODUCT_FAMILY ,CONTRACT_VALUE, NUMBEROFEMPLOYEES, ANNUALREVENUE) %>%
    summarise(
      Customers = n_distinct(CONTRACT_SOLDTOID),
      Contracts = n_distinct(CONTRACT_SAP_ID),
      Products = n_distinct(PRODUCT_CODE),
      ProdFamilies = n_distinct(PRODUCT_FAMILY),
      TOV = ceiling(sum(CONTRACT_VALUE, na.rm=TRUE)),
      APVC = ceiling(sum(CONTRACT_VALUE, na.rm=TRUE) / Customers)
      #MOV = ceiling(median(CONTRACT_VALUE, na.rm=TRUE)),
      #AOV = ceiling(mean(CONTRACT_VALUE, na.rm=TRUE))
      # AvgEmp = ceiling(mean(EMPLOYEES, na.rm=TRUE)),
      # AvgTurnover = ceiling(mean(TURNOVER, na.rm=TRUE))
    )
  
  return(agg.data)
  
}

#' ########################################################
#' Function to transpose column given data frame
#'
getTransposeData <- function(xaxis,yaxis,valuevar,data){
  
  if(valuevar == "Penetration") valuevar <- "Customers"
  
  agg.data <- getAggData(c(xaxis,yaxis), data)
  
  dependent <- yaxis
  factors <- setdiff(xaxis,dependent)
  if(length(factors) < 1) factors <- xaxis
  formula.var <- as.formula(paste( paste(factors,collapse = "+"), paste("~",dependent)))
  
  func = switch (valuevar,
                 Customers = "sum",
                 Contracts = "sum",
                 Products = "sum",
                 TOV = "sum",
                 APVC = "mean",
                 AOV = "mean",
                 MOV = "median")
  
  transposed.data <- reshape2::dcast(agg.data, 
                                     formula.var, 
                                     value.var = valuevar , 
                                     fun.aggregate = getFunction(func), 
                                     na.rm = TRUE)
  
  
  if(yaxis == "SEGMENT"){
    # order segment column header names 
    col.names <- setdiff(colnames(transposed.data),xaxis)
    
    col.order <- col.names[order(match(col.names,segments.order))]
    
    transposed.data <- transposed.data[,c(xaxis,col.order)]
  }else  if(yaxis == "NEW_SEGMENT"){
    # order segment column header names 
    col.names <- setdiff(colnames(transposed.data),xaxis)
    
    col.order <- col.names[order(match(col.names,new.segments.order))]
    
    transposed.data <- transposed.data[,c(xaxis,col.order)]
  }else  if(yaxis == "SEGMENT_SIZE"){
    # order segment column header names 
    col.names <- setdiff(colnames(transposed.data),xaxis)
    
    col.order <- col.names[order(match(col.names,new.segmentsize.order))]
    
    transposed.data <- transposed.data[,c(xaxis,col.order)]
  }
  
  return(transposed.data)
}


# '########################################################
#' Function to append universe columns to data
#'
# appendUniverseData <- function(univ.col, xaxis, valuevar, data){
#   
#   if(any(business.cols %in% xaxis)){
#     return("Data aggregated by business specific columns, adding universe columns might result in duplication")
#   }
#   
#   agg.by <- lapply(xaxis, as.symbol)
#   univ.col <- as.symbol(univ.col)
#   
#   agg.univ <- univ %>%
#     group_by_(.dots = agg.by) %>%
#     select(HQ_Level) %>%
#     # select(HQ_Level, SITE_Level, URL_Level) %>%
#     summarise(
#       Universe = sum(univ.col, na.rm=TRUE)
#       # HQ_Universe = sum(HQ_Level, na.rm=TRUE),
#       # Site_Universe = sum(Site_Level, na.rm=TRUE),
#       # URL_Universe = sum(URL_Level, na.rm=TRUE)
#     )
#   
#   # Append Universe Column
#   summary.data <- merge(data, 
#                         agg.univ, 
#                         by=xaxis,
#                         all.x = TRUE)
#   
#   # summary.data["Totals",] <- colSums(summary.data)
#   # summary.data$Totals <- rowSums(summary.data)
#   
#   if(valuevar == "Penetration"){
#     summ.col.names <- colnames(summary.data)
#     summ.col.names <- setdiff(summ.col.names, c(xaxis,"Universe"))
#     
#     for(col in summ.col.names){
#       summary.data[,paste0(col,"_Penetration")] <- calc.penetration(summary.data[,col],summary.data[,"Universe"])
#     }
#     
#     # Keep only the columns with penetration percentage
#     keep.cols <- c(xaxis,paste0(summ.col.names,"_Penetration"))
#     
#     summary.data <- summary.data[,keep.cols]
#   }
#   
#   return(summary.data)
# }


# '########################################################
#' function to get summary data for display
#'
getSummaryData <- function(data = raw.data, xaxis, yaxis, valuevar) {
  
  agg.data <- getAggData(xaxis, data)
  
  if("NONE" == yaxis){
    transposed.data <- agg.data
  }else{
    transposed.data <- getTransposeData(xaxis,yaxis,valuevar,data)
  }
  
  
  # Add formating for cells
  transposed.data <- setColumnFormat(transposed.data)
  
  return(transposed.data)
  
}



# '########################################################
#' function to set format for table columns
#'
setColumnFormat <- function(table.to.format){
  
  numeric.cols <- colnames(table.to.format[,sapply(table.to.format, is.numeric)])
  
  data.table <- DT::datatable(table.to.format, 
                              filter = "top",
                              selection = list(target="cell"),
                              options = list(scrollX = TRUE,
                                             pageLength = 10,
                                             lengthMenu = c(10, 50, 100)
                                             )
                              )
    
  for(i in 1:length(numeric.cols)){
    
    col.name <- numeric.cols[i]
    
    col.color <- switch (paste0("case",i%%5),
      case0 = "lightblue",
      case1 = "lightgray",
      case2 = "beige",
      case3 = "lavender",
      case4 = "LemonChiffon"
    )
    
    data.table <- data.table %>%
                    DT::formatStyle(
                      col.name,
                      background = styleColorBar(range(table.to.format[,col.name], 
                                                       na.rm = TRUE), 
                                                 col.color)
                    )
  }
  
  return(data.table)
  
}


calc.penetration <- function(cust , univ){
  penetration <- round(cust/univ*100,2)  
}

#' Function that find if a value is between two values 
is.between <- function(x, a, b) {
  x > a & x < b
}

#' function to find string contains multiple strings
#' 
'%contains%' <- function(column ,toMatch) {
  x <- TRUE
  if(nchar(toMatch) > 1){
    toMatch <- toMatch %>% gsub(pattern=" ",replacement="") %>% gsub(pattern=",",replacement="|")
    
    x <- stringr::str_detect(column, regex(toMatch, ignore_case = T))
  }
  
  return(x)
}





