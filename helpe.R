

#' function to find string contains multiple strings
#' 
'%containswithspace%' <- function(column ,toMatch) {
  x <- TRUE
  if(nchar(toMatch) > 1){
    
    toMatch <- paste0("\\b",gsub(toMatch, pattern="\\n$",replacement=""),"\\b") %>% 
                gsub(pattern="\\n",replacement="\\\\b|\\\\b") %>% gsub(pattern="\\s*,\\s*",replacement="\\\\b|\\\\b")
    print(toMatch)
    x <- stringr::str_detect(column, regex(toMatch, ignore_case = T))
  }
  
  return(x)
}

#' function to find string contains multiple strings
#'
'%contains%' <- function(column ,toMatch) {
  x <- TRUE
  if(nchar(toMatch) > 1){
    toMatch <- toMatch %>% 
                gsub(pattern="\\n$",replacement="") %>%
                gsub(pattern="\\s+",replacement=" ") %>% 
                gsub(pattern="\\s*,\\s*",replacement="|") %>%
                gsub(pattern=" ",replacement="|") %>% 
                gsub(pattern="\\n",replacement="|")
    
      print(toMatch)
      
    x <- stringr::str_detect(column, regex(toMatch, ignore_case = T))
  }

  return(x)
}


#' plot heat map
#' 
#' 
plotheatmap <- function(plotdf, plotdf2=NULL, var1, var2,
                        excludevar = NULL,
                        excludeval = NULL,
                        includeval = NULL,
                        rotate=FALSE){
  
  plotdf <- plotdf[, c(var1,var2)]
  plotdf[] <- lapply(plotdf, as.character)
  
  if(!is.null(excludevar)){
    
    plotdf <- plotdf %>% filter(plotdf[[excludevar]] %in% setdiff(includeval,excludeval))
    
  }
  
  countsdf <- table(plotdf[[var1]], plotdf[[var2]])
  
  #subset df2
  # if(!is.null(plotdf2)){
  #   
  #   plotdf2 <- plotdf2[, c(var1,var2)]
  #   plotdf2[] <- lapply(plotdf2, as.character)
  #   
  #   plotdf2 <- plotdf2 %>% filter(plotdf2[[var1]] %in% unique(plotdf[[var1]]))
  #   plotdf2 <- plotdf2 %>% filter(plotdf2[[var2]] %in% unique(plotdf[[var2]]))
  #   
  #   countsdf2 <- table(plotdf2[[var1]], plotdf2[[var2]])
  #   
  #   countsdf <- countsdf / (countsdf + countsdf2) * 100
  #   
  # }else{
  #   countsdf <- countsdf / sum(countsdf) * 100
  # }
  
  #library(tidyverse)
  
  countsdf <- as.matrix(countsdf)
  
  dat2 <- countsdf %>% tbl_df()
  colnames(dat2) <- c(var1, var2, "n")
  
  pl <- NULL
  if(rotate){
    pl <- ggplot(dat2, aes(var2, var1)) +
      geom_tile(aes(fill = n)) +
      geom_text(aes(label = paste(round(n, 1)," %"))) +
      scale_fill_gradient(low = "white", high = "red")
  }else{
    pl <- ggplot(dat2, aes(var1, var2)) +
      geom_tile(aes(fill = n)) +
      geom_text(aes(label = n)) +
      scale_fill_gradient(low = "white", high = "red")
  }
  
  return(pl)
  
}