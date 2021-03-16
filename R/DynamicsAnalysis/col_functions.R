#Between year comparison (within replicates)
#adapted from codyn

turnover_twoyears <- function(d1, d2, 
                              species.var, 
                              metric=c("total", "extinction","colonisation")){
  
  # allows partial argument matching
  metric = match.arg(metric)
  
  # create character vectors of unique species from each df
  d1spp <- as.character(unique(d1[[species.var]]))
  d2spp <- as.character(unique(d2[[species.var]]))
  
  # ID shared species
  commspp <- intersect(d1spp, d2spp)
  
  # count number not present in d2
  disappear <- length(d1spp)-length(commspp)
  
  # count number that appear in d2
  appear <- length(d2spp)-length(commspp)
  
  # calculate total richness
  totrich <- sum(disappear, appear, length(commspp))
  
  # output based on metric 
  if(metric == "total"){
    output <- ((appear+disappear)/totrich)
  } else {
    if(metric == "colonisation"){
      output <- appear/totrich
    } else {
      if(metric == "extinction"){
        output <- disappear/totrich
      }
    }
  }
  
  # results
  return(output)
}

turnover_allyears <- function(df, 
                              time.var, 
                              species.var, 
                              abundance.var, 
                              metric=c("total", "extinction","colonisation")) {
  
  # allows partial argument matching
  metric = match.arg(metric) 
  
  # check time and abundance are numeric
  check_numeric(df, time.var, abundance.var)
  
  # sort and remove 0s
  df <- df[order(df[[time.var]]),]
  df <- df[which(df[[abundance.var]]>0),]
  
  ## split data by year
  templist <- split(df, df[[time.var]])
  
  ## create consecutive pairs of time points
  t1 <- templist[-length(templist)]
  t2 <- templist[-1]
  
  ## calculate turnover for across all time points
  out <- Map(turnover_twoyears, t1, t2, species.var, metric)
  output <- as.data.frame(unlist(out))
  names(output)[1] = metric
  
  ## add time variable column
  alltemp <- unique(df[[time.var]])
  output[time.var] =  alltemp[2:length(alltemp)]
  
  # results
  return(output)
}

turnover <- function(df, time.var, 
                     species.var, 
                     abundance.var, 
                     replicate.var=NA, 
                     metric="total") {
  
  if(is.na(replicate.var)){
    
    # check there unique species x time combinations
    check_single_onerep(df, time.var, species.var)
    
    # calculate turnover
    output <- turnover_allyears(df, time.var, species.var, abundance.var, metric)
    
    
  } else {
    
    # remove unused levels if replicate.var is a factor
    df[replicate.var] <- if(is.factor(df[[replicate.var]])) {
      factor(df[[replicate.var]])
    } else {
      df[replicate.var]
    }
    
    # check unique species x time x replicate combinations
    check_single(df, time.var, species.var, replicate.var)
    
    # sort and apply turnover to all replicates
    df <- df[order(df[[replicate.var]]),]
    X <- split(df, df[replicate.var])
    out <- lapply(X, FUN=turnover_allyears, time.var, species.var, abundance.var, metric)
    ID <- unique(names(out))
    out <- mapply(function(x, y) "[<-"(x, replicate.var, value = y) ,
                  out, ID, SIMPLIFY = FALSE)
    output <- do.call("rbind", out)
  }
  
  # results
  row.names(output) <- NULL
  return(as.data.frame(output))
}