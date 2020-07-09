dataWrangle <- function(dataCut) {
  
  require(mapedit)
  
  source("functions/functions_internal.R", local = TRUE)
  
  
  # -------------------------------------------------------------------------
  # STEP ONE - SETUP --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step one of three"))
  # -------------------------------------------------------------------------
  
  columnNames <- scan("functions/variables.txt", character(), quote = "", quiet = TRUE)
  
  nVec <- 1:length(dataCut) 
  
  save_noDetail <- list()
  outputTrimmed <- list()
  
  countThreshold <- 3 
  
  
  # -------------------------------------------------------------------------
  # STEP TWO - DATA WRANGLING -----------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step two of three"))
  # -------------------------------------------------------------------------
  
  pb <- progress_bar$new(total = length(nVec), width = 100, format = "|:bar| :percent elapsed =:elapsed, remaining ~:eta", clear = FALSE)
  
  for(i in nVec) { 
    
    tmp = dataCut[[i]] %>% as.data.table
    tmpNames = names(tmp)
    
    
    # Add column osm_id if missing
    check = sum(tmpNames %in% "osm_id")
    if(check == 0) { tmp[, osm_id := paste0("O_", i, "_", 1:.N)] } ###############
    if(check == 1) { tmp = tmp }
    
    
    # Select only columns of interest
    tmpNames = names(tmp)[names(tmp) %in% unique(c("osm_id", "feature", "geometry", columnNames))]
    tmp = tmp[, mget(tmpNames)]
    
    
    # Adjust tmpNames
    tmpNames = names(tmp)[!str_detect(names(tmp), "geometry")]
    tmpNames = tmpNames[!str_detect(tmpNames, "count")]
    
    
    # Replace blank cells or cells saying "yes" or "no" with NA
    tmp[ , (tmpNames) := lapply(.SD, function(x) { na_if(x, "") }), .SDcols = tmpNames]
    tmp[ , (tmpNames) := lapply(.SD, function(x) { na_if(x, "yes") }), .SDcols = tmpNames]
    tmp[ , (tmpNames) := lapply(.SD, function(x) { na_if(x, "no") }), .SDcols = tmpNames]
    
    
    # Remove entries where all values are NA
    tmp[, count := ncol(.SD) - rowSums(is.na(.SD)), .SDcols = c(tmpNames, "geometry")]
    save_noDetail[[i]] = 
      tmp[count <= countThreshold] %>% 
      select(contains("osm_id"), contains("geometry"), contains("feature")) %>%
      st_as_sf()
    tmp = tmp[count > countThreshold] %>% rmCols
    if(!tmp$count %>% is.null) { tmp[, count := NULL] }
    
    outputTrimmed[[i]] = tmp
    
    pb$tick()
    
  }
  
  
  # -------------------------------------------------------------------------
  # STEP THREE - TIDYING & DUPLICATES -----------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step three of three"))
  # -------------------------------------------------------------------------
  
  # Remove empty list items
  save_noDetail <- save_noDetail %>% rmNullList %>% rmEmptyList %>% mapedit:::combine_list_of_sf()
  outputTrimmed <- outputTrimmed %>% rmNullList %>% rmEmptyList
  
  # Remove duplicates, append the unique items to the list
  {
    
    dups <- 
      outputTrimmed %>%
      modify(. %>% as_tibble %>% select(osm_id)) %>%
      bind_rows() %>%
      filter(!str_detect(osm_id, "O_")) %>% 
      filter(duplicated(osm_id)) %>%
      unlist() %>%
      as.vector()
    
    
    dupsTreated <- 
      outputTrimmed %>%
      modify(. %>% 
               filter(osm_id %in% dups) %>%
               select(-feature) %>% 
               st_as_sf) %>%
      mapedit:::combine_list_of_sf() %>%
      unique() 
    
    
    outputTrimmed <- 
      outputTrimmed %>% 
      modify(. %>% filter(!osm_id %in% dups) %>% rmCols) %>%
      rmEmptyList %>%
      modify(. %>% st_as_sf) %>%
      append(list(dupsTreated))
    
  }
  
  
  # -------------------------------------------------------------------------
  # PREPARING OUTPUT --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Complete, preparing output"))
  # -------------------------------------------------------------------------
  
  output = list(dataWrangled = outputTrimmed, 
                noDetail = save_noDetail)
  
  class(output) <- c(class(output), "OSMtidy_dataWrangle")
  return(output)
  
}
