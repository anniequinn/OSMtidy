dataFilter <- function(dataWrangle, filters = "filters.xlsx", rows = NULL) {
  
  require(purrr)
  
  source("functions/functions_internal.R", local = TRUE)
  
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Step one of one"))
  
  
  # FILTERS -----------------------------------------------------------------
  filter <- read_xlsx(filters) 
  if(!is.null(rows)) { filter <- filter %>% slice(rows) }
  filter <- filter %>% filter_all(any_vars(!is.na(.)))
  filtersBySearchTerm <- filter
  
  
  # APPLY FILTERS -----------------------------------------------------------
  dataInput <- dataWrangle$dataWrangled
  
  INPUT <- list()
  OUTPUT <- list()
  VALIDATE <- list()
  
  pb <- progress_bar$new(total = length(dataInput), width = 100, format = "|:bar| :percent elapsed =:elapsed, remaining ~:eta", clear = TRUE)
  
  
  # J LOOP
  for(j in 1:length(dataInput)) {
    
    outputList <- list()
    validateList <- list()
    
    input = dataInput[[j]] 
    
    
    # I LOOP
    for(i in 1:nrow(filtersBySearchTerm)) {
      
      
      # Rowwise terms from the filter spreadsheet
      searchTerm = filtersBySearchTerm[i,]$searchTerm %>% str_to_lower()
      descTerm = filtersBySearchTerm[i,]$descTerm
      searchColumn_contains = filtersBySearchTerm[i,]$searchColumn_contains
      validate = filtersBySearchTerm[i,]$validate
      if(is.na(validate)) { validate = FALSE } # Update validate
      
      
      # Column names to search
      if(!is.na(searchColumn_contains)) { colVec = names(input)[str_detect(names(input), searchColumn_contains)] }
      if(is.na(searchColumn_contains)) { colVec = names(input) }
      colNames = c("osm_id", "geometry", colVec) %>% unique
      DT <- input %>% select(all_of(colNames)) %>% as.data.table
      
      
      if(ncol(DT) > 2) { 
        
        # Filtered output
        cols = colNames[-which(colNames == "geometry")]
        
        output <- 
          DT[DT[, Reduce(`|`, lapply(.SD, function(x) { str_detect(x, searchTerm) })), .SDcols = cols]] %>% 
          rmCols %>%
          as_tibble() %>% 
          mutate(desc = descTerm)
        output
        
        if(nrow(output) == 0) { output <- NULL }
        
        
        # UPDATE INPUT
        # If no items filtered, then input remains the same
        if(is.null(output)) { input <- input }
        
        # If items filtered, then remove them from the input
        if(!is.null(output)) { input <- input %>% filter(!osm_id %in% output$osm_id) }
        
        
        # FOR LOOP OUTPUT
        if(validate == FALSE) { 
          outputList[[i]] = tryCatch(output %>% select(osm_id, geometry, desc), error = function(e) NULL)
        }
        
        if(validate == TRUE) { 
          validateList[[i]] = 
            tryCatch(
              output %>%
                select(osm_id, geometry, desc, everything()) %>%
                mutate(geometry = st_as_text(geometry),
                       filter = searchTerm) %>%
                as_tibble,
              error = function(e) NULL)
        }
        
      }
      
    } # END I LOOP
    
    validateList = validateList %>% rmNullList %>% rmEmptyList
    outputList = outputList %>% rmNullList %>% rmEmptyList %>% modify(. %>% st_as_sf) %>% mapedit:::combine_list_of_sf()
    
    INPUT[[j]] = input
    OUTPUT[[j]] = outputList
    VALIDATE[[j]] = validateList
    
    pb$tick()
    
  } # END J LOOP
  
  
  # -------------------------------------------------------------------------
  # PREPARING OUTPUT --------------------------------------------------------
  # -------------------------------------------------------------------------
  timeStamp <- paste0(Sys.time() %>% format("%H:%M:%S"), " ")
  message(paste0(timeStamp, "Complete, preparing output"))
  # -------------------------------------------------------------------------
  
  # Unfiltered - i.e. not captured by current filters
  INPUT <- INPUT %>% rmNullList() %>% rmEmptyList() %>% modify(. %>% as_tibble)
  
  # Filtered - i.e. captured by current filters
  OUTPUT %>% summary
  OUTPUT <- tryCatch(
    OUTPUT %>% 
      rmNullList() %>% 
      rmEmptyList() %>% 
      modify(. %>% st_as_sf) %>% 
      mapedit:::combine_list_of_sf() %>% 
      st_set_crs(4326) %>% 
      as_tibble(),
    error = function(e) NULL)
  
  # Filtered data requiring validation
  VALIDATE <- VALIDATE %>% purrr::flatten() %>% rmNullList %>% rmEmptyList() %>% modify(. %>% rmCols %>% as_tibble)
  
  outputFinal = list(unfiltered = INPUT, filtered = OUTPUT, validate = VALIDATE)
  class(outputFinal) <- c(class(outputFinal), "OSMtidy_dataFilter")
  return(outputFinal)
  
}