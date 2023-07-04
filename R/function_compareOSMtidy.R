# Function to compare counts between a baseline and a scenario
compareOSMtidy <- function(baselineName,countOSMtidy_baseline,
                            scenarioName, countOSMtidy_scenario) {
  
  # Internal function to create data frame comparing the baseline and scenario 
  # by an individual classification
  internal_compare_OSMtidy_df <- 
    function(baselineName, countOSMtidy_baseline_element, 
             scenarioName, countOSMtidy_scenario_element, classification) {
      
      # Join baseline and scenario data and compare
      output_element <-
        countOSMtidy_baseline_element %>%
        full_join(countOSMtidy_scenario_element, by = classification) %>% # Join the baseline and scenario data
        mutate(n_prop = (n.x - n.y) / n.x, # Calculate proportion still functional ((baseline total - scenario affected) / baseline total)
               area_prop = (area.x - area.y) / area.x,
               length_prop = (length.x - length.y) / length.x) %>%
        mutate(length_prop = ifelse(length_prop < 0, 0, length_prop)) %>% # length_prop is tricky with flood extents, sometimes becoming larger than the baseline - round up negative values to 0 until we have a more sophisticated fix
        rename("n_{baselineName}" := n.x, 
               "area_{baselineName}" := area.x, 
               "length_{baselineName}" := length.x,
               "n_{scenarioName}" := n.y, 
               "area_{scenarioName}" := area.y, 
               "length_{scenarioName}" := length.y) %>%
        naniar::replace_with_na_all(condition = ~.x == 0) %>% # Replace 0 with NA across entire dataframe
        mutate_all(~ifelse(is.nan(.), NA, .)) %>% # Replace NaN (numeric) with NA across entire dataframe
        naniar::replace_with_na_all(condition = ~.x %in% naniar::common_na_strings) %>% # Replace NaN (and other similar character strings) with NA across entire dataframe
        rowwise() %>% # Across row
        mutate(total_prop = mean(c(n_prop, area_prop, length_prop), na.rm = TRUE)) %>% # Calculate mean of three columns ignoring all NA values
        ungroup() %>%
        mutate(total_prop = ifelse(total_prop < 0.01, 0, total_prop)) %>% # Round down for edge weights < 1% functionality remaining (indicating e.g. 1419/1420 m^2 of a building has been flooded)
        mutate_all(~ifelse(is.nan(.), NA, .)) %>% # Replace NaN (numeric) with NA across entire dataframe
        naniar::replace_with_na_all(condition = ~.x %in% naniar::common_na_strings) # Replace NaN (and other similar character strings) with NA across entire dataframe
      
      return(output_element)
      
    }
  
  # Internal function to specify which classification to compare by
  internal_compare_OSMtidy_by <- 
    function(baselineName, countOSMtidy_baseline = countOSMtidy_baseline, 
             scenarioName, countOSMtidy_scenario = countOSMtidy_scenario,
             classification = "desc") {
      
      if (classification == "category") {
        
        output <-
          internal_compare_OSMtidy_df(
            baselineName = baselineName,
            countOSMtidy_baseline_element = countOSMtidy_baseline$byCategory_all,
            scenarioName = scenarioName,
            countOSMtidy_scenario_element = countOSMtidy_scenario$byCategory_all,
            classification = "category")
        
      }
      
      else if (classification == "physicalObject") {
        
        output <-
          internal_compare_OSMtidy_df(
            baselineName = baselineName,
            countOSMtidy_baseline_element = countOSMtidy_baseline$byPhysicalObject_all,
            scenarioName = scenarioName,
            countOSMtidy_scenario_element = countOSMtidy_scenario$byPhysicalObject_all,
            classification = "physicalObject")
        
      }
      
      else if (classification == "desc") {
        
        output <-
          internal_compare_OSMtidy_df(
            baselineName = baselineName,
            countOSMtidy_baseline_element = countOSMtidy_baseline$byDesc_all,
            scenarioName = scenarioName,
            countOSMtidy_scenario_element = countOSMtidy_scenario$byDesc_all,
            classification = "desc")
        
      }
      
      return(output)
      
    }
  
  check <- "byPhysicalObject_all"
  
  if(any(names(countOSMtidy_baseline) %in% check) & 
     any(names(countOSMtidy_scenario) %in% check)) {
    
    compare_byCategory <-
      internal_compare_OSMtidy_by(
        baselineName = baselineName,
        countOSMtidy_baseline = countOSMtidy_baseline, 
        scenarioName = scenarioName,
        countOSMtidy_scenario = countOSMtidy_scenario,
        classification = "category")
    
    compare_byPhysicalObject <-
      internal_compare_OSMtidy_by(
        baselineName = baselineName,
        countOSMtidy_baseline = countOSMtidy_baseline, 
        scenarioName = scenarioName,
        countOSMtidy_scenario = countOSMtidy_scenario,
        classification = "physicalObject")
    
    compare_byDesc <-
      internal_compare_OSMtidy_by(
        baselineName = baselineName,
        countOSMtidy_baseline = countOSMtidy_baseline, 
        scenarioName = scenarioName,
        countOSMtidy_scenario = countOSMtidy_scenario,
        classification = "desc")
    
    output <- 
      list(compare_byCategory, compare_byPhysicalObject, compare_byDesc)
    
    names(output) <- 
      c("compare_byCategory", "compare_byPhysicalObject", "compare_byDesc")
    
  } else {
    
    output <-
      internal_compare_OSMtidy_by(
        baselineName = baselineName,
        countOSMtidy_baseline = countOSMtidy_baseline, 
        scenarioName = scenarioName, 
        countOSMtidy_scenario = countOSMtidy_scenario,
        classification = "desc")
    
  }
  
  class(output) <- "OSMtidy_countCompared"
  
  return(output)
  
}