compareOSMtidy <- function(baselineName,
                            countOSMtidy_baseline,
                            scenarioName,
                            countOSMtidy_scenario) {
  
  check <- "byPhysicalObject_all"
  
  if(any(names(countOSMtidy_baseline) %in% check) & 
     any(names(countOSMtidy_scenario) %in% check)) {
    
    compare_byCategory <-
      compareBy(baselineName = baselineName,
                countOSMtidy_baseline, 
                scenarioName = scenarioName,
                countOSMtidy_scenario,
                classification = "category")
    
    compare_byPhysicalObject <-
      compareBy(baselineName = baselineName,
                countOSMtidy_baseline, 
                scenarioName = scenarioName,
                countOSMtidy_scenario,
                classification = "physicalObject")
    
    compare_byDesc <-
      compareBy(baselineName = baselineName,
                countOSMtidy_baseline, 
                scenarioName = scenarioName,
                countOSMtidy_scenario,
                classification = "desc")
    
    output <- 
      list(compare_byCategory, compare_byPhysicalObject, compare_byDesc)
    
    names(output) <- 
      c("compare_byCategory", "compare_byPhysicalObject", "compare_byDesc")
    
  } else {
    
    output <-
      compareBy(baselineName = baselineName,
                countOSMtidy_baseline, 
                scenarioName = scenarioName, 
                countOSMtidy_scenario,
                classification = "desc")
    
  }
  
  return(output)
  
}


compareBy <- function(baselineName,
                      countOSMtidy_baseline, 
                      scenarioName,
                      countOSMtidy_scenario,
                      classification = "desc") {
  
  if (classification == "category") {
    
    output <-
      compareInternal(
        baselineName = baselineName,
        countOSMtidy_baseline_element = countOSMtidy_baseline$byCategory_all,
        scenarioName = scenarioName,
        countOSMtidy_scenario_element = countOSMtidy_scenario$byCategory_all,
        classification = "category")
    
  }
  
  else if (classification == "physicalObject") {
    
    output <-
      compareInternal(
        baselineName = baselineName,
        countOSMtidy_baseline_element = countOSMtidy_baseline$byPhysicalObject_all,
        scenarioName = scenarioName,
        countOSMtidy_scenario_element = countOSMtidy_scenario$byPhysicalObject_all,
        classification = "physicalObject")
    
  }
  
  else if (classification == "desc") {
    
    output <-
      compareInternal(
        baselineName = baselineName,
        countOSMtidy_baseline_element = countOSMtidy_baseline$byDesc_all,
        scenarioName = scenarioName,
        countOSMtidy_scenario_element = countOSMtidy_scenario$byDesc_all,
        classification = "desc")
    
  }
  
  return(output)
  
}


compareInternal <- function(baselineName,
                            countOSMtidy_baseline_element,
                            scenarioName,
                            countOSMtidy_scenario_element,
                            classification) {
  
  # Join baseline and scenario data and compare
  output_element <-
    countOSMtidy_baseline_element %>%
    full_join(countOSMtidy_scenario_element, by = classification) %>% # Join the baseline and scenario data
    mutate(n_prop = (n.x - n.y) / n.x, # Calculate proportion still functional ((baseline total - scenario affected) / baseline total)
           area_prop = (area.x - area.y) / area.x,
           length_prop = (length.x - length.y) / length.x) %>%
    rename("n_{baselineName}" := n.x, 
           "area_{baselineName}" := area.x, 
           "length_{baselineName}" := length.x,
           "n_{scenarioName}" := n.y, 
           "area_{scenarioName}" := area.y, 
           "length_{scenarioName}" := length.y) %>%
    mutate(across(everything(), ~na_if(., 0))) %>% # Replace 0 with NA across entire dataframe
    mutate(across(everything(), ~na_if(., "NaN"))) %>% # Replace NaN with NA across entire dataframe
    rowwise() %>% # Across row
    mutate(total_prop = mean(c(n_prop, area_prop, length_prop), na.rm = TRUE)) %>% # Calculate mean of three columns ignoring all NA values
    mutate(across(everything(), ~na_if(., "NaN"))) %>% # Replace NaN with NA across entire dataframe
    
    return(output_element)
  
}