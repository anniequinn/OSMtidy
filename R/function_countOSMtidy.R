countOSMtidy <- function(dg, AHgen = FALSE) {
  
  # https://github.com/r-spatial/sf/issues/1649
  # https://r-spatial.github.io/sf/articles/sf7.html
  # sf::sf_use_s2()
  sf::sf_use_s2(FALSE) # Until review for updated sf/s2 S^2 spherical space, revert to R^2 flat space
  
  output <- list()
  
  if(AHgen == FALSE) {
    
    # By desc
    
    count_desc_type <-
      dg %>% 
      countType(group = quo(desc), AHgen = FALSE)
    
    count_desc_all <-
      count_desc_type %>%
      select(-type) %>%
      group_by(desc) %>% 
      summarise_each(funs(sum)) %>%
      ungroup
    
    output <- 
      list(count_desc_all, count_desc_type)
    
    names(output) <- 
      c("byDesc_all", "byDesc_byType")
    
  } else if(AHgen == TRUE) {
    
    # By category
    
    count_category_type <- 
      dg %>% 
      countType(group = quo(category_mod), AHgen = TRUE) %>%
      rename(category = category_mod)
    
    count_category_all <-
      count_category_type %>%
      select(-type) %>%
      group_by(category) %>% 
      summarise_each(funs(sum)) %>%
      ungroup
    
    
    # By Urban Systems Abstraction Hierarchy physical object
    
    count_physicalObject_type <-
      dg %>% countType(group = quo(physicalObject), AHgen = TRUE)
    
    count_physicalObject_all <-
      count_physicalObject_type %>%
      select(-type) %>%
      group_by(physicalObject) %>% 
      summarise_each(funs(sum)) %>%
      ungroup
    
    
    # By desc
    
    count_desc_type <-
      dg %>% countType(group = quo(desc), AHgen = TRUE)
    
    count_desc_all <-
      count_desc_type %>%
      select(-type) %>%
      group_by(desc) %>% 
      summarise_each(funs(sum)) %>%
      ungroup
    
    
    output <- 
      list(count_category_all, count_category_type, 
           count_physicalObject_all, count_physicalObject_type, 
           count_desc_all, count_desc_type)
    
    names(output) <- 
      c("byCategory_all", "byCategory_byType", 
        "byPhysicalObject_all", "byPhysicalObject_byType",
        "byDesc_all", "byDesc_byType")
    
  }
  
  class(output) <- "OSMtidy_count"
  
  return(output)
  
}