countOSMtidy <- function(dg) {

  count_category_type <- 
    dg %>% 
    countType(group = quo(category_mod)) %>%
    rename(category = category_mod)
  
  count_category_all <-
    count_category_type %>%
    select(-type) %>%
    group_by(category) %>% 
    summarise_each(funs(sum)) %>%
    ungroup
  
  
  count_physicalObject_type <-
    dg %>% countType(group = quo(physicalObject))
  
  count_physicalObject_all <-
    count_physicalObject_type %>%
    select(-type) %>%
    group_by(physicalObject) %>% 
    summarise_each(funs(sum)) %>%
    ungroup
  
  
  count_desc_type <-
    dg %>% countType(group = quo(desc))
  
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
  
  return(output)
  
}