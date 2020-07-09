filterOverview <- function(filters = "filters.xlsx") {
  
  dt <- openxlsx::read.xlsx(filters) %>% as_tibble()
  dt <- 
    dt %>%
    mutate(type = ifelse(str_detect(descTerm, "remove|Remove"), "remove",
                         ifelse(str_detect(descTerm, "keyword|Keyword"), "keywords", "filter"))) %>%
    filter(!is.na(type)) %>%
    mutate(type = factor(type, levels = c("filter", "keywords", "remove")))
  
  output1 <- 
    dt %>%
    group_by(type, validate) %>%
    summarise(n = n()) %>%
    ungroup %>%
    mutate(validate = paste0("validate = ", validate)) %>%
    spread(validate, n) %>%
    mutate(total = ifelse(type != "keywords", `validate = FALSE` + `validate = TRUE`, `validate = TRUE`))
  
  output2 <- dt %>% group_by(type, name1, name2, descTerm) %>% summarise() %>% ungroup %>% split(., .$type) %>% modify(. %>% select(-type))
  
  output2$summary <- output1
  output <- output2[c("summary", "filter", "keywords")]
  
  class(output) <- c(class(output), "OSMtidy_filterOverview")
  return(output)
  
}