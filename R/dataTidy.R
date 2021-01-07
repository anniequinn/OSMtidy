dataTidy <- function(dataList) {

  require(purrr)

  outputList <- list()

  for(i in 1:length(dataList)) {

    input = dataList[[i]]

    if(sum(is.character(input)) == 1) {
      data <- .dataImport(input)
    } else {
      data <- input
    }

    if(!is.data.frame(data)) { dt <- data %>% .prepFiltered() } else { dt <- data }

    if(sum(names(dt) %in% "desc") == 0) { dt <- dt %>% mutate(desc = NA) }

    outputList[[i]] <-
      list(unfiltered = dt %>% filter(is.na(desc)) %>% .rmCols(),
           removeKeywordFilters = dt %>% filter(str_detect(desc, "Keyword|keyword")) %>% mutate(desc = "remove") %>% .rmCols(),
           remove = dt %>% filter(str_detect(desc, "Remove|remove")) %>% .rmCols(),
           filtered = dt %>% filter(!str_detect(desc, "Remove|remove|Keyword|keyword") & !is.na(desc)) %>% select(osm_id, desc, geometry)) %>%
      .rmEmptyList()

  }

  outputList <- outputList %>% flatten()

  output <-
    list(unfiltered =
           tryCatch({
             outputList[which(outputList %>% names == "unfiltered")] %>%
               modify(. %>% st_as_sf()) %>%
               .bind_rows_sf() %>%
               as_tibble() %>%
               .rmCols
           }, error = function(e) NULL),

         removeKeywordFilters =
           tryCatch({
             outputList[which(outputList %>% names == "removeKeywordFilters")] %>%
               modify(. %>% st_as_sf()) %>%
               .bind_rows_sf() %>%
               as_tibble() %>%
               .rmCols
           }, error = function(e) NULL),

         remove =
           tryCatch({
             outputList[which(outputList %>% names == "remove")] %>%
               modify(. %>% st_as_sf()) %>%
               .bind_rows_sf() %>%
               as_tibble() %>%
               .rmCols
           }, error = function(e) NULL),

         filtered =
           tryCatch({
             outputList[which(outputList %>% names == "filtered")] %>%
               modify(. %>% st_as_sf()) %>%
               .bind_rows_sf() %>%
               as_tibble() %>%
               .rmCols
           }, error = function(e) NULL)
    ) %>%
    .rmEmptyList() %>%
    .rmNullList()

  class(output) <- c(class(output), "OSMtidy_dataTidy")
  return(output)

}
