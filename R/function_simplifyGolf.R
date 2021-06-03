simplifyGolf <- function(dg,
                         descSearch = "golf", ## removed descSearch parentheses and descNew argument
                         maxIterations = 100,
                         rbind = TRUE) {

  source("functions/functions_internal_simplifyIntersects.R", local = TRUE)

  output <-
    dg %>%
    filter(str_detect(str_to_lower(desc), str_to_lower(descSearch))) %>%

    simplifyIntersects(maxIterations = maxIterations) %>% ## removed descNew argument

    mutate(type = st_geometry_type(geometry)) %>%
    select(desc, type, geometry)

  if(rbind == TRUE) {
    output <-
      list(dg %>%
             filter(!str_detect(str_to_lower(desc), str_to_lower(descSearch))),
           output) %>%
      .bind_rows_sf
  }

  return(output)

}
