simplifyRail <- 
  function(dg, descSearch = "Rail station",
           descNew = "Public transport; Rail station",
           threshold_distance = 1000, threshold_area = 1000,
           maxIterations = 100, rbind = TRUE) {

  source("R/functions_internal_postProcessing.R", local = TRUE)
  source("R/functions_internal_simplifyPoints.R", local = TRUE)

  output <-
    dg %>%
    filter(str_detect(desc, descSearch)) %>%

    simplifyPoints(desc = descNew,
                   distance = threshold_distance,
                   maxIterations = maxIterations) %>%

    function_thresholdRail(threshold = threshold_area) %>%

    mutate(type = st_geometry_type(geometry)) %>%
    select(desc, type, geometry)

  if(rbind == TRUE) {
    output <-
      list(dg %>%
             filter(!str_detect(desc, descSearch)),
           output) %>%
      .bind_rows_sf
  }

  return(output)

}
