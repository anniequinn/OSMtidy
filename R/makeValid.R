makeValid <- function(dg) {
  
  # https://github.com/r-spatial/sf/issues/1649
  # https://r-spatial.github.io/sf/articles/sf7.html
  # sf::sf_use_s2()
  sf::sf_use_s2(FALSE) # Until review for updated sf/s2 S^2 spherical space, revert to R^2 flat space

  output <-
    tryCatch(
      dg %>%
        st_as_sf(crs = 4326) %>%
        st_make_valid %>%
        mutate(type = st_geometry_type(geometry) %>% as.character) %>%
        select(desc = desc, type, geometry) %>%
        split(., .$type),

      error = function(e) {

        output <- dg %>% rowwise %>% mutate(valid = st_is_valid(geometry))
        output <-
          list(output %>%
                 filter(valid == FALSE) %>%
                 ungroup %>%
                 st_as_sf() %>%
                 st_make_valid,
               output %>%
                 filter(valid == TRUE) %>%
                 st_as_sf %>%
                 st_make_valid()) %>%
          .bind_rows_sf %>%
          st_as_sf() %>%
          st_make_valid() %>%
          mutate(type = st_geometry_type(geometry) %>% as.character) %>%
          select(desc = desc, type, geometry) %>%
          split(., .$type)

        return(output)

      })

  vec <- c("POINT", "LINESTRING", "POLYGON")

  output <-
    c(lapply(vec, function(x) {
      tryCatch(
        suppressWarnings(
          output[["GEOMETRYCOLLECTION"]] %>%
            st_make_valid %>%
            st_collection_extract(x) %>%
            st_cast(x, warn = FALSE)
        ),
        error = function(e) NULL)
    }),

    lapply(vec, function(x) {
      tryCatch(
        output[[paste0("MULTI", x)]] %>%
          st_make_valid %>%
          st_cast(x, warn = FALSE),
        error = function(e) NULL)
    }),

    lapply(vec, function(x) {
      tryCatch( output[[x]]
                %>% st_make_valid,
                error = function(e) NULL)
    })) %>%
    Filter(Negate(is.null), .) %>%
    .bind_rows_sf() %>%
    mutate(type = st_geometry_type(geometry))

  return(output)

}