.bind_rows_sf <- function(dl, crs = 4326) {

  require(dplyr)
  require(sf)

  dt <-
    rbindlist(
      lapply(dl, function(x) {
        tmp <- select_(as.data.table(x, stringsAsFactors = FALSE),
                       paste0("-", attr(x, "sf_column", exact = TRUE)))
        if(nrow(tmp) == 0) {
          tmp <- data.table(tmpCol = rep(NA, x %>% nrow))
        }
        return(tmp)
      }),
      fill = TRUE)

  geo <- lapply(dl, function(x) { x %>% as_tibble %>% st_as_sf %>% st_geometry }) %>% unlist(recursive = FALSE) %>% st_sfc

  output <- st_sf(dt, geometry = geo, crs = st_crs(crs)) %>% .rmCols

  return(output)

}

.rmCols <- function(input) { input %>% Filter(function(x) !all(is.na(x)), .) }

.rmNullList <- function(input) { input %>% Filter(Negate(is.null), .) }

# .rmEmptyList <- function(input) { input %>% Filter(function(x) dim(x)[1] > 0, .) }

.rmEmptyList <- function(input) { input[ sapply(input, function(x) { !is.null(dim(x)[1]) }) ] }



# .simplifyList <- function(dataList) {
#
#   listNames <- sapply(dataList, function(x) x %>% names %>% paste(collapse = ""))
#
#   dataList <- dataList %>% setNames(listNames)
#
#   listNames <- listNames %>% unique
#
#   lapply(listNames, function(x) { dataList[x] %>% bind_rows })
#
# }

.create_unique_ids <- function(n, seed_no = 1, char_len = 5){
  set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)

  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  res
}

.dataImport <- function(vecExcel) {

  require(openxlsx)
  require(purrr)

  dl <- lapply(1:length(vecExcel), function(i) {

    lapply(vecExcel[[i]] %>% getSheetNames(), function(x) {

      read.xlsx(vecExcel[[i]], x) %>%
        as_tibble

    })

  }) %>%

    flatten()

  return(dl)

}

.prepFiltered <- function(data) {

  if(sum(sapply(data, function(x) { x$geometry %>% is.character })) > 0) {

    if(sum(names(data[[1]]) %in% "desc") == 1) {

      data <-
        data %>%
        modify(. %>% filter(!is.na(desc)))

    }

    data <-
      data %>%
      modify(. %>%
               rowwise %>%
               mutate(geometry = st_as_sfc(geometry)) %>%
               ungroup)

  }


  output <-
    data %>%
    .rmEmptyList() %>%
    modify(. %>% rowwise %>% st_as_sf %>% ungroup) %>%
    .bind_rows_sf %>%
    as_tibble() %>%
    .rmCols
  if(sum(names(output) %in% "desc") == 0) { output <- output %>% mutate(desc = "") }
  output <- output %>% mutate(desc = ifelse(desc == "", NA, desc))
  return(output)

}

.exportExcel <- function(tibbleList, filename) {

  require(openxlsx)

  wb <- openxlsx::createWorkbook()

  input <- tibbleList

  # sheetnames <- paste0("Sheet", seq_along(input))
  sheetnames <- names(input) %>% str_sub(1,20)
  lsn = length(sheetnames)
  snid = .create_unique_ids(lsn, char_len = 3)
  sheetnames <- paste0(1:lsn, "_", snid, "_", sheetnames)

  Map(function(data, nameofsheet){

    openxlsx::addWorksheet(wb, nameofsheet)
    openxlsx::writeDataTable(wb, nameofsheet, data, rowNames = FALSE)

  }, input, sheetnames)

  openxlsx::saveWorkbook(wb, file = filename)

}
