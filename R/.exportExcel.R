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