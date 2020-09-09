#Read all excel sheet from one document and transform into df that is in a list
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


#Next function defines the necessary parameters to the funtion above
data_reading_params = function(input) {
  #Path to commodities excel
  input$path_commodities <- "C:/Users/Hp/Desktop/Master Thesis/code rewamp/data/com.xlsx"
  #Peth to yield curve data
  input$path_yieldcurve = "C:/Users/Hp/Desktop/Master Thesis/code rewamp/data/yields.xlsx"
  input
}