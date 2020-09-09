####Data Transformation Functions


#Convert data frames of a list into xts format
xts_converter <- function(data){
  data = lapply(data, function(x){xts(x[,-1], order.by = as.POSIXct(x[,1],format="%Y-%m-%d UTC"))})
}

# It can filter every n-th element of the data
filtering_df <- function(df, str_point, n){
  df.new = df[seq(str_point, nrow(df), n), ]
  df.new
}


