
#Define variables for the analysis
desc_stat_par = function(input){
  input$acf = list(lags = 5, 
                   row_names =c( "GER0", "GER1","GER2","JPY0","JPY1","JPY2",
                                 "AUS0", "AUS1", "AUS2", "US0", "US1", "US2",
                                 "UK0", "UK1", "UK2", "FR0", "FR1", "FR2", "NOK0", "NOK01", "NOK2"))
  input
}


acf_val<-function(data,acf){
  #Giving back ACF for all columns of df-s in a list
  #Output:
  #Rows show column names
  #Columns show the lags of ACF
  
  df = data.frame(matrix(ncol = acf[["lags"]]+1, nrow = 0))
  
  for (i in 1:length(data)){
    for (j in 1:ncol(data[[i]])){
      acf_df=t(as.data.frame(acf(data[[i]][,j], lag.max=acf[["lags"]], plot = F)[["acf"]]))
      df = rbind(df,acf_df)
    }
  }
  rownames(df) = acf$row_names
  df
}




stationarity_test = function(df){
  res = list()
  res$pp = apply(df, 2, pp.test)
  res$kpss = apply(df, 2, kpss.test)
  res$adf = apply(df, 2, adf.test)
  res$pp = lapply(res$pp, function(x){x[['p.value']]})
  res$kpss = lapply(res$kpss, function(x){x[['p.value']]})
  res$adf = lapply(res$adf, function(x){x[['p.value']]})
  res
}
