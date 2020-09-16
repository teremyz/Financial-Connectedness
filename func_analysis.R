TE_params = function(input){
  # Inputs for Transfer Entrophy: number of lags, type, shuffles, nboot, p-value
  
  input$TE <- list(lx = 1, ly = 1, q = 1, 
                   entropy="Shannon", shuffles = 25, nboot = 100, 
                   seed = NULL, na.rm = TRUE, p = 0.05, burn = 10,
                   var_filt = F, p_var = 1)
  input
}

LG_params = function(input){
  # Inputs for Transfer Entrophy: number of lags, type, shuffles, nboot, p-value
  
  input$granger_param = list(order=1, p=0.05,
                             var_filt = F, p_var = 1)
  input
}

my_merge <- function(df1, df2){
  # Create own merging function
  
  cbind(df1, df2)
}

calc_var = function(x, y, p){
  # It fits pairwise var model and gives back its residuals
  df = merge(x, y)
  res_df = resid(VAR(df, p = p))
  res_df
}


TE <- function(data, TE_param){
  # Calculate TE for all columns of a df
  # Giving back p-value of the test conducted to TE
  
  TE_results = matrix(1L, nrow = ncol(data), ncol = ncol(data))

  #VAR filtered residuals are analyzed
  if (TE_param$var_filt == TRUE){
    # Iterate through all columns in pairs
     for (i in 1:ncol(data)){
      # transfer_entropy gives the two-way connectedness by one interation
      # Not necessary to exmaine the the connections forth and back
      for (j in i:ncol(data)){
        # Columns are not analyzed with itselves
        if (i != j){
          #Filter the residuals
          df = calc_var(data[,i], data[,j], TE_param$p_var) 
          #Calculate transfer entropy on var-filtered residuals
          te = transfer_entropy(df[,1], df[,2], lx = TE_param[["lx"]], ly = TE_param[["ly"]], q = TE_param[["q"]],
                                entropy = TE_param[["entropy"]], shuffles = TE_param[["shuffles"]],
                                nboot = TE_param[["nboot"]], na.rm = T, quiet = T, burn=TE_param[['burn']])
          # Save the results
          TE_results[i,j] = te[["coef"]][1,4]
          TE_results[j,i] = te[["coef"]][2,4]  }}}
    # Raw data is analyzed
    } else {
      # Iterate through all columns in pairs
      for (i in 1:ncol(data)){
        # transfer_entropy gives the two-way connectedness by one interation
        # Not necessary to exmaine the the connections forth and back
        for (j in i:ncol(data)){
          # Columns are not analyzed with itselves
          if (i != j){          
          #Calculate transfer entropy on raw data
            te = transfer_entropy(data[,i], data[,j], lx = TE_param[["lx"]], ly = TE_param[["ly"]], q = TE_param[["q"]],
                                entropy = TE_param[["entropy"]], shuffles = TE_param[["shuffles"]],
                                nboot = TE_param[["nboot"]], na.rm = T, quiet = T, burn = TE_param[['burn']])
            # Save the results
            TE_results[i,j] = te[["coef"]][1,4]
            TE_results[j,i] = te[["coef"]][2,4]            
          }}
        }}
  # Rename the columns
  colnames(TE_results) = colnames(data)
  rownames(TE_results) = colnames(data)
  TE_results
}


LG <- function(data, granger_param){
  # Calculating Linear Granger Causality for all column of a df
  # Return is a matrix containing the p-values
  res =  matrix(1L, nrow = ncol(data), ncol = ncol(data))
  if (granger_param$var_filt == FALSE){
    for(i in 1:ncol(data)){
      for (j in i:ncol(data)){
        if (i != j){
          lg1 = grangertest(data[,i], data[,j], order = granger_param[["order"]])
          lg2 = grangertest(data[,j], data[,i], order = granger_param[["order"]])
          res[i,j] = lg1[2,4]
          res[j,i] = lg2[2,4]}}}
  } else {
    for(i in 1:ncol(data)){
      for (j in i:ncol(data)){
        if (i != j){
          df = calc_var(data[,i], data[,j], granger_param$p_var)
          lg1 = grangertest(df[,1], df[,2], order = granger_param[["order"]])
          lg2 = grangertest(df[,2],df[,1], order = granger_param[["order"]])
          res[i,j]=lg1[2,4]
          res[j,i]=lg2[2,4]}}}
    }
  
  colnames(res) = colnames(data)
  rownames(res) = colnames(data)
  res
}



complete_anal <- function(data, TE_param, granger_param, anal = list(LG = T, TE = T, filt_LG = T, filt_TE = T)){
  # This function call TE and LG functions
  # VAR filtered data va be examined optionally
  # anal parameter is a list with 4 elements named LG, TE, filt_LG, filt_TE
  # Giving back 4 identity matrix: TE and Granger caus. calculated from non-filterd and filtered data
  
  res = list()
  
  # Use LG function to get Linear Granger Causality on raw data
  if (anal$LG == T) {
    granger_param[['var_filt']] = F
    res$LG_mt = LG(data, granger_param)}
  
  # Use TE function to get transfer entropy on raw data
  if (anal$TE == T) {
    TE_param[['var_filt']] = F
    res$TE_mt = TE(data, TE_param)}
  
  # Use LG function to get Linear Granger Causality on filtered data
  if (anal$filt_LG == T) {
    granger_param[['var_filt']] = T
    res$filt_LG_mt = LG(data, granger_param)}
  
  # Use TE function to get transfer entropy on filtered data
  if (anal$filt_TE == T) {
    TE_param[['var_filt']] = T
    res$filt_TE_mt = TE(data, TE_param)}
  res
}

