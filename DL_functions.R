
DL_input = function(input){
  input[["DL"]] = list()
  input[["DL"]][["lambda"]] = 0.0609
  input[["DL"]][["matur"]] = c(3,6,seq(12,120,by=12),180,240,360)
  input[["DL"]][["three_factor"]] = T
  input
}


NS_reg = function (yt, beta2, beta3){
  res = lm(yt ~ beta2 + beta3)
  res[["coefficients"]]
  }



nelson_siegel_3 = function(rates,input_DL){
  lambda = input_DL$lambda
  maturity = input_DL$matur
  
  beta2 = (1-exp(-lambda*maturity))/(lambda*maturity)
  beta3 = (1-exp(-lambda*maturity))/(lambda*maturity)-exp(-lambda*maturity)

  df = apply(rates, 1, NS_reg, beta2 = beta2, beta3 = beta3)
}


iter_NS_3 = function(yield, input_DL){
  df = lapply(yield, nelson_siegel_3, input_DL = input_DL)
  df = lapply(df,t)
  df
}
