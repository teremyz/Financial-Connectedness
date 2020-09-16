
check_and_diff = function(col, differ = 1, sign_level = 0.05){
  adf = adf.test(col)[['p.value']]
  if(adf > sign_level){
    col = as.vector(diff(zoo::zoo(col), na.pad=TRUE))
  }
  col[-1]
}


apply_diff = function(df, differ = 1, sign_level = 0.05){
  df = apply(df, 2, check_and_diff, differ = differ, sign_level = sign_level)
}













