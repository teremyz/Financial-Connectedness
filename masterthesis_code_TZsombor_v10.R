library(readxl)
library(YieldCurve)
library(RTransferEntropy)
library(future)
library(glue)
#library(corrplot)
plan(multiprocess)
library(ggplot2)
library(xts)
library(qgraph)
library(lmtest)
#library(gsubfn)
#library(psych)
library(aTSA)
library(vars)
library(pracma)
library(tseries)
library(Hmisc)
library(dplyr)
library(tidyverse)
require(gridExtra)
library(grid)
library(MTS)

#####################################################################################
#######################  Creating Input and output variables  #######################

#creting output variables
output <- vector(mode="list", length=0)
output$dynamic_anal = list()

#creating input variables
input <- vector(mode="list", length=0)
#Path to commodities excel
input$path_commodities <- "C:\\Users\\Hp\\Desktop\\Master Thesis\\data\\com.xlsx"
#Peth to yield curve data
input$path_yieldcurve = "C:\\Users\\Hp\\Desktop\\Master Thesis\\data\\yields.xlsx"

#Maturuities of term structure
input$DL_maturity <- c(3,6,seq(12,120,by=12),180,240,360)
# Inputs for Transfer Entrophy: number of lags, type, shuffles, nboot, p-value
input$TE <- list(lx=1, ly=1, q=1, entropy="Shannon",shuffles=25,nboot=100,seed=NULL, na.rm=TRUE, p=0.05, burn=10)
# Lambda for Diebold-Lee factors. It is a given value based on Diebold-Li (2006)
input$lambda = 0.0609
#Input for linear granger causality test: number of lags, p-value, names of variabes
input$granger_param = list(order=1,p=0.05, row_names=c("crude", "gas", "gold", "silver", "al", "cop", "corn", "weat",
                                                       "GER0", "GER1","GER2","JPY0","JPY1","JPY2",
                                                       "AUS0", "AUS1", "AUS2", "US0", "US1", "US2",
                                                       "UK0", "UK1", "UK2", "FR0", "FR1", "FR2", "NOK0", "NOK01", "NOK2"))
# Number of lags for ACF
input$acf = list(lags = 5)
# Input to split_ts function
input$split_dates = list(start0="2000-01-10", end0="2007-06-30",
                         start1="2007-07-01", end1="2009-07-31",
                         start2="2009-08-01", end2="2020-03-02")




##############################################################################################################
#####################################   Functions   ######################################################

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

#Convert data frames of a list into xts file
xts_converter <- function(data){
  for (i in 1:length(data)){
    data[[i]]=xts(data[[i]][,-1], order.by = as.POSIXct(data[[i]][,1],format="%Y-%m-%d UTC"))
  }
  data
}

# It can filter every n-th element of the data
filtering_df <- function(df, str_point, n){
  df.new = df[seq(str_point, nrow(df), n), ]
  df.new
}

#It iterates the latest function on an elements of a list that contains data frames
iterate_filt <- function(df_list, str_point, n){
  for (i in 1:length(df_list)){
    df_list[[i]] = filtering_df(df_list[[i]],  str_point, n)
  }
  df_list
}

#Nelson Siegel Decomposition of term structure to 3 factors (level, slope, curvature)
nelson_siegel_3 <- function(rates,maturity,lambda){
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  beta2 = (1-exp(-lambda*maturity))/(lambda*maturity)
  beta3 = (1-exp(-lambda*maturity))/(lambda*maturity)-exp(-lambda*maturity)
  for (i in 1:nrow(rates)){
    yt = rates[i,]
    linear <- lm(t(yt) ~ beta2 + beta3)
    df[i,] = linear[["coefficients"]]
    
    }
  df$dates = as.POSIXct(index(rates),format="%Y-%m-%d UTC")
  df = xts(df[,1:3],order.by = df$dates)
  colnames(df) <- c("beta 1", "beta 2", "beta 3")
  df
}

#Nelson Siegel Decomposition of term structure to 3 factors (level, slope)
nelson_siegel_2 <- function(rates,maturity,lambda){
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  beta2 = (1-exp(-lambda*maturity))/(lambda*maturity)
  for (i in 1:nrow(rates)){
    yt = rates[i,]
    linear <- lm(t(yt) ~ beta2)
    df[i,] = linear[["coefficients"]]
  }
  df$dates = as.POSIXct(index(rates),format="%Y-%m-%d UTC")
  df = xts(df[,1:2],order.by = df$dates)
  colnames(df) <- c("beta 1", "beta 2")
  df
}

#Iterates NS functions through a list
#You can make a decision about 2 or 3 factors decomposition with the optional parmeter
iterate_NS<-function(rates,maturity,lambda,two_param=F){
  list_df=list()
  for (i in 1:length(rates)){
    if (two_param){
      df = nelson_siegel_2(rates[[i]],maturity,lambda)
      }else{
      df = nelson_siegel_3(rates[[i]],maturity,lambda)
    }
    list_df[[i]]=df
  }
  names(list_df) = c(names(rates))
  list_df
  }

###################################################################################
###############################   Descriptive statistics  ##########################

#Giving back a dataframe about the most important descriptive statistics
desc_stat<-function(data){
  df2 <- data.frame(matrix(ncol = 16, nrow = 0))
  for (i in 1:length(data)){
    df2=rbind(df2,t(PerformanceAnalytics::table.Stats(output[["DL_coeff"]][[i]])))
  }
  df2
}


#Giving back acf for all df in a list
acf_val<-function(data,acf){
  df = data.frame(matrix(ncol = acf[["lags"]]+1, nrow = 0))
  for (i in 1:length(data)){
    for (j in 1:ncol(data[[i]])){
      acf_df=t(as.data.frame(acf(data[[i]][,j], lag.max=acf[["lags"]], plot = F)[["acf"]]))
      df = rbind(df,acf_df)
    }
  }
  df
}



# Making 3 stationarity tests for lists of univariate time series: ADF, KPSS, PP
# Return is equal to the p values of the tests
# number of lags is equal to 5
stationarity_tests<-function(data){
  df = data.frame(matrix(ncol=3,nrow=0))
  colnames(df)=c("ADF","KPSS","PP")
  for (i in 1:length(data)){
    adf=adf.test(data[[i]])[['type1']][[5,3]]
    kpss=kpss.test(data[[i]])[1,2]
    pp=pp.test(data[[i]])[1,3]
    app_df = as.data.frame(list(adf,kpss,pp))
    colnames(app_df)=c("ADF","KPSS","PP")
    df = rbind(df,app_df)
  }
  colnames(df)=c("ADF","KPSS","PP")
  df
}


# Take the first difference of all time series in a list
# You can pick the order of diff
time_diff<-function(df_list, differences=1){
  for (i in 1:length(df_list)){
    df_list[[i]] = diff(df_list[[i]], differences=differences)[-1,]
  }
  df_list
}

#Merging lists cosisting of data frames
merger<-function(commodities, DL_coeff){
  merged = commodities[[1]]
  for (i in 2:length(commodities)){
    merged=merge(merged,commodities[[i]], join="inner")
  }
  for (j in 1:length(DL_coeff)){
    merged=merge(merged,DL_coeff[[j]], join="inner")
  }
  merged
}

################################   Analysis   ######################

#Calculate TE for all time series of a df
#Giving back a matrix, One element is equal to 1 if the p-value is lass than a given value
#If it is greater than the cell is equal to 0
TE<- function(data, TE_param){
  TE_results = matrix(0L, nrow = ncol(data), ncol = ncol(data))
  for (i in 1:ncol(data)){
    for (j in i:ncol(data)){
      if (i != j){  
      te = transfer_entropy(data[,i], data[,j], lx=TE_param[["lx"]], ly=TE_param[["ly"]], q=TE_param[["q"]],
                              entropy = TE_param[["entropy"]], shuffles=TE_param[["shuffles"]], nboot=TE_param[["nboot"]],na.rm=T,quiet=T,burn=TE_param[['burn']])
      TE_results[i,j] = te[["coef"]][1,4]
      TE_results[j,i] = te[["coef"]][2,4]
        }
      }
    }
  colnames(TE_results)=colnames(data)
  rownames(TE_results)=colnames(data)
  TE_results
}

#Calculating Linear Granger Causality for all column of a df
#Return is a matrix containing of 0 and 1
#It is equal to 1 if p value is lass than a given value, otherwise the element is 0
LG<-function(data, granger_param){
  res =  matrix(0L, nrow = ncol(data), ncol = ncol(data))
  for(i in 1:ncol(data)){
    for (j in 1:ncol(data)){
      if (i!=j){
        lg1=grangertest(data[,i], data[,j], order = granger_param[["order"]])
        lg2=grangertest(data[,j],data[,i], order = granger_param[["order"]])
        res[i,j]=lg1[2,4]
        res[j,i]=lg2[2,4]
      }
    }
  }
  colnames(res)=granger_param[["row_names"]]
  rownames(res)=granger_param[["row_names"]]
  res
}


# Splitting the time series into two parts
# The start and end date of splitting can be set
split_ts=function(df_list, split_dates){
  new_df = list(first=list(), crisis=list(),third=list())
  for (i in 1:length(df_list)){
   new_df[["first"]][[i]]=window(df_list[[i]], start = as.Date(split_dates[["start0"]]), end = as.Date(split_dates[["end0"]]))
   new_df[["crisis"]][[i]]=window(df_list[[i]], start = as.Date(split_dates[["start1"]]), end = as.Date(split_dates[["end1"]]))
   new_df[["third"]][[i]]=window(df_list[[i]], start = as.Date(split_dates[["start2"]]), end = as.Date(split_dates[["end2"]]))
    }
  new_df
}

#it fits pairwise var model and gives back its residuals
calc_var = function(x,y,p){
  df = merge(x,y)
  var.model = VAR(df, p = p, type = "none")
  res_df=resid(var.model)
  res_df
}

#Running transfer entropy on var filtered residuals
var_TE=function(data,TE_param,p_var){
  TE_results = matrix(0L, nrow = ncol(data), ncol = ncol(data))
  for (i in 1:ncol(data)){
    for (j in i:ncol(data)){
      if (i != j){
        df = calc_var(data[,i], data[,j],p_var)
        te = transfer_entropy(df[,1], df[,2], lx=TE_param[["lx"]], ly=TE_param[["ly"]], q=TE_param[["q"]],
                              entropy = TE_param[["entropy"]], shuffles=TE_param[["shuffles"]], nboot=TE_param[["nboot"]],na.rm=T,quiet=T,burn=TE_param[['burn']])
        TE_results[i,j] = te[["coef"]][1,4]
        TE_results[j,i] = te[["coef"]][2,4]
      }      }
  }
  TE_results
}

#Calculating Granger causality for all variable
var_LG = function(data, granger_param,p_var){
  res =  matrix(0L, nrow = ncol(data), ncol = ncol(data))
  for(i in 1:ncol(data)){
    for (j in 1:ncol(data)){
      if (i!=j){
        df = calc_var(data[,i], data[,j],p_var)
        lg1=grangertest(df[,1], df[,2], order = granger_param[["order"]])
        lg2=grangertest(df[,2],df[,1], order = granger_param[["order"]])
        res[i,j]=lg1[2,4]
        res[j,i]=lg2[2,4]
      }
    }
  }
  colnames(res)=granger_param[["row_names"]]
  rownames(res)=granger_param[["row_names"]]
  res
}

#This function call TE and LG functions
# Giving back 4 identity matrix: TE calculated from non-filterd and filtered data
#Linear Granger Causalty calc. from non-filtered and filtered data
complete_anal<-function(data1,data2, TE_param, granger_param, transfer = T){
  merged = na.remove(merger(data1, data2))
  if (transfer){
    TE_mt=TE(merged,TE_param)
    filt_TE = var_TE(merged,TE_param,TE_param[["lx"]])
    filt_LG = var_LG(merged,granger_param,TE_param[["lx"]])
    LG_mt=LG(merged,granger_param)
    res = list(TE=TE_mt,LG=LG_mt,f_TE = filt_TE, f_LG = filt_LG)
    res
  }  
  else{
    filt_LG = var_LG(merged,granger_param,TE_param[["lx"]])
    LG_mt=LG(merged,granger_param)
    res = list(LG=LG_mt, f_LG = filt_LG)
    res
    
  }
}

#USing rolling window tecnique to carry out the dynamic analysis
#There are 4 groups in my database: commodities, level, slope and curvature for different countries
#It can calculate the number of links inside the groups
dynamic_anal_inside =function(data1, window, TE_param){
  res = data.frame(matrix(nrow=0,ncol=1))
  for (i in 1:(nrow(data1)-window)){
    te = TE(data1[i:(window+i),],TE_param)
    res[i,1] = sum(te)
  }
  res
}


#It creates dataframe consisting of individual factors for different countries
#The input is a df list containing the 3 factors for different countries
#The outpu is a df consisting one factor for different countries
#With col number you can specify the deserved factor (1-level, 2-slope, 3-curvature)
beta_pairing = function(list_df,col_numb){
  df =list_df[[1]][,col_numb]
  for (i in 2:length(list_df)){
    df = merge(df, list_df[[i]][,col_numb])
  }
  df
}



#It creates a graph with the 4 different groups
main_graph<-function(qg){
  plot_g = qgraph(qg[["con"]], groups = qg[["groups"]], layout = "groups", layoutScale = c(1, 1), title=qg[["title"]],
                  label.font = 2, label.cex = 2, shape = "circle", labels = qg[["names.fi"]], esize = 5, 
                  maximum = max(qg[["con"]]), color = c(rep("white",5)), node.width = 1, label.cex = 1, label.color = qg[["col"]], 
                  edge.color =  c(rep("red",1),rep("blue",4)), curve = 1, border.width = 1.2, border.color = qg[["col"]], asize = 2.5)
  text(x = 0.9, y = 1.15, labels = substitute(paste(d), list(d = as.character(qg[["dt"]][1]))), xpd = NA, cex = 1)
  legend(-0.7, 0.5, legend =qg[["legend"]], cex=0.5, box.col = "white", bg = "transparent",text.col =qg[["text_col"]],horiz=T )
}


# Making 3 stationarity tests for lists of univariate time series: ADF, KPSS, PP
# Return is equal to the p values of the tests
# number of lags is equal to 5
stationarity_tests<-function(data){
  df = data.frame(matrix(ncol=3,nrow=0))
  colnames(df)=c("ADF","KPSS","PP")
  for (i in 1:length(data)){
    adf=adf.test(data[[i]])[['p.value']]                #hány laggot érdemes betenni?
    kpss=kpss.test(data[[i]])[['p.value']] 
    pp=pp.test(data[[i]])[['p.value']] 
    app_df = as.data.frame(list(adf,kpss,pp))
    colnames(app_df)=c("ADF","KPSS","PP")
    df = rbind(df,app_df)
  }
  colnames(df)=c("ADF","KPSS","PP")
  df
}

#It checks all yield curve factors wheter each is stationary
#If previous statement is not true then the function takes the difference of ts
#It gives back a df list with stationary ts
factor_diff<-function(df_list,differences = 1){
  for (i in 1:length(df_list)){
    for (j in 1:3){ 
      adf_p =adf.test(df_list[[i]][complete.cases(df_list[[i]]),j])[['p.value']]
      if (adf_p>0.05){
        df_list[[i]][,j] = diff(df_list[[i]][,j], differences=differences)
      }
      adf_p =adf.test(df_list[[i]][complete.cases(df_list[[i]]),j])[['p.value']]
      print(adf_p)
    }
    
  }
  df_list
}





# It can filter every n-th element of the data
filtering_df <- function(list_df, str_point, n){
  for ( i in 1:length(list_df)){
    temp_df = list_df[[i]]
    list_df[[i]] = temp_df[seq(from = str_point, to = nrow(list_df[[i]]), by = n), ]
  }
  list_df
}


line_chart= function(line, beta_numb,title){
  df = data.frame(date=index(beta_pairing(line$data,beta_numb)), coredata(beta_pairing(line$data,beta_numb)))
  n_df =df  %>% gather(key = "countries", value = "value", -date)
  n_df$date = as.Date(n_df$date,tz='UTC')
  fig = ggplot(n_df, aes(x = date, y = value)) + 
    geom_line(aes(color = countries, linetype = countries),size=1) + 
    scale_color_manual(name='Countries', values = line$color, labels =line$country_names)+
    scale_linetype_manual(name='Countries',values =  line$type, labels = line$country_names)+
    xlab(line$xlab)+ylab(line$ylab)+
    ggtitle(title)+
    scale_x_date(limits = c(as.Date(line$s_date),as.Date(line$e_date)),expand = c(0,100))+theme_minimal()
  fig
}



#It calculates the pairwise connectedness using TE on VAR-filtered data
pairwise_TE=function(data1,data2,TE_param){
  TE_results = matrix(1L, nrow = ncol(data1)+ncol(data2), ncol = ncol(data1)+ncol(data2))
  for (i in 1:ncol(data1)){
    for(j in 1:ncol(data2)){
      df = calc_var(data1[,i], data2[,j],TE_param[["lx"]])
      te = transfer_entropy(df[,1], df[,2], lx=TE_param[["lx"]], ly=TE_param[["ly"]], q=TE_param[["q"]],
                            entropy = TE_param[["entropy"]], shuffles=TE_param[["shuffles"]], nboot=TE_param[["nboot"]],na.rm=T,quiet=T,burn=TE_param[['burn']])
      TE_results[i,(j+ncol(data1))] = te[["coef"]][1,4]
      TE_results[(j+ncol(data1)),i] = te[["coef"]][2,4]
      
    }
  }
  TE_results 
}

#Rolling window technique for dynamic analysis with TE
#Yielding a significance matrix for all period
dynamic_pairwise = function(data1,data2,window,TE_param){
  res = list()
  counter = 1
  for (i in seq(1,(nrow(data1)-window),4)){
    mt = pairwise_TE(data1[i:(window+i-1),],data2[i:(window+i-1),],TE_param)
    res[[counter]] = mt
    counter=counter+1
  }
  res
}


#Transforming matrix containing significance level to 0 and 1 based on given treshold level
matrix_transformer = function(mt,treshold){
  y = function(x) if (x < treshold) 1 else 0
  mt <- structure(vapply(mt, y, numeric(1)), dim=dim(mt))
  mt
}


dynamic_ts = function(mt_list,treshold, dynamic){
  df = data.frame(ncol=3)
  for (i in 1:length(mt_list)){
    ts = sum(matrix_transformer(mt_list[[i]],treshold))
    df[i,1] = ts
    df[i,2] = ts/dynamic$poss
    df[i,3] = ts/dynamic$all
  }
  names(df)=c('sum','poss','all')
  df$date = dynamic$date[seq(2+dynamic$window,length(dynamic$date),dynamic$step)]
  df = data.frame(df)
}

#Creating line chart 
commodity_line = function(df_list,line){
  df = data.frame(matrix(nrow=1053,ncol = 8))
  for (i in 1:length(df_list)){
    df[,i] = coredata(df_list[[i]])
    df[,i] = (df[,i] - mean(df[,i]))/sd(df[,i])
  }
  df$date = index(df_list[[1]])
  df$date = as.Date(df$date,tz='UTC')
  df = df %>% gather(key = "countries", value = "value", 1:8)
  fig = ggplot(df, aes(x = date, y = value)) + 
    geom_line(aes(color = countries, linetype = countries), size = 1) + 
    scale_color_manual(name='Commodities', values = line$color, labels =line$country_names)+
    scale_linetype_manual(name='Commodities',values =  line$type, labels = line$country_names)+
    xlab('Date')+ylab('Price')+
    ggtitle('Commodity Prices in USD')+theme_minimal()+scale_x_date(expand = c(0,100))
  fig
}

#Producing dynamic linear granger causality
LG_dynamic_pairwise = function(data1,data2,window,granger_param){
  res = list()
  counter = 1
  for (i in seq(1,(nrow(data1)-window),4)){
    mt = pairwise_LG(data1[i:(window+i-1),],data2[i:(window+i-1),],granger_param)
    res[[counter]] = mt
    counter=counter+1
  }
  res
}

#Producing static linear granger causality between groups
pairwise_LG=function(data1,data2,granger_param){
  LG_results = matrix(1L, nrow = ncol(data1)+ncol(data2), ncol = ncol(data1)+ncol(data2))
  for (i in 1:ncol(data1)){
    for(j in 1:ncol(data2)){
      lg1=grangertest(data1[,i], data2[,j], order = granger_param[["order"]])
      lg2=grangertest(data2[,j],data1[,i], order = granger_param[["order"]])
      LG_results[i,j+ncol(data1)]=lg1[2,4]
      LG_results[j+ncol(data1),i]=lg2[2,4]
      
    }
  }
  LG_results 
}

#Producing static linear granger causality within groups
within_pairwise_LG=function(data1,data2,granger_param){
  LG_results = matrix(1L, nrow = ncol(data1), ncol = ncol(data1))
  for (i in 1:ncol(data1)){
    for(j in 1:ncol(data2)){
      if (i !=j){
        lg1=grangertest(data1[,i], data2[,j], order = granger_param[["order"]])
        lg2=grangertest(data2[,j],data1[,i], order = granger_param[["order"]])
        LG_results[i,j]=lg1[2,4]
        LG_results[j,i]=lg2[2,4]
      }
    }
  }
  LG_results 
}

#Producing dynamic linear granger causality within groups
LG_within_dynamic_pairwise = function(data1,data2,window,granger_param){
  res = list()
  counter = 1
  for (i in seq(1,(nrow(data1)-window),4)){
    mt = within_pairwise_LG(data1[i:(window+i-1),],data2[i:(window+i-1),],granger_param)
    res[[counter]] = mt
    counter=counter+1
  }
  res
}


############################################################################################################################
############################################################################################################################
###############################   Running the Code   ######################################################################



#Read the data and convert to xts format
output$raw_commodities<- xts_converter(read_excel_allsheets(input$path_commodities))
output$raw_yield <- xts_converter(read_excel_allsheets(input$path_yieldcurve))

#proba = do.call(rbind, lapply(split(output[['raw_commodities']][[1]], "weeks"), first))

#Filtering to get every Monday
output$raw_commodities<-filtering_df(output$raw_commodities,5,5)
output$raw_yield<-filtering_df(output$raw_yield,5,5)


#Input for commodity line chart
input$comm_line = list(color = c('blue','aquamarine3','brown4','blueviolet','cyan3','coral','black','goldenrod'),
                       type = rep(c('twodash','solid', 'longdash','dashed'),2),
                       country_names = c("crude", "gas", "gold", "silver", "al", "cop", "corn", "weat"),
                       expand=100,  s_date="2000-01-03", e_date="2020-03-02",
                       xlab='Date',ylab='Price',title = 'Commodity Prices')

commodity_line(output$raw_commodities,input$comm_line)



#It transforms yield curves to the DL factores
#2 factors decomposition can be picked
output$DL_coeff =iterate_NS(output[["raw_yield"]],input$DL_maturity, input$lambda,two_param=F)


#Descriptive statistics about DL coefficients
output$desc_stat<-desc_stat(output[["DL_coeff"]]) #Sorok nevei!



#Input for factor visualization
input$line = list(country_names = c("GER","JPY","AUS","US","UK","FR","NOK"),
                  color = c("darkgreen", "goldenrod","darkblue","darkviolet","black","coral","orange4"),
                  type = c(rep(c('twodash','solid', 'longdash'),2),'dashed','solid'),
                  data = output[["DL_coeff"]], expand = 100,
                  s_date="2000-01-03", e_date="2020-03-02",
                  xlab='Date',ylab='Value')



grid.newpage()
fig1 = line_chart(input$line, 1,'Level Factor')
fig2 = line_chart(input$line, 2,'Slope Factor')
fig3 = line_chart(input$line, 3,'Curvature Factor')
grid.text("MAIN TITLE",just=c("centre", "centre"),x = 0.5,y=0.95)
grid.arrange(fig1, fig2,fig3, nrow=3, top = textGrob("Factors for Countries from 2000-01-03 to 2020-03-02",gp=gpar(fontsize=16,font=3)))

rm(fig1,fig2,fig3)




#Getting the ACF of all time series about Diebold-Lee factors
#The number of columns are equal to the desierd lag number
#The number of rows are equal to the 3 betas times the number of countries
output$acf = acf_val(output[["DL_coeff"]],input$acf)



#It tests time series of factors to stationarity by PP, KPSS, ADF test
factor_stat_test<-function(df_list){
  df = data.frame(matrix(ncol=21,nrow = 3))
  c= 1
  for (i in 1:length(df_list)){
    for (j in 1:3){ 
      df[1,c] = pp.test(df_list[[i]][complete.cases(df_list[[i]]),j])[['p.value']] 
      df[2,c] = kpss.test(df_list[[i]][complete.cases(df_list[[i]]),j])[['p.value']] 
      df[3,c] =adf.test(df_list[[i]][complete.cases(df_list[[i]]),j])[['p.value']]
      c=1+c
    }
    
  }
  df
}


output[['DL_stat_test']][['before']] = factor_stat_test(output[["DL_coeff"]])


#It converts factors to statinary ts if they are not stationary based on ADF
output[["DL_coeff"]]=factor_diff(output$DL_coeff)


output[['DL_stat_test']][['after']] = factor_stat_test(output[["DL_coeff"]])


#Renaming columns
names(output[['DL_stat_test']][['before']]) = c("GER0", "GER1","GER2","JPY0","JPY1","JPY2",
                                                    "AUS0", "AUS1", "AUS2", "US0", "US1", "US2",
                                                    "UK0", "UK1", "UK2", "FR0", "FR1", "FR2", "NOK0", "NOK01", "NOK2")

names(output[['DL_stat_test']][['after']]) = c("GER0", "GER1","GER2","JPY0","JPY1","JPY2",
                                                "AUS0", "AUS1", "AUS2", "US0", "US1", "US2",
                                                "UK0", "UK1", "UK2", "FR0", "FR1", "FR2", "NOK0", "NOK01", "NOK2")



#Stationarity test to ts of commodities
output$stationarity=stationarity_tests(output[["raw_commodities"]])


#Taking the diff of time series in comodity list
output[["raw_commodities"]] = time_diff(output[["raw_commodities"]])


#Test stationarity again
output$diff_stationarity=stationarity_tests(output[["raw_commodities"]])


#These split into two part commodity and DL coefficient matrix
output$splitted_com=split_ts(output[["raw_commodities"]],input$split_dates)
output$splitted_DL=split_ts(output[["DL_coeff"]],input$split_dates)


#Set working directory
setwd('C:/Users/Hp/Desktop/Master Thesis/results/')

#Set multiprocessing again
plan(multiprocess)


#Analysis for the whole period
output$res = complete_anal(output[["raw_commodities"]],output[["DL_coeff"]], input$TE, input$granger_param,transfer = F)
#Saving the results
#saveRDS(output[['res']], file = "static_whole_period.Rds")
output$res <- readRDS("static_whole_period.Rds")

output$res2 = complete_anal(output[["raw_commodities"]],output[["DL_coeff"]][c(1,3)], input$TE, input$granger_param,transfer = F)

pr = na.remove(output[["DL_coeff"]][c(1,3)])

lg1=grangertest(output[["raw_commodities"]][["gold"]], output[["raw_commodities"]][["crude"]], order = 1)
lg2=grangertest(output[["raw_commodities"]][["crude"]],output[["raw_commodities"]][["gold"]], order = 1)
lg1[2,4]
lg2[2,4]

X=merge(output[["raw_commodities"]][["gold"]],output[["raw_commodities"]][["crude"]])
GrangerTest(X,p=1)


# Analysis for the FIRST period
#output[["split_res"]][["first"]] = complete_anal(output[["splitted_com"]][[1]],output[["splitted_DL"]][[1]], input$TE, input$granger_param,transfer = F)
#Saving the results
#saveRDS(output[["split_res"]][["first"]], file = "static_first_period.Rds")
output[["split_res"]][["first"]] <- readRDS("static_first_period.Rds")


# Analysis for the SECOND (crisis) period
#output[["split_res"]][["crisis"]] = complete_anal(output[["splitted_com"]][[2]],output[["splitted_DL"]][[2]], input$TE, input$granger_param,transfer = F)
#Saving the results
output[["split_res"]][["crisis"]] <- readRDS("static_crisis_period.Rds")

#saveRDS(output[["split_res"]][["crisis"]], file = "static_crisis_period.Rds")


# Analysis for the THIRD period
#output[["split_res"]][["third"]] = complete_anal(output[["splitted_com"]][[3]],output[["splitted_DL"]][[3]], input$TE, input$granger_param,transfer = F)
#Saving the results
#saveRDS(output[["split_res"]][["third"]], file = "static_third_period.Rds")
output[["split_res"]][["third"]] <- readRDS("static_third_period.Rds")


##########################################################################################
###################################    Q-GRAPH    ########################################

#Input for graph visualization:
#connectedness matrix, size and number of groups, names of the nodes, color of groups, date, legend color
input$qgraph = list( groups = list(Commodities = c(1:8), Level = c(seq(9,29,3)), Slope = seq(10,29,3),Curvature = seq(11,29,3)), 
                    names.fi = c("crude", "gas", "gold", "silver", "al", "cop", "corn", "wheat",
                                 "GER0", "GER1","GER2","JP0","JP1","JP2",
                                 "AU0", "AU1", "AU2", "US0", "US1", "US2",
                                 "UK0", "UK1", "UK2", "FR0", "FR1", "FR2", "NO0", "NO1", "NO2"),
                    col = c(rep("red",8),rep(c('blue','green4','mediumorchid4'),7)),
                    legend= c("Commodities", "0 - Level","1 - Slope","2 - Curvaature"),
                    title="Connectedness among Yield Curve Factors and Commodities using Linear Granger Causality: 2000-01-01/2020-03-01",
                    dt =  '', ground_col = c(rep("white",29)),
                    text_col = c( "red","blue",'green4','mediumorchid4'))


main_graph<-function(mt,qg){
  Q <- qgraph(mt, minimum = 0.25, cut = 0.4, vsize = 1.5, groups = qg$groups,color = qg[['ground_col']],
              legend = F, borders = TRUE, layout = "circular", shape = "circle",edge.color = qg[["col"]],
              labels = qg[["names.fi"]],label.font = 2, label.cex = 2, shape = "circle", esize = 5, node.width = 3, label.cex = 1,label.color = qg[["col"]],
              border.color = qg[["col"]],border.width = 4,edge.width=0.5,border.width = 0.25)
  legend(0.8, -0.4, legend =qg[["legend"]], cex=1, box.col = "transparent", bg = "transparent",text.col =qg[["text_col"]],horiz=F ,pch=21,col = qg[["text_col"]],y.intersp = 0.5)
  title(qg$title,line=2.5)
  Q
}

input$sign = 0.05

#####Whole period static Analysis

input$qgraph[['title']] = "Significant Relationships (5%) of Yield Curve Factors and Commodities using LG: 2000-01-01/2020-03-01"
main_graph(matrix_transformer(output[["res"]][["LG"]],input$sign),input$qg)

input$qgraph[['title']] = "Significant Relationships (5%) of Yield Curve Factors and Commodities using TE on filtered data: 2000-01-01/2020-03-01"
main_graph(matrix_transformer(output[["res"]][["f_TE"]],input$sign),input$qg)

#It can show the top x most effective propagators or receivers
top_x = function(data,x,sign,names){
  res = data.frame(index=names)
  df = matrix_transformer(data,sign)
  res$from =  rowSums(df)
  res$to = colSums(df)
  res$all = res$to +res$from
  res=res[with(res, order(all,decreasing=TRUE)),]
  res[1:x,]
  }

#It can give the most effective propagators based on all connections
output[['top']][['TE']] =top_x(output[["res"]][["f_TE"]],5,input$sign,input$qgraph[['names.fi']])
output[['top']][['LG']] =top_x(output[["res"]][["LG"]], 5,input$sign,input$qgraph[['names.fi']])


# Producing the in and out edges between groups in a table
group_transfer = function(data,sign){
  data = matrix_transformer(data,sign)
  res = data.frame(ncol=4,nraw = 4)
  idx= list(comm=c(1:8),level=seq(9,29,3),slope=seq(10,29,3),curv=seq(11,29,3))
  row=1
  col=1
  for (i in idx){
    for (j in idx){
      res[row,col]=sum(data[i,j])
      col=col+1
      }
    col=1
    row=row+1
  }
  res$all = rowSums(res)
  res = rbind(res,colSums(res))
  res
}

output$group_level[['f_TE']]=group_transfer(output[["res"]][["f_TE"]],0.05)
output$group_level[['LG']]=group_transfer(output[["res"]][["LG"]],0.05)




output$group_level[['first']][['f_TE']]=group_transfer(output[["split_res"]][["first"]][["f_TE"]],0.05)
output$group_level[['first']][['LG']]=group_transfer(output[["split_res"]][["first"]][["LG"]],0.05)



######Plots for the three period separately
#First
input$qgraph[['title']] = "Significant Relationships (5%) of Yield Curve Factors and Commodities using LG: 2000-01-10/2007-06-30"
main_graph(matrix_transformer(output[["split_res"]][["first"]][[2]],input$sign),input$qg)


input$qgraph[['title']] = "Significant Relationships (5%) of Yield Curve Factors and Commodities using TE: 2000-01-10/2007-06-30"
main_graph(matrix_transformer(output[["split_res"]][["first"]][['f_TE']],input$sign),input$qg)


#Crisis
input$qgraph[['title']] = "Significant Relationships (5%) of Yield Curve Factors and Commodities using LG: 2007-07-01/2009-07-31"
main_graph(matrix_transformer(output[["split_res"]][["crisis"]][[2]],input$sign),input$qg)

input$qgraph[['title']] = "Significant Relationships (5%) of Yield Curve Factors and Commodities using TE: 2007-07-01/2009-07-31"
main_graph(matrix_transformer(output[["split_res"]][["crisis"]][['f_TE']],input$sign),input$qg)


#Third
input$qgraph[['title']] = "Significant Relationships (5%) of Yield Curve Factors and Commodities using LG: 2009-08-01/2020-03-02"
main_graph(matrix_transformer(output[["split_res"]][["third"]][[2]],input$sign),input$qg)

input$qgraph[['title']] = "Significant Relationships (5%) of Yield Curve Factors and Commodities using TE: 2009-08-01/2020-03-02"
main_graph(matrix_transformer(output[["split_res"]][["third"]][['f_TE']],input$sign),input$qg)




#####Robustness analysis
#Plots for the whole period with 1% signifinace level
input$qgraph[['title']] = "Significant Relationships (1%) of Yield Curve Factors and Commodities using TE: 2000-01-01/2020-03-01"
main_graph(matrix_transformer(output[["res"]][["f_TE"]],0.01),input$qg)


input$qgraph[['title']] = "Significant Relationships (1%) of Yield Curve Factors and Commodities using LG: 2000-01-01/2020-03-01"
main_graph(matrix_transformer(output[["res"]][["LG"]],0.01),input$qg)


output$rob001[['f_TE']]=group_transfer(output[["res"]][["f_TE"]],0.01)

output$rob001[['LG']]=group_transfer(output[["res"]][["LG"]],0.01)


output$group_level[['f_TE']]=group_transfer(output[["res"]][["f_TE"]],0.05)
output$group_level[['LG']]=group_transfer(output[["res"]][["LG"]],0.05)

x = output$rob001[['f_TE']]/output$group_level[['f_TE']]


#####################################################################################################################################
###################################   Dynamic Analysis   ###################################
setwd('C:/Users/Hp/Desktop/Master Thesis/results/')

#### Variables for dynamic analysis:
input$dynamic = list(comm_out_df = na.remove(merger(output[["raw_commodities"]],output[["DL_coeff"]])),
                     window = 156, comm_numb = length(output[["raw_commodities"]]), DL_coef = 3,
                     comm_in_df = na.omit(merger(output[["raw_commodities"]],output[["DL_coeff"]]))[,1:length(output[["raw_commodities"]])],
                     level_in_df = na.omit(beta_pairing(output[["DL_coeff"]],1)),
                     slope_in_df = na.omit(beta_pairing(output[["DL_coeff"]],2)),
                     curve_in_df = na.omit(beta_pairing(output[["DL_coeff"]],3)),
                     poss = 3*8*7, all = 812, date = index(output$DL_coeff[[1]]),step = 4)


plan(multiprocess)
#The functions below will count the number of significant edges for a determined group of financial product

#########TE
#Pairwise connectedness between the different groups. Inside connectedness is not incuded
#Across commodities and factors
#output[["dynamic_anal"]][['TE']][['comm_slope']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['comm_slope']] , file = "TE_dynamic_comm_slope.Rds")
output[["dynamic_anal"]][['TE']][['comm_slope']] <- readRDS("TE_dynamic_comm_slope.Rds")


#output[["dynamic_anal"]][['TE']][['comm_curve']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['comm_curve']] , file = "TE_dynamic_comm_curve.Rds")
output[["dynamic_anal"]][['TE']][['comm_curve']] <- readRDS("TE_dynamic_comm_curve.Rds")


#output[["dynamic_anal"]][['TE']][['comm_level']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["level_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['comm_level']]  , file = "TE_dynamic_comm_level.Rds")
output[["dynamic_anal"]][['TE']][['comm_level']] <- readRDS("TE_dynamic_comm_level.Rds")


#########LG
#output[["dynamic_anal"]][['LG']][['comm_slope']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['comm_slope']] , file = "LG_dynamic_comm_slope.Rds")
output[["dynamic_anal"]][['LG']][['comm_slope']] <- readRDS("LG_dynamic_comm_slope.Rds")


#output[["dynamic_anal"]][['LG']][['comm_curve']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['comm_curve']] , file = "LG_dynamic_comm_curve.Rds")
output[["dynamic_anal"]][['LG']][['comm_curve']] <- readRDS("LG_dynamic_comm_curve.Rds")


#output[["dynamic_anal"]][['LG']][['comm_level']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["level_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['comm_level']]  , file = "LG_dynamic_comm_level.Rds")
output[["dynamic_anal"]][['LG']][['comm_level']] <- readRDS("LG_dynamic_comm_level.Rds")



#########################################
####Across different factors
##TE
#output[["dynamic_anal"]][['TE']][['level_slope']] = dynamic_pairwise(input[["dynamic"]][["level_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['level_slope']]  , file = "TE_dynamic_level_slope.Rds")
output[["dynamic_anal"]][['TE']][['level_slope']] <- readRDS("TE_dynamic_level_slope.Rds")


#output[["dynamic_anal"]][['TE']][['level_curve']] = dynamic_pairwise(input[["dynamic"]][["level_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['level_curve']]  , file = "TE_dynamic_level_curve.Rds")
output[["dynamic_anal"]][['TE']][['level_curve']] <- readRDS("TE_dynamic_level_curve.Rds")


#output[["dynamic_anal"]][['TE']][['curve_slope']] = dynamic_pairwise(input[["dynamic"]][["curve_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['curve_slope']]  , file = "TE_dynamic_curve_slope.Rds")
output[["dynamic_anal"]][['TE']][['curve_slope']] <- readRDS("TE_dynamic_curve_slope.Rds")

###LG
#output[["dynamic_anal"]][['LG']][['level_slope']] = LG_dynamic_pairwise(input[["dynamic"]][["level_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['level_slope']]  , file = "LG_dynamic_level_slope.Rds")
output[["dynamic_anal"]][['LG']][['level_slope']] <- readRDS("LG_dynamic_level_slope.Rds")


#output[["dynamic_anal"]][['LG']][['level_curve']] = LG_dynamic_pairwise(input[["dynamic"]][["level_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['level_curve']]  , file = "LG_dynamic_level_curve.Rds")
output[["dynamic_anal"]][['LG']][['level_curve']] <- readRDS("LG_dynamic_level_curve.Rds")


#output[["dynamic_anal"]][['LG']][['curve_slope']] = LG_dynamic_pairwise(input[["dynamic"]][["curve_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['curve_slope']]  , file = "LG_dynamic_curve_slope.Rds")
output[["dynamic_anal"]][['LG']][['curve_slope']] <- readRDS("LG_dynamic_curve_slope.Rds")




###Within-connections
###TE
#Commodity inside
#output[["dynamic_anal"]][['TE']][['comm_ins']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["comm_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['comm_ins']]  , file = "TE_comm_ins.Rds")
output[["dynamic_anal"]][['TE']][['comm_ins']] <- readRDS("TE_comm_ins.Rds")


#Level inside
#output[["dynamic_anal"]][['TE']][['level_ins']] = dynamic_pairwise(input[["dynamic"]][["level_in_df"]],input[["dynamic"]][["level_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['level_ins']]  , file = "TE_level_ins.Rds")
output[["dynamic_anal"]][['TE']][['level_ins']] <- readRDS("TE_level_ins.Rds")


#Slope inside
#output[["dynamic_anal"]][['TE']][['slope_ins']] = dynamic_pairwise(input[["dynamic"]][["slope_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['slope_ins']]  , file = "TE_slope_ins.Rds")
output[["dynamic_anal"]][['TE']][['slope_ins']] <- readRDS("TE_slope_ins.Rds")

#Curvature inside
#output[["dynamic_anal"]][['TE']][['curv_ins']] = dynamic_pairwise(input[["dynamic"]][["curve_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["dynamic_anal"]][['TE']][['curv_ins']]  , file = "TE_curv_ins.Rds")
output[["dynamic_anal"]][['TE']][['curv_ins']] <- readRDS("TE_curv_ins.Rds")



###LG
#Commodity inside
#output[["dynamic_anal"]][['LG']][['comm_ins']] = LG_within_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["comm_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['comm_ins']]  , file = "LG_comm_ins.Rds")
output[["dynamic_anal"]][['LG']][['comm_ins']] <- readRDS("LG_comm_ins.Rds")


#Level inside
#output[["dynamic_anal"]][['LG']][['level_ins']] = LG_within_dynamic_pairwise(input[["dynamic"]][["level_in_df"]],input[["dynamic"]][["level_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['level_ins']]  , file = "LG_level_ins.Rds")
output[["dynamic_anal"]][['LG']][['level_ins']] <- readRDS("LG_level_ins.Rds")


#Slope inside
#output[["dynamic_anal"]][['LG']][['slope_ins']] = LG_within_dynamic_pairwise(input[["dynamic"]][["slope_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['slope_ins']]  , file = "LG_slope_ins.Rds")
output[["dynamic_anal"]][['LG']][['slope_ins']] <- readRDS("LG_slope_ins.Rds")

#Curvature inside
#output[["dynamic_anal"]][['LG']][['curv_ins']] = LG_within_dynamic_pairwise(input[["dynamic"]][["curve_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["dynamic_anal"]][['LG']][['curv_ins']]  , file = "LG_curv_ins.Rds")
output[["dynamic_anal"]][['LG']][['curv_ins']] <- readRDS("LG_curv_ins.Rds")




#Aggregating significance matrices across groups by calculating connectedness measures in case of dynamic analysis
dynamic_ts = function(mt_list,treshold, dynamic,cut = 8){
  df = data.frame(ncol=6)
  for (i in 1:length(mt_list)){
    data = matrix_transformer(mt_list[[i]],treshold)
    factor = sum(data[(cut+1):ncol(data),1:cut])
    comm = sum(data[1:cut,(cut+1):ncol(data)])
    df[i,1] = comm/dynamic$all
    df[i,2] = factor/dynamic$all
    df[i,3] = (comm+factor)/dynamic$all
    df[i,4] = comm/dynamic$poss
    df[i,5] = factor/dynamic$poss
    df[i,6] = (comm+factor)/dynamic$poss
    
  }
  names(df)=c('comm','factor','all')
  df$date = dynamic$date[seq(2+dynamic$window,length(dynamic$date),dynamic$step)]
  df = data.frame(df)
  df
}

# Aggregating within groups
within_conn_to_ts = function(mt_list,treshold,dynamic){
  df = data.frame(ncol=3)
  for(i in 1:length(mt_list)){
    diag(mt_list[[i]]) = diag(mt_list[[i]])+1
    x = sum(matrix_transformer(mt_list[[i]],treshold))
    df[i,1] = x/dynamic$all
    df[i,2] = x/dynamic$poss
  }
  names(df)=c('all','poss')
  df$date = dynamic$date[seq(2+dynamic$window,length(dynamic$date),dynamic$step)]
  df
}


#Creating a list for the results
output$dynamic_ts=list()

###Transform dynamic results to the amuont of significant relationship for a given time and treshold (5%)
#It produces the ratio of the sum of sign. rel. and all possible rel.

###Commodities and factors
##TE
output$dynamic_ts[['comm_TE']][['comm_level']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['comm_level']],input$sign,input$dynamic)
output$dynamic_ts[['comm_TE']][['comm_slope']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['comm_slope']],input$sign,input$dynamic)
output$dynamic_ts[['comm_TE']][['comm_curve']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['comm_curve']],input$sign,input$dynamic)
##LG
output$dynamic_ts[['comm_LG']][['comm_level']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['comm_level']],input$sign,input$dynamic)
output$dynamic_ts[['comm_LG']][['comm_slope']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['comm_slope']],input$sign,input$dynamic)
output$dynamic_ts[['comm_LG']][['comm_curve']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['comm_curve']],input$sign,input$dynamic)


###Across Factors and factors
input$dynamic[['poss']] = 7*7*3
##TE
output$dynamic_ts[['fact_TE']][['level_slope']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['level_slope']],input$sign,input$dynamic, cut=7)
output$dynamic_ts[['fact_TE']][['level_curve']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['level_curve']],input$sign,input$dynamic, cut=7)
output$dynamic_ts[['fact_TE']][['curve_slope']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['level_curve']],input$sign,input$dynamic, cut=7)
##LG
output$dynamic_ts[['fact_LG']][['level_slope']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['level_slope']],input$sign,input$dynamic, cut=7)
output$dynamic_ts[['fact_LG']][['level_curve']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['level_curve']],input$sign,input$dynamic, cut=7)
output$dynamic_ts[['fact_LG']][['curve_slope']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['level_curve']],input$sign,input$dynamic, cut=7)

###Within groups
input$dynamic[['poss']] = 8*7 +3*7*6

##TE
output$dynamic_ts[['inside_TE']][['comm_ins']] = within_conn_to_ts(output[["dynamic_anal"]][['TE']][['comm_ins']],input$sign,input$dynamic)
output$dynamic_ts[['inside_TE']][['level_ins']] = within_conn_to_ts(output[["dynamic_anal"]][['TE']][['level_ins']],input$sign,input$dynamic)
output$dynamic_ts[['inside_TE']][['slope_ins']] = within_conn_to_ts(output[["dynamic_anal"]][['TE']][['slope_ins']],input$sign,input$dynamic)
output$dynamic_ts[['inside_TE']][['curv_ins']] = within_conn_to_ts(output[["dynamic_anal"]][['TE']][['curv_ins']],input$sign,input$dynamic)

##LG
output$dynamic_ts[['inside_LG']][['comm_ins']] = within_conn_to_ts(output[["dynamic_anal"]][['LG']][['comm_ins']],input$sign,input$dynamic)
output$dynamic_ts[['inside_LG']][['level_ins']] = within_conn_to_ts(output[["dynamic_anal"]][['LG']][['level_ins']],input$sign,input$dynamic)
output$dynamic_ts[['inside_LG']][['slope_ins']] = within_conn_to_ts(output[["dynamic_anal"]][['LG']][['slope_ins']],input$sign,input$dynamic)
output$dynamic_ts[['inside_LG']][['curv_ins']] = within_conn_to_ts(output[["dynamic_anal"]][['LG']][['curv_ins']],input$sign,input$dynamic)



#Calculateing the overall connectedness inn the network
overall_conn = function(dynamic_ts){
  res=data.frame(matrix(0L,ncol=2,nrow = length(dynamic_ts[[1]][[1]][,'all'])))
  #TE
  for (i in c(1,3,5)){
    for(j in 1:length(dynamic_ts[[i]])){
      res[,1] = res[,1] + dynamic_ts[[i]][[j]][,'all']
    }}
  #LG
  for (i in c(2,4,6)){
    for(j in 1:length(dynamic_ts[[i]])){
      res[,2] = res[,2] + dynamic_ts[[i]][[j]][,'all']
    }}
  names(res)= c('TE','LG')
  res$overall = res$TE+res$LG
  res$date = dynamic_ts[[1]][[1]][,'date']
  res
}

#Overall network connectedness
output$dynamic_ts[['overall']]=overall_conn(output$dynamic_ts)


#Input for plotting
input$dynamic_line = list(names =c('LG', 'TE'),
                          color = c("darkblue","darkgreen"),
                          type = c('solid','solid'), xlab='Date',ylab='Percentage',
                          title = 'Overall network connectedness',
                          ylim = 0.5,label_name = 'Using')


overall_plot = function(df,line){
  n_df =df  %>% gather(key = "countries", value = "value", -date)
  n_df$date = as.Date(n_df$date,tz='UTC')
  fig = ggplot(n_df, aes(x = date, y = value)) +
    geom_line(aes(color = countries, linetype = countries),size=1) + 
    
    scale_color_manual(name=line$label_name, values = line$color, labels =line$names)+
    scale_linetype_manual(name=line$label_name,values =  line$type, labels = line$names)+
    
    xlab(line$xlab)+ylab(line$ylab)+
    ggtitle(line$title)+
    scale_x_date(expand = c(0,100))+theme_minimal()+theme(legend.position='right')+ylim(0,line$ylim)+theme(text = element_text(size=20))
  fig
  }


overall_plot(output$dynamic_ts[['overall']][,c(1:2,4)],input$dynamic_line)

####Visualization of dynamic results
dynamic_line = function(df_list,col,line,date_col = 7){
  df = data.frame(date=df_list[[1]][,date_col])
  for ( i in 1:length(df_list)){
    df[,i+1] = df_list[[i]][,col]
  }
  n_df =df  %>% gather(key = "countries", value = "value", -date)
  n_df$date = as.Date(n_df$date,tz='UTC')
  fig = ggplot(n_df, aes(x = date, y = value)) +
    geom_area(aes(color = countries, linetype = countries, fill = countries),alpha=0.8) + 
    scale_color_manual(name=line$label_name, values = line$color, labels =line$names)+
    scale_linetype_manual(name=line$label_name,values =  line$type, labels = line$names)+
    scale_fill_manual(name=line$label_name, values = line$color, labels =line$names)+
    xlab(line$xlab)+ylab(line$ylab)+
    ggtitle(line$title)+
    scale_x_date(expand = c(0,100))+theme_minimal()+theme(legend.position='bottom')+ylim(0,line$ylim)
  fig
}



input$dynamic_line = list(names =c( 'comm/level','comm/slope','comm/curve'),
                          color = c("darkgreen", "darkgoldenrod2","darkblue"),
                          type = c('solid','solid', 'solid'), xlab='Date',ylab='Percentage',
                          title = 'Proportion of significant links to all possible relations across groups using TE',
                          ylim = 0.5,label_name = 'From/to')


#From commodities to factors using TE
input$dynamic_line[['title']] =  '(B) From commodities to factors using TE'
fig2=dynamic_line(output$dynamic_ts[['comm_TE']],4,input$dynamic_line)

#From commodities to factors using LG
input$dynamic_line[['title']] =  '(A) From commodities to factors using LG'
fig1=dynamic_line(output$dynamic_ts[['comm_LG']],4,input$dynamic_line)


#From factors to commodities using TE
input$dynamic_line[['names']] = c('level/comm','slope/comm','curv/comm')
input$dynamic_line[['color']] = c('aquamarine3', 'chocolate4','darkorange2')
input$dynamic_line[['title']] =  '(D) From factors to commodities using TE'
fig4=dynamic_line(output$dynamic_ts[['comm_TE']],5,input$dynamic_line)


#From factors to commodities using LG
input$dynamic_line[['title']] =  '(C) From factors to commodities using LG'
fig3=dynamic_line(output$dynamic_ts[['comm_LG']],5,input$dynamic_line)


#grid.text("MAIN TITLE",just=c("centre", "centre"),x = 0.5,y=0.95)
grid.arrange(fig1, fig2,fig3,fig4, nrow=2,ncol=2, top = textGrob("Proportion of existing and possible links across commodities and factors using 5% sign. level",gp=gpar(fontsize=16,font=4)))



#####Ploting from factor to factor

#From factors1 to factors2 using TE
input$dynamic_line[['color']] = c("darkgreen", "darkgoldenrod2","darkblue")

input$dynamic_line[['title']] =  '(B) From factors to factors using TE'
input$dynamic_line[['names']] = c('level/slope','level/curv','curv/slope')
fig2=dynamic_line(output$dynamic_ts[['fact_TE']],4,input$dynamic_line)


#From factors1 to factors2 using LG
input$dynamic_line[['title']] =  '(A) From factors to factors using LG'
fig1=dynamic_line(output$dynamic_ts[['fact_LG']],4,input$dynamic_line)


#From factors2 to factors1 using TE
input$dynamic_line[['color']] = c('aquamarine3', 'chocolate4','darkorange2')
input$dynamic_line[['title']] =  '(D) From factors to factors using TE'
input$dynamic_line[['names']] = c('slope/level','curve/level','slope/curve')
fig4=dynamic_line(output$dynamic_ts[['fact_TE']],5,input$dynamic_line)


#From factors2 to factors1 using LG
input$dynamic_line[['title']] =  '(C) From factors to factors using LG'
fig3=dynamic_line(output$dynamic_ts[['fact_LG']],5,input$dynamic_line)

grid.arrange(fig1, fig2,fig3,fig4, nrow=2,ncol=2, top = textGrob("Proportion of existing and possible links across factor groups using 5% sign. level",gp=gpar(fontsize=16,font=4)))


#####Ploting within relations
input$dynamic_line = list(names =c( 'commodity','level','slope','curve'),
                          color = c("darkgreen", "darkgoldenrod2","darkblue",'aquamarine3'),
                          type = c('solid','solid', 'solid','solid'), xlab='Date',ylab='Percentage',
                          title = 'Proportion of significant links to all possible relations across groups using TE',
                          ylim = 0.6, label_name = 'Group')


#####Relation inside the groups
##LG
input$dynamic_line[['title']]='(A) Using LG'
fig1=dynamic_line(output$dynamic_ts[['inside_LG']],2,input$dynamic_line,date_col=3)
##TE
input$dynamic_line[['title']]='(B) Using TE'
fig2=dynamic_line(output$dynamic_ts[['inside_TE']],2,input$dynamic_line,date_col=3)

grid.arrange(fig1, fig2, nrow=2,ncol=1, top = textGrob("Proportion of existing and possible links inside groups using 5% sign. level",gp=gpar(fontsize=16,font=4)))


#############################################################################
#######################Robustness of Dynamic Analysis########################

############    Significance level    ############

input$sign = 0.01
##TE
output$rob_dyn[['comm_TE']][['comm_level']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['comm_level']],input$sign,input$dynamic)
output$rob_dyn[['comm_TE']][['comm_slope']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['comm_slope']],input$sign,input$dynamic)
output$rob_dyn[['comm_TE']][['comm_curve']] = dynamic_ts(output[["dynamic_anal"]][['TE']][['comm_curve']],input$sign,input$dynamic)
##LG
output$rob_dyn[['comm_LG']][['comm_level']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['comm_level']],input$sign,input$dynamic)
output$rob_dyn[['comm_LG']][['comm_slope']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['comm_slope']],input$sign,input$dynamic)
output$rob_dyn[['comm_LG']][['comm_curve']] = dynamic_ts(output[["dynamic_anal"]][['LG']][['comm_curve']],input$sign,input$dynamic)




input$dynamic_line = list(names =c( 'comm/level','comm/slope','comm/curve'),
                          color = c("darkgreen", "darkgoldenrod2","darkblue"),
                          type = c('solid','solid', 'solid'), xlab='Date',ylab='Percentage',
                          title = 'Proportion of significant links to all possible relations across groups using TE',
                          ylim = 0.25,label_name = 'From/to')

#From commodities to factors using LG
input$dynamic_line[['title']] =  '(A) Linear relations from commodities to factors using LG'
fig1=dynamic_line(output$rob_dyn[['comm_LG']],4,input$dynamic_line)

#From commodities to factors using TE
input$dynamic_line[['title']] =  '(B) Non-linear relations from commodities to factors using TE'
fig2=dynamic_line(output$rob_dyn[['comm_TE']],4,input$dynamic_line)


grid.arrange(fig1, fig2, nrow=2,ncol=1, top = textGrob("Robustness test - Proportion of existing and possible links across commodities and factors using 1% sign. level",gp=gpar(fontsize=16,font=4)))




############    Different Rolling Window Size    ############

setwd('C:/Users/Hp/Desktop/Master Thesis/results/')

#### Variables for dynamic analysis:
input$dynamic = list(comm_out_df = na.remove(merger(output[["raw_commodities"]],output[["DL_coeff"]])),
                     window = 156, comm_numb = length(output[["raw_commodities"]]), DL_coef = 3,
                     comm_in_df = na.omit(merger(output[["raw_commodities"]],output[["DL_coeff"]]))[,1:length(output[["raw_commodities"]])],
                     level_in_df = na.omit(beta_pairing(output[["DL_coeff"]],1)),
                     slope_in_df = na.omit(beta_pairing(output[["DL_coeff"]],2)),
                     curve_in_df = na.omit(beta_pairing(output[["DL_coeff"]],3)),
                     poss = 3*8*7, all = 812, date = index(output$DL_coeff[[1]]),step = 4)


input$dynamic[['window']] = 104

plan(multiprocess)
#The functions below will count the number of significant edges for a determined group of financial product

#########TE
#Pairwise connectedness between the different groups. Inside connectedness is not incuded
#Across commodities and factors
#output[["rob104_dynamic_anal"]][['TE']][['comm_slope']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["rob104_dynamic_anal"]][['TE']][['comm_slope']] , file = "rob104_TE_dynamic_comm_slope.Rds")
output[["rob104_dynamic_anal"]][['TE']][['comm_slope']] <- readRDS("rob104_TE_dynamic_comm_slope.Rds")


#output[["rob104_dynamic_anal"]][['TE']][['comm_curve']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["rob104_dynamic_anal"]][['TE']][['comm_curve']] , file = "rob104_TE_dynamic_comm_curve.Rds")
output[["rob104_dynamic_anal"]][['TE']][['comm_curve']] <- readRDS("rob104_TE_dynamic_comm_curve.Rds")


#output[["rob104_dynamic_anal"]][['TE']][['comm_level']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["level_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["rob104_dynamic_anal"]][['TE']][['comm_level']]  , file = "rob104_TE_dynamic_comm_level.Rds")
output[["rob104_dynamic_anal"]][['TE']][['comm_level']] <- readRDS("rob104_TE_dynamic_comm_level.Rds")


#########LG
#output[["rob104_dynamic_anal"]][['LG']][['comm_slope']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["rob104_dynamic_anal"]][['LG']][['comm_slope']] , file = "rob104_LG_dynamic_comm_slope.Rds")
output[["rob104_dynamic_anal"]][['LG']][['comm_slope']] <- readRDS("rob104_LG_dynamic_comm_slope.Rds")


#output[["rob104_dynamic_anal"]][['LG']][['comm_curve']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["rob104_dynamic_anal"]][['LG']][['comm_curve']] , file = "rob104_LG_dynamic_comm_curve.Rds")
output[["rob104_dynamic_anal"]][['LG']][['comm_curve']] <- readRDS("rob104_LG_dynamic_comm_curve.Rds")


#output[["rob104_dynamic_anal"]][['LG']][['comm_level']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["level_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["rob104_dynamic_anal"]][['LG']][['comm_level']]  , file = "rob104_LG_dynamic_comm_level.Rds")
output[["rob104_dynamic_anal"]][['LG']][['comm_level']] <- readRDS("rob104_LG_dynamic_comm_level.Rds")


input$dynamic[['window']] = 208

#The functions below will count the number of significant edges for a determined group of financial product

#########TE
#Pairwise connectedness between the different groups. Inside connectedness is not incuded
#Across commodities and factors
#output[["rob208_dynamic_anal"]][['TE']][['comm_slope']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["rob208_dynamic_anal"]][['TE']][['comm_slope']] , file = "rob208_TE_dynamic_comm_slope.Rds")
output[["rob208_dynamic_anal"]][['TE']][['comm_slope']] <- readRDS("rob208_TE_dynamic_comm_slope.Rds")


#output[["rob208_dynamic_anal"]][['TE']][['comm_curve']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["rob208_dynamic_anal"]][['TE']][['comm_curve']] , file = "rob208_TE_dynamic_comm_curve.Rds")
output[["rob208_dynamic_anal"]][['TE']][['comm_curve']] <- readRDS("rob208_TE_dynamic_comm_curve.Rds")


#output[["rob208_dynamic_anal"]][['TE']][['comm_level']] = dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["level_in_df"]], input[["dynamic"]][["window"]], input$TE)
#saveRDS(output[["rob208_dynamic_anal"]][['TE']][['comm_level']]  , file = "rob208_TE_dynamic_comm_level.Rds")
output[["rob208_dynamic_anal"]][['TE']][['comm_level']] <- readRDS("rob208_TE_dynamic_comm_level.Rds")


#########LG
#output[["rob208_dynamic_anal"]][['LG']][['comm_slope']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["slope_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["rob208_dynamic_anal"]][['LG']][['comm_slope']] , file = "rob208_LG_dynamic_comm_slope.Rds")
output[["rob208_dynamic_anal"]][['LG']][['comm_slope']] <- readRDS("rob208_LG_dynamic_comm_slope.Rds")


#output[["rob208_dynamic_anal"]][['LG']][['comm_curve']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["curve_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["rob208_dynamic_anal"]][['LG']][['comm_curve']] , file = "rob208_LG_dynamic_comm_curve.Rds")
output[["rob208_dynamic_anal"]][['LG']][['comm_curve']] <- readRDS("rob208_LG_dynamic_comm_curve.Rds")


#output[["rob208_dynamic_anal"]][['LG']][['comm_level']] = LG_dynamic_pairwise(input[["dynamic"]][["comm_in_df"]],input[["dynamic"]][["level_in_df"]], input[["dynamic"]][["window"]], input$granger_param)
#saveRDS(output[["rob208_dynamic_anal"]][['LG']][['comm_level']]  , file = "rob208_LG_dynamic_comm_level.Rds")
output[["rob208_dynamic_anal"]][['LG']][['comm_level']] <- readRDS("rob208_LG_dynamic_comm_level.Rds")




input$dynamic[['window']] = 104
input$sign = 0.05

#TE
output$rob_dyn104[['comm_TE']][['comm_level']] = dynamic_ts(output[["rob104_dynamic_anal"]][['TE']][['comm_level']],input$sign,input$dynamic)
output$rob_dyn104[['comm_TE']][['comm_slope']] = dynamic_ts(output[["rob104_dynamic_anal"]][['TE']][['comm_slope']],input$sign,input$dynamic)
output$rob_dyn104[['comm_TE']][['comm_curve']] = dynamic_ts(output[["rob104_dynamic_anal"]][['TE']][['comm_curve']],input$sign,input$dynamic)
##LG
output$rob_dyn104[['comm_LG']][['comm_level']] = dynamic_ts(output[["rob104_dynamic_anal"]][['LG']][['comm_level']],input$sign,input$dynamic)
output$rob_dyn104[['comm_LG']][['comm_slope']] = dynamic_ts(output[["rob104_dynamic_anal"]][['LG']][['comm_slope']],input$sign,input$dynamic)
output$rob_dyn104[['comm_LG']][['comm_curve']] = dynamic_ts(output[["rob104_dynamic_anal"]][['LG']][['comm_curve']],input$sign,input$dynamic)



input$dynamic[['window']] = 208

#TE
output$rob_dyn208[['comm_TE']][['comm_level']] = dynamic_ts(output[["rob208_dynamic_anal"]][['TE']][['comm_level']],input$sign,input$dynamic)
output$rob_dyn208[['comm_TE']][['comm_slope']] = dynamic_ts(output[["rob208_dynamic_anal"]][['TE']][['comm_slope']],input$sign,input$dynamic)
output$rob_dyn208[['comm_TE']][['comm_curve']] = dynamic_ts(output[["rob208_dynamic_anal"]][['TE']][['comm_curve']],input$sign,input$dynamic)
##LG
output$rob_dyn208[['comm_LG']][['comm_level']] = dynamic_ts(output[["rob208_dynamic_anal"]][['LG']][['comm_level']],input$sign,input$dynamic)
output$rob_dyn208[['comm_LG']][['comm_slope']] = dynamic_ts(output[["rob208_dynamic_anal"]][['LG']][['comm_slope']],input$sign,input$dynamic)
output$rob_dyn208[['comm_LG']][['comm_curve']] = dynamic_ts(output[["rob208_dynamic_anal"]][['LG']][['comm_curve']],input$sign,input$dynamic)




#Visualization of Robustness test
input$dynamic_line = list(names =c( 'comm/level','comm/slope','comm/curve'),
                          color = c("darkgreen", "darkgoldenrod2","darkblue"),
                          type = c('solid','solid', 'solid'), xlab='Date',ylab='Percentage',
                          title = 'Proportion of significant links to all possible relations across groups using TE',
                          ylim = 0.5,label_name = 'From/to')

#From commodities to factors using LG
input$dynamic_line[['title']] =  '(A) Linear relations from commodities to factors using LG and 2-year-long window'
fig1=dynamic_line(output[["rob_dyn104"]][['comm_LG']],4,input$dynamic_line)

#From commodities to factors using TE
input$dynamic_line[['title']] =  '(B) Non-linear relations from commodities to factors using TE and 2-year-long window'
fig2=dynamic_line(output[["rob_dyn104"]][['comm_TE']],4,input$dynamic_line)


#From commodities to factors using LG
input$dynamic_line[['title']] =  '(C) Linear relations from commodities to factors using LG and 4-year-long window'
fig3=dynamic_line(output[["rob_dyn208"]][['comm_LG']],4,input$dynamic_line)

#From commodities to factors using TE
input$dynamic_line[['title']] =  '(D) Non-linear relations from commodities to factors using TE and 4-year-long window'
fig4=dynamic_line(output[["rob_dyn208"]][['comm_TE']],4,input$dynamic_line)




grid.arrange(fig1, fig2, fig3, fig4, nrow=2,ncol=2, top = textGrob("Robustness test - Proportion of existing and possible links inside groups using 5% sign. level and different rolling window sizes",gp=gpar(fontsize=16,font=4)))



############################################################################
#Plot for the loading of Diebold-Li decomposition
DL_plot = function(time, step, lambda){
  df = data.frame(mat=seq(step,time,step))
  df$beta0=1
  df$beta1=(1-exp(-lambda*df$mat))/(lambda*df$mat)
  df$beta2=(1-exp(-lambda*df$mat))/(lambda*df$mat)-exp(-lambda*df$mat)
  
  n_df =df  %>% gather(key = "countries", value = "value", 2:4)
  fig = ggplot(n_df, aes(x = mat, y = value)) + 
    geom_line(aes(color = countries, linetype = countries),size=1.5) + 
    scale_color_manual(name='Factors', values = c("darkgreen", "black","darkblue"), labels =c('Level','Slope','Curvature'))+
    scale_linetype_manual(name='Factors',values =  c('solid','dashed','twodash'), labels = c('Level','Slope','Curvature'))+
    xlab('Maturity in Months')+ylab('Loadings')+
    ggtitle('Factor Loadings')+theme_minimal()
  fig
}

DL_plot(120,0.001, input$lambda)

