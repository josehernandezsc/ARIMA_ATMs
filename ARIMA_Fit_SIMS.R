library(tidyverse)

install.packages("FitAR")
install.packages("tseries")
install.packages("fUnitRoots")
install.packages("forecast")
library(tseries)
library(fUnitRoots)
library(forecast)
library(lmtest)
library(FitAR)
library(Metrics)


setwd("~/Documents/IBM Consultant/Educación/Py-R - Sao Paulo/MVP/Data")
df1<-as_tibble(read.csv("Atm Dvv 1_1.csv"))
df2<-as_tibble(read.csv("Atm Dvv 1_2.csv"))
df3<-as_tibble(read.csv("Atm Dvv 2.csv"))
df4<-as_tibble(read.csv("Atm Dvv 3.csv"))
df5<-as_tibble(read.csv("Atm Dvv 4_4.csv"))

func_sel <- function(x) { 
  x %>%       
    select(IDATM,FECHA,CONSUMO)
}

func_mut <- function(x) { 
  x %>%       
    mutate(FECHA=as.Date(FECHA,"%d/%m/%Y"))
}

df5<-df5%>%mutate(FECHA=as.Date(FECHA,"%d/%m/%y"))

result<-list(df1,df2,df3,df4,df5)%>%lapply(func_sel)
result<-result%>%lapply(func_mut)

df<-bind_rows(result)

df<-df%>%mutate(CONSUMO=replace(CONSUMO,CONSUMO==" -   ",0))

df<-df%>%arrange(FECHA)%>%arrange(IDATM)


#### Set number of ATMs ####
n=200
###########################
data <- vector(mode = "list", length = n)
start<-vector(mode="list",length=n)

counter<-0
for (i in unique(df$IDATM)){
  point<-df%>%subset(IDATM==i,select=c(FECHA,CONSUMO))
  if (tail(point$FECHA)[6]=="2019-07-04"){
    data[[as.character(i)]]<-point
    start[[as.character(i)]]<-min(point$FECHA)
    counter<-counter+1
  }
  if (counter==n){
    break
  }
}

#for (i in 1:(n-1)){
#  m<-tail(data[[names(data)[n+1+i]]]$FECHA,1)
#  r<-tail(data[[names(data)[n+i]]]$FECHA,1)
  
#  if (m!=r){
#    print(paste0("Error en fechas desde ",names(data)[n+1+i]))
#    break
#  } else {
    
#  }
#  if (i==n-1){
#    print("Fechas correctas")
#  }
#}

##### INICIAR ENTRENAMIENTO ARIMA #####
simulation<-function(data,a,n){
  #a="1"
  
  y=data[[a]]$CONSUMO
  
  start_=start[[a]]
  #A-Fuller test
  #adf.test(y)
  
  #convert to time series data
  year<-substr(start_,1,4)%>%as.integer()
  month<-substr(start_,6,7)%>%as.integer()
  day<-substr(start_,9,10)%>%as.integer()
  
  
  ### Set number of days to forecast ###
  n_forecast <- 20
  ######################################
  
  
  
  train_model <- function(train_ar,test_ar,n_forecast){
    #fitARIMA<-ets(train_ar)
    fitARIMA<-auto.arima(train_ar,seasonal=TRUE,stationary=FALSE,max.p=8,max.q=4)
    predictions<-forecast(fitARIMA,h=n_forecast)
    metric<-smape(test_ar[1:n_forecast],predictions[[4]][1:n_forecast])
    #print(paste0("mape: ",mape(test_ar[1:n_forecast],predictions[[4]][1:n_forecast])))
    result<-list(fitARIMA,metric)
    return (result)
  }
  
  model_list=list()
  # Change order of ma from 3 to 7
  best_res<-999
  for (i in 1:3){
    
    y_clean<-y%>%ts(start=c(year,month,day),frequency=30)%>%tsclean()%>%ma(order=2*i+1)
    
    consumo<-na.omit(y_clean)
    size <- length(consumo)
    m<-size-n_forecast
    b<-m+1
    train_ar<-consumo[1:m]
    test_ar<-y[b:size]
    
    result<-train_model(train_ar,test_ar,n_forecast)
    #new_res<-result[[2]][1]
    #print(result[[2]][1])
    if (result[[2]][1]<best_res){
      best_res=result[[2]][1]
      best_model=result[[1]]
    }
    #print(result[length(result)])
  }
  print(paste0("Starting simulations for ATM ",a))
  
  sim<-c()
  for (i in 1:n){
    if (i%%200==0){
      #print(paste0("Simulation number: ",i))
    }
    sim<-c(sim,simulate(best_model,n_forecast,future = TRUE))
  }
  return (matrix(sim,ncol=n_forecast,byrow=TRUE))
}


sims<-c()
atm<-c()
### SET NUMBER OF SIMULATIONS ###
n_sim<-1000
#################################
time<-c()
for (i in 1:n){
  start_t<-Sys.time()
  a=names(data)[n+i]
  atm<-c(atm,rep(a,n_sim))
  sims<-rbind(sims,simulation(data,a,n_sim))
  end<-Sys.time()
  time<-c(time,end-start_t)
  if (i>=10){
    time_mean<-mean(time)
    time_remaining<-round(time_mean*(n-i)/60)
  } else {
    time_remaining <- "Estimating remaining"
  }
  
  print(paste0("Aprox time remaining: ",time_remaining, " minutes"))
  print(paste0(i," out of ",n))
  
}

# Replace all negative values with zeros

sims[sims<0]<-0

table_result<-data.frame(atm,sims)

write.csv(table_result,"atm_simulations_200.csv")
# PARTIR RESULTADO EN DOS DATA.FRAMES Y EXPORTAR A DOS CSV PARA DISMINUIR TAMAÑO DE FILE
table_result_1<-head(table_result,n*n_sim/2)
table_result_2<-tail(table_result,n*n_sim/2)

write.csv(table_result_1,"atm_simulations_200_1.csv")
write.csv(table_result_2,"atm_simulations_200_2.csv")
#####




