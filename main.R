mainDir <- getwd()

library(httr)
library(jsonlite)
library(RCurl)
library(dplyr)
library(lubridate)
library(tseries)
library(fpp2)
library(Quandl)
library(TSstudio)
library(caret)
library(xgboost)
library(Ckmeans.1d.dp)
TOKEN="3fb180be75bca1213cce6c5aed47f63e90d9262f9636b5e9da14c9c56e4ba584"

# res=GET("https://api.esios.ree.es/indicators", add_headers(Authorization = paste('Token token=', TOKEN, sep="")))
# res
# rawToChar(res$content)
# data = fromJSON(rawToChar(res$content))
# names(data)
# write.csv(data$indicators, "data/indicators_df.csv")

#https://www.energychisquared.com/post/c%C3%B3mo-conseguir-datos-de-esios-con-su-api/

# Definición del ID del indicador
indicador <- 600

# Token
httpheader <- 'Authorization: Token token="3fb180be75bca1213cce6c5aed47f63e90d9262f9636b5e9da14c9c56e4ba584"'

# Hasta qué fecha fin quiero los datos
hoy <- ymd_hms(paste0(Sys.Date(), " 00:00:00" ), tz = "Europe/Madrid")

# URL
uri <- paste0("https://api.esios.ree.es/indicators/", indicador, "?start_date=", "01-01-2014", "T", "00:00:00+02:00&end_date=", as.Date(hoy), "T", "23:50:00+02:00&geo_agg=sum&geo_ids&time_trunc=hour&time_agg=&locale=es")

# Extracción del JSON
extraccion <- getURI(uri, httpheader = httpheader)
extraccion <- fromJSON(txt=extraccion)

# Limpieza de la tabla
extraccion <- extraccion[["indicator"]][["values"]]
  
  # extraccion %>%
  # as.data.frame() %>%
  # select(indicator.values.tz_time, indicator.values.value) #%>%
  # mutate(name = "eólica") %>%
  # select(indicator.values.tz_time, name, indicator.values.value)

# Corrección de la fecha para tenerla en hora local peninsular
  #extraccion$indicator.values.tz_time <- ymd_hms(extraccion$indicator.values.tz_time, tz = "Europe/Madrid", locale = Sys.getlocale("LC_TIME"))
extraccion$tz_time <- ymd_hms(extraccion$tz_time, tz = "Europe/Madrid", locale = Sys.getlocale("LC_TIME"))

# Nombre del archivo a exportar
nombre_archivo <- "SPOT"

# Exportación a csv
write.csv2(extraccion, row.names = FALSE, file = paste0("data/", nombre_archivo, ".csv"))

############
df<-read.csv("data/SPOT.csv", sep=";", dec=",")
#Plot serie temporal
df<-df[df$geo_name=="EspaÃ±a",]
df$tz_time <- strptime(df$tz_time, "%Y-%m-%d %H:%M:%S")
df$y_m <- format(as.POSIXct(df$tz_time), "%m-%Y")

df <- df %>% mutate(day = day(tz_time))
df <- df %>% mutate(month = month(tz_time))
df <- df %>% mutate(year = year(tz_time))
df <- df %>% mutate(day = day(tz_time))
df <- df %>% mutate(hour = hour(tz_time))

df_month <- df %>% group_by(month,year) %>% summarise(mean=mean(value)) %>% arrange( year)
df_month$y_m <- seq.Date(as.Date("2014-01-01"), as.Date("2021-01-01"), by = "month")


# df_month$y_m <- format(as.Date(df_month$y_m,"%Y-%m"), "%m-%Y")
# arrange(df_month , year)
par(mfrow=c(1,2))
plot(df$tz_time, df$value)
plot(df_month$y_m, df_month$mean)

extraccion$datetime <- xct(extraccion$datetime)

format(as.POSIXct(df$tz_time), "%m-%Y")
apply.weekly

# ARIMA
par(mfrow=c(1,1))
boxplot(df_month$mean)
price<-df_month$mean
price_diff <- diff(df_month$mean)
summary(price)
summary(price_diff)


price_ts <-ts(price, start = c(2014,1), frequency = 12) 
price_diff_ts <-ts(price_diff, start = c(2014,1), frequency = 12) 

par(mfrow=c(1,2))
plot.ts(price_ts)
plot.ts(price_diff_ts)


#Check trend-stationary


#Dickey-Fuller Test for variable
adf.test(price, alternative="stationary", k=0) #`p-0.2315 non-staniarity`
adf.test(price, alternative="explosive", k=0) #explosive opposite to alternative price_diff is stationary

#summary(lm(dppi ~ lppi, na.action=na.omit))
#summary(lm(dppi ~ lppi + trend, na.action=na.omit))

# Augmented Dickey-Fuller test
adf.test(price, alternative="stationary")

# DF and ADF tests for differenced variable
adf.test(price_diff, k=0)
adf.test(price_diff)

# p value<0.0 -> reject de NULL, accept the ALTERNATIVE stationary. We need de differentiated variable

# Check seasonality
ggseasonplot(price_diff_ts)+ggtitle("Seasonal Plot")+ylab("EUR/MWh")
ggsubseriesplot(price_diff_ts)

# Fit with the seasonal naive method as our benchmark y_t = y_{t-s} + e_t 
fit_naive <- snaive(price_ts)
print(summary(fit_naive)) # RMSE = 15.06643
checkresiduals(fit_naive)

# Fit ETS method
fit_ets <- ets(price_ts)
print(summary(fit_ets)) # RMSE = 6.882522
checkresiduals(fit_ets)

# Fit ARIMA model
#fit_arima<-auto.arima(ts(price_ts[1:(length(price_ts)-2)], start = c(2014,1), frequency =12 ), d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
fit_arima<-auto.arima(price_ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima)) # 6.165574
checkresiduals(fit_arima)
sqrt(46.16) #Residual SD = 6.794115

# FORECAST with the ARIMA model
fcst <-forecast(fit_arima, h=24)
autoplot(fcst)
summary(fcst)



#Import data from QUANDL

# res=GET("https://api.esios.ree.es/indicators", add_headers(Authorization = paste('Token token=', TOKEN, sep="")))
# res
# rawToChar(res$content)
# data = fromJSON(rawToChar(res$content))
# names(data)
# write.csv(data$indicators, "data/indicators_df.csv")

#https://www.energychisquared.com/post/c%C3%B3mo-conseguir-datos-de-esios-con-su-api/


# Token
httpheader <- 'Authorization: Token token="3fb180be75bca1213cce6c5aed47f63e90d9262f9636b5e9da14c9c56e4ba584"'
TOKEN_AEMET="eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJlbnJpcXVlZnVzdGVycGFsb3BAZ21haWwuY29tIiwianRpIjoiYmZjYTMyZDgtNzM3NS00Y2JhLWI4N2ItMmVmZDQ5OGJlOWQyIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2MTAxODc1MTksInVzZXJJZCI6ImJmY2EzMmQ4LTczNzUtNGNiYS1iODdiLTJlZmQ0OThiZTlkMiIsInJvbGUiOiIifQ.HrHgDiE3K2WpI9SWhvKFdGyPbk_dS2znQXLnpgxJDp8"
TOKEN_QUANDL="RKjfjJyE35Chjbp2sYWH"
Quandl.api_key(TOKEN_QUANDL)

# Hasta qué fecha fin quiero los datos
START_DATE <- ymd_hms(paste0("2013-12-01", " 00:00:00" ), tz = "Europe/Madrid")
END_DATE <- ymd_hms(paste0(Sys.Date(), " 00:00:00" ), tz = "Europe/Madrid")


# Definición del ID del indicador
# https://www.quandl.com/tools/r
#https://help.quandl.com/article/200-how-do-i-convert-a-daily-time-series-to-a-monthly-download-in-r
#Emissions Allowances Prices (EUA)
indicator <- "CHRIS/ICE_C1"
df_co2 <- Quandl(indicator, start_date=START_DATE, end_date=END_DATE, collapse="monthly", type = "ts")
df_co2 <- df_co2[,"Settle"]
autoplot(df_co2) + ylab("EUR/t") + ggtitle("Emissions Allowances Prices (EUA)") + theme_bw()

# Coal Prices (API2)
indicator <- "CHRIS/CME_MTF2.6"
df_coal <- Quandl(indicator, start_date=START_DATE, end_date=END_DATE, collapse = "monthly", type = "ts")
autoplot(df_coal)+ylab("USD") + ylab("USD/t") + ggtitle("Coal Prices (API2)") + theme_bw()

# Natural Gas Prices (TTF)
indicator <- "CHRIS/ICE_TFM1.4"
df_ng <- Quandl(indicator, start_date=START_DATE, end_date=END_DATE, collapse="monthly", type = "ts")
autoplot(df_ng) + ylab("EUR/MWh") + ggtitle("Natural Gas Prices (TTF)") + theme_bw()

# Reservoir water
#https://www.epdata.es/evolucion-agua-embalsada-espana/a4f8c28d-52a6-43f7-9ad1-d769e23116d0
water <- read.csv("data/evolucion_del_agua_embals.csv", sep=";", dec=",")
water <- water[water$Año>=2014,]
water <- water[-((nrow(water)-6):nrow(water)),]
water[(nrow(water)-3):nrow(water), "Año"]=2020
water[(nrow(water)-3):nrow(water), "Agua.embalsada"]=27.266
water[(nrow(water)-2):nrow(water), "Periodo"]<-c("Semana 51", "Semana 52", "Semana 53")
water$tz_time <- seq.Date(as.Date("2014-01-01"), as.Date("2021-01-01"), by = "week")
water <- water %>% mutate(day = day(tz_time))
water <- water %>% mutate(month = month(tz_time))
water <- water %>% mutate(year = year(tz_time))
water <- water %>% mutate(day = day(tz_time))
water <- water %>% mutate(hour = hour(tz_time))
water$Agua.embalsada<-as.numeric(water$Agua.embalsada)
water_month <- water %>% group_by(month,year) %>% summarise(mean=mean(Agua.embalsada)) %>% arrange(year)
water_month <- ts(water_month, start = c(2014,1), frequency = 52) 
autoplot(water_month[,"mean"]) + ylab("hm3") + ggtitle("Reservoir water") + theme_bw()


collapse(water)
water$y_m <- seq.Date(as.Date("2014-01-01"), as.Date("2021-01-01"), by = "week")




# Esios data
indicators_df<-read.csv("data/indicators_df.csv", sep=",")
# 'Precio mercado SPOT Diario' 1212
# 'Generación programada P48 Exportación Total': 10238
# 'Generación programada P48 Importación Total': 10237
# 'Generación programada P48 Cogeneración': 10011
# 'Generación programada P48 Eólica': 10010
# 'Generación programada P48 Carbón': 10008
# 'Generación programada P48 Consumo bombeo': 95
# 'Generación programada P48 Gas Natural Cogeneración': 87
# 'Generación programada P48 Biomasa': 91
# 'Generación programada P48 Solar térmica': 85
# 'Generación programada P48 Solar fotovoltaica': 84
# 'Generación programada P48 Ciclo combinado': 79
# 'Generación programada P48 Turbinación bombeo': 73
# 'Generación programada P48 Nuclear': 74
# 'Generación programada P48 Hidráulica no UGH': 72
# 'Generación programada P48 Hidráulica UGH': 71
# 'Generación programada P48 UGH + no UGH': 10063
# 'Generación programada PBF total': 10258
# 'Demanda real': 1293
# 'Demanda programada': 545
# 'Demanda prevista': 544




# Token
httpheader <- 'Authorization: Token token="3fb180be75bca1213cce6c5aed47f63e90d9262f9636b5e9da14c9c56e4ba584"'

# Hasta qué fecha fin quiero los datos
hoy <- ymd_hms(paste0(Sys.Date(), " 00:00:00" ), tz = "Europe/Madrid")
indicator=c(600, 10238, 10237, 10011, 10010, 10008, 95, 87, 91, 85, 84, 79, 73, 74, 72, 71, 10063, 10258, 1293, 545, 544)
file_names=c('spot_price', 'GP48_export', 'GP48_import','GP48_cogen','GP48_wind','GP48_coal','GP48_pumping_con','GP48_cogen_ng','GP48_biomass','GP48_thermosolar','GP48_PV','GP48_CC','GP48_pumping_tur','GP48_nuclear','GP48_hydro_noUGH','GP48_hydro_UGH','GP48_hydro_total','GP48_total','Demand_actual','Demand_scheduled','Demand_forecasted')

for (i in 7:7){
  hoy <- ymd_hms(paste0(Sys.Date(), " 00:00:00"), tz = "Europe/Madrid")
  uri <- paste0("https://api.esios.ree.es/indicators/", indicator[i], "?start_date=", "01-01-2014", "T", "00:00:00+02:00&end_date=", as.Date(hoy), "T", "23:50:00+02:00&geo_agg=sum&geo_ids&time_trunc=hour&time_agg=&locale=es")
  extraccion <- getURI(uri, httpheader = httpheader)
  extraccion <- fromJSON(txt=extraccion)
  extraccion <- extraccion[["indicator"]][["values"]]
  extraccion$tz_time <- ymd_hms(extraccion$tz_time, tz = "Europe/Madrid", locale = Sys.getlocale("LC_TIME"))
  extraccion<-as.data.frame(extraccion)
  extraccion$geo_ids<-NULL
  write.csv(extraccion, paste0("data/", file_names[i], ".csv"))
}
#61584 61344 60154 61584 61584 61383 35254 61584 61584 55045 44814 61584 37668 61584 61584 61584 61584 61584 61584 61584 61584  c(1,2,3,6,7,10,11,13)
l=rep(1,23)
for (i in 1:length(indicator)) {
  df_aux <- read.csv(paste0("data/", file_names[i], ".csv"))
  if(i==1){
    df_esios <- df_aux
    df_esios$tz_time<- as.POSIXct(df_esios$tz_time)
    df_esios <- df_esios %>% mutate(day = day(tz_time))
    df_esios <- df_esios %>% mutate(month = month(tz_time))
    df_esios <- df_esios %>% mutate(year = year(tz_time))
    df_esios <- df_esios %>% mutate(day = day(tz_time))
    df_esios <- df_esios %>% mutate(hour = hour(tz_time))
    df_esios_month <- df_esios %>% group_by(month,year) %>% summarise(mean=mean(value)) %>% arrange(year)
    #df_esios_month <- ts(df_esios_month, start = c(2014,1), frequency = 12) 
    df_esios_month$tz_time <- seq.Date(as.Date("2014-01-01"), as.Date("2021-01-01"), by = "month")
  }else{
    df_aux<-df_aux[,c("value","tz_time")]
    df_aux$tz_time<- as.POSIXct(df_aux$tz_time)
    df_aux <- df_aux %>% mutate(day = day(tz_time))
    df_aux <- df_aux %>% mutate(month = month(tz_time))
    df_aux <- df_aux %>% mutate(year = year(tz_time))
    df_aux <- df_aux %>% mutate(day = day(tz_time))
    df_aux <- df_aux %>% mutate(hour = hour(tz_time))
    df_aux_month <- df_aux %>% group_by(month,year) %>% summarise(mean=mean(value)) %>% arrange(year)
    #df_aux_month <- ts(df_aux_month, start = c(2014,1), frequency = 12) 
    df_aux_month$tz_time <- seq.Date(as.Date("2014-01-01"), as.Date("2021-01-01"), by = "month")
    df_aux_month<-df_aux_month[,c("tz_time","mean")]
    df_esios_month <- merge(df_esios_month, df_aux_month, by="tz_time")
  }
}

colnames(df_esios_month)<-c("tz_time", "month", "year", file_names)
write.csv(df_esios_month, "data/df_esios_month.csv")
df_esios_month<-read.csv("data/df_esios_month.csv")
#tz_time<- as.POSIXct(df_esios_month$tz_time)
df_esios_month <-ts(df_esios_month, start = c(2014,1), frequency = 12) 
#df_esios_month$tz_time<-tz_time
autoplot(df_esios_month)

# Predictors
df_esios_month<-as_data_frame(df_esios_month)

water_month<-as_data_frame(water_month)$mean
df<-data_frame(df_esios_month,
           co2=df_co2[2:length(df_co2)],
           coal=c(df_coal[84], df_coal),
           ng=df_ng[2:length(df_co2)],
           water_res=c(water_month,water_month[length(water_month)]))
df$tz_time <- seq.Date(as.Date("2014-01-01"), as.Date("2021-01-01"), by = "month")
write.csv(df, "data/df.csv")

df<-read.csv("data/df.csv")
df[,c("X", "X.1", "tz_time", "year")]<-NULL
df<-df[1:(nrow(df)-1),]
colnames(df)
features=c('month', 'year','spot_price', 'GP48_export', 'GP48_import','GP48_cogen','GP48_wind','GP48_coal','GP48_pumping_con','GP48_cogen_ng','GP48_biomass','GP48_thermosolar','GP48_PV','GP48_CC','GP48_pumping_tur','GP48_nuclear','GP48_hydro_noUGH','GP48_hydro_UGH','GP48_hydro_total','GP48_total','Demand_actual','Demand_scheduled','Demand_forecasted', 'co2', 'coal', 'ng', 'water_res')

#df$tz_time <- seq.Date(as.Date("2014-01-01"), as.Date("2021-01-01"), by = "month")
df_ts<-ts(df, start = c(2014,1), frequency = 12) 
autoplot(df_ts) + theme_bw()




#MACHINE LEARNING


# Split data into traindf and testdf
set.seed(0)


# Partition methodology

split_df_ts <- ts_split(ts.obj = df_ts, sample.out = 12)
TrainingSet <- split_df_ts$train
TestingSet <- split_df_ts$test

# Linear Regression
Model<-lm(spot_price~., data=TrainingSet )
summary(Model)


#Model evaluation test (prediction)
#Predicted values with the test set
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set

#Scatter plot of Training set - Model performance (Displays scatter plot and performance metrics)

plot(as.data.frame(TrainingSet)$spot_price, Model.training, col = "blue",xlim=c(20,70), ylim=c(20,70))
abline(a=0,b=1, col="red")
plot(as.data.frame(TestingSet)$spot_price, Model.testing, col = "blue", xlim=c(20,70), ylim=c(20,70))
abline(a=0,b=1, col="red")




# Pearson's correlation coefficient
R.training <- cor(as.data.frame(TrainingSet)$spot_price, Model.training, method = "pearson")
R.testing <- cor(as.data.frame(TestingSet)$spot_price, Model.testing, method = "pearson")

# R2 es el cuadrado del coeficiente de correlación de Pearson 
R.training^2
R.testing^2

# RMSE
sqrt(mean((as.data.frame(TestingSet)$spot_price-Model.testing)^2))

# MAE
mean(abs(as.data.frame(TestingSet)$spot_price-Model.testing))


# XGBOOST
# TrainingSet_matrix <- 
#   as.data.frame(TrainingSet) %>% 
#   select(-spot_price) %>% 
#   as.matrix() %>% 
#   xgb.DMatrix(data = ., )
# 
# TestingSet_matrix <- 
#   as.data.frame(TestingSet) %>% 
#   select(-spot_price) %>% 
#   as.matrix() %>% 
#   xgb.DMatrix(data = .,)

params <- list( set.seed=1,
                booster = "gbtree",
                eval_metric = "error",
                objective ="reg:linear")


x_TrainingSet<-as_data_frame(TrainingSet) %>% select(-spot_price)
y_TrainingSet <-as_data_frame(TrainingSet)  %>% select(spot_price)
x_TestingSet<-as_data_frame(TestingSet) %>% select(-spot_price)
y_TestingSet <-as_data_frame(TestingSet)  %>% select(spot_price)
Model <-xgboost(data = as.matrix(x_TrainingSet), 
                label = y_TrainingSet$spot_price,
                params = params,
                nrounds=20,
                verbose = 1)
                
xgb.plot.shap(data =as.matrix(x), model=Model, top_n =5)      
importance <- xgb.importance(feature_names = colnames(as.matrix(x)), model = Model)
xgb.ggplot.importance(importance_matrix = importance)


Model.training<-predict(Model, as.matrix(x_TrainingSet))
Model.testing<-predict(Model, as.matrix(x_TestingSet))

#Scatter plot of Training set - Model performance (Displays scatter plot and performance metrics)

plot(as.data.frame(TrainingSet)$spot_price, Model.training, col = "blue",xlim=c(20,70), ylim=c(20,70))
abline(a=0,b=1, col="red")
plot(as.data.frame(TestingSet)$spot_price, Model.testing, col = "blue", xlim=c(20,70), ylim=c(20,70))
abline(a=0,b=1, col="red")




# Pearson's correlation coefficient
R.training <- cor(as.data.frame(TrainingSet)$spot_price, Model.training, method = "pearson")
R.testing <- cor(as.data.frame(TestingSet)$spot_price, Model.testing, method = "pearson")

# R2 es el cuadrado del coeficiente de correlación de Pearson 
R.training^2
R.testing^2

# RMSE
sqrt(mean((as.data.frame(TestingSet)$spot_price-Model.testing)^2))

# MAE
mean(abs(as.data.frame(TestingSet)$spot_price-Model.testing))








cbind(df_co2)
df_co2
df_coal
df_ng
water_month
df_esios_month

# Definición del ID del indicador
indicador <- 600

# Token
httpheader <- 'Authorization: Token token="3fb180be75bca1213cce6c5aed47f63e90d9262f9636b5e9da14c9c56e4ba584"'
# Hasta qué fecha fin quiero los datos

uri <- paste0("	http://datos.gob.es/apidata/catalog/dataset/", indicador, "?start_date=", "01-01-2014", "T", "00:00:00+02:00&end_date=", as.Date(hoy), "T", "23:50:00+02:00&geo_agg=sum&geo_ids&time_trunc=hour&time_agg=&locale=es")
myfile <- getURL('https://www.epdata.es/evolucion-agua-embalsada-espana/a4f8c28d-52a6-43f7-9ad1-d769e23116d0/DataDownload.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
mydat <- read.csv(textConnection(myfile), header=T)

CHRIS/ICE_C1-ECX-EUA-Futures-Continuous-Contract-1-C1-Front-Month
# Extracción del JSON
extraccion <- getURI(uri, httpheader = httpheader)
extraccion <- fromJSON(txt=extraccion)
Quandl()
# Limpieza de la tabla
extraccion <- extraccion[["indicator"]][["values"]]












# ACF and PACF
acf(price)
pacf(price)

acf(price_diff)
pacf(price_diff)

# ARIMA(1,0,0) or AR(1) (p,d,q)
arima(price, order = c(1,0,0))

# ARIMA(2,0,0) or AR(2)
arima(price, order = c(2,0,0))

# ARIMA(0,0,1) or MA(1)
arima(price, order = c(0,0,1))

# ARIMA(1,0,1) or AR(1) MA(1)
arima(price, order = c(1,0,1))

# ARIMA on differenced variable 
# ARIMA(1,1,0)
arima(price_diff, order = c(1,0,0))

# ARIMA(0,1,1)
arima(price_diff, order = c(0,0,1))

# ARIMA(1,1,1)
arima(price_diff, order = c(1,0,1))

# ARIMA(1,1,3)
arima(price_diff, order = c(1,0,3))

# ARIMA(2,1,3)
arima(price_diff, order = c(2,0,3))


# ARIMA(1,0,1) forecasting
mydata.arima101 <- arima(price, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima101, n.ahead=10)
plot.ts(price)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")

# ARIMA(1,1,1) forecasting
mydata.arima111 <- arima(d.Y, order = c(1,0,1))
mydata.pred1 <- predict(mydata.arima111, n.ahead=100)
plot.ts(d.Y)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")

