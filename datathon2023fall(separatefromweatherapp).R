library(readxl)



#base code
packages_to_install<-c("forecast","ggplot2","gridExtra","readr","kableExtra")
install.packages(packages_to_install)
library(forecast)
library(ggplot2)
library(gridExtra)
library(readr)
library(kableExtra)
library(readxl)

#reading given data and sea level data
tempdata <- read.csv("/Users/k/Downloads/RU 23-24/Datathon_Fall2023_Dataset.csv", stringsAsFactors=F,header=T)
str(tempdata)
tempdatanorep <- unique(tempdata$Year)

co2data <- read_excel("/Users/k/Downloads/statistic_co2emission.xlsx")

#Convert the data into a time series
data = ts(tempdata$Anomaly,start=1850,frequency=12)
head(data)

#Plot the given data to visualize it.
autoplot(data,col='indianred4')+
  theme_bw(base_size=14)+
  ggtitle('GlobalLandandOceanTemperatureanomalies(1901-2000means0)')+
  xlab('')+ylab('Temperature(Celsius)')

#Create train and test datasets. Then create a forecast model on the train data
h=36
data_train = window(data,end=c(2019,08))
data_test = window(data,start=c(2019,09))
data_stl12 = stlf(data_train,method='naive',h=h)

#Obtain the actual forecasted values
forecast_final=(data_stl12$mean)
forecast_final
#cross-validation
#definethedesiredSTLmodelasafunction(includingallparameters),thensendfulldata(NOTjusttrainingdata),h,andwindowtoitusingtsCV()
#IfyouendupnotusingstlyouwillstillhavetodoaversionofthistogettheCVerror.
h=36
window=36
fstl=function(x,h){forecast(stlf(x,h=h))}
error=tsCV(data,fstl,h=h,window=window)

#FUNCTIONcv_mape #Returnscross-validationmeanabsolutepercenterror
cv_mape = function(error, actual){
  actual_table = data.frame(matrix(NA, nrow = length(actual), ncol =h))
  for(i in 1:(length(actual)-window)){ if((i+window+h-1)<=length(actual)){actual_table[i+window-1,]=actual[(i+window):(i+window+h-1)]}
    else{actual_table[i+window-1,1:(length(actual)-(i+window-1))]=actual[(i+window):(length(actual))]}
  }
  return(100*mean(abs(as.matrix(error)/as.matrix(actual_table)),na.rm=T))
}

#Performancetabledataframe
#'rmse'=RootMeanSquareError

  
  perf_stl=data.frame(rbind(
    
    cbind('rmse', 
          formatC(round(accuracy(data_stl12)[,'RMSE'],5),format='f',digits=5), 
          formatC(round(sqrt(mean((data_test-data_stl12$mean)^2)),5),format='f',digits=5), 
          formatC(round(sqrt(mean(error^2,na.rm=T)),5),format='f',digits=5)),
    
    #'mae'=MeanAbsoluteError
    
    cbind('mae', 
          formatC(round(accuracy(data_stl12)[,'MAE'],5),format='f',digits=5), 
          formatC(round(mean(abs(data_test-data_stl12$mean)),5),format='f',digits=5), 
          formatC(round(mean(abs(error),na.rm=T),5),format='f',digits=5)),
    #'mape'=MeanAbsolutePercentError
    cbind('mape', 
          formatC(round(accuracy(data_stl12)[,'MAPE'],5),format='f',digits=5), 
          formatC(round(mean(100*(abs(data_test-data_stl12$mean))/data_test),5),format='f',digits=5), 
          formatC(round(cv_mape(error,data),5),format='f',digits=5))),
    stringsAsFactors=F)
  #Codeforturningthedataframeaboveintoaniceplotusingkable.
  kable(perf_stl,caption='Performance-TemperatureAnomalieshorizon=12,window=36',align='r',col.names=c('','train','test','cv'))%>%
    kable_styling(full_width=F,position='l')%>% 
    column_spec(2,width='7em')%>%
    column_spec(3,width='4.5em')%>% 
    column_spec(4,width='4.5em')
  
###############################################################

# getting data
temp_data_filtered <- subset(tempdata, Year >= 195900 & Year <= 202200)
indtempdata <- unique(temp_data_filtered)

co2_data_filtered <- subset(co2data,"Years 1959 to 2022"  >= 1959 & "Years 1959 to 2022" <= 2022)


# yearly global surface temperature
mean_temp <- aggregate(Anomaly ~ Year, data = indtempdata, FUN = mean)

# correlation between CO2 emissions and mean temperature
correlation <- cor(meantempnorep$Anomaly, co2data$ppm)


# correlation ?????
ggplot(data = temp_data_filtered, aes(x = co2_data_filtered$Value, y = mean_temp$Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "CO2 Emissions", y = "Mean Global Surface Temperature Anomalies") +
  geom_text(aes(label = paste("Correlation =", round(correlation, 2))),
            x = max(co2_data_filtered$Value), y = max(mean_temp$Value),
            hjust = 1, vjust = -1)

#co2 time series
co2_ts <- ts(co2_data$Value, start = 1959, frequency = 1)

# Forecast CO2 levels from 2022 to 2100
co2_forecast <- forecast(auto.arima(co2_ts), h = 78 )  

#forecasted CO2 levels
plot(co2_forecast, main = "CO2 Levels Forecast (2022 to 2100)")

# merged dataset with temperature and CO2 data
merged_data <- merge(temp_data_filtered, co2_data_filtered, by = "Year")

#used chatgpt for this so like
# Filter the data for the desired years (1959-2022)
temp_data_filtered <- subset(temp_data, Year >= 1959 & Year <= 2022)
co2_data_filtered <- subset(co2data, Year >= 1959 & Year <= 2022)

# Create a merged dataset with temperature and CO2 data
merged_data <- merge(temp_data_filtered, co2_data_filtered, by = "Year")

# Create a GLM model to predict temperature anomalies based on CO2 emissions
model <- glm(Value.x ~ Value.y, data = merged_data)

# Extend the CO2 data to forecast until 2100 using a linear trend (adjust as needed)
co2_forecast_data <- data.frame(
  Year = seq(1959, 2100, by = 1),  # Extend to 2100
  Value = predict(model, newdata = data.frame(Value.y = seq(1959, 2100, by = 1)))
)

# Forecast CO2 emissions until 2100 using a simple linear trend
co2_forecast <- ts(co2_forecast_data$Value, start = min(co2_data_filtered$Year), frequency = 1)

# Forecast temperature anomalies based on the CO2 forecast (adjust as needed)
temp_forecast_data <- data.frame(
  Year = seq(1959, 2100, by = 1),  # Extend to 2100
  Value = predict(model, newdata = data.frame(Value.y = co2_forecast_data$Value))
)

# Forecast temperature anomalies until 2100 using the GLM model
temp_forecast <- ts(temp_forecast_data$Value, start = min(temp_data_filtered$Year), frequency = 1)

# Summary of the CO2 forecast data (up to 2100)
summary(co2_forecast_data)

# Summary of the temperature forecast data (up to 2100)
summary(temp_forecast_data)

# Plot the relationship and regression line (including forecasts until 2100)
ggplot(merged_data, aes(x = Value.y, y = Value.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_line(data = temp_forecast_data, aes(x = co2_forecast_data$Value, y = temp_forecast_data$Value), color = "red") +
  labs(x = "CO2 Emissions", y = "Mean Global Surface Temperature Anomalies")

# Create a GLM model to predict temperature anomalies based on CO2 emissions
model <- glm(Value.x ~ Value.y, data = merged_data)

# Summary of the GLM model
summary(model)

# Extract the coefficients
coefficients <- coef(model)

# Calculate the correlation between CO2 emissions and temperature anomalies
correlation <- cor(merged_data$Value.x, merged_data$Value.y)

# Plot the relationship and regression line
ggplot(merged_data, aes(x = Value.y, y = Value.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "CO2 Emissions", y = "Mean Global Surface Temperature Anomalies") +
  geom_text(aes(label = paste("Correlation =", round(correlation, 2), "\n",
                              "Intercept =", round(coefficients[1], 2), "\n",
                              "CO2 Coefficient =", round(coefficients[2], 2))),
            x = max(merged_data$Value.y), y = max(merged_data$Value.x),
            hjust = 1, vjust = -1)
            