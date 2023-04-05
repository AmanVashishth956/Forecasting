library(lubridate)
library(tseries)
library(Metrics)
library(forecast)
library(tseries)
library(FinTS)
library(fGarch)
library(zoo)
library(rugarch)


print(getwd())
setwd("C:/Users/Ajay Kumar GGN/Box Sync/Working Folder/IIT/Semester 3/TSA/M21AIE208")
print(getwd())

yen_data <- read.csv("yen.csv")

cat("No of columns in the data",ncol(yen_data))
cat("No of datapoints",nrow(yen_data))

sapply(yen_data, class)

yen_data$Date <- dmy(yen_data$Date)
yen_data$Yen <- as.integer(yen_data$Yen)

sapply(yen_data, class)

plot(yen_data, type='l', xlab ='Year', ylab = 'INR YEN Exchange Rate')

# Extract the year and convert to numeric format
yen_data$year <- as.numeric(format(yen_data$Date, "%Y"))

train_set <- subset(yen_data, year== 2022,
                    select = c(Yen))
test_set <- subset(yen_data, Date >= as.Date('2023-01-01') & Date <= as.Date('2023-01-11'))

adf.test(ts(train_set))
kpss.test(ts(train_set))
Box.test(train_set, lag = 1, type = "Ljung-Box")

## ACF and PACF plots

acf(train_set, lag.max=100)
pacf(train_set, lag.max=100)

##ARIMA models and its variation based on ARIMA(p,d,q)

## Model 1: ARIMA(5,0,1)

model1 = arima(train_set, order = c(5,0,1))
tsdiag(model1)
summary(model1)


## Model 1: ARIMA(10,0,1)

model2 = arima(train_set, order = c(10,1,1))
tsdiag(model2)
summary(model2)

## forecast for 10 steps ahead

forecast = predict(model1, n.ahead = 8)
## Evaluation of model performance on Test set

rmse(test_set$Yen, forecast$pred)
mse(test_set$Yen, forecast$pred)
mape(test_set$Yen, forecast$pred)

#Debold Mariano test
dm.test(residuals(model1), residuals(model2), h=1)

################################ ARCH/GARCH model ##############################

## Test. for ARCH Effects

ArchTest(ts(train_set))

## Because the p-value is < 0.05, we reject the null hypothesis and conclude the presence of ARCH(1) effects.

## Estimating ARCH Models

arch_model <- garchFit(~garch(5,0), data = train_set, trace = F)
summary(arch_model)
plot.zoo(fitted(arch_model))
predict(arch_model, n.ahead = 8, plot=TRUE, crit_val = 2)




## fGARCH 

tGarch_spec <- ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(5,1), submodel="TGARCH"), mean.model=list(armaOrder=c(0,0)), distribution.model="std")
tGarch_model <- ugarchfit(spec = tGarch_spec, data = ts(train_set))
tGarch_model
coef(tGarch_model)

## sGARCH

sGarch_Spec <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(5,1)), mean.model=list(armaOrder=c(0,0)), distribution.model="std")
sGarch_model <- ugarchfit(spec = sGarch_Spec, data = ts(train_set))
sGarch_model
coef(sGarch_model)




