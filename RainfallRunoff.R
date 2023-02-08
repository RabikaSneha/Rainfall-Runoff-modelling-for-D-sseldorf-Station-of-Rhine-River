library(tidyverse)
library(ggplot2)

#Loading the data
AllData<- read.csv("C:/Users/ssrrs/Desktop/New folder/Data.txt", sep = "\t")
RF<- AllData[ -c(1,2,3) ] # deleting columns


# Checking the format, it is not a date yet !
str(RF)


library(lubridate)
# The wanna-be-date column looks like ymd_hms
RF$Date <- ymd_hms(RF$Date)

# Checking the date format
str(RF)

#---------------------------------------------------------------------------

# Assign the variables you want to plot to separate objects
var1 <- RF$Discharge

RF$Precipitation <- as.numeric(gsub("[^[:digit:]\\.]", "", RF$Precipitation))
var2 <- RF$Precipitation

RF$Evaporation <- as.numeric(gsub("[^[:digit:]\\.]", "", RF$Evaporation))
var3 <- RF$Evaporation

RF$Temperature <- as.numeric(gsub("[^[:digit:]\\.]", "", RF$Temperature))
var4 <- RF$Temperature


library(lattice)

xyplot(var1 ~ RF$Date, type = "l", ylab = "Discharge (m^3/s)",xlab = "Date")
xyplot(var2 ~ RF$Date, type = "l", ylab = "Precipitation (mm)",xlab = "Date")
xyplot(var3 ~ RF$Date, type = "l", ylab = "Evaporation(hPa)",xlab = "Date")
xyplot(var4 ~ RF$Date, type = "l", ylab = "Temperature(grad C)",xlab = "Date")




library(gridExtra)

library(grid)
grid.arrange(
  xyplot(var1 ~ RF$Date, type = "l", ylab = "Discharge (m^3/s)",xlab = "Date"),
  xyplot(var2 ~ RF$Date, type = "l", ylab = "Precipitation (mm)",xlab = "Date"),
  xyplot(var3 ~ RF$Date, type = "l", ylab = "Evaporation(hPa)",xlab = "Date"),
  xyplot(var4 ~ RF$Date, type = "l", ylab = "Temperature(grad C)",xlab = "Date"),
  ncol = 2,
  nrow = 2,top=textGrob("Time Series Data Visualization",gp=gpar(fontsize=20,font=3)))


#----------------------------------------------------------------------


library(caret)


# Split data into a training and testing set
set.seed(1)
trainIndex <- which(year(RF$Date) >= 2014 & year(RF$Date) <= 2017)
trainData <- RF[trainIndex,-c(1) ]

testIndex <- which(year(RF$Date) >= 2018 & year(RF$Date) <= 2018)
testData <- RF[testIndex,-c(1) ]


# Define response and predictor variables
response <- "Discharge"
predictors <- c("Precipitation", "Evaporation","Temperature")


#---------------------------------------------------------------------------
#modeling
#lm
model_lm <- train(x = trainData[,predictors], y = trainData[,response], method = "lm")
summary(model_lm)



install.packages(c("rpart","rpart.plot"))
library(rpart)
library(rpart.plot)
install.packages("caret")
library(caret)
model_cart <- train(x = trainData[,predictors], y = trainData[,response], method = "rpart",trControl = trainControl("cv", number = 10),
                    tuneLength = 10)

plot(model_cart)
# maximizes the model accuracy
model_cart$bestTune
# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
model_cart$finalModel
text(model_cart$finalModel,  digits = 4,cex=1, col = "blue")
plot(model_cart$finalModel)
rpart.plot(model_cart$finalModel)


summary(model_cart)


#Random Forest
install.packages(c("gbm","randomForest"))
library(gbm)
library(randomForest)
model_rf<-randomForest(Discharge~.,trainData, ntree = 500,mtry=1,min.node.size = 20,
                       tuneGrid = t_grid)
summary(model_rf)
plot(model_rf)


#Gradient boosting machine (gbm)

model_gbm<-gbm::gbm(trainData$Discharge ~.,
                    data = trainData,distribution = "gaussian",
                    n.trees = 400)
summary(model_gbm)
plot(model_gbm)

#---------------------------------------------------------------------------
#performance evaluation
pred_lm <- predict(model_lm, newdata = testData[, c(response, predictors)])
pred_cart <- predict(model_cart, newdata = testData[, c(response, predictors)])
pred_rf <- predict(model_rf, newdata = testData[, c(response, predictors)])
pred_gbm <- predict(model_gbm, newdata = testData[, c(response, predictors)])

ggplot(testData, aes(x = Discharge)) + 
  geom_point(aes(y = pred_lm)) + 
  geom_abline(slope =1, intercept = 1, color = "red") + 
  ggtitle("Actual vs Predicted Discharge (Linear Model)") + 
  xlab("Actual Discharge") + 
  ylab("Predicted Discharge")


mae_lm <- mean(abs(pred_lm - testData$Discharge))
mae_cart <- mean(abs(pred_cart - testData$Discharge))
mae_rf <- mean(abs(pred_rf - testData$Discharge))
mae_gbm <- mean(abs(pred_gbm - testData$Discharge))

#R2:observation vs prediction

r2_lm <- cor(pred_lm,testData$Discharge)^2

r2_cart <- cor(pred_cart,testData$Discharge)^2

r2_rf <- cor(pred_rf,testData$Discharge)^2

r2_gbm <- cor(pred_gbm,testData$Discharge)^2


#variable importance (feature importance)
install.packages("vip")
library(vip)

#model specific
vip(model_lm)

vip(model_cart)

vip(model_rf)

vip(model_gbm)

#model agnostic(generic)
fit_lm <- lm(Discharge ~ ., data = trainData)
vip_lm <- vip(fit_lm, num_features = ncol(trainData)-1)

fit_cart <- randomForest(Discharge ~ ., data = trainData)
vip_cart <- vip(fit_cart)

pvip_lm <- vip(fit_lm, method="permute", target="Discharge", metric="rsquared", pred_wrapper=predict)+ labs(title="LM")
pvip_cart <- vip(fit_cart, method="permute", target="Discharge", metric="rsquared", pred_wrapper=predict)+ labs(title="CART")

pvip_rf<-vip(model_rf,method="permute",
                       target="Discharge",metric="rsquared",pred_wrapper=predict)+labs(title="RF")

pvip_gbm<-vip(model_gbm,method="permute",
              target="Discharge",metric="rsquared",pred_wrapper=predict)+labs(title="GBM")


#patchwork
library(patchwork)

plot_pvip_all<-pvip_lm+pvip_cart+pvip_rf+pvip_gbm


plot_pvip_all

#----------------------------------------------------------------------------
#partial dependence plot (pdp or PD plot)
install.packages("pdp")
library(pdp)
library(dplyr)
library(ggplot2)
model_lm%>% partial (pred.var="Evaporation")%>%autoplot()
model_cart%>% partial (pred.var="Evaporation")%>%autoplot()
model_rf%>% partial(pred.var="Evaporation")%>%autoplot()
model_gbm%>% partial(pred.var="Temperature",n.trees = 400)%>%autoplot()

pdp_lm<-model_lm%>%partial(pred.var=c("Temperature","Evaporation"))%>%autoplot()+labs(title="LM")
pdp_cart<-model_cart%>%partial(pred.var=c("Evaporation","Temperature"))%>%autoplot()+labs(title="CART")
pdp_rf<-model_rf%>%partial(pred.var=c("Evaporation","Temperature"))%>%autoplot()+labs(title="RF")
pdp_gbm<-model_gbm%>%partial(pred.var=c("Temperature","Evaporation"),n.trees = 400)%>%autoplot()+labs(title="GBM")

plot_pdp_all<-pdp_lm+pdp_cart+pdp_rf+pdp_gbm

plot_pdp_all



#----------------------------------------------------------------------------
#Final summary
ml_runoff<- AllData[ -c(1,2,3,4) ]
plot_runoff_hist<-ml_runoff%>%ggplot(aes(Discharge))+geom_histogram()
plot_Temperature_Discharge<-ml_runoff%>%ggplot(aes(Temperature,Discharge))+geom_point()+geom_smooth(method="lm")
plot_Precipitation_Discharge<-ml_runoff%>%ggplot(aes(Precipitation,Discharge))+geom_point()+geom_smooth(method="lm")
plot_Evaporation_Discharge<-ml_runoff%>%ggplot(aes(Evaporation,Discharge))+geom_point()+geom_smooth(method="lm")
 

Final_plot<-(plot_runoff_hist/plot_Temperature_Discharge/plot_Evaporation_Discharge)|plot_pvip_all|plot_pdp_all
Final_plot

plot_Temperature_Discharge
plot_Evaporation_Discharge
plot_Precipitation_Discharge
