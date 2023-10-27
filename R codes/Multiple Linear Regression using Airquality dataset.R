#Use airquality dataset
# reg <- lm(Ozone ~ Solar + Wind + Temp.)

library(ggplot2)
data("airquality")
View(airquality)
aq <- airquality
summary(aq)
str(aq)

#Handling NA's
aq$Ozone[is.na(aq$Ozone)]<-mean(aq$Ozone, na.rm = T)
aq$Solar.R[is.na(aq$Solar.R)]<-mean(aq$Solar.R,na.rm = T)

#Normalization
nor <- function(x){(x-min(x)) / (max(x)-min(x)) }
aq_norm <- as.data.frame(lapply(aq[c(1,2,3,4)], nor))
aq[c(1,2,3,4)] <- aq_norm

attach(aq)

#model
MULTIREG <- lm(Ozone~Solar.R + Wind + Temp)
summary(MULTIREG)
attributes(MULTIREG)

#test data
checkozone = data.frame(Solar.R = 150, Wind = 9, Temp =80)

#Predict
result = predict(MULTIREG, checkozone)
result

##Visualization of model
plot(Ozone ~ MULTIREG$fit)
plot(MULTIREG$fit + MULTIREG$res ~ MULTIREG$fit)
abline(0,1,col=5, lwd=3) #0 is the intercept and 1 is the slope.
?abline
#fit - Fitted Values, also known as predicted Values, are the value that a 
#regression predicts for the dependent Variable(or response Variable) based on the
#Independent variables(or predictor variables) in the model.

#res - Residuals,  also known as errors, are the difference between the 
#observed values of the dependant variables and the corresponding predicted
#(fitted) values from the regression model.

