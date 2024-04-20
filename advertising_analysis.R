#########################Advertising Dataset#######################
#(a) Upload the data "Advertising.csv" and explore it
advertise<-read.csv(file.choose())
View(advertise)
attach(advertise)
dim(advertise)
colnames(advertise)
str(advertise)
pairs(advertise, panel=panel.smooth)
#(b) Find the Covariance and Correlation Matrix of Sales, TV, Radio and Newspaper.
cor(advertise) #correlation 
cov(advertise) #covariance
#MODEL
model1<-lm(Sales~., advertise)
summary(model1) #Sales = 2.938889  + 0.045765*TV + 0.188530*Radio ??? 0.001037*Newspaper
model2<-lm(Sales~TV+Radio)
summary(model2)
predict(model2)
resid(model2)
plot(predict(model2),resid(model2), xlab = "Fitted Values", ylab = "Residuals")
hist(resid(model2), main = paste("Histogram of Residuals"), xlab = "Residuals")
par(mfrow=c(2,2))
plot(model2) 
predict(model2, as.data.frame(cbind(TV=50, Radio = 50)))
model3=lm(Sales~TV+Radio+TV*Radio)
summary(model3)
poly_model=lm(Sales~TV+I(TV*TV)+I(TV*TV*TV))
summary(poly_model) # R-sq value is 0.622


######################Auto Dataset###############################

#########################Advertising Dataset#######################
#(a) Upload the data "auto.csv" and explore it
auto<-read.csv(file.choose())
View(auto)
attach(auto)
dim(auto)
colnames(auto)
str(auto)
sum(is.na(auto))
auto<-auto[-9] #deleting  name col. as their are many car names which serve no purpose
#CONVERTING CHAR TO NUMERIC
auto[sapply(auto, is.character)] <- lapply(auto[sapply(auto, is.character)], as.factor)
auto[sapply(auto, is.factor)] <- lapply(auto[sapply(auto, is.factor)], as.numeric)
str(auto)

pairs(auto, panel=panel.smooth)
#(b) Find the Covariance and Correlation
cor(auto) #correlation 
cov(auto) #covariance
#MODEL
model1<-lm(mpg~., auto)
summary(model1) #mpg = -2.128e+01-2.927e-01*cylinders+1.603e-02*displacement+7.942e-03*horsepower -6.870e-03*weight+1.539e-01*acceleration+7.734e-01*year+1.346e+00*origin
model2<-lm(mpg~year+weight+origin+acceleration+displacement)
summary(model2)
predict(model2)
resid(model2)
plot(predict(model2),resid(model2), xlab = "Fitted Values", ylab = "Residuals")
hist(resid(model2), main = paste("Histogram of Residuals"), xlab = "Residuals")
par(mfrow=c(2,2))
plot(model2) 
model3<-lm(mpg~year+weight+year*weight)
summary(model3)
poly_model<-lm(mpg~year+I(year*year)+I(year*year*year))
summary(poly_model) # R-sq value is 0.3706
