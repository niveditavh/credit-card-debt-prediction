Credit_debt <- read.csv("E:/Stats_Practice/test/Credit_nive.csv")
pairs(Credit_debt)

install.packages("psych")
library(psych)
pairs.panels(Credit_debt, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs(Credit_debt,panel=panel.smooth)
cor(Credit_debt)
model1 <- lm(log(creddebt)~income*debtinc+I(income^2)+I(debtinc^2),data=new_Credit)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
install.packages("car")
library(car)
ncvTest(new_Credit)
vif(model1)

model2<-lm(creddebt~income*debtinc*othdebt*employ+I(income^2)+I(debtinc^2)+I(othdebt^2)+I(employ^2),data=new_Credit)
summary(model2)

model3<-update(model2,~.-income:debtinc:othdebt:employ)
summary(model3)

model4<-update(model3,~.-debtinc:othdebt:employ)
summary(model4)

model5<-update(model4,~.-income:othdebt:employ)
summary(model5)

model6<-update(model5,~.-income:debtinc:employ)
summary(model6)

model7<-update(model6,~.-income:debtinc:othdebt)
summary(model7)

model8<-update(model7,~.-othdebt:employ)
summary(model8)

model9<-update(model8,~.-debtinc:employ)
summary(model9)

model10<-update(model9,~.-income:employ)
summary(model10)

model11<-update(model10,~.-debtinc:othdebt)
summary(model11)

model12<-update(model11,~.-income:othdebt)
summary(model12)

model13<-update(model12,~.-I(othdebt^2))
summary(model13)

model14<-update(model13,~.-I(employ^2))
summary(model14)

model15<-update(model14,~.-employ )
summary(model15)

model16<-update(model15,~.-I(income^2))
summary(model16)

model17<-update(model16,~.-debtinc)
summary(model17)
par(mfrow=c(2,2))
plot(model17)

model18<-step(model2)
summary(model18)
plot(model18)

modelq<- lm(log(creddebt)~income*debtinc*othdebt,data=new_Credit)
 
model19<-step(modelq)
summary(model19)
plot(model19)
par(mfrow=c(2,2))
vif(model19)
boxplot(new_Credit)

# 601 value lm(formula = log(creddebt) ~ income + debtinc + I(income^2) + income:debtinc, data = new_Credit)
#601 value lm(formula = log(creddebt) ~ income + debtinc + I(income^2), data = new_Credit)
#6372 log(creddebt) ~ income + debtinc + I(income^2) + income:debtinc +I(debtinc^2), data = new_Credit
#6377 lm(formula = log(creddebt) ~ income + debtinc + I(income^2) + I(debtinc^2), data = new_Credit)
#6584 log(creddebt) ~ sqrt(income) + sqrt(debtinc) +income:debtinc, data = new_Credit


model20<-lm(log(creddebt) ~ sqrt(income) + sqrt(debtinc) +income:debtinc, data = new_Credit)
summary(model20)
par(mfrow=c(2,2))
plot(model20)
library(car)
vif(model20)

install.packages("lm.beta")
library(lm.beta)
lm.beta(model20)
install.packages("performance")
install.packages("see")
install.packages("patchwork")
library(performance)
library(see)
library(patchwork)
#this will give color graph
check_model(model20)

#bestmodel
install.packages("haven")
install.packages("leaps")
library(haven)
new_Credit_1 <- read.csv("E:/Stats_Practice/test/Credit_nive.csv")
attach(new_Credit_1)

library(car)
library(leaps)
bestmodels<- regsubsets(creddebt~income+debtinc+othdebt+employ,data=new_Credit_1,nbest=2,method="backward")
summary(bestmodels)
par(mfrow=c(1,1))
plot(bestmodels,scale="adjr2")
plot(bestmodels,scale="bic")


#removing outliers
boxplot(new_Credit_1)
boxplot(new_Credit_1$income,new_Credit_1$debtinc,new_Credit_1$creddebt, plot=FALSE)$out
outliers <- boxplot(new_Credit_1$income,new_Credit_1$debtinc,new_Credit_1$creddebt, plot=FALSE)$out

x<-new_Credit_1
x<- x[-which(x %in% outliers),]
boxplot(x)

plot(x)



new_approach<-lm(log(creddebt)~employ+address+debtinc+income+I(address^2)+I(debtinc^2)+I(income^2)+employ:income,data=x)

Q1_income <- quantile(new_Credit$income, .25)
Q3_income <- quantile(new_Credit$income, .75)
IQR_income <- IQR(new_Credit$income)
Q1_debtinc <- quantile(new_Credit$debtinc, .25)
Q3_debtinc <- quantile(new_Credit$debtinc, .75)
IQR_debtinc <- IQR(new_Credit$debtinc)
Q1_creddebt <- quantile(new_Credit$creddebt, .25)
Q3_creddebt <- quantile(new_Credit$creddebt, .75)
IQR_creddebt <- IQR(new_Credit$creddebt)
Q1_othdebt <- quantile(new_Credit$othdebt, .25)
Q3_othdebt <- quantile(new_Credit$othdebt, .75)
IQR_othdebt <- IQR(new_Credit$othdebt)
names(new_Credit)[names(new_Credit) == "ï..age"] <- "age"
Creditdata_removed <- subset(new_Credit, new_Credit$income > (Q1_income - 1.5*IQR_income) & new_Credit$income < (Q3_income + 1.5*IQR_income)
                          & new_Credit$debtinc > (Q1_debtinc - 1.5*IQR_debtinc) & new_Credit$debtinc < (Q3_debtinc + 1.5*IQR_debtinc)
                          & new_Credit$creddebt > (Q1_creddebt - 1.5*IQR_creddebt) & new_Credit$creddebt < (Q3_creddebt + 1.5*IQR_creddebt)
                          & new_Credit$othdebt > (Q1_othdebt - 1.5*IQR_othdebt) & new_Credit$othdebt < (Q3_othdebt + 1.5*IQR_othdebt))
boxplot(Creditdata_removed)

model_1 <- lm(log(creddebt) ~ sqrt(income) + sqrt(debtinc), data = Creditdata_removed)
summary(model_1)
par(mfrow=c(2,2))
plot(model_1)

check_model(model_1)
pairs(Creditdata_removed)
pairs(Creditdata_removed,panel=panel.smooth)
model_2<-step(lm(log(creddebt)~income+debtinc+I(income^2)+I(debtinc^2),data=Creditdata_removed))
summary(model_2)
plot(model_2)
vif(model_2)

model_3<-lm(log(creddebt)~income+debtinc+income:debtinc,data=Creditdata_removed)
summary(model_3)
par(mfrow=c(2,2))
plot(model_3)
vif(model_3)


pairs(Creditdata_removed,panel=panel.smooth)
#0.5834 vif is fine ncv is fail durbin fail
tony<-lm(creddebt~income+debtinc+othdebt,data=Creditdata_removed)
summary(tony)
par(mfrow=c(2,2))
plot(tony)
library(car)
vif(tony)
ncvTest(tony)
durbinWatsonTest(tony)

#0.599 vif is fine ncv fine durbin fine
tony_2<-lm(log(creddebt)~income+debtinc+othdebt,data=Creditdata_removed)
summary(tony_2)
par(mfrow=c(2,2))
plot(tony_2)
vif(tony_2)
ncvTest(tony_2)
durbinWatsonTest(tony_2)

#0.4967 vif is fine ncv fail durbin is fine
tony_3<-lm(log(creddebt)~income+debtinc,data=Creditdata_removed)
summary(tony_3)
par(mfrow=c(2,2))
plot(tony_3)
vif(tony_3)
ncvTest(tony_3)
durbinWatsonTest(tony_3)

#0.6315 vif failing ncv fine durbin fail
tony_4<-lm(log(creddebt)~income+debtinc+othdebt+income:debtinc:othdebt,data=Creditdata_removed)
summary(tony_4)
par(mfrow=c(2,2))
plot(tony_4)
vif(tony_4)
ncvTest(tony_4)
durbinWatsonTest(tony_4)

#0.4984 vif interaction failing and ncv is fail  durbin is fine
tony_5<-lm(log(creddebt)~income+debtinc+income:debtinc,data=Creditdata_removed)
summary(tony_5)
par(mfrow=c(2,2))
plot(tony_5)
vif(tony_5)
ncvTest(tony_5)
durbinWatsonTest(tony_5)

#0.5118 vif not good ncv is fail durbin fine
tony_6<-lm(log(creddebt)~income+debtinc+I(income^2),data=Creditdata_removed)
summary(tony_6)
par(mfrow=c(2,2))
plot(tony_6)
vif(tony_6)
ncvTest(tony_6)
durbinWatsonTest(tony_6)

#0.5545 vif is not good ncv fail durbin is fine
tony_7<-lm(log(creddebt)~income+debtinc+I(income^2)+I(debtinc^2),data=Creditdata_removed)
summary(tony_7)
par(mfrow=c(2,2))
plot(tony_7)
vif(tony_7)
ncvTest(tony_7)
durbinWatsonTest(tony_7)

tony_8<-lm(log(creddebt)~income+debtinc+othdebt+employ,data=Creditdata_removed)
summary(tony_8)
par(mfrow=c(2,2))
plot(tony_8)
vif(tony_8)
ncvTest(tony_8)
durbinWatsonTest(tony_8)

tony_9<-step(lm(log(creddebt)~income+othdebt,data=Creditdata_removed))
summary(tony_9)
par(mfrow=c(2,2))
plot(tony_9)
vif(tony_9)
ncvTest(tony_9)
durbinWatsonTest(tony_9)

tony_15<-lm(log(creddebt)~income*debtinc*othdebt+I(income^2)+I(debtinc^2)+I(othdebt^2),data=Creditdata_removed)
tony_16<-step(tony_15)
summary(tony_16)
par(mfrow=c(2,2))
plot(tony_16)
vif(tony_16)
ncvTest(tony_16)
durbinWatsonTest(tony_16)




tony_20<-lm(log(creddebt)~debtinc+othdebt,data=Creditdata_removed)
summary(tony_20)
par(mfrow=c(2,2))
plot(tony_20)
vif(tony_20)
ncvTest(tony_20)
durbinWatsonTest(tony_20)

library(car)
library(leaps)
models<-regsubsets(creddebt~.,data=Creditdata_removed,nbest=2,method="backward")
summary(models)
plot(models,scale="adjr2")
plot(models,scale="bic")


g<-lm(log(creddebt)~income+debtinc+othdebt,data=Creditdata_removed)
summary(g)
par(mfrow=c(2,2))
plot(g)
library(car)
vif(g)
ncvTest(g)
durbinWatsonTest(g)
library(performance)
library(see)
library(patchwork)
#this will give color graph
check_model(g)

install.packages("ggplot2")
library(ggplot2)

#create histogram of residuals
ggplot(data = Creditdata_removed, aes(x = g$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')



auto <- read.csv("E:/Stats_Practice/Auto.csv")
pairs(auto)
d_subset = auto[,c("I")]
write.csv(d_subset, file="auto.csv")

auto_1<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=auto)
summary(auto_1)
par(mfrow=c(2,2))
plot(auto_1)

perfect<-lm(log(creddebt)~employ+address+log(debtinc)+log(othdebt),data=Credit_debt)
summary(perfect)
par(mfrow=c(2,2))
library(car)
plot(perfect)
vif(perfect)
ncvTest(perfect)
durbinWatsonTest(perfect)
cooks.distance(perfect)
library(performance)
library(see)
library(patchwork)
check_model(perfect)

model1<-lm(creddebt~.,data=Credit_debt)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
library(car)
vif(perfect1)
ncvTest(model1)
durbinWatsonTest(perfect1)
cooks.distance(perfect1)
library(performance)
library(see)
library(patchwork)
check_model(model1)

model3<-lm(log(creddebt)~log(debtinc)+othdebt+employ,data=Credit_debt)
summary(model3)$coefficients
par(mfrow=c(2,2))
plot(model3)
vif(model3)
ncvTest(model3)
durbinWatsonTest(model3)
library(performance)
library(see)
library(patchwork)
check_model(model3)

hist(Credit_debt$sqrt(debtinc))
hist(log(Credit1$creddebt))
str(Credit_debt)

summary(Credit_debt)
typeof(Credit_debt$debtinc)




set.seed(100)
sub <- sample(nrow(Credit_debt), floor(nrow(Credit_debt) * 0.7))
train <- Credit_debt[sub, ]
test <- Credit_debt[-sub, ]
head(train)
head(test)

train.glm <- lm(log(creddebt)~log(debtinc)+othdebt+employ,data=Credit_debt)
summary(train.glm)
str(log(Credit_debt$creddebt))

installed.packages("Stat2Data")
library(Stat2Data)
two <- lm(creddebt~ ., data = Credit_debt)
anova(two)

two.way<-(lm(creddebt~.,data=Credit_debt))
summary(two.way)

Credit_debt$ed <- as.factor(Credit_debt$ed)
summary(Credit_debt)